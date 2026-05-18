"""3D Multi-Layer Icosahedral Atmosphere
=======================================

Each cell stores: T (temperature), wind (3D tangent vector), w (vertical scalar).
Stacked in n_layers altitude layers on a subdivided icosahedron.

Seven-stage pipeline — identical physics to the original grid model:

    1. Heat     — sun warms surface
    2. Cool     — radiation to space (exponential decay)
    3. Pressure — ∇T → wind acceleration (horizontal + vertical)
    4. Body     — Coriolis rotation (horizontal) + Gravity (vertical)
    5. Advect   — wind carries T, wind, w (horizontal + vertical, upwind)
    6. Friction — drag slows wind (exponential decay)
    7. Boundary — w=0 at ground and tropopause

Every operation is local (neighbor lookups only). The icosahedral mesh provides
uniform cell sizes with no pole singularity.

Energy stability is guaranteed by the same invariants as the grid model:

    Coriolis  — Rodrigues rotation, preserves |wind|² exactly (±0)
    Advection — upwind differencing, monotone, CFL-bounded (±0)
    Cooling   — exponential decay, −∝T²
    Friction  — exponential decay, −∝KE
    Heating   — bounded constant (+)

Energy in: bounded. Energy out: grows with E. → Finite equilibrium.
"""

import math
import numpy as np
from dataclasses import dataclass


@dataclass
class Params:
    dt: float = (
        math.pi / 120
    )  # ~0.026s — gives exactly 60-second days at omega=0.4, 10Hz
    solar: float = 0.15
    cooling: float = 0.02
    c_sq: float = 0.15
    drag: float = 0.02
    omega: float = 0.4
    g: float = 0.06
    n_layers: int = 8
    tilt: float = 23.44 * math.pi / 180  # Earth's axial tilt in radians


# ───────────────────────────────────────────────
#  Horizontal operators (icosahedral neighbor graph)
# ───────────────────────────────────────────────


def horizontal_gradient(mesh, F):
    """∇F on the icosahedral mesh.  F: (L, C) → (L, C, 3).

    Each edge contributes (F_j − F_i) × ê_ij, scatter-summed to cell i.
    Weight: mesh.gradient_weight = 2/N per cell (exact for isotropic stencil,
    the direct analog of /2 in central differences on a regular grid).
    """
    dF = F[:, mesh.edge_dst] - F[:, mesh.edge_src]
    flux = dF[:, :, np.newaxis] * mesh.edge_tangent[np.newaxis, :, :]
    grad = np.zeros((F.shape[0], mesh.n_cells, 3))
    for k in range(F.shape[0]):
        np.add.at(grad[k], mesh.edge_src, flux[k])
    grad *= mesh.gradient_weight[np.newaxis, :, np.newaxis]
    return grad


def advect_horizontal(mesh, F, wind, dt):
    """Upwind advection of F by wind on the icosahedral mesh.

    For each edge: project cell wind onto edge tangent.
    Negative projection → wind blows FROM that neighbor (upwind).
    Pull (F_j − F_i) weighted by upwind speed.
    Weight: mesh.advection_weight = 4/N = 2× gradient weight
    (upwind half-stencil needs double the correction).

    F: (L, C) scalar  or  (L, C, 3) vector.
    """
    F_new = F.copy()
    is_vec = F.ndim == 3

    for k in range(F.shape[0]):
        w_edge = np.einsum("ej,ej->e", wind[k, mesh.edge_src], mesh.edge_tangent)
        w_up = np.minimum(w_edge, 0.0)

        if is_vec:
            dF = F[k, mesh.edge_dst] - F[k, mesh.edge_src]
            acc = np.zeros((mesh.n_cells, 3))
            np.add.at(acc, mesh.edge_src, w_up[:, np.newaxis] * dF)
            F_new[k] -= dt * acc * mesh.advection_weight[:, np.newaxis]
        else:
            dF = F[k, mesh.edge_dst] - F[k, mesh.edge_src]
            acc = np.zeros(mesh.n_cells)
            np.add.at(acc, mesh.edge_src, w_up * dF)
            F_new[k] -= dt * acc * mesh.advection_weight

    return F_new


# ───────────────────────────────────────────────
#  Vertical operators (1D column — same as original grid model)
# ───────────────────────────────────────────────


def vertical_gradient(F):
    """Central difference in the vertical.  Bounded at surface and top."""
    dFdz = np.zeros_like(F)
    if F.shape[0] > 1:
        dFdz[1:-1] = (F[2:] - F[:-2]) / 2.0
        dFdz[0] = F[1] - F[0]
        dFdz[-1] = F[-1] - F[-2]
    return dFdz


def advect_vertical(F, w, dt):
    """Upwind advection in the vertical.  Identical to original grid model."""
    dF_back = np.zeros_like(F)
    dF_back[1:] = F[1:] - F[:-1]
    dF_fwd = np.zeros_like(F)
    dF_fwd[:-1] = F[1:] - F[:-1]

    w_exp = w[..., np.newaxis] if F.ndim == 3 else w
    dFdz = np.where(w_exp > 0, dF_back, dF_fwd)
    return F - dt * w_exp * dFdz


# ───────────────────────────────────────────────
#  Body forces
# ───────────────────────────────────────────────


def coriolis_rotate(wind, mesh, omega, dt):
    """Exact Coriolis rotation on the tangent plane.

    Rodrigues' formula with n·v = 0 (wind is tangent):

        v_rot = v cos(f) − (n × v) sin(f)

    where f = 2 ω sin(lat) dt,  n = unit normal = cell position.

    This is mathematically identical to the grid model's rotation matrix:
        u, v = u cos(f) + v sin(f),  −u sin(f) + v cos(f)

    Preserves |wind|² exactly.  No energy injection.  No projection needed.
    """
    f = 2.0 * omega * mesh.sin_lat * dt
    cos_f = np.cos(f)[np.newaxis, :, np.newaxis]
    sin_f = np.sin(f)[np.newaxis, :, np.newaxis]
    n = mesh.positions[np.newaxis, :, :]
    return wind * cos_f - np.cross(n, wind) * sin_f


# ───────────────────────────────────────────────
#  The 7-stage pipeline
# ───────────────────────────────────────────────


def step_atmosphere_3d(mesh, T, wind, w, p, sun_dir):
    """One timestep.  Seven stages, all local, all stable."""

    # 1. Heat — sun warms surface layer
    T[0] += p.dt * p.solar * np.maximum(0.0, mesh.positions @ sun_dir)

    # 2. Cool — exponential decay (all layers)
    T *= 1.0 - p.dt * p.cooling

    # 3. Pressure — ∇T accelerates wind
    wind -= p.dt * p.c_sq * horizontal_gradient(mesh, T)
    w -= p.dt * p.c_sq * vertical_gradient(T)

    # 4. Body forces
    #    a. Coriolis — exact rotation, preserves KE
    wind = coriolis_rotate(wind, mesh, p.omega, p.dt)
    #    b. Gravity — constant downward pull
    w -= p.dt * p.g
    #    c. CFL clamp — keep vertical advection stable
    w_max = 1.0 / p.dt
    np.clip(w, -w_max, w_max, out=w)

    # 5. Advection — upwind, horizontal then vertical
    T = advect_horizontal(mesh, T, wind, p.dt)
    wind = advect_horizontal(mesh, wind, wind, p.dt)
    w = advect_horizontal(mesh, w, wind, p.dt)

    T = advect_vertical(T, w, p.dt)
    wind = advect_vertical(wind, w, p.dt)
    w = advect_vertical(w, w, p.dt)

    # Re-project wind to tangent plane
    # (advection between cells with different tangent planes introduces radial drift)
    dots = np.einsum("lkj,kj->lk", wind, mesh.positions)
    wind -= dots[:, :, np.newaxis] * mesh.positions[np.newaxis, :, :]

    # 6. Friction — exponential decay
    wind *= 1.0 - p.dt * p.drag
    w *= 1.0 - p.dt * p.drag

    # 7. Boundaries — tropopause and ground
    w[0, :] = 0.0
    w[-1, :] = 0.0

    return T, wind, w
