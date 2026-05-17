"""Single-Layer Icosahedral Atmosphere
======================================

2D version (no altitude).  Same 7-stage pipeline as the grid model, but on the
icosahedral neighbor graph instead of a regular lat-lon array.

Each cell stores: T (temperature), wind (3D tangent vector on unit sphere).

Gradient and advection use the mesh-precomputed isotropic weights
(gradient_weight = 2/N, advection_weight = 4/N) — the exact analog of
the /2 divisor in the grid model's central difference.
"""

import numpy as np
from dataclasses import dataclass


@dataclass
class Params:
    dt: float = 0.4
    solar: float = 0.15
    cooling: float = 0.02
    c_sq: float = 0.15
    drag: float = 0.02
    omega: float = 0.4


def gradient(mesh, T):
    """∇T on the icosahedral mesh.  T: (C,) → (C, 3)."""
    dT = T[mesh.edge_dst] - T[mesh.edge_src]
    grad = np.zeros((mesh.n_cells, 3), dtype=np.float64)
    np.add.at(grad, mesh.edge_src, dT[:, np.newaxis] * mesh.edge_tangent)
    grad *= mesh.gradient_weight[:, np.newaxis]
    return grad


def advect_scalar(mesh, F, wind, dt):
    """Upwind advection of scalar field F by tangent wind.

    Same logic as the 3D version but for a single layer.
    """
    w_edge = np.einsum('ej,ej->e', wind[mesh.edge_src], mesh.edge_tangent)
    w_up = np.minimum(w_edge, 0.0)
    dF = F[mesh.edge_dst] - F[mesh.edge_src]
    acc = np.zeros(mesh.n_cells, dtype=np.float64)
    np.add.at(acc, mesh.edge_src, w_up * dF)
    return F - dt * acc * mesh.advection_weight


def coriolis_rotate(wind, mesh, omega, dt):
    """Exact Coriolis rotation via Rodrigues' formula.

    v_rot = v cos(f) − (n × v) sin(f),  f = 2 ω sin(lat) dt.
    Preserves |wind|² exactly.
    """
    f = 2.0 * omega * mesh.sin_lat * dt
    cos_f = np.cos(f)[:, np.newaxis]
    sin_f = np.sin(f)[:, np.newaxis]
    return wind * cos_f - np.cross(mesh.positions, wind) * sin_f


def step_atmosphere(mesh, T, wind, p, sun_lon):
    """One timestep of the single-layer icosahedral atmosphere."""

    # 1. Heat
    sun_dir = np.array([np.cos(sun_lon), np.sin(sun_lon), 0.0])
    T = T + p.dt * p.solar * np.maximum(0.0, mesh.positions @ sun_dir)

    # 2. Cool
    T = T * (1.0 - p.dt * p.cooling)

    # 3. Pressure
    wind = wind - p.dt * p.c_sq * gradient(mesh, T)

    # 4. Coriolis
    wind = coriolis_rotate(wind, mesh, p.omega, p.dt)

    # 5. Advection
    T_new = advect_scalar(mesh, T, wind, p.dt)
    wx = advect_scalar(mesh, wind[:, 0], wind, p.dt)
    wy = advect_scalar(mesh, wind[:, 1], wind, p.dt)
    wz = advect_scalar(mesh, wind[:, 2], wind, p.dt)
    wind = np.column_stack((wx, wy, wz))

    # Re-project to tangent plane
    dots = np.einsum('ij,ij->i', wind, mesh.positions)
    wind = wind - dots[:, np.newaxis] * mesh.positions

    # 6. Friction
    wind = wind * (1.0 - p.dt * p.drag)

    return T, wind
