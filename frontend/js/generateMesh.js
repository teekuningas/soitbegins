/**
 * Procedural Earth mesh generator.
 *
 * Generates an icosahedron sphere, subdivides it, samples elevation from
 * cubemap textures, displaces vertices, and assigns colors by elevation.
 *
 * Coordinate system: Z-up (matching simulation and WebGL rendering).
 * Cubemap coordinate system: Y-up → we swap: cubemap_x=x, cubemap_y=z, cubemap_z=y
 */

// --- Icosahedron Generation (Z-up) ---

const PHI = (1 + Math.sqrt(5)) / 2;

function normalizeVec(v) {
  const len = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
  return [v[0] / len, v[1] / len, v[2] / len];
}

function midpoint(a, b) {
  return normalizeVec([(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2]);
}

function makeIcosahedron() {
  // 12 vertices of a unit icosahedron (Z-up)
  const verts = [
    [-1, PHI, 0], [1, PHI, 0], [-1, -PHI, 0], [1, -PHI, 0],
    [0, -1, PHI], [0, 1, PHI], [0, -1, -PHI], [0, 1, -PHI],
    [PHI, 0, -1], [PHI, 0, 1], [-PHI, 0, -1], [-PHI, 0, 1]
  ].map(normalizeVec);

  const faces = [
    [0, 11, 5], [0, 5, 1], [0, 1, 7], [0, 7, 10], [0, 10, 11],
    [1, 5, 9], [5, 11, 4], [11, 10, 2], [10, 7, 6], [7, 1, 8],
    [3, 9, 4], [3, 4, 2], [3, 2, 6], [3, 6, 8], [3, 8, 9],
    [4, 9, 5], [2, 4, 11], [6, 2, 10], [8, 6, 7], [9, 8, 1]
  ];

  return { vertices: verts, faces: faces };
}

function subdivide(mesh, levels) {
  let { vertices, faces } = mesh;

  for (let level = 0; level < levels; level++) {
    const midCache = {};
    const newFaces = [];

    function getMidpoint(i, j) {
      const key = i < j ? `${i}_${j}` : `${j}_${i}`;
      if (midCache[key] !== undefined) return midCache[key];
      const mid = midpoint(vertices[i], vertices[j]);
      vertices.push(mid);
      const idx = vertices.length - 1;
      midCache[key] = idx;
      return idx;
    }

    for (const [a, b, c] of faces) {
      const ab = getMidpoint(a, b);
      const bc = getMidpoint(b, c);
      const ca = getMidpoint(c, a);
      newFaces.push([a, ab, ca], [b, bc, ab], [c, ca, bc], [ab, bc, ca]);
    }

    faces = newFaces;
  }

  return { vertices, faces };
}

// --- Cubemap Sampling ---

// Cubemap face directions (Y-up coordinate system used by generate_cubemap.py)
// For a direction (cx, cy, cz) in cubemap space:
//   Mesh→Cubemap: cx = mesh_x, cy = mesh_z, cz = mesh_y
function sampleCubemap(faceImages, faceSize, meshX, meshY, meshZ) {
  // Convert mesh coords (Z-up) to cubemap coords (Y-up)
  const cx = meshX;
  const cy = meshZ;
  const cz = meshY;

  const absCx = Math.abs(cx);
  const absCy = Math.abs(cy);
  const absCz = Math.abs(cz);

  let faceIdx, u, v;

  if (absCx >= absCy && absCx >= absCz) {
    if (cx > 0) {
      // +X face: dir = (1, -v, -u)  → u = -cz/cx, v = -cy/cx
      faceIdx = 0;
      u = -cz / absCx;
      v = -cy / absCx;
    } else {
      // -X face: dir = (-1, -v, u)  → u = cz/(-cx), v = -cy/(-cx)
      faceIdx = 1;
      u = cz / absCx;
      v = -cy / absCx;
    }
  } else if (absCy >= absCx && absCy >= absCz) {
    if (cy > 0) {
      // +Y face: dir = (u, 1, v)  → u = cx/cy, v = cz/cy
      faceIdx = 2;
      u = cx / absCy;
      v = cz / absCy;
    } else {
      // -Y face: dir = (u, -1, -v)  → u = cx/(-cy), v = -cz/(-cy)
      faceIdx = 3;
      u = cx / absCy;
      v = -cz / absCy;
    }
  } else {
    if (cz > 0) {
      // +Z face: dir = (u, -v, 1)  → u = cx/cz, v = -cy/cz
      faceIdx = 4;
      u = cx / absCz;
      v = -cy / absCz;
    } else {
      // -Z face: dir = (-u, -v, -1)  → u = -cx/(-cz), v = -cy/(-cz)
      faceIdx = 5;
      u = -cx / absCz;
      v = -cy / absCz;
    }
  }

  // Convert u,v from [-1,1] to pixel coordinates [0, faceSize-1]
  const px = Math.min(faceSize - 1, Math.max(0, Math.round((u + 1) * 0.5 * (faceSize - 1))));
  const py = Math.min(faceSize - 1, Math.max(0, Math.round((v + 1) * 0.5 * (faceSize - 1))));

  // Sample the grayscale value (0-255)
  const pixelIdx = py * faceSize + px;
  return faceImages[faceIdx][pixelIdx];
}

// --- Color Assignment (matches parseObj.js logic) ---

function getColor(radius) {
  const lowLimit = 1.0;
  const highLimit = 1.03;

  if (radius <= lowLimit) {
    return [0.0, 0.0, 1.0]; // Ocean: blue
  } else if (radius >= highLimit) {
    return [0.54, 0.27, 0.075]; // Mountain: brown
  } else {
    const factor = (radius - lowLimit) / (highLimit - lowLimit);
    return [
      factor * 0.54 + (1 - factor) * 0.5,
      factor * 0.27 + (1 - factor) * 1.0,
      factor * 0.075 + (1 - factor) * 0.0
    ];
  }
}

// --- Main Generation ---

function generateEarthMesh(faceImages, faceSize, subdivisions) {
  const mesh = subdivide(makeIcosahedron(), subdivisions);
  const { vertices, faces } = mesh;

  // Displace vertices by elevation
  const displacedVertices = vertices.map(v => {
    const elevByte = sampleCubemap(faceImages, faceSize, v[0], v[1], v[2]);
    // elevByte: 0=sea level, 255=8848m. Displacement factor same as draw_earth.py
    const elevation = (elevByte / 255.0) * 8848.0;
    const scale = Math.max(1.0, 1.0 + elevation / 50000.0);
    return [v[0] * scale, v[1] * scale, v[2] * scale];
  });

  // Build triangle list (flat shading — each face gets its own vertices)
  const triangles = [];
  for (const [a, b, c] of faces) {
    const v1 = displacedVertices[a];
    const v2 = displacedVertices[b];
    const v3 = displacedVertices[c];
    const r1 = Math.sqrt(v1[0] * v1[0] + v1[1] * v1[1] + v1[2] * v1[2]);
    const r2 = Math.sqrt(v2[0] * v2[0] + v2[1] * v2[1] + v2[2] * v2[2]);
    const r3 = Math.sqrt(v3[0] * v3[0] + v3[1] * v3[1] + v3[2] * v3[2]);

    triangles.push([
      { position: v1, color: getColor(r1) },
      { position: v2, color: getColor(r2) },
      { position: v3, color: getColor(r3) }
    ]);
  }

  return triangles;
}

// --- Web Worker Interface ---

async function loadCubemapFace(url) {
  const response = await fetch(url);
  const blob = await response.blob();
  const bitmap = await createImageBitmap(blob);

  // Draw to offscreen canvas to extract pixel data
  const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
  const ctx = canvas.getContext('2d');
  ctx.drawImage(bitmap, 0, 0);
  const imageData = ctx.getImageData(0, 0, bitmap.width, bitmap.height);

  // Extract grayscale values (R channel of RGBA)
  const grayscale = new Uint8Array(bitmap.width * bitmap.height);
  for (let i = 0; i < grayscale.length; i++) {
    grayscale[i] = imageData.data[i * 4]; // R channel
  }

  return { data: grayscale, size: bitmap.width };
}

self.addEventListener("message", async function (event) {
  const { cubemapBaseUrl, subdivisions, nParts } = event.data;

  console.log("MeshWorker: Generating earth mesh...");
  console.log(`  Subdivisions: ${subdivisions}, target triangles: ${20 * Math.pow(4, subdivisions)}`);

  // Load all 6 cubemap faces
  const faceNames = ['posx', 'negx', 'posy', 'negy', 'posz', 'negz'];
  const facePromises = faceNames.map(name =>
    loadCubemapFace(`${cubemapBaseUrl}/elevation_${name}.png`)
  );
  const faceResults = await Promise.all(facePromises);

  const faceSize = faceResults[0].size;
  const faceImages = faceResults.map(r => r.data);

  console.log(`  Cubemap loaded: ${faceSize}x${faceSize} per face`);

  // Generate mesh
  const triangles = generateEarthMesh(faceImages, faceSize, subdivisions);
  console.log(`  Generated ${triangles.length} triangles`);

  // Split into chunks for incremental loading
  function splitToChunks(array, parts) {
    const result = [];
    const copy = array.slice();
    for (let i = parts; i > 0; i--) {
      result.push(copy.splice(0, Math.ceil(copy.length / i)));
    }
    return result;
  }

  self.postMessage(splitToChunks(triangles, nParts));
});
