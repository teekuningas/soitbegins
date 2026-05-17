"""
generate_cubemap.py

Converts an Equirectangular GeoTIFF (alwdgg.tif) into a 6-face Cubemap.
This provides a singularity-free, continuous-space elevation texture for the WebGL frontend
and the backend physics engine.
"""

import os
import numpy as np
import rasterio
from PIL import Image

# Output resolution per cubemap face
FACE_SIZE = 512

# Create output directory
os.makedirs('output', exist_ok=True)

def create_face_vectors(face_idx, size):
    """
    Generates 3D unit vectors for every pixel on a given cubemap face.
    face_idx:
      0: +X (Right)
      1: -X (Left)
      2: +Y (Top)
      3: -Y (Bottom)
      4: +Z (Front)
      5: -Z (Back)
    """
    # Create a grid of u, v coordinates from -1 to 1
    u, v = np.meshgrid(np.linspace(-1, 1, size), np.linspace(-1, 1, size))
    
    # +X, -X, +Y, -Y, +Z, -Z
    if face_idx == 0:   # +X (Right)
        x, y, z = np.ones_like(u), -v, -u
    elif face_idx == 1: # -X (Left)
        x, y, z = -np.ones_like(u), -v, u
    elif face_idx == 2: # +Y (Top)
        x, y, z = u, np.ones_like(u), v
    elif face_idx == 3: # -Y (Bottom)
        x, y, z = u, -np.ones_like(u), -v
    elif face_idx == 4: # +Z (Front)
        x, y, z = u, -v, np.ones_like(u)
    elif face_idx == 5: # -Z (Back)
        x, y, z = -u, -v, -np.ones_like(u)

    # Normalize to unit vectors
    norm = np.sqrt(x**2 + y**2 + z**2)
    return x / norm, y / norm, z / norm

def generate_cubemaps():
    print("Loading equirectangular dataset...")
    # Read the GeoTIFF
    with rasterio.open('data/alwdgg.tif') as dataset:
        img = dataset.read(1) # Read the first band (elevation)
        
        # Dimensions of the source image
        h, w = img.shape
        
        face_names = ['posx', 'negx', 'posy', 'negy', 'posz', 'negz']
        
        for i in range(6):
            print(f"Generating face {i+1}/6 ({face_names[i]})...")
            
            # 1. Get the 3D unit vector for each pixel in this face
            X, Y, Z = create_face_vectors(i, FACE_SIZE)
            
            # 2. Convert 3D vectors to latitude and longitude
            # Assuming standard spherical coordinates:
            # latitude = asin(Y)
            # longitude = atan2(Z, X)
            
            lat = np.arcsin(Y)
            lon = np.arctan2(Z, X)
            
            # 3. Map lat/lon to pixel coordinates in the equirectangular image
            # Latitude: -pi/2 to pi/2 -> height to 0
            v_coords = (0.5 - lat / np.pi) * (h - 1)
            
            # Longitude: -pi to pi -> 0 to width
            u_coords = (0.5 + lon / (2 * np.pi)) * (w - 1)
            
            # 4. Nearest neighbor sampling (for speed and simplicity here, 
            #    could be upgraded to bilinear interpolation using scipy.ndimage.map_coordinates)
            u_coords = np.clip(np.round(u_coords).astype(int), 0, w - 1)
            v_coords = np.clip(np.round(v_coords).astype(int), 0, h - 1)
            
            # Sample the elevation data
            face_data = img[v_coords, u_coords]
            
            # 5. Normalize for the 8-bit texture (temporary encoding scheme)
            # We want to map elevation into an RGB image.
            # For now, let's encode raw elevation into grayscale where 0 = sea level, 255 = highest peak.
            # In a production game, we might pack data into RGB channels (e.g. R=Elevation, G=Biome, B=Moisture).
            # Max elevation on Earth is ~8848m.
            face_data = np.clip(face_data, 0, 8848) # Clamp to sea level and Everest
            face_data_normalized = (face_data / 8848.0 * 255).astype(np.uint8)
            
            # Save as PNG
            img_out = Image.fromarray(face_data_normalized, mode='L')
            img_out.save(f"output/elevation_{face_names[i]}.png")
            
    print("Cubemap generation complete. Assets saved to output/")

if __name__ == "__main__":
    generate_cubemaps()
