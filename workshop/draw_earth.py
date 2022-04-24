import rasterio

import numpy as np

from glumpy import app, gl, glm, gloo


# define shaders

vertex = """
uniform mat4   u_model;         // Model matrix
uniform mat4   u_view;          // View matrix
uniform mat4   u_projection;    // Projection matrix
attribute vec4 a_color;         // Vertex color
attribute vec3 a_position;      // Vertex position
varying vec4   v_color;         // Interpolated fragment color (out)
void main()
{
    v_color = a_color;
    gl_Position = u_projection * u_view * u_model * vec4(a_position,1.0);
}
"""

fragment = """
varying vec4   v_color;         // Interpolated fragment color (in)
void main()
{
    gl_FragColor = v_color;
}
"""


# create sphere mesh

def icosa_positions():
    phi = (1.0 + np.sqrt(5.0)) * 0.5
    a = 1.0
    b = 1.0 / phi

    v1 = np.array([ 0,  b, -a])
    v2 = np.array([ b,  a,  0])
    v3 = np.array([-b,  a,  0])
    v4 = np.array([ 0,  b,  a])
    v5 = np.array([ 0, -b,  a])
    v6 = np.array([-a,  0,  b])
    v7 = np.array([ 0, -b, -a])
    v8 = np.array([ a,  0, -b])
    v9 = np.array([ a,  0,  b])
    v10 = np.array([-a,  0, -b])
    v11 = np.array([ b, -a,  0])
    v12 = np.array([-b, -a,  0])

    return [v1 / np.linalg.norm(v1), 
            v2 / np.linalg.norm(v2), 
            v3 / np.linalg.norm(v3), 
            v4 / np.linalg.norm(v4), 
            v5 / np.linalg.norm(v5), 
            v6 / np.linalg.norm(v6), 
            v7 / np.linalg.norm(v7), 
            v8 / np.linalg.norm(v8), 
            v9 / np.linalg.norm(v9), 
            v10 / np.linalg.norm(v10), 
            v11 / np.linalg.norm(v11), 
            v12 / np.linalg.norm(v12)]


# define helper to subdivide icosahedrons

def subdivide_project(positions, indices):

    new_positions = positions.copy()
    new_indices = []

    for triag in indices:
        v1 = positions[triag[0] - 1]
        v2 = positions[triag[1] - 1]
        v3 = positions[triag[2] - 1]

        mp12 = np.array([ (v1[0] + v2[0]) / 2,
                          (v1[1] + v2[1]) / 2,
                          (v1[2] + v2[2]) / 2 ] )
        mp12 = mp12 / np.linalg.norm(mp12)

        mp13 = np.array([ (v1[0] + v3[0]) / 2,
                          (v1[1] + v3[1]) / 2,
                          (v1[2] + v3[2]) / 2 ])
        mp13 = mp13 / np.linalg.norm(mp13)

        mp23 = np.array([ (v2[0] + v3[0]) / 2,
                          (v2[1] + v3[1]) / 2,
                          (v2[2] + v3[2]) / 2 ])
        mp23 = mp23 / np.linalg.norm(mp23)
        
        v1i = triag[0] 
        v2i = triag[1]
        v3i = triag[2]

        def find_existing(vert):
            return None
            for pos_idx, pos in enumerate(new_positions):
                if np.allclose(vert, pos):
                    return pos_idx + 1
            
        mp12i = find_existing(mp12)
        if not mp12i: 
            mp12i = len(new_positions) + 1
            new_positions.append(mp12)

        mp13i = find_existing(mp13)
        if not mp13i: 
            mp13i = len(new_positions) + 1
            new_positions.append(mp13)

        mp23i = find_existing(mp23)
        if not mp23i: 
            mp23i = len(new_positions) + 1
            new_positions.append(mp23)

        new_indices.extend([
            [v1i, mp12i, mp13i],
            [mp12i, v2i, mp23i],
            [mp23i, v3i, mp13i],
            [mp13i, mp12i, mp23i]
        ])

    return new_positions, new_indices


positions = icosa_positions()
indices = [[ 3, 2, 1 ],
           [ 2, 3, 4 ],
           [ 6, 5, 4 ], 
           [ 5, 9, 4 ], 
           [ 8, 7, 1 ], 
           [ 7,10, 1 ],
           [12,11, 5 ], 
           [11,12, 7 ],  
           [10, 6, 3 ], 
           [ 6,10,12 ],  
           [ 9, 8, 2 ], 
           [ 8, 9,11 ],
           [ 3, 6, 4 ],
           [ 9, 2, 4 ],
           [10, 3, 1 ],
           [ 2, 8, 1 ],
           [12,10, 7 ],
           [ 8,11, 7 ],
           [ 6,12, 5 ],
           [11, 9, 5 ]]

positions, indices = subdivide_project(positions, indices)
positions, indices = subdivide_project(positions, indices)
positions, indices = subdivide_project(positions, indices)
positions, indices = subdivide_project(positions, indices)
positions, indices = subdivide_project(positions, indices)


# read the GDEM data
path = 'data/GDEM-10km-BW.tif'
with rasterio.open(path) as rds:

    # for each position in our mesh
    for pos_idx, position in enumerate(positions):

        lon = np.arctan2(position[1], position[0])
        lon = (lon/np.pi)*180

        lat = np.arcsin(position[2])
        lat = (lat/np.pi)*180

        # find corresponding elevation
        elev = list(rds.sample([(lon, lat)]))[0][0]

        # and scale the pos
        positions[pos_idx] = position * ( 1 + elev / 1000)


# create a program to draw the mesh

V = np.zeros(len(positions), [("a_position", np.float32, 3),
                              ("a_color",    np.float32, 4)])
V["a_position"] = positions

def get_color(pos):
    if np.linalg.norm(pos) <= 1.0:
        return [0, 0, 1, 1]
    else:
        return [34/255, 139/255, 34/255, 1]

V["a_color"] = [get_color(positions[idx]) for idx in range(len(positions))]


V = V.view(gloo.VertexBuffer)

I = np.array(indices, dtype=np.uint32).flatten() - 1
I = I.view(gloo.IndexBuffer)

earth = gloo.Program(vertex, fragment)
earth.bind(V)

earth['u_model'] = np.eye(4, dtype=np.float32)
earth['u_view'] = glm.translation(0, 0, -5)
theta = 0


window = app.Window(width=1024, height=1024,  color=(0.30, 0.30, 0.35, 1.00))

@window.event
def on_draw(dt):
    global phi, theta
    window.clear()

    earth.draw(gl.GL_TRIANGLES, I)
    
    # Rotate earth
    theta += 0.5 # degrees
    model = np.eye(4, dtype=np.float32)
    glm.rotate(model, -90, 1, 0, 0)
    glm.rotate(model, theta, 0, 1, 0)
    earth['u_model'] = model


@window.event
def on_resize(width, height):
    earth['u_projection'] = glm.perspective(45.0, width / float(height), 2.0, 100.0)

@window.event
def on_init():
    gl.glEnable(gl.GL_DEPTH_TEST)


# Start the app
app.run()
