
function parseObj(obj) {
  var lines = obj.split('\n');
  var ff = [];
  var vv = [];
  for (i = 0; i < lines.length; i++) {
    if (lines[i].startsWith('f ')) {
      ff.push(lines[i]);
    }
    if (lines[i].startsWith('v ')) {
      vv.push(lines[i]);
    }
  }
  var vertices = [];
  for (i = 0; i < ff.length; i++) {
    var face = ff[i].split(" ").slice(1, 4).map(x => parseInt(x.split("/")[0]) - 1);  
    var triangle = face.map(idx => vv[idx].split(" ").slice(1, 4).map(x => parseFloat(x)));
    vertices.push(triangle);
  }

  function getColor(vert) {
      var length = Math.sqrt(vert[0]*vert[0] + vert[1]*vert[1] + vert[2]*vert[2]);

      var lowlimit = 1.0
      var highlimit = 1.03

      var factor = (length - lowlimit) / (highlimit - lowlimit)
            
      if (length <= lowlimit) {
        return [0.0, 0.0, 1.0]; 
      } else if (length >= highlimit) {
        return [0.54, 0.27, 0.075];
      } else {
        return [(factor * 0.54 + (1 - factor) * 0.5),
                (factor * 0.27 + (1 - factor) * 1.0),
                (factor * 0.075 + (1 - factor) * 0.0)]
      }
  }

  var meshTriangles = []
  for (i = 0; i < vertices.length; i++) {
 
    var vert1 = {
      "position": vertices[i][0],
      "color": getColor(vertices[i][0])
    }
    var vert2 = {
      "position": vertices[i][1],
      "color": getColor(vertices[i][1])
    }
    var vert3 = {
      "position": vertices[i][2],
      "color": getColor(vertices[i][2])
    }
    
    meshTriangles.push([vert1, vert2, vert3]);
  }

  return meshTriangles;

}

self.addEventListener("message", function (event) {
  console.log("Got message from main");
  var data = event.data;
  var triangles = parseObj(data);
  self.postMessage(triangles);
});
