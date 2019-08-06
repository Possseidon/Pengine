#version 420

uniform mat3 model_rmatrix;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 mvp_matrix;

in vec3 vpos;
in vec2 vtexcoord;
in vec2 vborderlow;
in vec2 vborderhigh;
in vec3 vnormal;
in vec3 vtangent;
in vec3 vbitangent;

out vec2 ftexcoord;
out vec3 fnormal;
out vec3 ftangent;
out vec3 fbitangent;
out vec3 fcam;
out vec3 fpos;
out vec3 frawpos;
flat out vec2 fborderlow;
flat out vec2 fborderhigh;

vec3 cam()
{
  return -vec3(view_matrix[3] * view_matrix);
}

void main()
{
  vec4 p = model_matrix * vec4(vpos, 1);
  fpos = p.xyz / p.w;
  frawpos = vpos;
  fcam = cam();
  
  ftexcoord = vtexcoord;
  fnormal = normalize(model_rmatrix * vnormal);
  ftangent = normalize(model_rmatrix * vtangent);
  fbitangent = normalize(model_rmatrix * vbitangent);
  fborderlow = vborderlow;
  fborderhigh = vborderhigh;
  
  gl_Position = mvp_matrix * vec4(vpos, 1);
}