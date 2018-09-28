#version 420

uniform mat3 model_rmatrix;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 mvp_matrix;

in vec3 vpos;
in vec3 vcolor;
in vec3 vnormal;

out vec3 fcolor;
out vec3 fnormal;
out vec3 fcam;
out vec3 fpos;
out vec3 frawpos;

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
  
  fcolor = vcolor;
  fnormal = normalize(model_rmatrix * vnormal);
  
  gl_Position = mvp_matrix * vec4(vpos, 1);
}