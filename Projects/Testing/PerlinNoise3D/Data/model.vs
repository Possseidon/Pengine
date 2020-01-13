#version 420

uniform mat3 model_rmatrix;
uniform mat4 model_matrix;
uniform mat4 view_matrix;
uniform mat4 mvp_matrix;

in vec3 vpos;
in vec3 vtex_factors;
in vec4 vtex_border0;
in vec4 vtex_border1;
in vec4 vtex_border2;
in vec3 vnormal;

out vec3 fpos;
out vec3 ftex_factors;
flat out mat3x4 ftex_borders;
out vec3 fnormal;
flat out vec3 fcam;

vec3 cam()
{
  return -vec3(view_matrix[3] * view_matrix);
}

void main()
{
  vec4 p = model_matrix * vec4(vpos, 1);
  fpos = p.xyz / p.w;
  ftex_factors = vtex_factors;
  ftex_borders = mat3x4(vtex_border0, vtex_border1, vtex_border2);
  fnormal = normalize(model_rmatrix * vnormal);
  fcam = cam();  
  
  gl_Position = mvp_matrix * vec4(vpos, 1);
}
