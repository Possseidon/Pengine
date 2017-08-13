#version 420

uniform mat3 view_rmatrix;
uniform mat4 projection_matrix;

in vec3 vpos;
in float vpitch;

out float fpitch;

void main()
{
  fpitch = vpitch;
  gl_Position = projection_matrix * vec4(view_rmatrix * vpos, 1);
}