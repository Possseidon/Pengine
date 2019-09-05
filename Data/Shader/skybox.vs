#version 430

uniform mat3 view_rmatrix;
uniform mat4 projection_matrix;

in vec3 vpos;

out vec3 fpos;

void main()
{
  fpos = vpos;
  gl_Position = projection_matrix * vec4(view_rmatrix * vpos, 1);
}