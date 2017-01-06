#version 420

uniform mat3 r;
uniform mat4 p;

in vec3 vpos;
in float vpitch;

out float fpitch;

void main()
{
  fpitch = vpitch;
  gl_Position = p * vec4(r * vpos, 1);
}