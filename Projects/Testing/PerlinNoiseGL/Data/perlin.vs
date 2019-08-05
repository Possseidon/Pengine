#version 420

uniform float aspect;
uniform vec2 offset;
uniform float scale;

in vec2 vpos;

out vec2 fpos;

void main()
{
  fpos = vpos;
  fpos.x *= aspect;
  fpos += offset;
  fpos /= scale;
  gl_Position = vec4(vpos, 0, 1);
}
