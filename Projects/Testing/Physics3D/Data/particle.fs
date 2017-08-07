#version 420

uniform sampler2D tex;

in vec4 fcolor;
in vec2 ftexcoord;

out vec4 outcolor;

void main()
{
  outcolor = texture(tex, ftexcoord) * fcolor;
  if (outcolor.a == 0)
    gl_FragDepth = 1;
  else
    gl_FragDepth = gl_FragCoord.z;
}