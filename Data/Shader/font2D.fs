#version 420

uniform sampler2D fontmap;

in vec2 ftexcoord;
in vec4 fcolor;

out vec4 outcolor;

void main()
{
  outcolor = fcolor * texture(fontmap, ftexcoord);
}