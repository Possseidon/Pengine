#version 420

uniform mat4 mvp;

in vec3 vpos;
in vec3 voffset;
in vec2 vtexcoord;
in vec4 vcolor;
in float vchar;

out vec2 ftexcoord;
out vec4 fcolor;

void main()
{
  fcolor = vcolor;
  int c = int(vchar);
  ftexcoord = vec2((vtexcoord.x + c % 16) / 16,
                  1 - ((1 - vtexcoord.y + c / 16) / 16));                   
  gl_Position = mvp * vec4(vpos.x + voffset.x, vpos.y + voffset.y, vpos.z + voffset.z, 1);
}