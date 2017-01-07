#version 420

uniform float aspect;

in vec3 vpos;
in vec2 vtexcoord;
in vec4 vcolor;
in vec2 vborderlow;
in vec2 vborderhigh;

out vec2 ftexcoord;
out vec4 fcolor;
flat out vec2 fborderlow;
flat out vec2 fborderhigh;

void main()
{
  ftexcoord = vtexcoord;
  fcolor = vcolor;
  
  fborderlow  = vborderlow;
  fborderhigh = vborderhigh;
  
  gl_Position = vec4(vpos.x / aspect, vpos.y, vpos.z, 1);
}