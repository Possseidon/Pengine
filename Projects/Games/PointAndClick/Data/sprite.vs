#version 420

uniform float aspect;

in vec3 vpos;
in vec4 vcolor;
in float vfade;
in vec2 vborderlow0;
in vec2 vborderhigh0;
in vec2 vborderlow1;
in vec2 vborderhigh1;
in vec2 vtexcoord0;
in vec2 vtexcoord1;
    
flat out vec4 fcolor;
flat out float ffade;
flat out vec2 fborderlow0;
flat out vec2 fborderhigh0;
flat out vec2 fborderlow1;
flat out vec2 fborderhigh1;
out vec2 ftexcoord0;
out vec2 ftexcoord1;

void main()
{
  fcolor = vcolor;
  
  fcolor = vcolor;
  ffade = vfade;
  fborderlow0 = vborderlow0;
  fborderhigh0 = vborderhigh0;
  fborderlow1 = vborderlow1;
  fborderhigh1 = vborderhigh1;
  ftexcoord0 = vtexcoord0;
  ftexcoord1 = vtexcoord1;

  gl_Position = vec4(vpos.x / aspect, vpos.y, -vpos.z * (1 - 1e-6), 1);
}