#version 420

uniform float aspect;

in vec3 vpos;
in vec4 vcolor;
in float vfade;
in vec2 vtexlow0;
in vec2 vtexhigh0;
in vec2 vtexlow1;
in vec2 vtexhigh1;
in vec2 vtexcoord0;
in vec2 vtexcoord1;
in float vborder;
    
out vec2 fpos;
flat out vec4 fcolor;
flat out float ffade;
flat out vec2 ftexlow0;
flat out vec2 ftexhigh0;
flat out vec2 ftexlow1;
flat out vec2 ftexhigh1;
out vec2 ftexcoord0;
out vec2 ftexcoord1;
flat out int fborder;

void main()
{  
  fpos = vpos.xy;
  fcolor = vcolor;
  ffade = vfade;
  ftexlow0 = vtexlow0;
  ftexhigh0 = vtexhigh0;
  ftexlow1 = vtexlow1;
  ftexhigh1 = vtexhigh1;
  ftexcoord0 = vtexcoord0;
  ftexcoord1 = vtexcoord1;
  fborder = int(vborder);

  gl_Position = vec4(vpos.x / aspect, vpos.y, -vpos.z / 1024 * (1 - 1e-6), 1);
}