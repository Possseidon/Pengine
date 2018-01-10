#version 420

uniform sampler2D tex;

flat in vec4 fcolor;
flat in float ffade;
flat in vec2 fborderlow0;
flat in vec2 fborderhigh0;
flat in vec2 fborderlow1;
flat in vec2 fborderhigh1;
in vec2 ftexcoord0;
in vec2 ftexcoord1;

out vec4 outcolor;

void main()
{
  vec2 ctexcoord0 = clamp(ftexcoord0, fborderlow0, fborderhigh0);
  vec2 ctexcoord1 = clamp(ftexcoord1, fborderlow1, fborderhigh1);
  
  vec4 color0 = texture(tex, ctexcoord0);
  vec4 color1 = texture(tex, ctexcoord1);
  
  outcolor = mix(color0, color1, ffade) * fcolor; 
  
  if (outcolor.a == 0)
    gl_FragDepth = 1;
  else
    gl_FragDepth = gl_FragCoord.z;
}