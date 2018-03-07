#version 420
  
struct border
{
  vec2 s;
  vec2 dx;
  vec2 dy;
  vec2 fade;
  int parent;
  // 3 space
};
    
layout (std140) uniform borderdata
{
  border borders[256];
};

uniform sampler2D tex;

in vec2 fpos;
flat in vec4 fcolor;
flat in float ffade;
flat in vec2 ftexlow0;
flat in vec2 ftexhigh0;
flat in vec2 ftexlow1;
flat in vec2 ftexhigh1;
in vec2 ftexcoord0;
in vec2 ftexcoord1;
flat in int fborder;

out vec4 outcolor;

void main()
{
  vec2 ctexcoord0 = clamp(ftexcoord0, ftexlow0, ftexhigh0);
  vec2 ctexcoord1 = clamp(ftexcoord1, ftexlow1, ftexhigh1);

  vec4 color0 = texture(tex, ctexcoord0);
  vec4 color1 = texture(tex, ctexcoord1);

  outcolor = mix(color0, color1, ffade) * fcolor;
    
  if (outcolor.a == 0)
    discard;
  
  int i = fborder;
  while (i != -1)
  {
    border b = borders[i];
    vec2 borderpos;
    borderpos.x = (b.dy.y * (fpos.x - b.s.x) - b.dy.x * (fpos.y - b.s.y)) / (b.dx.x * b.dy.y - b.dx.y * b.dy.x);
    if (abs(b.dy.x) > abs(b.dy.y))
      borderpos.y = (fpos.x - b.s.x - borderpos.x * b.dx.x) / b.dy.x;
    else
      borderpos.y = (fpos.y - b.s.y - borderpos.x * b.dx.y) / b.dy.y;

    vec2 alphalow = clamp(borderpos * 2 / b.fade, 0, 1);
    vec2 alphahigh = clamp((1 - borderpos) * 2 / b.fade, 0, 1);
    vec2 alpha = min(alphalow, alphahigh);

    // round perfect
    alpha = 1 - alpha;
    outcolor.a *= 1 - sqrt(alpha.x * alpha.x + alpha.y * alpha.y);
    
    // round simple
    // outcolor.a *= alpha.x * alpha.y;

    // edge
    // outcolor.a *= min(alpha.x, alpha.y); 
                                 
    if (outcolor.a == 0)
      discard;
  
    i = borders[i].parent;
  }
}