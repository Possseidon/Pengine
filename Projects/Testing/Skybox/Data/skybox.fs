#version 420

struct stripe 
{
  vec3 color;
  float pitch;    
};

layout (std430) uniform stripedata 
{
  stripe stripes[16];
  int stripeCount;
};

in vec3 fpos;

out vec4 outcolor;

void main()
{  
  if (stripeCount < 2)
  {
    outcolor = vec4(0, 0, 0, 0);
    return;
  }
  
  float pitch = acos(dot(normalize(fpos), vec3(0, -1, 0)));
  
  int s;
  
  for (s = 1; s < stripeCount; s++)
    if (stripes[s].pitch >= pitch)
    {
      s += 0; // fixes weird INTEL bug (s is wrong sometimes)
      break;
    }
  
  if (s == 0)
    outcolor = vec4(stripes[0].color, 1);
  else if (s == stripeCount)
    outcolor = vec4(stripes[stripeCount - 1].color, 1);
  else
    outcolor = vec4(mix(stripes[s - 1].color, stripes[s].color, (pitch - stripes[s - 1].pitch) / (stripes[s].pitch - stripes[s - 1].pitch)), 1);
}