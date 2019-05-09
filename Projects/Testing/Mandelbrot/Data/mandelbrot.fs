#version 420

uniform float time;
uniform int steps;

in vec2 fpos;

out vec4 outcolor;

vec3 hsv2rgb(vec3 c)
{
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec2 co_sqr(vec2 a)
{
  return vec2(a.x * a.x - a.y * a.y, 2 * a.x * a.y);
}

void main()
{
  vec2 c, z;
  z = c = fpos;
  int i = 0;
  while (i < steps)
  {
    z = co_sqr(z) + c;
    if (dot(z, z) >= 4)
      break;   
    i++;
  }
  
  if (i == steps)
    outcolor = vec4(0, 0, 0, 1);
  else
    // outcolor = vec4(hsv2rgb(vec3(float(i) / steps * 10, 1, 1)), 1);
    outcolor = vec4(vec3(i % 8 / 7.0, i / 8 % 8 / 7.0, i / 64 % 8 / 7.0), 1);
}
