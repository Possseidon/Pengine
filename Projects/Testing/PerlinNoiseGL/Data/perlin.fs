#version 420

const int size_x = 16;
const int size_y = 16;
const int size_z = 16;

uniform float time;
layout (std140) uniform gradients
{
  vec3 grad[size_x * size_y * size_z];
};

in vec2 fpos;

out vec4 outcolor;

vec3 getGradient(ivec3 pos)
{
  pos %= ivec3(size_x, size_y, size_z);
  return grad[(pos.x * size_x + pos.y) * size_y + pos.z];
}

float cubic(float a, float b, float w)
{
  return a + w * w * (3 - 2 * w) * (b - a);  
}

float dotgradient(ivec3 grid, vec3 pos)
{
  return dot(getGradient(grid), pos - grid);
}

float noise(vec3 pos)
{
  ivec3 ga = ivec3(floor(pos));
  ivec3 gh = ga + ivec3(1);

  ivec3 gb = ivec3(gh.x, ga.yz);
  ivec3 gc = ivec3(ga.x, gh.y, ga.z);
  ivec3 gd = ivec3(gh.xy, ga.z);

  ivec3 ge = ivec3(ga.xy, gh.z);
  ivec3 gf = ivec3(gh.x, ga.y, gh.z);
  ivec3 gg = ivec3(ga.x, gh.yz);

  float a = dotgradient(ga, pos);
  float b = dotgradient(gb, pos);
  float c = dotgradient(gc, pos);
  float d = dotgradient(gd, pos);
  float e = dotgradient(ge, pos);
  float f = dotgradient(gf, pos);
  float g = dotgradient(gg, pos);
  float h = dotgradient(gh, pos);

  vec3 delta = pos - ga;
  float i = cubic(a, b, delta.x);
  float j = cubic(c, d, delta.x);
  float k = cubic(e, f, delta.x);
  float l = cubic(g, h, delta.x);

  float m = cubic(i, j, delta.y);
  float n = cubic(k, l, delta.y);

  return cubic(m, n, delta.z) / sqrt(3.0 / 4.0);
}

vec3 hsv2rgb(vec3 c)
{
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main()
{
  vec3 pos = vec3(fpos, time * 0.01) * vec3(size_x, size_y, size_z);

  // outcolor = vec4(vec3(noise(pos) * 0.5 + 0.5), 1);

  outcolor = vec4(0);
  for (int i = 0; i < 12; i++)
  {
    float v = clamp((noise(vec3(pos.xy, pos.z + i * 2.4262))), 0, 1);
    outcolor.rgb += hsv2rgb(vec3(i / 12.0, 1, v));
  }

}
