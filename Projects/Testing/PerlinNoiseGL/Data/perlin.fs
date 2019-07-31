#version 420

const int size_x = 50;
const int size_y = 50;

uniform float time;
layout (std140) uniform gradients
{
  vec2 grad[size_x * size_y];
};

in vec2 fpos;

out vec4 outcolor;

vec2 rot90(vec2 v)
{
 	return vec2(v.y, -v.x);
}

vec2 getGradient(ivec2 pos, float v)
{
  pos %= ivec2(size_x, size_y);
  vec2 g = grad[pos.x * size_x + pos.y];
  return g * sin(time + v * pos.x) + rot90(g) * cos(time + v * pos.y);
}

float cubic(float a, float b, float w)
{
  return a + w * w * (3 - 2 * w) * (b - a);  
}

float dotgradient(ivec2 grid, vec2 pos, float v)
{
  return dot(getGradient(grid, v), pos - grid);
}

float noise(vec2 pos, float v)
{
  ivec2 ga = ivec2(pos);
  ivec2 gb = ivec2((ga.x + 1), ga.y);
  ivec2 gc = ivec2(ga.x, (ga.y + 1));
  ivec2 gd = ga + ivec2(1);

  float a = dotgradient(ga, pos, v);
  float b = dotgradient(gb, pos, v);
  float c = dotgradient(gc, pos, v);
  float d = dotgradient(gd, pos, v);

  vec2 delta = pos - ga;
  float e = cubic(a, b, delta.x);
  float f = cubic(c, d, delta.x);

  return cubic(e, f, delta.y) / sqrt(0.5);
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main()
{
  vec2 pos = mod(fpos, 1) * vec2(size_x, size_y );

  outcolor = vec4(0);
  for (int i = 0; i < 12; i++)
  {
    float v = clamp(1 - abs(noise(pos, i / 12.0 * 3.1415926535)) * 10, 0, 1);
		outcolor.rgb += hsv2rgb(vec3(i / 12.0, 1, v));
  }
}
