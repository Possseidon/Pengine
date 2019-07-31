#version 420

const int size_x = 20;
const int size_y = 20;

uniform float time;
layout (std140) uniform gradients
{
  vec2 gradients[size_x][size_y];
};

in vec2 fpos;

out vec4 outcolor;

float cubic(float a, float b, float w)
{
  return a + w * w * (3 - 2 * w) * (b - a);  
}

float dotgradient(ivec2 grid, vec2 pos)
{
  return dot(gradients[grid.x][grid.y], pos - grid);
}

void main()
{  
  vec2 pos = mod(fpos, 1) * vec2(size_x + 1, size_y + 1);
  
  ivec2 ga = ivec2(pos);
  ivec2 gb = ivec2((ga.x + 1) % size_x, ga.y);
  ivec2 gc = ivec2(ga.x, (ga.y + 1) % size_y);
  ivec2 gd = (ga + ivec2(1)) % ivec2(size_x, size_y);

  float a = dotgradient(ga, pos);
  float b = dotgradient(gb, pos);
  float c = dotgradient(gc, pos);
  float d = dotgradient(gd, pos);
  
  vec2 delta = pos - ga;
  float e = cubic(a, b, delta.x);
  float f = cubic(c, d, delta.x);  
  outcolor = vec4(vec3(cubic(e, f, delta.y)), 1);
  outcolor /= sqrt(0.5);
  outcolor *= 0.5;
  outcolor += 0.5;
}
