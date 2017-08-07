#version 420

uniform mat4 mv;
uniform mat4 mvp;

in vec3 vpos;
in float vrotation;
in vec2 voffset;
in vec4 vcolor;
in vec2 vtexcoord;

out vec4 fcolor;
out vec2 ftexcoord;

vec3 right()
{
  return normalize(vec3(transpose(mv)[0]));
}

vec3 up()
{
  return normalize(vec3(transpose(mv)[1]));
}

void main()
{
  fcolor = vcolor;
  ftexcoord = vtexcoord;
  vec3 r = right();
  vec3 u = up();
  vec3 br = cos(vrotation) * r - sin(vrotation) * u;
  vec3 bu = sin(vrotation) * r + cos(vrotation) * u;
  gl_Position = mvp * vec4(vpos + br * voffset.x + bu * voffset.y, 1);
}