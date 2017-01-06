#version 420

uniform mat4 mvp;

in vec3 vpos;
in vec3 voffset;
in vec2 vtexcoord;
in vec4 vcolor;
in float vchar;

out vec2 ftexcoord;
out vec4 fcolor;

vec3 right()
{
  return normalize(vec3(
    mvp[0][0],
    mvp[1][0],
    mvp[2][0]
  ));
}

vec3 up()
{
  return normalize(vec3(
    mvp[0][1],
    mvp[1][1],
    mvp[2][1]
  ));
}

void main()
{
  fcolor = vcolor;
  int c = int(vchar);
  ftexcoord = vec2((vtexcoord.x + c % 16) / 16,
                  1 - ((1 - vtexcoord.y + c / 16) / 16));                   
  gl_Position = mvp * vec4(
    vpos +
    voffset.x * right() + 
    voffset.y * up(), 
    1
  );
}