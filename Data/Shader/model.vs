#version 420

uniform mat3 mr;
uniform mat4 m;
uniform mat4 v;
uniform mat4 mvp;

in vec3 vpos;
in vec2 vtexcoord;
in vec3 vnormal;
in vec3 vtangent;
in vec3 vbitangent;
in vec2 vborderlow;
in vec2 vborderhigh;

out vec2 ftexcoord;
out vec3 fnormal;
out vec3 ftangent;
out vec3 fbitangent;
out vec3 fcam;
out vec3 fpos;
out vec3 frawpos;
flat out vec2 fborderlow;
flat out vec2 fborderhigh;

vec3 cam()
{
  return -vec3(v[3] * v);
}

void main()
{
  vec4 p = m * vec4(vpos, 1);
  fpos = p.xyz / p.w;
  frawpos = vpos;
  fcam = cam();
  
  ftexcoord = vtexcoord;
  fnormal = normalize(mr * vnormal);
  ftangent = normalize(mr * vtangent);
  fbitangent = normalize(mr * vbitangent);
  fborderlow = vborderlow;
  fborderhigh = vborderhigh;
  
  gl_Position = mvp * vec4(vpos, 1);
}