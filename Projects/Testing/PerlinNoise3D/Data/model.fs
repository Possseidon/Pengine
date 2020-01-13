#version 420

const float PI = 3.1415926535;
const float NAN = 0.0 / 0.0;

const int MAXLIGHTS_DIRECTIONAL = 4;
const int MAXLIGHTS_POINT = 16;
const int MAXLIGHTS_SPOT = 16;

const int SAMPLE_COUNT = 8;
const float SAMPLE_SPREAD = 1e-3;

uniform int triFactorsStyle;
uniform int texFactorsStyle;

uniform sampler2D diffusemap;
uniform sampler2D specularmap;
uniform sampler2D normalmap;

uniform sampler2DArrayShadow directionalshadowmaps;
uniform samplerCubeArrayShadow pointshadowmaps;
uniform sampler2DArrayShadow spotshadowmaps;

uniform bool depthonly = false;

struct directionalLight // size 24
{
  vec3 color;
  // 1 space
  vec3 direction;
  bool shadowed;
  mat4 matrix;
};

struct pointLight // size 12
{
  vec3 color;
  float attenuation;
  vec3 position;
  bool shadowed;
  float nearClip;
  float farClip;
  // 2 space
};

struct spotLight // size 32
{
  vec3 color;
  float attenuation;
  vec3 position;
  float cutoff;
  vec3 direction;
  float cutoffBonus;
  bool shadowed;
  float fov;
  //float nearClip;
  //float farClip;
  // 2 space
  mat4 matrix;
};

layout (std140) uniform lightdata
{
  vec3 ambient;
  int dLightCount;  
  directionalLight dLights[MAXLIGHTS_DIRECTIONAL];
  int pLightCount;
  // 3 space
  pointLight pLights[MAXLIGHTS_POINT];
  int sLightCount;
  // 3 space
  spotLight sLights[MAXLIGHTS_SPOT];  
};

in vec3 fpos;
in vec3 ftex_factors;
flat in mat3x4 ftex_borders;
in vec3 fnormal;
flat in vec3 fcam;

out vec4 outcolor;

vec3 normal;
vec3 tex_factors_normalized;
vec3 tri_factors;
vec4 diffuse_color;
vec4 specular_color;
vec4 normal_color;
vec3 offsetpos;

float cosAngle(vec3 a, vec3 b)
{
  return dot(a, b);
}

float getAngle(vec3 a, vec3 b)
{
  return degrees(acos(clamp(cosAngle(a, b), -1, 1)));
}

vec3 calcTriFactors()
{
  return abs(vec3(
    cosAngle(normal, vec3(1, 0, 0)), 
    cosAngle(normal, vec3(0, 1, 0)), 
    cosAngle(normal, vec3(0, 0, 1))));  
}

vec4 calcTexture(sampler2D tex, int i)
{
  mat3x4 tri_textures = mat3x4(
    texture(tex, ftex_borders[i].xy + mod(fpos.yz * 2, 1) * (ftex_borders[i].zw - ftex_borders[i].xy)),
    texture(tex, ftex_borders[i].xy + mod(fpos.zx * 2, 1) * (ftex_borders[i].zw - ftex_borders[i].xy)),
    texture(tex, ftex_borders[i].xy + mod(fpos.xy * 2, 1) * (ftex_borders[i].zw - ftex_borders[i].xy)));
  return tri_textures * tri_factors;
}

vec4 calcCombinedTexture(sampler2D tex)
{
  return mat3x4(
    calcTexture(tex, 0),
    calcTexture(tex, 1),
    calcTexture(tex, 2)) * tex_factors_normalized;
}

float cubic(float w)
{
  return w * w * (3 - 2 * w);  
}

vec3 cubic3(vec3 w)
{
  return w * w * (3 - 2 * w);  
}

vec3 cospolate(vec3 w)
{
  return 0.5 - cos(w * PI) * 0.5;  
}

/*
mat3x4 getAllTextures(sampler2D tex)
{
  return mat3x4(
    getTexture(tex, 0),
    getTexture(tex, 1),
    getTexture(tex, 2));  
}

vec4 getCombinedTexture(sampler2D tex)
{
  return getAllTextures(tex) * tex_factors_normalized;
}
*/

/*
vec4 getCombinedTexture(sampler2D tex)
{
  return 
    (tex_factors_normalized.x != 0 ? tex_factors_normalized.x * getTexture(tex, 0) : vec4(0)) +
    (tex_factors_normalized.y != 0 ? tex_factors_normalized.y * getTexture(tex, 1) : vec4(0)) +
    (tex_factors_normalized.z != 0 ? tex_factors_normalized.z * getTexture(tex, 2) : vec4(0));
}
*/

/*
vec4 getCombinedTexture(sampler2D tex)
{
  return 
    tex_factors_normalized.x * getTexture(tex, 0) +
    tex_factors_normalized.y * getTexture(tex, 1) +
    tex_factors_normalized.z * getTexture(tex, 2);
}
*/

vec4 getCombinedTexture(sampler2D tex)
{
  return vec4(tex_factors_normalized, 1);
}

vec2 getRandomSample(vec3 seed, int i)
{
  float angle = i * 2 * PI / SAMPLE_COUNT;
  float distance = 1;
  return vec2(sin(angle), cos(angle)) * distance;
}

float getAttenuationFactor(vec3 lightpos, float attenuation)
{
  return 1 / (1 + (length(lightpos - fpos)) * attenuation); 
}

float calcDiffuseFactor(vec3 lightvec, vec3 normal)
{
  return max(dot(lightvec, normal), 0);
}
/*
vec3 calcDiffuse(vec3 color, vec3 lightvec, int i)
{
  mat3x4 diffuse = getTriTextures(diffusemap, i);
  mat3x4 normals = getTriTextures(normalmap, i) * 2 - 1;
  diffuse[0] *= calcDiffuseFactor(-lightvec, normalize(vec3(0, normals[0].xy) + normal * normals[0].z));
  diffuse[1] *= calcDiffuseFactor(-lightvec, normalize(vec3(normals[1].y, 0, normals[1].x) + normal * normals[1].z));
  diffuse[2] *= calcDiffuseFactor(-lightvec, normalize(vec3(normals[2].xy, 0) + normal * normals[2].z));
  return (diffuse * getTriFactors()).rgb * color;
}
*/

/*
vec3 calcDiffuse(vec3 color, vec3 lightvec, int i)
{
  mat3x4 diffuse = getTriTextures(diffusemap, i);
  mat3x4 normals = getTriTextures(normalmap, i) * 2 - 1;
  diffuse[0] *= calcDiffuseFactor(-lightvec, normalize(vec3(0, normals[0].xy) + normal * normals[0].z));
  diffuse[1] *= calcDiffuseFactor(-lightvec, normalize(vec3(normals[1].y, 0, normals[1].x) + normal * normals[1].z));
  diffuse[2] *= calcDiffuseFactor(-lightvec, normalize(vec3(normals[2].xy, 0) + normal * normals[2].z));
  return (diffuse * getTriFactors()).rgb * color;
}
*/

/*
mat3x3 calcAllDiffuse(vec3 color, vec3 lightvec)
{
  return mat3x3(
    calcDiffuse(color, lightvec, 0),
    calcDiffuse(color, lightvec, 1),
    calcDiffuse(color, lightvec, 2));
}

vec3 calcCombinedDiffuse(vec3 color, vec3 lightvec)
{
  return calcAllDiffuse(color, lightvec) * tex_factors_normalized;
}
*/

/*
vec3 calcCombinedDiffuse(vec3 color, vec3 lightvec)
{
  return 
    (tex_factors_normalized.x != 0 ? tex_factors_normalized.x * calcDiffuse(color, lightvec, 0) : vec3(0)) +
    (tex_factors_normalized.y != 0 ? tex_factors_normalized.y * calcDiffuse(color, lightvec, 1) : vec3(0)) +
    (tex_factors_normalized.z != 0 ? tex_factors_normalized.z * calcDiffuse(color, lightvec, 2) : vec3(0));
}
*/

/*
vec3 calcCombinedDiffuse(vec3 color, vec3 lightvec)
{
  return 
    tex_factors_normalized.x * calcDiffuse(color, lightvec, 0) +
    tex_factors_normalized.y * calcDiffuse(color, lightvec, 1) +
    tex_factors_normalized.z * calcDiffuse(color, lightvec, 2);
}
*/

vec3 calcCombinedDiffuse(vec3 color, vec3 lightvec)
{
  return color;
}

vec3 calcSpecular(vec3 color, vec3 reflected)
{
  return color * getCombinedTexture(specularmap).rgb * pow(calcDiffuseFactor(reflected, normalize(fcam - fpos)), 10);
}

float getDepthValue(float n, float f, vec3 d)
{
  d = abs(d);
  return (f + n) / (f - n) - (2 * f * n) / (f - n) / max(d.x, max(d.y, d.z)) / 2 + 0.5;
}

vec3 calcDirectionalLight(int i)
{
  if (dLights[i].color == vec3(0))
    return vec3(0);

  float cosTheta = dot(normal, -dLights[i].direction);
  
  if (cosTheta <= 0)
    return vec3(0);
  
  float shadowFactor = 1;
  if (dLights[i].shadowed)
  {      
    vec4 v = dLights[i].matrix * vec4(offsetpos, 1);
    v.xyz /= v.w;
    if (v.x < -1 || v.x > 1 ||
        v.y < -1 || v.y > 1 ||
        v.z < -1 || v.z > 1)
       return vec3(0);
    v.xyz = v.xyz / 2 + 0.5;
    
    shadowFactor = 0;
    for (int s = 0; s < SAMPLE_COUNT; s++)
      shadowFactor += texture(directionalshadowmaps, vec4(v.xy + getRandomSample(offsetpos, s) * SAMPLE_SPREAD, i, v.z)) / SAMPLE_COUNT;
  }
  
  if (shadowFactor == 0)
    return vec3(0);
  
  if (cosTheta < 1e-1)
    shadowFactor *= cosTheta / 1e-1;
  
  vec3 result = calcCombinedDiffuse(dLights[i].color, dLights[i].direction);
  result += calcSpecular(dLights[i].color, reflect(dLights[i].direction, normal));
  return result * shadowFactor;
}

vec3 calcPointLight(int i)
{
  if (pLights[i].color == vec3(0))
    return vec3(0);
    
  vec3 lightray = offsetpos - pLights[i].position;
  float lightveclength = length(lightray);  
  vec3 lightvec = normalize(lightray);
  float cosTheta = dot(normal, -lightvec);
  
  if (cosTheta <= 0)
    return vec3(0);
  
  float shadowFactor = 1;
  
  if (pLights[i].shadowed)
    shadowFactor = texture(pointshadowmaps, vec4(lightvec, i), getDepthValue(pLights[i].nearClip, pLights[i].farClip, lightray));
  
  if (shadowFactor == 0)
    return vec3(0);
  
  if (cosTheta < 1e-1)
    shadowFactor *= cosTheta / 1e-1;
  
  vec3 result = calcCombinedDiffuse(pLights[i].color, lightvec);
  result += calcSpecular(pLights[i].color, reflect(lightvec, normal));
  result *= getAttenuationFactor(pLights[i].position, pLights[i].attenuation);
  return result * shadowFactor;
}

vec3 calcSpotLight(int i)
{
  if (sLights[i].color == vec3(0))
    return vec3(0);
  
  vec3 lightvec = offsetpos - sLights[i].position;
  float lightveclength = length(lightvec);  
  lightvec = normalize(lightvec);
  float cosTheta = dot(normal, -lightvec);
  
  if (cosTheta <= 0)
    return vec3(0);

  float shadowFactor = 1;
  if (sLights[i].shadowed)
  {      
    vec4 v = sLights[i].matrix * vec4(offsetpos, 1);
    v.xyz /= v.w;
    if (v.x < -1 || v.x > 1 ||
        v.y < -1 || v.y > 1 ||
        v.z < -1 || v.z > 1)
       return vec3(0);
    v.xyz = v.xyz / 2 + 0.5;
          
    shadowFactor = 0;
    for (int s = 0; s < SAMPLE_COUNT; s++)
      shadowFactor += texture(spotshadowmaps, vec4(v.xy + getRandomSample(offsetpos, s) * SAMPLE_SPREAD, i, v.z)) / SAMPLE_COUNT;
  }
  if (shadowFactor == 0)
    return vec3(0);
  
  if (cosTheta < 5e-2)
    shadowFactor *= cosTheta / 5e-2;
  
  float angle = getAngle(lightvec, sLights[i].direction) * 2;
  float fullCutoff = sLights[i].cutoff + sLights[i].cutoffBonus;
  if (angle >= fullCutoff)
    return vec3(0);
  vec3 result = calcCombinedDiffuse(sLights[i].color, lightvec);
  result += calcSpecular(sLights[i].color, reflect(lightvec, normal));
  result *= getAttenuationFactor(sLights[i].position, sLights[i].attenuation);
  if (sLights[i].cutoffBonus != 0)
    result *= 0.5 + cos(PI * clamp((angle - sLights[i].cutoff) / sLights[i].cutoffBonus, 0, 1)) / 2;
  return result * shadowFactor;
}

vec3 applyStyle(vec3 value, int style)
{
  switch (style)
  {
    case 1:
      return cubic3(value);
    case 2:
      return cospolate(value);
  }
  return value;
}

void main()
{
  normal = normalize(fnormal);

  tex_factors_normalized = applyStyle(ftex_factors, texFactorsStyle);
  tex_factors_normalized /= dot(tex_factors_normalized, vec3(1));

  tri_factors = applyStyle(calcTriFactors(), triFactorsStyle);
  tri_factors /= dot(tri_factors, vec3(1));

  diffuse_color = calcCombinedTexture(diffusemap);
  specular_color = calcCombinedTexture(specularmap);
  normal_color = calcCombinedTexture(normalmap);

  outcolor = diffuse_color * vec4(ambient, 1);
  if (outcolor.a == 0)
    discard;

  if (depthonly)
    return;
  /*
  offsetpos = fpos + normal * 0.1;
  
  for (int i = 0; i < dLightCount; i++)
    outcolor.rgb += calcDirectionalLight(i);
  
  for (int i = 0; i < pLightCount; i++)
    outcolor.rgb += calcPointLight(i);
  
  for (int i = 0; i < sLightCount; i++)
    outcolor.rgb += calcSpotLight(i);
    */
}