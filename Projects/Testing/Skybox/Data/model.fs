#version 420

#define PI 3.1415926535

#define MAXLIGHTS_DIRECTIONAL 4
#define MAXLIGHTS_POINT 16
#define MAXLIGHTS_SPOT 16

uniform sampler2D diffusemap;
uniform sampler2D specularmap;
uniform sampler2D normalmap;

uniform sampler2DArrayShadow directionalshadowmaps;
uniform samplerCubeArrayShadow pointshadowmaps;
uniform sampler2DArrayShadow spotshadowmaps;

uniform bool depthonly;

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

in vec2 ftexcoord;
in vec3 fnormal;
in vec3 ftangent;
in vec3 fbitangent;
in vec3 fpos;
in vec3 fcam;
flat in vec2 fborderlow;
flat in vec2 fborderhigh;

in vec3 frawpos;

vec2 ctexcoord;

out vec4 outcolor;

const int sampleCount = 8;
const float sampleSpread = 1e-3;

vec2 getRandomSample(vec3 seed, int i)
{
  float angle = i * 2 * PI / sampleCount;
  float distance = 1;
  return vec2(sin(angle), cos(angle)) * distance;
}

float getAttenuationFactor(vec3 lightpos, float attenuation)
{
  if (attenuation == 0)
    return 1;
  else
    return 1 / (1 + (length(lightpos - fpos)) * attenuation); 
}

float calcDiffuseFactor(vec3 lightvec, vec3 normal)
{
  return max(dot(lightvec, normal), 0);  
}

float getAngle(vec3 a, vec3 b)
{
  return acos(clamp(dot(a, b), -1, 1)) * 180 / PI;
}

vec3 calcDiffuse(vec3 color, vec3 lightvec, vec3 normal)
{  
  return color * texture(diffusemap, ctexcoord).rgb * calcDiffuseFactor(-lightvec, normal); 
}

vec3 calcSpecular(vec3 color, vec3 reflected)
{
  return color * texture(specularmap, ctexcoord).rgb * pow(calcDiffuseFactor(reflected, normalize(fcam - fpos)), 10);
}

float getDepthValue(float n, float f, vec3 d)
{
  d = abs(d);
  return ((f + n) / (f - n) - (2 * f * n) / (f - n) / max(d.x, max(d.y, d.z))) / 2 + 0.5;
}

void main()
{  
  ctexcoord = clamp(ftexcoord, fborderlow, fborderhigh);
 
  outcolor = vec4(ambient * texture(diffusemap, ctexcoord).rgb, texture(diffusemap, ctexcoord).a);
  if (outcolor.a == 0)
  {
    gl_FragDepth = 1;
    return;
  }  
  gl_FragDepth = gl_FragCoord.z;
  
  if (depthonly)
    return;
  
  vec3 texnormal = texture(normalmap, ctexcoord).xyz * 2 - 1;  
  vec3 normal = normalize(ftangent * texnormal.x + fbitangent * texnormal.y + fnormal * texnormal.z);
  vec3 offsetpos = fpos + fnormal * 0.1;
  
  // directional lights
  for (int i = 0; i < dLightCount; i++)
  {
    if (dLights[i].color == vec3(0))
      continue;
 
    float cosTheta = dot(fnormal, -dLights[i].direction);
    
    if (cosTheta <= 0)
      continue;
    
    float shadowFactor = 1;
    if (dLights[i].shadowed)
    {      
      vec4 v = dLights[i].matrix * vec4(offsetpos, 1);
      v.xyz /= v.w;
      if (all(greaterThan(v.xyz, vec3(-1))) && all(lessThan(v.xyz, vec3(+1)))) 
      {
        v.xyz = v.xyz / 2 + 0.5;
      
        //float factor = length(-dLights[i].matrix[2]) / (length(dLights[i].matrix[0]));
        //float bias = 0;//max(2e-3 * tan(acos(cosTheta)), 5e-3) * factor;
        shadowFactor = 0;
        for (int s = 0; s < sampleCount; s++)
          shadowFactor += texture(directionalshadowmaps, vec4(v.xy + getRandomSample(offsetpos, s) * sampleSpread /* factor * 0.2*/, i, v.z/* - bias*/)) / sampleCount;
      }
    }
    
    if (shadowFactor == 0)
      continue;
    
    if (cosTheta < 1e-1)
      shadowFactor *= cosTheta / 1e-1;
    
    vec3 add = calcDiffuse(dLights[i].color, dLights[i].direction, normal);
    add += calcSpecular(dLights[i].color, reflect(dLights[i].direction, normal));
    outcolor.rgb += add * shadowFactor;
  }
  
  // point lights
  for (int i = 0; i < pLightCount; i++)
  {
    if (pLights[i].color == vec3(0))
      continue;
      
    vec3 lightray = offsetpos - pLights[i].position;
    float lightveclength = length(lightray);  
    vec3 lightvec = normalize(lightray);
    float cosTheta = dot(fnormal, -lightvec);
    
    if (cosTheta <= 0)
      continue;
    float shadowFactor = 1;
    if (pLights[i].shadowed)
    {      
      //float bias = max(1e-3 * tan(acos(cosTheta)), 2e-3) / lightveclength;
      shadowFactor = texture(pointshadowmaps, vec4(lightvec, i), getDepthValue(pLights[i].nearClip, pLights[i].farClip, lightray)/* - bias*/);
    }
    if (shadowFactor == 0)
      continue;
    
    if (cosTheta < 1e-1)
      shadowFactor *= cosTheta / 1e-1;
    
    vec3 add = calcDiffuse(pLights[i].color, lightvec, normal);
    add += calcSpecular(pLights[i].color, reflect(lightvec, normal));
    add *= getAttenuationFactor(pLights[i].position, pLights[i].attenuation);
    outcolor.rgb += add * shadowFactor;
  }
  
  // spot lights
  for (int i = 0; i < sLightCount; i++)
  {
    if (sLights[i].color == vec3(0))
      continue;
    
    vec3 lightvec = offsetpos - sLights[i].position;
    float lightveclength = length(lightvec);  
    lightvec = normalize(lightvec);
    float cosTheta = dot(fnormal, -lightvec);
    
    if (cosTheta <= 0)
      continue;
    float shadowFactor = 1;
    if (sLights[i].shadowed)
    {      
      vec4 v = sLights[i].matrix * vec4(offsetpos, 1);
      v.xyz /= v.w;
      if (v.x < -1 || v.x > 1 ||
          v.y < -1 || v.y > 1 ||
          v.z < -1 || v.z > 1)
         continue;
      v.xyz = v.xyz / 2 + 0.5;
            
      //float bias = max(5e-6 * tan(acos(cosTheta)), 1e-5) / lightveclength * sLights[i].fov;
      shadowFactor = 0;
      for (int s = 0; s < sampleCount; s++)
        shadowFactor += texture(spotshadowmaps, vec4(v.xy + getRandomSample(offsetpos, s) * sampleSpread, i, v.z/* - bias*/)) / sampleCount;
    }
    if (shadowFactor == 0)
      continue;
    
    if (cosTheta < 5e-2)
      shadowFactor *= cosTheta / 5e-2;
    
    float angle = getAngle(lightvec, sLights[i].direction) * 2;
    float fullCutoff = sLights[i].cutoff + sLights[i].cutoffBonus;
    if (angle >= fullCutoff)
      continue;
    vec3 add = calcDiffuse(sLights[i].color, lightvec, normal);
    add += calcSpecular(sLights[i].color, reflect(lightvec, normal));
    add *= getAttenuationFactor(sLights[i].position, sLights[i].attenuation);
    if (sLights[i].cutoffBonus != 0)
      add *= 0.5 + cos(PI * clamp((angle - sLights[i].cutoff) / sLights[i].cutoffBonus, 0, 1)) / 2;
    outcolor.rgb += add * shadowFactor;
  }
}