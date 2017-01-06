// Utility functions for shaders

// get the right vector of the camera
vec3 right()
{
  return normalize(vec3(transpose(mv)[0]));
}

// get the up vector of the camera
vec3 up()
{
  return normalize(vec3(transpose(mv)[1]));
}

// get the look vector of the camera
vec3 look()
{
  return -normalize(vec3(transpose(mv)[2]));
}

// get the position of the camera
vec3 cam()
{
  return -vec3(mv[3] * mv);
}

// get the cosine of the angle between two vectors (performance without cos(arccos(n)))
float cosAngle(vec3 a, vec3 b)
{
  return dot(a, b) / (length(a) * length(b));
}

float cosAngle(vec4 a, vec4 b)
{
  return dot(a, b) / (length(a) * length(b));
}

// get the angle in radians between two vectors
float getAngle(vec3 a, vec3 b)
{
  return acos(dot(a, b) / (length(a) * length(b)));
}

float getAngle(vec2 a, vec3 2)
{
  return acos(dot(a, b) / (length(a) * length(b)));
}
