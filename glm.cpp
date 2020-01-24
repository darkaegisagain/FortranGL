//
// Created by Michael Larson
// January 13 2020
//
// I don't care how this is used and I am not repsonsible for it's use, just keep
// my name at the top of the source.
//
// Thanks
//
// Michael Larson
//


#include <ISO_Fortran_binding.h>
#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <stdio.h>
#include <string.h>

using namespace glm;

/************************************************************************************
This is an interface to the GLM library, it's all in C++ so we have to define this in
a separate file and mark it as extern "C" to keep the names sane.

Initially this was used to move porting the interface to fortran faster, but why
reinvent the wheel when GLM has all the support I need without bugs...
 ***********************************************************************************/

extern "C" {

void glfIdentity(CFI_cdesc_t *fmat)
{
  assert(fmat);

  mat4 mat = mat4(1.0f);

  memcpy(fmat->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

  void glfRotate(CFI_cdesc_t *fmat, float angle, float x, float y, float z)
{
  assert(fmat);

  mat4 mat;
  
  memcpy((void *)&mat[0][0], fmat->base_addr, 16 * sizeof(float));

  mat = rotate(mat, angle, vec3(x, y, z));
  
  memcpy(fmat->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

void glfTranslate(CFI_cdesc_t *fmat, float x, float y, float z)
{
  assert(fmat);

  mat4 mat;

  memcpy((void *)&mat[0][0], fmat->base_addr, 16 * sizeof(float));
  
  mat = translate(mat, vec3(x, y, z));

  memcpy(fmat->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

void glfScale(CFI_cdesc_t *fmat, float x, float y, float z)
{
  assert(fmat);

  mat4 mat;

  memcpy((void *)&mat[0][0], fmat->base_addr, 16 * sizeof(float));
  
  mat = scale(mat, vec3(x, y, z));

  memcpy(fmat->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

void glfLookAt(CFI_cdesc_t *fmat, CFI_cdesc_t *eye, CFI_cdesc_t *center, CFI_cdesc_t *up)
{
  assert(fmat);
  assert(eye);
  assert(center);
  assert(up);
  vec3 temp_eye, temp_center, temp_up;

  memcpy((void *)&temp_eye[0], eye->base_addr, 4 * sizeof(float));
  memcpy((void *)&temp_center[0], center->base_addr, 4 * sizeof(float));
  memcpy((void *)&temp_up[0], up->base_addr, 4 * sizeof(float));

  mat4 mat = lookAt(temp_eye, temp_center, temp_up);

  memcpy(fmat->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

void glfPerspective(CFI_cdesc_t *data, float fovy, float aspect, float near, float far)
{
  assert(data);

  mat4 mat = perspective(fovy, aspect, near, far);

  memcpy(data->base_addr, (void *)&mat[0][0], 16 * sizeof(float));
}

void glfCalcMVP(CFI_cdesc_t *fmvp, CFI_cdesc_t *fmodel, CFI_cdesc_t *fview, CFI_cdesc_t *fprojection)
{
  assert(fmvp);
  assert(fmodel);
  assert(fview);
  assert(fprojection);
  mat4 mvp, model, view, projection;
  float *pfloat;

  memcpy((void *)&model[0][0], fmodel->base_addr, 16 * sizeof(float));
  memcpy((void *)&view[0][0], fview->base_addr, 16 * sizeof(float));
  memcpy((void *)&projection[0][0], fprojection->base_addr, 16 * sizeof(float));

  mvp = projection * view * model;
  
  memcpy(fmvp->base_addr, (void *)&mvp[0][0], 16 * sizeof(float));
}


} // extern "C"
