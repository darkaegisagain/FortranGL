/*
  Created by Michael Larson
  January 13 2020

  I don't care how this is used and I am not repsonsible for it's use, just keep
  my name at the top of the source.

  Thanks

  Michael Larson
*/

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>

#include <ISO_Fortran_binding.h>

#ifndef __DEBUG__
#define __DEBUG__ 1
#endif

// Fortran callback functions
extern void glfresize_func(GLFWwindow *window, int width, int height);
extern void glfkey_func(GLFWwindow *window, int key, int scancode, int action, int mods);
extern void glfcursor_func(GLFWwindow *window, double xpos, double ypos);
extern void glfmousebutton_func(GLFWwindow *window, int button, int action, int mods);


static void error_callback(int error, const char* description)
{
    fputs(description, stderr);
}

// Convert a Fortran string to a C string
static char *f_str_to_c_str(CFI_cdesc_t *data)
{
  assert(data);
  assert(data->base_addr);
  assert(data->rank == 0);
  assert(data->type == 12);
  GLuint index = 0;
  char *fstr = data->base_addr;
  int max_len = data->elem_len;
  int i = 0;

  char *cstr = (char *)malloc(max_len + 1);

  bzero(cstr, max_len + 1);
  for(i=0; i<max_len && fstr[i] != 0; i++) {
    cstr[i] = fstr[i];
  }

  cstr[i] = 0;
  
  return cstr;
}

// Init glf
GLFWwindow *glfInit(int width, int height, int major, int minor, CFI_cdesc_t *data)
{
  GLFWwindow *window;
  char *window_name = f_str_to_c_str(data);
  
  glfwSetErrorCallback(error_callback);
  if (!glfwInit()) {
    exit(EXIT_FAILURE);
  }
  
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, major);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, minor);
  #ifdef __APPLE__
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  #endif
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  window = glfwCreateWindow(640, 480, window_name, NULL, NULL);
  free(window_name);
  
  if (!window) {
    glfwTerminate();
    exit(EXIT_FAILURE);
  }

  glfwMakeContextCurrent(window);

  if (glewInit()) {
    exit(EXIT_FAILURE);
  }

  glfwSetWindowSizeCallback(window, glfresize_func);
  glfwSetKeyCallback(window, glfkey_func);
  glfwSetCursorPosCallback(window, glfcursor_func);
  glfwSetMouseButtonCallback(window, glfmousebutton_func);
  
  return window;
}

GLuint glfError(GLuint line)
{
  GLenum err = glGetError();

  if (err) {
    printf("GL_ERROR line %d ", line);
  }

  switch(err) {
  case GL_NO_ERROR:
    break;

  case GL_INVALID_ENUM:
    printf("GL_INVALID_ENUM\n");
    break;
    
  case GL_INVALID_VALUE:
    printf("GL_INVALID_VALUE\n");
    break;
    
  case GL_INVALID_OPERATION:
    printf("GL_INVALID_OPERATION\n");
    break;
    
  case GL_INVALID_FRAMEBUFFER_OPERATION:
    printf("GL_INVALID_FRAMEBUFFER_OPERATION\n");
    break;
    
  case GL_OUT_OF_MEMORY:
    printf("GL_OUT_OF_MEMORY\n");
    break;

  default:
    assert(0);
  }

  return err;
}

void glfEnable(GLenum cap)
{
  glEnable(cap);
}

void glfdisable(GLenum cap)
{
  glDisable(cap);
}

void glfClearBuffers(GLint color, GLint depth, GLint stencil)
{
  GLuint mask = 0;
  
  if (color)
    mask |= GL_COLOR_BUFFER_BIT;
  
  if (depth)
    mask |= GL_DEPTH_BUFFER_BIT;
  
  if (stencil)
    mask |= GL_STENCIL_BUFFER_BIT;

  glClear(mask);
}



/****************************************************************************************/
/* Array Buffer support */
/****************************************************************************************/

GLuint glfGenArrayBuffer()
{
  GLuint buffer = 0;

  glGenBuffers(1, &buffer);

  return buffer;
}

void glfBindArrayBuffer(GLuint buffer)
{
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
}

void glfArrayBufferData(CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;
  
  glBufferData(GL_ARRAY_BUFFER, len, data->base_addr, GL_STATIC_DRAW);
}

void glfArrayBufferSubData(size_t offset, CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;
    
  offset *= data->elem_len;

  glBufferSubData(GL_ARRAY_BUFFER, len, offset, data->base_addr);
}



/****************************************************************************************/
/* Element Buffer support */
/****************************************************************************************/

GLuint glfGenElementBuffer()
{
  GLuint buffer = 0;

  glGenBuffers(1, &buffer);

  return buffer;
}

void glfBindElementBuffer(GLuint buffer)
{
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer);
}

void glfElementBufferData(CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;
        
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, len, data->base_addr, GL_STATIC_DRAW);
}

void glfElementBufferSubData(size_t offset, CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;
    
  offset *= data->dim->extent;
  
  glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, len, offset, data->base_addr);
}


/****************************************************************************************/
/* Uniform Buffer support */
/****************************************************************************************/
GLuint glfGenUniformBuffer()
{
  GLuint buffer = 0;

  glGenBuffers(1, &buffer);

  return buffer;
}

void glfUniformBindBufferData(GLuint buffer, CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  assert(data->rank == 0);
  size_t len = data->elem_len;
  
  glBindBuffer(GL_UNIFORM_BUFFER, buffer);
  glBindBufferRange(GL_UNIFORM_BUFFER, 0, buffer, 0, len);
  
  glBufferData(GL_UNIFORM_BUFFER, len, data->base_addr, GL_STREAM_DRAW);
}

void glfUniformBufferData(CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;
  
  glBufferData(GL_UNIFORM_BUFFER, len, data->base_addr, GL_STREAM_DRAW);
}


/****************************************************************************************/
/* Vertex Array Buffer support */
/****************************************************************************************/
GLuint glfGenVAO()
{
  GLuint buffer = 0;

  glGenVertexArrays(1, &buffer);

  return buffer;
}

void glfBindVAO(GLuint buffer)
{
  glBindVertexArray(buffer);
}


/****************************************************************************************/
/* Shader compilation routines */
/****************************************************************************************/
static GLuint _glfCompileShader(const char *vertex_str, const char *fragment_str)
{
  // Create an empty vertex shader handle
  GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);

  // Send the vertex shader source code to GL
  glShaderSource(vertexShader, 1, &vertex_str, 0);

  // Compile the vertex shader
  glCompileShader(vertexShader);

  GLint isCompiled = 0;
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &isCompiled);
  if(isCompiled == GL_FALSE) {
    GLint maxLength = 0;
    glGetShaderiv(vertexShader, GL_INFO_LOG_LENGTH, &maxLength);

    // The maxLength includes the NULL character
    char *error = (char *)malloc(maxLength);
    glGetShaderInfoLog(vertexShader, maxLength, &maxLength, error);

    // We don't need the shader anymore.
    glDeleteShader(vertexShader);

    // Use the infoLog as you see fit.
    printf("Error compiling the vertex shader: %s\n", error);

    // In this simple program, we'll just leave
    return 0;
  }

  // Create an empty fragment shader handle
  GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

  // Send the fragment shader source code to GL
  glShaderSource(fragmentShader, 1, &fragment_str, 0);

  // Compile the fragment shader
  glCompileShader(fragmentShader);

  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &isCompiled);
  if(isCompiled == GL_FALSE) {
    GLint maxLength = 0;
    glGetShaderiv(fragmentShader, GL_INFO_LOG_LENGTH, &maxLength);

    // The maxLength includes the NULL character
    char *error = (char *)malloc(maxLength);
    glGetShaderInfoLog(fragmentShader, maxLength, &maxLength, error);

    // We don't need the shader anymore.
    glDeleteShader(fragmentShader);
    glDeleteShader(vertexShader);

    // Use the infoLog as you see fit.
    printf("Error compiling the fragment shader: %s\n", error);

    // In this simple program, we'll just leave
    return 0;
  }

  // Vertex and fragment shaders are successfully compiled.
  // Now time to link them together into a program.
  // Get a program object.
  GLuint program = glCreateProgram();

  // Attach our shaders to our program
  glAttachShader(program, vertexShader);
  glAttachShader(program, fragmentShader);

  // Link our program
  glLinkProgram(program);

  // Note the different functions here: glGetProgram* instead of glGetShader*.
  GLint isLinked = 0;
  glGetProgramiv(program, GL_LINK_STATUS, (int *)&isLinked);
  if (isLinked == GL_FALSE) {
    GLint maxLength = 0;
    glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);

    // The maxLength includes the NULL character
    char *error = (char *)malloc(maxLength);
    glGetProgramInfoLog(program, maxLength, &maxLength, error);

    // We don't need the program anymore.
    glDeleteProgram(program);
    // Don't leak shaders either.
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    // Use the infoLog as you see fit.

    // In this simple program, we'll just leave
    return 0;
  }

  // Always detach shaders after a successful link.
  glDetachShader(program, vertexShader);
  glDetachShader(program, fragmentShader);

  return program;
}



/****************************************************************************************/
/* GLSL program support */
/****************************************************************************************/

GLuint glfCompileShaders(CFI_cdesc_t *vdata, CFI_cdesc_t *fdata)
{
  char *vertex_shader = f_str_to_c_str(vdata);
  char *fragment_shader = f_str_to_c_str(fdata);
  GLuint program;

  program = _glfCompileShader(vertex_shader, fragment_shader);

  if (program == 0) {
    printf("GLSL compilation failed\n\n%s\n\n%s\n", vertex_shader, fragment_shader);
  }

  printf("%s\n",vertex_shader);
  printf("%s\n",fragment_shader);
  
  free(vertex_shader);
  free(fragment_shader);
  
  if (program) {
      glUseProgram(program);
  }
  
  return program;  
}

GLuint glfCompileShaderFiles(CFI_cdesc_t *vdata, CFI_cdesc_t *fdata)
{
  const int max_str = 64 * 1024;
  char *vertex_filename = f_str_to_c_str(vdata);
  char *fragment_filename = f_str_to_c_str(fdata);
  char *vertex_shader = (char *)malloc(max_str);
  char *fragment_shader = (char *)malloc(max_str);
  size_t file_len;
  GLuint program;

  bzero(vertex_shader, max_str);
  bzero(fragment_shader, max_str);

  // read vertex shader file into a string
  FILE *fp = fopen(vertex_filename, "r");

  if (fp == NULL)
    goto error_exit;

  file_len = fread(vertex_shader, 1, max_str, fp);

  assert(file_len < max_str);

  fclose(fp);

  // read fragment shader into a file
  fp = fopen(fragment_filename, "r");

  if (fp == NULL)
    goto error_exit;

  file_len = fread(fragment_shader, 1, max_str, fp);

  assert(file_len < max_str);

  fclose(fp);

  // compile shader to a program
  program = _glfCompileShader(vertex_shader, fragment_shader);

  if (program == 0) {
    printf("GLSL compilation failed\n\n%s\n\n%s\n", vertex_shader, fragment_shader);
  }

  printf("%s\n",vertex_shader);
  printf("%s\n",fragment_shader);
  
  free(vertex_filename);
  free(fragment_filename);
  
  free(vertex_shader);
  free(fragment_shader);
  
  if (program) {
      glUseProgram(program);
  }
  
  return program;

  
 error_exit:
  
  free(vertex_filename);
  free(fragment_filename);
    
  free(vertex_shader);
  free(fragment_shader);

  return 0;
}


/****************************************************************************************/
/* Vertex binding support PC */
/****************************************************************************************/
GLuint glfBindVertexAttribPointers(CFI_cdesc_t *data)
{
  char *vertex_desc_str = f_str_to_c_str(data);
  char *str = vertex_desc_str;
  GLuint types[32], counts[32];
  size_t offsets[32];
  GLuint max_attrib = 0;
  GLuint vertex_size = 0;
  
  if (strlen(vertex_desc_str) == 0)
    return 1;
      
  for(int i=0; *str; i++) {
    GLuint type_size = 0;
    GLuint type_count = 0;

    offsets[i] = vertex_size;
    
    switch(toupper(*str)) {
    case 'F':
      types[i] = GL_FLOAT;
      type_size = 4;
      break;
      
    case 'D':
      types[i] = GL_DOUBLE;
      type_size = 8;
      break;
      
    case 'I':
      types[i] = GL_INT;
      type_size = 4;
      break;

    default:
      goto error_return;
    }

    if (*str == 0)
      goto error_return;
    
    str++;

    type_count = *str++ - '0';
    counts[i] = type_count;

    vertex_size += type_size * type_count;
    
    max_attrib++;
  }
    
  printf("Vertex Desc %s vertex_size %d attrib_count %d\n", vertex_desc_str, vertex_size, max_attrib);

  for(int i=0; i<max_attrib; i++) {
    if (types[i] == GL_FLOAT)
      glVertexAttribPointer(i, counts[i], GL_FLOAT, GL_FALSE, vertex_size, (void *)offsets[i]);
    else if (types[i] == GL_INT)
      glVertexAttribIPointer(i, counts[i], GL_INT, vertex_size, (void *)offsets[i]);
    else if (types[i] == GL_DOUBLE)
      glVertexAttribLPointer(i, counts[i], GL_DOUBLE, vertex_size, (void *)offsets[i]);      
    else
      assert(0);
    
    glEnableVertexAttribArray(i);
  }

  // disable unused attribs
  GLuint max_gl_attribs = 0;
  glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &max_gl_attribs);

  for(int i=max_attrib; i<max_gl_attribs; i++) {
    glDisableVertexAttribArray(i);
  }
    
  free(vertex_desc_str);
  
  return 0;

 error_return:
  free(vertex_desc_str);
  
  return 1;
}


/****************************************************************************************/
/* Draw calls */
/****************************************************************************************/
void glfDrawArrays(GLuint type, GLuint start, GLuint count)
{
  glDrawArrays(type, start, count);
}

void glfDrawElements(GLuint type, GLuint start, GLuint count, CFI_cdesc_t *data)
{
  assert(data);
  GLuint *indices = (GLuint *)data->base_addr;

  indices = &indices[start];
  
  glDrawElements(type, count, GL_UNSIGNED_INT, indices);
}
