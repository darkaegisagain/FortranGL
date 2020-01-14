#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <ISO_Fortran_binding.h>

#ifndef __DEBUG__
#define __DEBUG__ 1
#endif

typedef struct {
  float x, y, z, w;
  float r, g, b, a;
} PC_Vertex;

char pc_vertex_shader[] =
  "#version 330 core\n"
  "layout (location = 0) in vec4 aPos;   // the position variable has attribute position 0\n"
  "layout (location = 1) in vec4 aColor; // the color variable has attribute position 1\n" 
  "out vec4 ourColor; // output a color to the fragment shader\n"
  "void main()\n"
  "{\n"
  "    gl_Position = aPos;\n"
  "    ourColor = aColor; // set ourColor to the input color we got from the vertex data\n"
  "}\n";

char pc_fragment_shader[] =
  "#version 330 core\n"
  "out vec4 FragColor;\n"
  "in vec4 ourColor;\n"
  "void main()\n"
  "{\n"
  "    FragColor = ourColor;\n"
  "}\n";

static void error_callback(int error, const char* description)
{
    fputs(description, stderr);
}

static void key_callback(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
}

GLFWwindow *glfInit(int width, int height)
{
  GLFWwindow *window;

  glfwSetErrorCallback(error_callback);
  if (!glfwInit())
    {
      exit(EXIT_FAILURE);
    }
  
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  #ifdef __APPLE__
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  #endif
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  window = glfwCreateWindow(640, 480, "FortranGL", NULL, NULL);
  if (!window)
    {
      glfwTerminate();
      exit(EXIT_FAILURE);
    }

  glfwMakeContextCurrent(window);

  if (glewInit())
    {
      exit(EXIT_FAILURE);
    }
  
  return window;
}

void glfSetCurrentContext(GLFWwindow *window)
{
  glfwMakeContextCurrent(window);
}

void glfSwapBuffers(GLFWwindow *window)
{
  glfwSwapBuffers(window);
}

int glfWindowShouldClose(GLFWwindow *window)
{
  return glfwWindowShouldClose(window);
}

void glfGetWindowSize(GLFWwindow *window, int *width, int *height)
{
  glfwGetWindowSize(window, width, height);
}

void glfDestroyWindow(GLFWwindow *window)
{
  glfwDestroyWindow(window);
}

void glfPollEvents()
{
  glfwPollEvents();
}


#if __DEBUG__
GLuint _glfError(const char *function, int line)
{
  GLenum err = glGetError();

  if (err) {
    printf("%s:%d ", function, line);
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
#else
#define _glfError(function, line)
#endif

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

GLuint glfGenArrayBuffer()
{
  GLuint buffer = 0;

  glGenBuffers(1, &buffer);

  printf("%s %d\n",__FUNCTION__, buffer);
  
  return buffer;
}

void glfBindArrayBuffer(GLuint buffer)
{
  printf("%s %d\n",__FUNCTION__, buffer);

  glBindBuffer(GL_ARRAY_BUFFER, buffer);

  _glfError(__FUNCTION__, __LINE__);
}

void glfArrayBufferData(CFI_cdesc_t *data)
{
  assert(data);
  assert(data->dim);
  size_t len = data->elem_len;
  len *= data->dim->extent;

  #if 0
  PC_Vertex *verts = (PC_Vertex *)data->base_addr;
  for(int i=0; i<3; i++)
    {
      printf("Vert[%d] pos:%f,%f,%f,%f col:%f,%f,%f,%f\n", i,
	     verts[i].x,verts[i].y,verts[i].z,verts[i].w,
	     verts[i].r,verts[i].g,verts[i].b,verts[i].a);
    }
  #endif
  
  glBufferData(GL_ARRAY_BUFFER, len, data->base_addr, GL_STATIC_DRAW);

  _glfError(__FUNCTION__, __LINE__);
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

  _glfError(__FUNCTION__, __LINE__);
}


GLuint glfGenVAO()
{
  GLuint buffer = 0;

  glGenVertexArrays(1, &buffer);

  printf("%s %d\n",__FUNCTION__, buffer);

  return buffer;
}

void glfBindVAO(GLuint buffer)
{
  printf("%s %d\n",__FUNCTION__, buffer);

  glBindVertexArray(buffer);
  
  _glfError(__FUNCTION__, __LINE__);
}


void glfDrawTriangleArrays(GLuint start, GLuint count)
{
  glDrawArrays(GL_TRIANGLES, start, count);
}

// PCVertex calls
GLuint glfBindPCVertexAttribPointers()
{
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(PC_Vertex), (void *)offsetof(PC_Vertex, x));
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(PC_Vertex), (void *)offsetof(PC_Vertex, r));
 
  _glfError(__FUNCTION__, __LINE__);
   
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  
  _glfError(__FUNCTION__, __LINE__);
    
  return 0;
}


GLuint _glfCompileShader(const char *vertex_str, const char *fragment_str)
{
  // Create an empty vertex shader handle
  GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);

  // Send the vertex shader source code to GL
  glShaderSource(vertexShader, 1, &vertex_str, 0);

  // Compile the vertex shader
  glCompileShader(vertexShader);

  GLint isCompiled = 0;
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &isCompiled);
  if(isCompiled == GL_FALSE)
    {
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
      return -1;
    }

  // Create an empty fragment shader handle
  GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);

  _glfError(__FUNCTION__, __LINE__);
    
  // Send the fragment shader source code to GL
  glShaderSource(fragmentShader, 1, &fragment_str, 0);

  _glfError(__FUNCTION__, __LINE__);
    
  // Compile the fragment shader
  glCompileShader(fragmentShader);

  _glfError(__FUNCTION__, __LINE__);
    
  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &isCompiled);
  if(isCompiled == GL_FALSE)
    {
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
      return -1;
    }

  _glfError(__FUNCTION__, __LINE__);
    
  // Vertex and fragment shaders are successfully compiled.
  // Now time to link them together into a program.
  // Get a program object.
  GLuint program = glCreateProgram();

  // Attach our shaders to our program
  glAttachShader(program, vertexShader);
  glAttachShader(program, fragmentShader);

  _glfError(__FUNCTION__, __LINE__);
    
  // Link our program
  glLinkProgram(program);

  _glfError(__FUNCTION__, __LINE__);
    
  // Note the different functions here: glGetProgram* instead of glGetShader*.
  GLint isLinked = 0;
  glGetProgramiv(program, GL_LINK_STATUS, (int *)&isLinked);
  if (isLinked == GL_FALSE)
    {
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
	return -1;
    }

  _glfError(__FUNCTION__, __LINE__);
    
  // Always detach shaders after a successful link.
  glDetachShader(program, vertexShader);
  glDetachShader(program, fragmentShader);

  _glfError(__FUNCTION__, __LINE__);
    
  return program;
}

int glfBindDefaultPCShaders()
{
  GLuint program;
  
  //printf("OpenGL shader language version: %s\n", glGetString(GL_SHADING_LANGUAGE_VERSION));

  program = _glfCompileShader(pc_vertex_shader, pc_fragment_shader);

  if (program)
    {
      glUseProgram(program);
    }
  
  return program;
}


