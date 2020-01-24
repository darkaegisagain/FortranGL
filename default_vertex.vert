#version 430

layout (binding = 0, column_major) uniform shader_data_t {
  mat4 mvp;
} shader_data;

layout (location = 0) in vec4 aPos;   	/* the position variable has attribute position 0 */
layout (location = 1) in vec4 aColor; 	/* the color variable has attribute position 1 */


out vec4 ourColor;    	      		/* output a color to the fragment shader */

void main()
{
  gl_Position = shader_data.mvp * aPos;
  
  ourColor = aColor;
}
