# FortranGL
Interface to OpenGL 4.5+ using GLFW and GLEW on C side with Fortran interface routines

This is intended to be copied and modified, there are too many type requirements to really make a generalized interface with OpenGL 4.5 and beyond. The C interface routines combine a bunch of mondane calls so you don't have to define each one... just
call them with an interace function.

The simplest case is the cube.f90
- Init GLF with a window and a minium OpenGL context of 4.5
- Compile your shader files
- Create a vertex buffer and build a cube in it along with the indices to describe the triangles
- Create a vertex buffer object to describe your vertex array (these are required beyond GL 3.3)
- Create a vertex buffer object in GL to hold your vertex information
- Use array buffer data to transfer your cube to the vertex buffer. Notes on this you need to define a Vertex type
  in fortrangl.f90, I should probably move this out and require it to be done locally in the application as Vertex types
  will differ from app to app. The default holds a position and a color.
- Create your model / view / projection matrix and bind / load the MVP matrix to a uniform buffer object
- Bind your vertex pointers using a descriptor string "F4F4" is used to describe a position / color vertex
- Start you drawing loop in GLFW

- FortranGL has detault callbacks to handle mouse / keyboard / window resize input in FortranGL.f90, these are bound in
  the initGLF routine so hack away and add your own.
  
  
