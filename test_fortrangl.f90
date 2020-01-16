
module glf
  use, intrinsic :: iso_c_binding
  type, bind(C) :: Vertex
     real(c_float) :: x, y, z, w
     real(c_float) :: r, g, b, a
  end type Vertex

  interface
     ! *********************************************************************************
     ! glf window calls
     ! *********************************************************************************
     function glfInit(width, height) bind(C, name="glfInit")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr) :: glfInit
       integer(c_int),intent(in),value :: width
       integer(c_int),intent(in),value :: height
     end function glfInit

     subroutine glfSetCurrentContext(window) bind(C, name="glfwMakeContextCurrent")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end subroutine glfSetCurrentContext
     
     integer function glfWindowShouldClose(window) bind(C, name="glfwWindowShouldClose")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end function glfWindowShouldClose
     
     subroutine glfGetWindowSize(window, pWidth, pHeight) bind(C, name="glfwGetWindowSize")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
       integer,pointer :: pWidth, pHeight
     end subroutine glfGetWindowSize

     subroutine glfDestroyWindow(window) bind(C, name="glfwDestroyWindow")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end subroutine glfDestroyWindow

     subroutine glfSwapBuffers(window) bind(C, name="glfwSwapBuffers")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in),value :: window
     end subroutine glfSwapBuffers     

     subroutine glfPollEvents() bind(C, name="glfwPollEvents")
     end subroutine glfPollEvents



     ! *********************************************************************************
     ! native opengl calls
     ! *********************************************************************************
     subroutine glfViewport(x, y, width, height) bind(C, name="glViewport")
       use, intrinsic :: iso_c_binding
       integer,intent(in),value :: x, y, width, height
     end subroutine glfViewport
     
     subroutine glfClearColor(red, green, blue, alpha) bind(C, name="glClearColor")
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_float),intent(in),value :: red, green, blue, alpha
     end subroutine glfClearColor

     subroutine glfClearBuffers(color, depth, stencil) bind(C, name="glfClearBuffers")
       use, intrinsic :: iso_c_binding
       implicit none
       integer,intent(in),value :: color, depth, stencil
     end subroutine glfClearBuffers



     ! *********************************************************************************
     ! glf array buffer calls
     ! *********************************************************************************
     function glfGenArrayBuffer() bind(C, name="glfGenArrayBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfGenArrayBuffer
     end function glfGenArrayBuffer
     
     subroutine glfBindArrayBuffer(buffer) bind(C, name="glfBindArrayBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindArrayBuffer
     
     subroutine glfArrayBufferData(data) bind(C, name="glfArrayBufferData")
       use, intrinsic :: iso_c_binding
       import Vertex
       implicit none
       type(Vertex),pointer,dimension(:),intent(in) :: data
     end subroutine glfArrayBufferData

     subroutine glfArrayBufferSubData(offset,data) bind(C, name="glfArrayBufferSubData")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_long),intent(in),value :: offset
       type(c_ptr),intent(in) :: data
     end subroutine glfArrayBufferSubData


     
     ! *********************************************************************************
     ! glf element buffer calls
     ! *********************************************************************************
     function glfGenElementBuffer() bind(C, name="glfGenElementBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfGenElementBuffer
     end function glfGenElementBuffer
     
     subroutine glfBindElementBuffer(buffer) bind(C, name="glfBindElementBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindElementBuffer

     subroutine glfElementBufferData(data) bind(C, name="glfElementBufferData")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in) :: data
     end subroutine glfElementBufferData
 
     subroutine glfElementBufferSubData(offset,data) bind(C, name="glfElementBufferSubData")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_long),intent(in),value :: offset
       type(c_ptr),intent(in) :: data
     end subroutine glfElementBufferSubData
 

     ! *********************************************************************************
     ! glf uniform buffer calls
     ! *********************************************************************************
     function glfGenUniformBuffer() bind(C, name="glfGenUniformBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfGenUniformBuffer
     end function glfGenUniformBuffer
     
     subroutine glfUniformBindBuffer(buffer) bind(C, name="glfBindUniformBuffer")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: buffer
     end subroutine glfUniformBindBuffer
     
     subroutine glfUniformBufferData(data) bind(C, name="glfUniformBufferData")
       use, intrinsic :: iso_c_binding
       implicit none
       real(c_float),pointer,dimension(:),intent(in) :: data
     end subroutine glfUniformBufferData

     subroutine glfUniformBufferSubData(offset,data) bind(C, name="glfUniformBufferSubData")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_long),intent(in),value :: offset
       real(c_float),intent(in) :: data
     end subroutine glfUniformBufferSubData

     subroutine glfUniformBindBufferBase(index, buffer) bind(C, name="glfUniformBindBufferBase")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: index, buffer
     end subroutine glfUniformBindBufferBase

     function glfUniformBlockIndex(glsl_program, uniform_block_str) bind(C, name="glfUniformBlockIndex")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: glsl_program 
       character(len=*),intent(in) :: uniform_block_str
       integer(c_int) :: glfUniformBlockIndex
     end function glfUniformBlockIndex

     function glfUniformMaxBufferBindings() bind(C, name="glfUniformMaxBufferBindings")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfUniformMaxBufferBindings
     end function glfUniformMaxBufferBindings

     

     ! *********************************************************************************
     ! glf vertex array buffer calls
     ! *********************************************************************************
     function glfGenVAO() bind(C, name="glfGenVAO")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfGenVAO
     end function glfGenVAO

     subroutine glfBindVAO(buffer) bind(C, name="glfBindVAO")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindVAO



     ! *********************************************************************************
     ! GLSL programm calls
     ! *********************************************************************************
     function glfCompileShaders(vertex_str, fragment_str) bind(C, name="glfCompileShaders")
       use, intrinsic :: iso_c_binding
       implicit none
       character(len=*),intent(in) :: vertex_str, fragment_str
       integer(c_int) :: glfCompileShaders
     end function glfCompileShaders

          
     
     ! *********************************************************************************
     ! Vertex Descriptor support for vertex attribs
     ! *********************************************************************************
     function glfBindVertexAttribPointers(vertex_desc_str) bind(C, name="glfBindVertexAttribPointers")
       use, intrinsic :: iso_c_binding
       implicit none
       character(len=*),intent(in) :: vertex_desc_str
       integer(c_int) :: glfBindVertexAttribPointers
     end function glfBindVertexAttribPointers


     
     ! *********************************************************************************
     ! Position Color Vertex support, default format
     ! *********************************************************************************
     function glfBindDefaultPCShaders() bind(C, name="glfBindDefaultPCShaders")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfBindDefaultPCShaders
     end function glfBindDefaultPCShaders

     function glfBindPCVertexAttribPointers() bind(C, name="glfBindPCVertexAttribPointers")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int) :: glfBindPCVertexAttribPointers
     end function glfBindPCVertexAttribPointers


     
     ! *********************************************************************************
     ! glf draw calls
     ! *********************************************************************************
     subroutine glfDrawTriangleArrays(offset, count) bind(C, name="glfDrawTriangleArrays")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: offset, count
     end subroutine glfDrawTriangleArrays

  end interface


  
  contains
    subroutine glfSetVertexPos(buffer, index, x, y, z, w)
      use, intrinsic :: iso_c_binding
      implicit none
      type(Vertex),pointer,dimension(:) :: buffer
      integer :: index
      real(c_float) :: x, y, z, w
    
      buffer(index)%x = x
      buffer(index)%y = y
      buffer(index)%z = z
      buffer(index)%w = w
    end subroutine glfSetVertexPos

    subroutine glfSetVertexCol(buffer, index, r, g, b, a)
      use, intrinsic :: iso_c_binding
      implicit none
      type(Vertex),pointer,dimension(:) :: buffer
      integer :: index
      real(c_float) :: r, g, b, a
    
      buffer(index)%r = r
      buffer(index)%g = g
      buffer(index)%b = b
      buffer(index)%a = a
    end subroutine glfSetVertexCol


    ! *********************************************************************************
    ! glf event handler calls
    ! *********************************************************************************
    subroutine glfresize_func(window, width, height) bind(C, name="glfresize_func")
      use,intrinsic :: iso_c_binding
      type(c_ptr),intent(in),pointer :: window
      integer(c_int),intent(in),value :: width, height

      print *, width, height

      call glfViewport(0, 0, width, height)
    end subroutine glfresize_func

    subroutine glfkey_func(window, key, scancode, action, mods) bind(C, name="glfkey_func")
      use,intrinsic :: iso_c_binding
      type(c_ptr),intent(in),pointer :: window
      integer(c_double),intent(in),value :: scancode, action, mods

      print *, key, scancode, action, mods

      !if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) then
      !  glfSetWindowShouldClose(window);
      !end if
    end subroutine glfkey_func

    subroutine glfcursor_func(window, xpos, ypos) bind(C, name="glfcursor_func")
      use,intrinsic :: iso_c_binding
      type(c_ptr),intent(in),pointer :: window
      integer(c_double),intent(in),value :: xpos, ypos

      print *, xpos, ypos
    end subroutine glfcursor_func

    subroutine glfmousebutton_func(window, button, action, mods) bind(C, name="glfmousebutton_func")
      use,intrinsic :: iso_c_binding
      type(c_ptr),intent(in),pointer :: window
      integer(c_int),intent(in),value :: button, action, mods

      print *, button, action, mods
    end subroutine glfmousebutton_func
end module glf


module fgl_tests

end module fgl_tests


Program test_fortrangl
  use glf
  use fgl_tests
  use iso_c_binding
  
  implicit none
  type(c_ptr) :: window
  type(Vertex),pointer,dimension(:) :: vertex_buffer
  integer(c_int) :: err, buffer, index_buffer, vao_buffer, program, ubo_block_index

  character(len=4096) :: vertex_shader, fragment_shader
  character (len=*),parameter :: NL = char(10) !hack for #version requiring a /n char for parser

  vertex_shader = "#version 330 core"//NL//"&
  &layout (location = 0) in vec4 aPos;   /* the position variable has attribute position 0 */ &
  &layout (location = 1) in vec4 aColor; /* the color variable has attribute position 1 */ &
  &out vec4 ourColor; // output a color to the fragment shader "//NL//"&
  &void main() &
  &{ &
  &gl_Position = aPos; &
  &ourColor = aColor; &
  &}"

  fragment_shader = "#version 330 core"//NL//"&
  &out vec4 FragColor; &
  &in vec4 ourColor; &
  &void main() &
  &{ &
  &FragColor = ourColor; &
  &}"

  window = glfInit(640, 480)

  program = glfCompileShaders(vertex_shader, fragment_shader)
  if (program == 0) stop

  ubo_block_index = glfUniformBlockIndex(program, "uniform_data")
  
  allocate(vertex_buffer(1:4096))

  call glfSetVertexPos(vertex_buffer, 1, -0.5, -0.5, 0.0, 1.0)
  call glfSetVertexPos(vertex_buffer, 2,  0.5, -0.5, 0.0, 1.0)
  call glfSetVertexPos(vertex_buffer, 3,  0.0,  0.5, 0.0, 1.0)

  call glfSetVertexCol(vertex_buffer, 1,  1.0,  0.0, 0.0, 1.0)
  call glfSetVertexCol(vertex_buffer, 2,  0.0,  1.0, 0.0, 1.0)
  call glfSetVertexCol(vertex_buffer, 3,  0.0,  0.0, 1.0, 1.0)


  buffer = glfGenArrayBuffer()
  if (buffer == 0) stop
  call glfBindArrayBuffer(buffer)

  vao_buffer = glfGenVAO()
  if (vao_buffer == 0) stop
  call glfBindVAO(vao_buffer)
  

  call glfArrayBufferData(vertex_buffer)
 
  err = glfBindVertexAttribPointers("F4F4") 
  if (err /= 0) stop
  
  call glfClearColor(0.0, 0.0, 0.0, 1.0)

  do while (glfWindowShouldClose(window) == 0)
     call glfSetCurrentContext(window)
     call glfClearBuffers(1,1,0)

     call glfDrawTriangleArrays(0, 3)
     
     call glfSwapBuffers(window)
     call glfPollEvents()
  end do

End Program test_fortrangl
