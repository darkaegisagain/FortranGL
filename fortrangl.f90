!
!  Created by Michael Larson
!  January 13 2020
!
!  I don't care how this is used and I am not repsonsible for it's use, just keep
!  my name at the top of the source.
!
!  Thanks
!
!  Michael Larson
!

!****************************************************************************************
! GLSL Types bucket
!****************************************************************************************
module glsl_types
  use, intrinsic :: iso_c_binding
  implicit none

  real(c_float),parameter :: PI = 3.14159

  type, bind(C) :: Vec4
     real(c_float),dimension(1:4) :: v
  end type Vec4

  type, bind(C) :: Mat4
     real(c_float),dimension(1:4,1:4) :: m
  end type Mat4

contains
  function deg2rad(angle)
    use, intrinsic :: iso_c_binding
    real(c_float),intent(in),value :: angle
    real(c_float) :: deg2rad

    deg2rad = angle * PI / 180.0
  end function deg2rad
end module glsl_types



module glf
  use, intrinsic :: iso_c_binding
  use glsl_types
  implicit none

  ! A Vertex is a derived type that the programmer is required to fill in if
  ! they want to use something other than Position / Color information
  type, bind(C) :: Vertex
     real(c_float) :: x, y, z, w
     real(c_float) :: r, g, b, a
  end type Vertex

  integer,parameter :: GL_POINTS = 0
  integer,parameter :: GL_LINES = 1
  integer,parameter :: GL_LINE_LOOP = 2
  integer,parameter :: GL_LINE_STRIP = 3
  integer,parameter :: GL_TRIANGLES = 4
  integer,parameter :: GL_TRIANGLE_STRIP = 5
  integer,parameter :: GL_TRIANGLE_FAN = 6

  integer,parameter :: GL_DEPTH_TEST = 2929
  
  interface
     ! *********************************************************************************
     ! glf window calls
     ! *********************************************************************************
     function glfInit(width, height, major, minor, window_name) bind(C, name="glfInit")
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: glfInit
       integer(c_int),intent(in),value :: width, height, major, minor
       character(len=*),intent(in) :: window_name
     end function glfInit

     subroutine glfSetCurrentContext(window) bind(C, name="glfwMakeContextCurrent")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in),value :: window
     end subroutine glfSetCurrentContext

     integer function glfWindowShouldClose(window) bind(C, name="glfwWindowShouldClose")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in),value :: window
     end function glfWindowShouldClose

     subroutine glfGetWindowSize(window, pWidth, pHeight) bind(C, name="glfwGetWindowSize")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in),value :: window
       integer,pointer :: pWidth, pHeight
     end subroutine glfGetWindowSize

     subroutine glfDestroyWindow(window) bind(C, name="glfwDestroyWindow")
       use, intrinsic :: iso_c_binding
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
       real(c_float),intent(in),value :: red, green, blue, alpha
     end subroutine glfClearColor

     subroutine glfClearBuffers(color, depth, stencil) bind(C, name="glfClearBuffers")
       use, intrinsic :: iso_c_binding
       integer,intent(in),value :: color, depth, stencil
     end subroutine glfClearBuffers

     subroutine glfEnable(enum) bind(C, name="glfEnable")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: enum
     end subroutine glfEnable
     
     subroutine glfDisable(enum) bind(C, name="glfDisable")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: enum
     end subroutine glfDisable
     
       


     ! *********************************************************************************
     ! misc glf calls
     ! *********************************************************************************
     subroutine glfError(line) bind(C, name="glfError")
       use, intrinsic :: iso_c_binding
       integer,intent(in),value :: line
     end subroutine glfError


     ! *********************************************************************************
     ! glf array buffer calls
     ! *********************************************************************************
     function glfGenArrayBuffer() bind(C, name="glfGenArrayBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfGenArrayBuffer
     end function glfGenArrayBuffer

     subroutine glfBindArrayBuffer(buffer) bind(C, name="glfBindArrayBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindArrayBuffer

     subroutine glfArrayBufferData(data) bind(C, name="glfArrayBufferData")
       use, intrinsic :: iso_c_binding
       import Vertex
       type(Vertex),pointer,dimension(:),intent(in) :: data
     end subroutine glfArrayBufferData

     subroutine glfArrayBufferSubData(offset,data) bind(C, name="glfArrayBufferSubData")
       use, intrinsic :: iso_c_binding
       integer(c_long),intent(in),value :: offset
       type(c_ptr),intent(in) :: data
     end subroutine glfArrayBufferSubData



     ! *********************************************************************************
     ! glf element buffer calls
     ! *********************************************************************************
     function glfGenElementBuffer() bind(C, name="glfGenElementBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfGenElementBuffer
     end function glfGenElementBuffer

     subroutine glfBindElementBuffer(buffer) bind(C, name="glfBindElementBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindElementBuffer

     subroutine glfElementBufferData(data) bind(C, name="glfElementBufferData")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in) :: data
     end subroutine glfElementBufferData

     subroutine glfElementBufferSubData(offset,data) bind(C, name="glfElementBufferSubData")
       use, intrinsic :: iso_c_binding
       integer(c_long),intent(in),value :: offset
       type(c_ptr),intent(in) :: data
     end subroutine glfElementBufferSubData



     ! *********************************************************************************
     ! glf uniform buffer calls
     ! *********************************************************************************
     function glfGenUniformBuffer() bind(C, name="glfGenUniformBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfGenUniformBuffer
     end function glfGenUniformBuffer

     subroutine glfBindUniformBuffer(buffer) bind(C, name="glfBindUniformBuffer")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindUniformBuffer

     subroutine glfUniformBufferData(data) bind(C, name="glfUniformBufferData")
       use, intrinsic :: iso_c_binding
       real(c_float),pointer,dimension(:),intent(in) :: data
     end subroutine glfUniformBufferData

     subroutine glfUniformBufferSubData(offset,data) bind(C, name="glfUniformBufferSubData")
       use, intrinsic :: iso_c_binding
       integer(c_long),intent(in),value :: offset
       real(c_float),intent(in) :: data
     end subroutine glfUniformBufferSubData

     subroutine glfUniformBindBufferBase(index, buffer) bind(C, name="glfUniformBindBufferBase")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: index, buffer
     end subroutine glfUniformBindBufferBase

     function glfUniformBlockIndex(glsl_program, uniform_block_str) bind(C, name="glfUniformBlockIndex")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: glsl_program 
       character(len=*),intent(in) :: uniform_block_str
       integer(c_int) :: glfUniformBlockIndex
     end function glfUniformBlockIndex

     subroutine glfUniformBlockBinding(glsl_program, index, binding) bind(C, name="glUniformBlockBinding")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: glsl_program, index, binding
     end subroutine glfUniformBlockBinding

     function glfUniformMaxBufferBindings() bind(C, name="glfUniformMaxBufferBindings")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfUniformMaxBufferBindings
     end function glfUniformMaxBufferBindings



     ! *********************************************************************************
     ! glf vertex array buffer calls
     ! *********************************************************************************
     function glfGenVAO() bind(C, name="glfGenVAO")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfGenVAO
     end function glfGenVAO

     subroutine glfBindVAO(buffer) bind(C, name="glfBindVAO")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: buffer
     end subroutine glfBindVAO



     ! *********************************************************************************
     ! GLSL program calls
     ! *********************************************************************************
     function glfCompileShaderFiles(vertex_filename, fragment_filename) bind(C, name="glfCompileShaderFiles")
       use, intrinsic :: iso_c_binding
       character(len=*),intent(in) :: vertex_filename, fragment_filename
       integer(c_int) :: glfCompileShaderFiles
     end function glfCompileShaderFiles

     ! don't use.... shader strings from fortran to C are a real pain to deal with...
     ! eventually I will write a simple compiler to save these into a __DATA__ section as
     ! strings from file which will make this part of the compilation
     function glfCompileShaders(vertex_str, fragment_str) bind(C, name="glfCompileShaders")
       use, intrinsic :: iso_c_binding
       character(len=*),intent(in) :: vertex_str, fragment_str
       integer(c_int) :: glfCompileShaders
     end function glfCompileShaders



     ! *********************************************************************************
     ! Vertex Descriptor support for vertex attribs
     ! *********************************************************************************
     function glfBindVertexAttribPointers(vertex_desc_str) bind(C, name="glfBindVertexAttribPointers")
       use, intrinsic :: iso_c_binding
       character(len=*),intent(in) :: vertex_desc_str
       integer(c_int) :: glfBindVertexAttribPointers
     end function glfBindVertexAttribPointers



     ! *********************************************************************************
     ! Position Color Vertex support, default format
     ! *********************************************************************************
     function glfBindDefaultPCShaders() bind(C, name="glfBindDefaultPCShaders")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfBindDefaultPCShaders
     end function glfBindDefaultPCShaders

     function glfBindPCVertexAttribPointers() bind(C, name="glfBindPCVertexAttribPointers")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: glfBindPCVertexAttribPointers
     end function glfBindPCVertexAttribPointers



     ! *********************************************************************************
     ! glf draw calls
     ! *********************************************************************************
     subroutine glfDrawArrays(mode, offset, count) bind(C, name="glfDrawArrays")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: mode, offset, count
     end subroutine glfDrawArrays

     subroutine glfDrawElements(mode, offset, count, indices) bind(C, name="glfDrawElements")
       use, intrinsic :: iso_c_binding
       integer(c_int),intent(in),value :: mode, offset, count
       integer(c_int),pointer,dimension(:),intent(in) :: indices
     end subroutine glfDrawElements


     
     ! *********************************************************************************
     ! glf GLU calls
     ! *********************************************************************************
     subroutine glfPerspective(mat, fovy, aspect, near, far) bind(C, name="glfPerspective")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(Mat4),intent(inout),pointer :: mat
       real(c_float),intent(in),value :: fovy, aspect, near, far
     end subroutine glfPerspective

     subroutine glfLookAt(mat, eye, center, up) bind(C, name="glfLookAt")
       use, intrinsic :: iso_c_binding
       import Mat4, Vec4
       type(Mat4),intent(inout),pointer :: mat
       type(Vec4),intent(in),pointer :: eye, center, up
     end subroutine glfLookAt

     subroutine glfCalcMVP(mvp, model, view, projection) bind(C, name="glfCalcMVP")
       use, intrinsic :: iso_c_binding
       import Mat4, Vec4
       type(Mat4),intent(inout),pointer :: mvp
       type(Mat4),intent(in),pointer :: model, view, projection
     end subroutine glfCalcMVP

     
     subroutine glfIdentity(mat) bind(C, name="glfIdentity")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(Mat4),intent(inout),pointer :: mat
     end subroutine glfIdentity
       
     subroutine glfRotate(mat, angle, x, y, z) bind(C, name="glfRotate")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(Mat4),intent(inout),pointer :: mat
       real(c_float),intent(in),value :: angle
       real(c_float),intent(in),value :: x, y, z
     end subroutine glfRotate
       
     subroutine glfTranslate(mat, x, y, z) bind(C, name="glfTranslate")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(Mat4),intent(inout),pointer :: mat
       real(c_float),intent(in),value :: x, y, z
     end subroutine glfTranslate
       
     subroutine glfScale(mat, x, y, z) bind(C, name="glfScale")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(Mat4),intent(inout),pointer :: mat
       real(c_float),intent(in),value :: x, y, z
     end subroutine glfScale

     subroutine glfDumpFramebufferToTGA(window, filename) bind(C, name="glfDumpFramebufferToTGA")
       use, intrinsic :: iso_c_binding
       import Mat4
       type(c_ptr),intent(in),value :: window
       character(len=*),intent(in) :: filename
     end subroutine glfDumpFramebufferToTGA
     
    end interface


contains
  subroutine glfSetVertexPos(buffer, index, x, y, z, w)
    use, intrinsic :: iso_c_binding
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
    integer(c_double),intent(in),value :: key, scancode, action, mods

    !if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) then
    !  glfSetWindowShouldClose(window);
    !end if
  end subroutine glfkey_func

  subroutine glfcursor_func(window, xpos, ypos) bind(C, name="glfcursor_func")
    use,intrinsic :: iso_c_binding
    type(c_ptr),intent(in),pointer :: window
    integer(c_double),intent(in),value :: xpos, ypos

    !print *, xpos, ypos
  end subroutine glfcursor_func

  subroutine glfmousebutton_func(window, button, action, mods) bind(C, name="glfmousebutton_func")
    use,intrinsic :: iso_c_binding
    type(c_ptr),intent(in),pointer :: window
    integer(c_int),intent(in),value :: button, action, mods

    !print *, button, action, mods
  end subroutine glfmousebutton_func
end module glf


