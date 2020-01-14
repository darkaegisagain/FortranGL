
module glf
  use, intrinsic :: iso_c_binding
  type, bind(C) :: PC_Vertex
     real(c_float) :: x, y, z, w
     real(c_float) :: r, g, b, a
  end type PC_Vertex
  
  interface
     function glfInit(width, height) bind(C, name="glfInit")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr) :: glfInit
       integer(c_int),intent(in),value :: width
       integer(c_int),intent(in),value :: height
     end function glfInit

     subroutine glfSetCurrentContext(window) bind(C, name="glfSetCurrentContext")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end subroutine glfSetCurrentContext
     
     integer function glfWindowShouldClose(window) bind(C, name="glfWindowShouldClose")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end function glfWindowShouldClose
     
     subroutine glfGetWindowSize(window, pWidth, pHeight) bind(C, name="glfGetWindowSize")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
       integer,pointer :: pWidth, pHeight
     end subroutine glfGetWindowSize

     subroutine glfDestroyWindow(window) bind(C, name="glfDestroyWindow")
       use, intrinsic :: iso_c_binding
       implicit none
       type(c_ptr),intent(in),value :: window
     end subroutine glfDestroyWindow


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

     subroutine glfSwapBuffers(window) bind(C, name="glfSwapBuffers")
       use, intrinsic :: iso_c_binding
       type(c_ptr),intent(in),value :: window
     end subroutine glfSwapBuffers     

     subroutine glfPollEvents() bind(C, name="glfPollEvents")
     end subroutine glfPollEvents



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
       import PC_Vertex
       implicit none
       type(PC_Vertex),pointer,dimension(:),intent(in) :: data
     end subroutine glfArrayBufferData

     subroutine glfArrayBufferSubData(offset,data) bind(C, name="glfArrayBufferSubData")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_long),intent(in),value :: offset
       type(c_ptr),intent(in) :: data
     end subroutine glfArrayBufferSubData


     
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



     
     subroutine glfDrawTriangleArrays(offset, count) bind(C, name="glfDrawTriangleArrays")
       use, intrinsic :: iso_c_binding
       implicit none
       integer(c_int),intent(in),value :: offset, count
     end subroutine glfDrawTriangleArrays

     
     ! Position Color Vertex support, default format
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
  end interface

  
  contains
    subroutine glfSetPCVertexPos(buffer, index, x, y, z, w)
      use, intrinsic :: iso_c_binding
      implicit none
      type(PC_Vertex),pointer,dimension(:) :: buffer
      integer :: index
      real(c_float) :: x, y, z, w
    
      buffer(index)%x = x
      buffer(index)%y = y
      buffer(index)%z = z
      buffer(index)%w = w
    end subroutine glfSetPCVertexPos

    subroutine glfSetPCVertexCol(buffer, index, r, g, b, a)
      use, intrinsic :: iso_c_binding
      implicit none
      type(PC_Vertex),pointer,dimension(:) :: buffer
      integer :: index
      real(c_float) :: r, g, b, a
    
      buffer(index)%r = r
      buffer(index)%g = g
      buffer(index)%b = b
      buffer(index)%a = a
    end subroutine glfSetPCVertexCol

end module glf


Program test_fortrangl
  use glf
  use gl
  use iso_c_binding

  implicit none
  type(c_ptr) :: window
  type(PC_Vertex),pointer,dimension(:) :: vertex_buffer
  integer(c_int) :: err, buffer, index_buffer, vao_buffer, program

  window = glfInit(640, 480)

  program = glfBindDefaultPCShaders()
  if (program == 0) stop

  allocate(vertex_buffer(1:4096))

  call glfSetPCVertexPos(vertex_buffer, 1, -0.5, -0.5, 0.0, 1.0)
  call glfSetPCVertexPos(vertex_buffer, 2,  0.5, -0.5, 0.0, 1.0)
  call glfSetPCVertexPos(vertex_buffer, 3,  0.0,  0.5, 0.0, 1.0)

  call glfSetPCVertexCol(vertex_buffer, 1,  1.0,  0.0, 0.0, 1.0)
  call glfSetPCVertexCol(vertex_buffer, 2,  0.0,  1.0, 0.0, 1.0)
  call glfSetPCVertexCol(vertex_buffer, 3,  0.0,  0.0, 1.0, 1.0)


  buffer = glfGenArrayBuffer()
  if (buffer == 0) stop
  call glfBindArrayBuffer(buffer)

  vao_buffer = glfGenVAO()
  if (vao_buffer == 0) stop
  call glfBindVAO(vao_buffer)
  

  call glfArrayBufferData(vertex_buffer)
 
  err = glfBindPCVertexAttribPointers() 
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
