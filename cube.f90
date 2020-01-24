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
! Shader data routines for uniform buffers
!****************************************************************************************
module shader_data
  use, intrinsic :: iso_c_binding
  use glsl_types
  use glf
  implicit none

  ! Shader data for the vertex shader
  ! A UBO is used to transfer this data to the shader
  type, bind(C) :: ShaderData
     type(Mat4) :: MVP
  end type ShaderData

  interface
     ! we have to have this routine to match types even though it will be a memcpy
     ! from a void pointer in C
     ! I could probably figure out how to cast it in Fortran but this is much easier
     subroutine ShaderBufferData(ubo, data) bind(C, name="glfUniformBindBufferData")
       use, intrinsic :: iso_c_binding
       import ShaderData
       integer(c_int),intent(in),value :: ubo
       type(ShaderData),pointer,intent(in) :: data
     end subroutine ShaderBufferData
  end interface
end module shader_data


!****************************************************************************************
! Geometry support
!****************************************************************************************
module geometry
  use glf
  use iso_c_binding
  implicit none

contains
  function makeCube(start, vertex_buffer)
    integer,intent(in) :: start
    type(Vertex),pointer,dimension(:),intent(in) :: vertex_buffer
    integer :: makeCube
    
    call glfSetVertexPos(vertex_buffer, 0, -0.5, -0.5, -0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 1, -0.5,  0.5, -0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 2,  0.5,  0.5, -0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 3,  0.5, -0.5, -0.5, 1.0)

    call glfSetVertexPos(vertex_buffer, 4, -0.5, -0.5,  0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 5, -0.5,  0.5,  0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 6,  0.5,  0.5,  0.5, 1.0)
    call glfSetVertexPos(vertex_buffer, 7,  0.5, -0.5,  0.5, 1.0)


    
    call glfSetVertexCol(vertex_buffer, 0,  1.0, 0.0, 0.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 1,  1.0, 1.0, 0.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 2,  1.0, 1.0, 1.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 3,  1.0, 0.0, 1.0, 1.0)
    
    call glfSetVertexCol(vertex_buffer, 4,  0.0, 0.0, 0.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 5,  0.0, 1.0, 0.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 6,  0.0, 1.0, 1.0, 1.0)
    call glfSetVertexCol(vertex_buffer, 7,  0.0, 0.0, 1.0, 1.0)

    makeCube = start + 8
  end function makeCube
  
  function makeFace(start, indices, v0, v1, v2, v3)
    integer,intent(in) :: start, v0, v1, v2, v3
    integer,pointer,dimension(:),intent(in) :: indices
    integer :: makeFace
    !local variables
    integer :: i

    i = start

    ! two triangles clockwise
    indices(i) = v0
    indices(i + 1) = v1
    indices(i + 2) = v2
    
    indices(i + 3) = v0
    indices(i + 4) = v2
    indices(i + 5) = v3

    makeFace = start + 6
  end function makeFace

  function makeCubeIndices(start, indices)
    integer,intent(in) :: start
    integer,pointer,dimension(:),intent(in) :: indices
    integer :: makeCubeIndices
    !local variables
    integer :: i

    i = start

    i = makeFace(i, indices, 0, 1, 2, 3)
    i = makeFace(i, indices, 4, 5, 1, 0)
    i = makeFace(i, indices, 7, 6, 5, 4)
    
    i = makeFace(i, indices, 3, 2, 6, 7)
    i = makeFace(i, indices, 1, 5, 6, 2)
    i = makeFace(i, indices, 0, 3, 7, 4)

    makeCubeIndices = i
  end function makeCubeIndices
end module geometry


!****************************************************************************************
! cube program
!****************************************************************************************
Program cube
  use glf
  use shader_data
  use geometry
  use iso_c_binding
  implicit none
  
  !local variables
  type(c_ptr) :: window
  type(Vertex),pointer,dimension(:) :: vertex_buffer
  type(ShaderData),pointer :: uniform_buffer
  integer(c_int) :: err, buffer, vao, ubo, program
  integer :: vertex_count, index_count
  integer,pointer,dimension(:) :: index_buffer
  type(Vec4),pointer :: eye, center, up
  real(c_float) :: fov, aspect
  type(Mat4),pointer :: Model, View, Projection, MVP
  real(c_float) :: angle
  
  
  ! Request a 640x480 window with a OpenGL context of 3.3 minium and "FortranGL" as the name
  window = glfInit(640, 480, 4, 2, "Cube FortranGL")

  ! compile shaders returns 0 on failure a valid program on success
  ! the program is bound as the currently in use program
  program = glfCompileShaderFiles("default_vertex.vert", "default_fragment.frag")
  if (program == 0) stop

  ! allocate and set vertices for a RGB cube
  allocate(vertex_buffer(0:4095))
  vertex_count = makeCube(0, vertex_buffer)

  ! allocate and set incides for a RGB cube
  allocate(index_buffer(0:1023))
  index_count = makeCubeIndices(0, index_buffer)
  
  ! Bind array buffer before creating VAO
  buffer = glfGenArrayBuffer()
  if (buffer == 0) stop
  call glfBindArrayBuffer(buffer)

  ! OpenGL 3.3 and beyond requires a VAO binding for draw operations
  vao = glfGenVAO()
  if (vao == 0) stop
  call glfBindVAO(vao)
  
  ! Array buffer data takes a pointer descriptor and figures out how big
  ! the allocation is so size isn't needed
  call glfArrayBufferData(vertex_buffer)

  ! A vertex descriptor in the form of F4F4 says you have 2 attributes
  ! Attribute 0 is a Float 4 config with 4 floats
  ! Attribute 1 is a Float 4 config with 4 floats
  ! You can use F,I,D for GL_FLOAT,GL_INTEGER,GL_DOUBLE
  ! You can use up to (GL_MAX_VERTEX_ATTRIBS) attribs
  ! The type, type_count and number of attribs forms the pitch
  ! for the vertex pointer all
  err = glfBindVertexAttribPointers("F4F4") 
  if (err /= 0) stop

  ! Unform buffer
  allocate(uniform_buffer)

  ! allocate camera matrices, should just adopt a simple camera model
  allocate(model)
  allocate(view)
  allocate(projection)
  allocate(MVP)
  
  call glfIdentity(model)
  call glfIdentity(view)
  call glfIdentity(projection)

  ! projection matrix
  fov = deg2rad(60.0)
  aspect = 640.0 / 480.0
  call glfPerspective(projection, fov, aspect, 0.1, 100.0)

  ! view matrix
  allocate(eye)
  allocate(center)
  allocate(up)

  eye%v = (/0.0, 0.0, -2.0, 1.0/)
  center%v = (/0.0, 0.0, 0.0, 1.0/)
  up%v = (/0.0, 1.0, 0.0, 1.0/)
  call glfLookAt(view, eye, center, up)

  call glfCalcMVP(MVP, model, view, projection)
  
  uniform_buffer%MVP = MVP
  
  ubo = glfGenUniformBuffer()
  if (ubo == 0) stop
  
  ! by default bind uniform buffer binds to index 0 in the vertex shader
  call ShaderBufferData(ubo, uniform_buffer)

  call glfError(1);
  
  ! Set clear color for clear buffers call
  call glfClearColor(0.0, 0.0, 0.0, 1.0)

  call glfEnable(GL_DEPTH_TEST)

  angle = 0.0
  do while (glfWindowShouldClose(window) == 0)
     call glfSetCurrentContext(window)

     call glfIdentity(model)
     call glfRotate(model, deg2rad(angle), 1, 1, 1)
     angle = angle + 1
     
     call glfCalcMVP(MVP, model, view, projection)
     uniform_buffer%MVP = MVP
     call ShaderBufferData(ubo, uniform_buffer)

     call glfClearBuffers(1,1,0)

     call glfDrawElements(GL_TRIANGLES, 0, index_count, index_buffer)
     call glfError(2)
     
     call glfSwapBuffers(window)
     call glfPollEvents()
  end do

End Program cube

