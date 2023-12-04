! Copyright (c) 2012-2013, 2019 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2013-2014, 2022 Harald Klimach <harald.klimach@dlr.de>
! Copyright (c) 2016 Tobias Girresser <tobias.girresser@student.uni-siegen.de>
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this
! list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
! this list of conditions and the following disclaimer in the documentation
! and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ******************************************************************************
!> author: Kannan Masilamani
!! This module contains spacer definition and routines related to spacers

?? include 'arrayMacros.inc'

module sdr_spacer_module
  use env_module,                only: rk, minLength, zeroLength
  use tem_param_module,          only: PI
  use tem_aux_module,            only: tem_abort
  use tem_logging_module,        only: logunit
  use tem_cylinder_module,       only: tem_cylinder_type,                      &
    &                                  tem_load_cylinder,                      &
    &                                  grw_cylinderArray_type
  use tem_sphere_module,         only: tem_sphere_type, tem_sphereCubeOverlap
  use tem_cube_module,           only: tem_cube_type
  use tem_transformation_module, only: tem_transformation_type  

  use aotus_module,              only: flu_State, aot_get_val,                 &
    &                                  aoterr_Fatal, aoterr_NonExistent,       &
    &                                  aoterr_WrongType
  use aot_table_module,          only: aot_table_open, aot_table_close,        &
    &                                  aot_table_length

  use sdr_cylinder_module,       only: append_cylinder2SpaObj
  use sdr_spatialObj_module,     only: grw_spatialObjArray_type,               &
    &                                  sdr_spatialObj_type, append,            &
    &                                  spacerInterwoven, cylinder

  implicit none
  private

  public :: grw_spacerInterwovenArray_type
  public :: init, append, truncate, destroy, empty, placeAt
  public :: sdr_spacer_type, sdr_load_spacer
  public :: sdr_spacerInterwovenCubeOverlap

  !> This type provides spacer length and width information
  type spacer_filament_type
    !> cylinder typ
    type(tem_cylinder_type) :: cylinder
    !> spacer filament gab/distance between two parallel filament in length or width
    real(kind=rk) :: filament_gap 
    !> number of filaments in length or width. Calculated by 
    !! cylinder length/filament gab
    integer :: nrFilament 
    !> unitNormal of the cylinder vector. needed for spacer
    !! filament offset direction.
    real(kind=rk) :: unitNormal(3)
  end type spacer_filament_type


  !> This type provides information to
  !! create spacer geometry
  type sdr_spacer_type
    type(spacer_filament_type) :: length !< length of the spacer
    type(spacer_filament_type) :: width !< width of the spacer
    !> direction of height computed by cross product of length and width
    !! normal
    real(kind=rk) :: h_normal(3)
    logical :: interwoven !< choose between interwoven and non-interwoven spacer
  end type sdr_spacer_type

  !> This type provides information to create each interwoven spacer filament
  type sdr_spacerInterwoven_type
    !> cylinder contains origin and vector defining magnitude and direction
    !! of the filament
    type(tem_cylinder_type) :: cylinder
    !> number of sin period in length or width
    real(kind=rk) :: nrPeriod
    !> direction of height computed by cross product of length and width
    !! normal
    real(kind=rk) :: h_normal(3)
    integer :: shiftSin !< shift sinus period for interwoven spacer
  end type sdr_spacerInterwoven_type

 

?? copy :: GA_decltxt(spacerInterwoven, type(sdr_spacerInterwoven_type))

contains
  ! ****************************************************************************
  !> \brief Loading spacer information from config file \n
  subroutine sdr_load_spacer(sprInterwovenArray, cylArray, spaObjArray, attr_pos,&
    &                        transform, conf, thandle )
    ! ---------------------------------------------------------------------------!
    !inferface variables
    !> growing array of spacers
    type(grw_spacerInterwovenArray_type), intent(inout) :: sprInterwovenArray
    !> growing array of cylinders
    type(grw_cylinderArray_type), intent(inout) :: cylArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    !> transformation for spatial object
    type(tem_transformation_type), intent(in) :: transform 
    !> lua state
    type(flu_state) :: conf 
    integer, intent(in) :: thandle !< handle for canonical objects
    ! --------------------------------------------------------------------------!
    ! local varaibles
    integer :: spr_handle, spr_subHandle
    integer :: iObj, nObjects
    ! --------------------------------------------------------------------------!

    write(logunit(2),*) 'Loading spacer: '

    call aot_table_open(L = conf, parent = thandle, thandle = spr_handle, &
      &                 key = 'object')
    call aot_table_open(L=conf, parent = spr_handle, thandle = spr_subHandle, &
      & pos = 1 )

    if ( spr_subHandle .eq. 0) then
      !object is a single table
      call aot_table_close(L=conf, thandle=spr_subHandle)
      call sdr_load_spacer_single(sprInterwovenArray, cylArray, spaObjArray, &
        &                         attr_pos, transform, conf, spr_handle)
    else
      !object is a multiple table
      call aot_table_close(L=conf, thandle=spr_subHandle)
      nObjects = aot_table_length(L=conf, thandle=spr_handle)
      do iObj=1,nObjects
        call aot_table_open(L=conf, parent=spr_handle, thandle=spr_suBHandle,&
          & pos=iObj)
        call sdr_load_spacer_single(sprInterwovenArray, cylArray, spaObjArray, &
          &                         attr_pos, transform, conf, spr_Subhandle)
        call aot_table_close(L=conf, thandle=spr_subHandle)
      end do
    end if

    call aot_table_close(L=conf, thandle=spr_Handle)

  end subroutine sdr_load_spacer
  ! ******************************************************************************

  ! ******************************************************************************
  !> This routine single spacer from object table
  subroutine sdr_load_spacer_single(sprInterwovenArray, cylArray, spaObjArray, &
    &                               attr_pos, transform, conf, thandle )
    ! ---------------------------------------------------------------------------!
    !inferface variables
    !> growing array of spacers
    type(grw_spacerInterwovenArray_type), intent(inout) :: sprInterwovenArray
    !> growing array of cylinders
    type(grw_cylinderArray_type), intent(inout) :: cylArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    !> transformation for spatial object
    type(tem_transformation_type), intent(in) :: transform 
    !> lua state
    type(flu_state) :: conf 
    integer, intent(in) :: thandle !< handle for canonical objects
    ! --------------------------------------------------------------------------!
    integer :: iError
    type(sdr_spacer_type) :: loc_spacer
    ! --------------------------------------------------------------------------!

    !load spacer length table
    write(logunit(2),*) '   Spacer filament length'
    call load_spacer_filament( me = loc_spacer%length, conf = conf, &
      &                        thandle = thandle, transform = transform, &
      &                        key = 'length' )
 
    !load spacer width table
    write(logunit(2),*) '   Spacer filament width'
    call load_spacer_filament( me = loc_spacer%width, conf = conf, &
      &                        thandle = thandle, transform = transform, &
      &                        key = 'width' )

    ! read interwoven logical to switch between 
    ! interwoven and non-interwoven spacers 
    call aot_get_val(L=conf, thandle=thandle, &
      &              val=loc_spacer%interwoven, ErrCode=iError, &
      &              key='interwoven', default=.false.)
    write(logunit(2),*) '              interwoven:', loc_spacer%interwoven


    if(loc_spacer%interwoven) then
      !interwoven spacer
      !compute height normal
      loc_spacer%h_normal(1) = loc_spacer%width%unitNormal(2) &
        &                    * loc_spacer%length%unitNormal(3) &
        &                    - loc_spacer%width%unitNormal(3) &
        &                    * loc_spacer%length%unitNormal(2)

      loc_spacer%h_normal(2) = loc_spacer%width%unitNormal(3) &
        &                    * loc_spacer%length%unitNormal(1) &
        &                    - loc_spacer%width%unitNormal(1) &
        &                    * loc_spacer%length%unitNormal(3)

      loc_spacer%h_normal(3) = loc_spacer%width%unitNormal(1) &
        &                    * loc_spacer%length%unitNormal(2) &
        &                    - loc_spacer%width%unitNormal(2) &
        &                    * loc_spacer%length%unitNormal(1)

      loc_spacer%h_normal = loc_spacer%h_normal&
        &    /sqrt(dot_product(loc_spacer%h_normal, &
        &                            loc_spacer%h_normal))  
      write(logunit(2),*) '           height normal: ', loc_spacer%h_normal

      !Filaments in length
      call append_spacerInterwoven(sprInterwovenArray = sprInterwovenArray, &
        &                          spaObjArray = spaObjArray,               &
        &                          attr_pos = attr_pos,                     &
        &                          filament_cur = loc_spacer%length,        &
        &                          filament_adj = loc_spacer%width,         &
        &                          h_normal = loc_spacer%h_normal,          &
        &                          shiftSin = 1)
      !filaments in width  
      call append_spacerInterwoven(sprInterwovenArray = sprInterwovenArray, &
        &                          spaObjArray = spaObjArray,               &
        &                          attr_pos = attr_pos,                     &
        &                          filament_cur = loc_spacer%width,         &
        &                          filament_adj = loc_spacer%length,        &
        &                          h_normal = loc_spacer%h_normal,          &
        &                          shiftSin = 0)
    else
      !non-interwoven spacer are treated as cylinders
      !Filaments in length
      call append_spacerNonInterwoven(cylArray = cylArray,                  &
        &                          spaObjArray = spaObjArray,               &
        &                          attr_pos = attr_pos,                     &
        &                          filament_cur = loc_spacer%length,        &
        &                          filament_adj = loc_spacer%width)
      !filaments in width  
      call append_spacerNonInterwoven(cylArray = cylArray,                  &
        &                          spaObjArray = spaObjArray,               &
        &                          attr_pos = attr_pos,                     &
        &                          filament_cur = loc_spacer%width,         &
        &                          filament_adj = loc_spacer%length)
    endif
    
  end subroutine sdr_load_spacer_single
  ! ****************************************************************************

  ! ****************************************************************************
  !> This routine load the spacer filament type information for both length
  !! and width
  ! ****************************************************************************
  subroutine load_spacer_filament(me, conf, thandle, transform, key )
    ! ---------------------------------------------------------------------------!
    !inferface variables
    type(spacer_filament_type), intent(out) :: me !< spacer data type
    type(flu_state) :: conf !< flu state
    integer, intent(in) :: thandle !< parent handle
    !> transformation for spatial object
    type(tem_transformation_type), intent(in) :: transform 
    character(len=*) :: key !< length or width to load from config
    ! ---------------------------------------------------------------------------!
    !local variable
    integer :: orig_handle
    integer :: iError
    real(kind=rk) :: length
    ! ---------------------------------------------------------------------------!

    !open length table for spacer length, axis and gap between two parallel 
    !filament
    call aot_table_open(L=conf, parent=thandle, thandle=orig_handle, &
      &                 key=trim(key))

    call tem_load_cylinder(me = me%cylinder,      &
      &                    conf = conf,           &
      &                    thandle = orig_handle, &
      &                    transform = transform  )

    !read gap between two parallel filament in length
    call aot_get_val(L=conf, thandle=orig_handle, &
      &              val=me%filament_gap, ErrCode=iError, key='filament_gap')
    if (btest(iError, aoterr_Fatal)) then
      write(logunit(0),*) &
        &  'FATAL Error occured, while retrieving spacer'//trim(key)//', gap :'
      if (btest(iError, aoterr_NonExistent)) &
        &  write(logunit(0),*) 'Variable not existent!'
      if (btest(iError, aoterr_WrongType)) &
        &  write(logunit(0),*) 'Variable has wrong type!'
      call tem_abort()
    end if

    call aot_table_close(L=conf,thandle=orig_handle)
 
    ! compute total number of filament
    length=sqrt(dot_product(me%cylinder%vec, me%cylinder%vec))
    me%nrFilament = nint(length/me%filament_gap)

    !compute unit normal
    me%unitNormal = me%cylinder%vec/length

    write(logunit(2),*) '                     gap: ', me%filament_gap
    write(logunit(2),*) '              nrFilament: ', me%nrFilament

  end subroutine load_spacer_filament


  ! ******************************************************************************
  !> This routine converts sdr_spacer_type to spacerInterwoven_type for 
  !! as each filament and add it to the growing array of spacerInterwoven_type
  subroutine append_spacerNonInterwoven(cylArray, spaObjArray, attr_pos, &
    &                                filament_cur, filament_adj)
    ! ---------------------------------------------------------------------------!
    !inferface variables
    !> growing array of cylinders
    type(grw_cylinderArray_type), intent(inout) :: cylArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    type(spacer_filament_type), intent(in) :: filament_cur !< current filament
    type(spacer_filament_type), intent(in) :: filament_adj !< adjacent filament
    ! ---------------------------------------------------------------------------!
    type(tem_cylinder_type) :: loc_cylinder
    integer :: nrs
    ! ---------------------------------------------------------------------------!
    do nrs = 1,filament_adj%nrFilament
      !origin of each filament 
      loc_cylinder%origin = filament_cur%cylinder%origin + &
            & ( nrs - 1.0_rk )*filament_adj%filament_gap&
            & * filament_adj%unitNormal
      !radius  
      loc_cylinder%radius = filament_cur%cylinder%radius
      !vec
      loc_cylinder%vec = filament_cur%cylinder%vec
      !only_surface
      loc_cylinder%only_surface = filament_cur%cylinder%only_surface

      !append each filament to growing array  
      call append_cylinder2SpaObj( cylArray, spaObjArray, attr_pos, &
        &                          loc_cylinder)
    enddo  

  end subroutine append_spacerNonInterwoven

  ! ******************************************************************************
  !> This routine converts sdr_spacer_type to spacerInterwoven_type for 
  !! as each filament and add it to the growing array of spacerInterwoven_type
  subroutine append_spacerInterwoven(sprInterwovenArray, spaObjArray, attr_pos, &
    &                                filament_cur, filament_adj, shiftSin, &
    &                                h_normal)
    ! ---------------------------------------------------------------------------!
    !inferface variables
    !> growing array of spacers
    type(grw_spacerInterwovenArray_type), intent(inout) :: sprInterwovenArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    type(spacer_filament_type), intent(in) :: filament_cur !< current filament
    type(spacer_filament_type), intent(in) :: filament_adj !< adjacent filament
    integer, intent(in) :: shiftSin !< shift sinus period for interwoven spacer
    real(kind=rk), intent(in) :: h_normal(3) !< normal direction for height
    ! ---------------------------------------------------------------------------!
    integer :: nrs
    type(sdr_spacerInterwoven_type) :: loc_spacerInterwoven
    type( sdr_spatialObj_type ) :: spatialObj 
    ! ---------------------------------------------------------------------------!
    !append spacer to spacer array
    spatialObj%attribute_position = attr_pos
    spatialObj%geometry_primitive = spacerInterwoven

    !convert loc_spacer to spacerInterwoven_type and 
    !add it to growing array of interwoven spacer
    do nrs = 1, filament_adj%nrFilament
      !origin of each filament
      loc_spacerInterwoven%cylinder%origin = &
        & filament_cur%cylinder%origin + &
        & ( nrs - 1.0_rk )*filament_adj%filament_gap &
        & * filament_adj%unitNormal
      !radius  
      loc_spacerInterwoven%cylinder%radius = filament_cur%cylinder%radius
      !vec
      loc_spacerInterwoven%cylinder%vec = filament_cur%cylinder%vec
      !only_surface
      loc_spacerInterwoven%cylinder%only_surface = filament_cur%cylinder%only_surface
      !h_normal
      loc_spacerInterwoven%h_normal = h_normal
      !shiftsin
      loc_spacerInterwoven%shiftSin = mod(nrs-shiftSin,2)
      ! number of sine period
      loc_spacerInterwoven%nrPeriod =0.5_rk*filament_cur%nrFilament

      !append each filament to growing array  
      call append( sprInterwovenArray, loc_spacerInterwoven )

      spatialObj%primitive_position = sprInterwovenArray%nVals

      !add spatialObj to spatialObj list
      call append(spaObjArray, spatialObj)
    enddo    

  end subroutine append_spacerInterwoven

  ! ****************************************************************************
  !> summary: This function checks intesection of solid cube and spacer interwoven
  function sdr_spacerInterwovenCubeOverlap(spacerInterwoven, cube) &
    & result(overlap)
    ! ---------------------------------------------------------------------------!
    !inferface variables
    !> spacer geometry data
    type(sdr_spacerInterwoven_type), intent(in) :: spacerInterwoven
    type(tem_cube_type), intent(in) :: cube 
    logical :: overlap !< return value
    ! ---------------------------------------------------------------------------!
    real(kind=rk) :: proj
    integer :: iVer
    type(tem_sphere_type) :: sphere
    real(kind=rk) :: cubeVer(15,3)
    ! ---------------------------------------------------------------------------!
    overlap = .false.
    !compute cube vertices for spacer since it need to check
    !vertices
    !!@note KM: we check for all vertices to find if any of the cube vertices
    !! intersect with cylinder
    cubeVer(1,:) = cube%center
    cubeVer(2,:) = cube%center + [ cube%halfwidth, 0.0_rk, 0.0_rk]
    cubeVer(3,:) = cube%center + [-cube%halfwidth, 0.0_rk, 0.0_rk]
    cubeVer(4,:) = cube%center + [0.0_rk,  cube%halfwidth, 0.0_rk]
    cubeVer(5,:) = cube%center + [0.0_rk, -cube%halfwidth, 0.0_rk]
    cubeVer(6,:) = cube%center + [0.0_rk, 0.0_rk,  cube%halfwidth]
    cubeVer(7,:) = cube%center + [0.0_rk, 0.0_rk, -cube%halfwidth]
    cubeVer(8,:) = cube%origin 
    cubeVer(9,:) = cube%origin + [cube%extent, 0.0_rk, 0.0_rk]
    cubeVer(10,:) = cube%origin + [0.0_rk, cube%extent, 0.0_rk]
    cubeVer(11,:) = cube%origin + [0.0_rk, 0.0_rk, cube%extent]
    cubeVer(12,:) = cube%origin + [cube%extent, cube%extent, 0.0_rk]
    cubeVer(13,:) = cube%origin + [cube%extent, 0.0_rk, cube%extent]
    cubeVer(14,:) = cube%origin + [0.0_rk, cube%extent, cube%extent]
    cubeVer(15,:) = cube%origin + [cube%extent, cube%extent, cube%extent]

    sphere%radius = spacerInterwoven%cylinder%radius
    sphere%only_surface = spacerInterwoven%cylinder%only_surface

    do iVer=1,15
      proj = dot_product( (cubever(iVer,:)-spacerInterwoven%cylinder%origin), &
      &                spacerInterwoven%cylinder%vec) &
      &  / dot_product(spacerInterwoven%cylinder%vec, spacerInterwoven%cylinder%vec)

      !check if projected point on cylinderis within cylinder length
      if ( proj .ge. 0.0_rk .and. proj  .lt. 1.0_rk ) then
        sphere%origin = spacerInterwoven%cylinder%origin &
          &           + proj*spacerInterwoven%cylinder%vec   &
                  !interwoven function
          &           + sin(2.0_rk*PI*proj*spacerInterwoven%nrPeriod &
          &           + PI*spacerInterwoven%shiftSin) &
          &           * spacerInterwoven%cylinder%radius  &
          &           * spacerinterwoven%h_normal

        overlap = tem_sphereCubeOverlap(sphere, cube)
        if( overlap ) return
      endif   
    enddo

  end function sdr_spacerInterwovenCubeOverlap
  ! ****************************************************************************

?? copy :: GA_impltxt(spacerInterwoven, type(sdr_spacerInterwoven_type))

end module sdr_spacer_module

!> \page spacers Spacers
!! Spacers are defined by an two spacer filaments one along the length and 
!! other along the width. Each spacer filament is defined by the \ref cylinders
!! and distance between consecutive spacer filaments
!!
!! Valid definition:
!!\verbatim
!! geometry={
!!   kind='spacer', 
!!     object={
!!       length={
!!         origin = {0.0,0.0,0.0}, -- starting point of the spacer
!!         vec = {1.0,0.0,0.0}, -length and axis of first filament
!!         radius = 0.001 -- radius of the filament
!!         only_surface = true, -- If not defined default is set to false
!!         filament_gap = 0.02, -- distance between filament
!!       },
!!       width={
!!         origin = {0.0,0.0,0.0}
!!         vec = {0.0,0.0,1.0},
!!         radius = 0.001,
!!         only_surface = true, -- If not defined default is set to false
!!         filament_gap = 0.02,
!!       },
!!       interwoven = true, --switch between interwoven and non-interwoven
!!                         -- spacer generation
!!     }
!! } -- single spacer
!!\endverbatim
!! The following seeder file is to generate mesh with spacer
!!  inside:
!! \include testsuite/spacer/seeder.lua
!! \n\n
!! Mesh generated with above seeder file:
!! \image html sdr_spacer.png
!! Example lua file is available at \link testsuite/spacer/seeder.lua
!! \example testsuite/spacer/seeder.lua

