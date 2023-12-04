! Copyright (c) 2012-2013, 2019 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2013, 2022 Harald Klimach <harald.klimach@dlr.de>
! Copyright (c) 2013 Simon Zimny <s.zimny@grs-sim.de>
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
?? include 'arrayMacros.inc'
! ******************************************************************************
!> author: Kannan Masilamani
!! This module contains periodic boundary definition and routines
! ******************************************************************************
module sdr_periodic_module
  use env_module,                only: rk, minLength, zeroLength
  use tem_logging_module,        only: tem_log, tem_toStr
  use tem_canonicalND_module,    only: tem_canonicalND_type,                   &
    &                                  tem_load_canonicalND
  use tem_triangle_module,       only: tem_triangle_type, &
    &                                  tem_triangleCubeOverlap
  use tem_plane_module,          only: tem_plane_type
  use tem_cube_module,           only: tem_cube_type
  use tem_transformation_module, only: tem_transformation_type  

  use sdr_attribute_module,      only: sdr_attribute_type
  use sdr_spatialObj_module,     only: grw_spatialObjArray_type,               &
    &                                  sdr_spatialObj_type, append,            &
    &                                  PeriodicPlane

  use aotus_module,              only: flu_State, aot_get_val, &
    &                                  aoterr_Fatal, aoterr_NonExistent,       &
    &                                  aoterr_WrongType
  use aot_table_module,          only: aot_table_open, aot_table_close,        &
    &                                  aot_table_length

  implicit none

  private

  public :: sdr_load_periodic
  public :: grw_periPlaneArray_type
  public :: sdr_periodicPlane_type
  public :: init, append, truncate, destroy, empty, placeAt
  public :: sdr_periodicPlaneCubeOverlap

  !> type contains Periodic plane information
  type sdr_PeriodicPlane_type
    !> intrinsic seeder plane
    type(tem_plane_type) :: plane
    !> Position of opposite periodic plane in the spatial object list
    integer :: oppPlane_pos
  end type sdr_PeriodicPlane_type

?? copy :: GA_decltxt(periPlane, type(sdr_periodicPlane_type))

contains

?? copy :: GA_impltxt(periPlane, type(sdr_periodicPlane_type))

  ! ****************************************************************************
  !> \brief load periodic table from config file.\n
  !!
  !! Periodic boundary is defined by two planes and the normal of both planes
  !! should point outwards the fluid domain.
  !!Defintion:
  !!\verbatim
  !!spaial_object={
  !!     attribute = {
  !!       kind = 'periodic',
  !!       label = 'per1',
  !!     },
  !!     geometry = {
  !!       kind = 'periodic',
  !!       object = {{plane1={
  !!                         vec={{0.0,2.0,0.0},
  !!                              {0.0,0.0,2.0}},
  !!                         origin={2.0,-1.0,-1.0}},
  !!                  plane2={
  !!                         vec={{0.0,2.0,0.0},
  !!                              {0.0,0.0,2.0}},
  !!                         origin={-2.0,-1.0,-1.0}},
  !!                  }}
  !!\endverbatim
  !! It is possible to define multiple periodic boundary
  ! ****************************************************************************
  subroutine sdr_load_periodic(periArray, spaObjArray, attr_pos, transform,    &
    &                          conf, thandle)
    ! ---------------------------------------------------------------------------!
    !> grwoing periodic plane data type
    type( grw_periPlaneArray_type ), intent(inout) :: periArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    !> transformation for spatial object
    type(tem_transformation_type), intent(in) :: transform 
    type( flu_state ) :: conf !< lua state
    integer, intent(in) :: thandle !< lua table identification
    ! ---------------------------------------------------------------------------!
    !local variable
    integer :: obj_handle
    integer :: plane_handle
    type(tem_canonicalND_type) :: canoND
    type(sdr_periodicPlane_type) :: periPlane 
    ! ---------------------------------------------------------------------------!

    call tem_log(2, ' Loading periodic geometry :')

    call aot_table_open( L = conf, parent = thandle,                           &
      &                  thandle = obj_handle, key='object')

    call tem_log(2, ' Plane 1 :')
    call aot_table_open(L=conf, parent=obj_handle, thandle=plane_handle, &
      &                 key='plane1')
    
    call tem_load_canonicalND( me        = canoND,      &
      &                        transform = transform,   &
      &                        conf      = conf,        &
      &                        thandle   = plane_handle )

    ! Convert canoNDPlane to periodic plane
    periPlane%plane = canoND%plane

    call append_periodic( periPlane = periPlane, &
      &                   periArray = periArray, &
      &                   spaObjArray = spaObjArray, &
      &                   attr_pos = attr_pos )

    !position of opposite periodic plane in the spatial object array
    !plane 1 is added first so opposite plane is at + 1 of spatialObjArray
    periArray%val( periArray%nVals )%oppPlane_pos = spaObjArray%nVals + 1

    call aot_table_close(L=conf,thandle=plane_handle)

    call tem_log(2, '')
    
    !read plane2
    call tem_log(2, ' Plane 2 :')
    call aot_table_open(L=conf, parent=obj_handle, thandle=plane_handle, &
      &                 key='plane2')

    call tem_load_canonicalND( me        = canoND,      &
      &                        transform = transform,   &
      &                        conf      = conf,        &
      &                        thandle   = plane_handle )

    ! Convert canoNDPlane to periodic plane
    periPlane%plane = canoND%plane
    call aot_table_close(L=conf,thandle=plane_handle)

    call append_periodic( periPlane = periPlane, &
      &                   periArray = periArray, &
      &                   spaObjArray = spaObjArray, &
      &                   attr_pos = attr_pos )

    !plane 2 is added second so opposite plane is at - 1 of spatialObjArray
    periArray%val( periArray%nVals )%oppPlane_pos = spaObjArray%nVals - 1

  end subroutine sdr_load_periodic
  ! ****************************************************************************
  

  ! ****************************************************************************
  !> This routine append periodic type to growing array of periodic
  subroutine append_periodic(periPlane, periArray, spaObjArray, attr_pos)
    ! --------------------------------------------------------------------------!
    !> seeder periodic type
    type( sdr_periodicPlane_type ), intent(in) :: periPlane
    !> grwoing periodic plane data type
    type( grw_periPlaneArray_type ), intent(inout) :: periArray
    !> growing array of geometrical objects.
    type( grw_spatialObjArray_type), intent(inout) :: spaObjArray
    !> Position of the attribute to connect this object to.
    integer, intent(in) :: attr_pos
    ! --------------------------------------------------------------------------!
    type(sdr_spatialObj_type) :: spatialObj 
    ! --------------------------------------------------------------------------!

    !Append periodicPlane 
    spatialObj%attribute_position = attr_pos
    spatialObj%geometry_primitive = PeriodicPlane

    call append( periArray, periPlane )

    spatialObj%primitive_position = periArray%nVals

    !add spatialObj to spatialObj list
    call append(spaObjArray, spatialObj)

  end subroutine append_periodic   
  ! ****************************************************************************


  ! ****************************************************************************
  !> Function compute intersection of plane with cube by checking
  !! intersection of two triangle of a plane with cube
  function sdr_periodicPlaneCubeOverlap( periPlane, cube )  result(overlaps)
    !> periodic plane to check intersection with cube
    type(sdr_periodicPlane_type),intent(in) :: periPlane
    type(tem_cube_type), intent(in) :: cube 
    logical overlaps
    ! --------------------------------------------------------------------------!
    integer :: iTri, interTri
    ! --------------------------------------------------------------------------!
    interTri = 0
 
    overlaps = .false.
   
    do iTri=1,2
      if( tem_triangleCubeOverlap( &
        &               periPlane%plane%triangle(iTri), cube) )   &
        & interTri = interTri + 1 
    end do

    if (interTri > 0 ) overlaps = .true.

  end function sdr_periodicPlaneCubeOverlap
  ! ****************************************************************************

  
end module sdr_periodic_module

!> \page periodicplanes Periodic planes
!! The periodic boundary condition are used to represent the domain 
!! with infinite length, which is used when the simulation domain is too long.
!! It can be used to reduce the simulation domain from higher dimension 
!! to lower dimension for example 3D to 2D or 3D to 1D.
!! On the periodic boundary, the velocity at the outlet is copied to 
!! the velocity at the inlet and vice versa. \n
!!
!! Periodic planes are used to define perodic boundary in the mesh.
!! Periodic plane table contains two planes, plane1 and plane2 i.e
!! fluid neighbor of plane1 is mapped to plane2 and vice versa. \n
!! Following conditions must be satisfied in defining periodic planes
!! \li \b normal diection of plane1 and plane2 should point outwards
!! fluid domain. \n
!! In seeder, the plane normal are 
!! definited by <b> right hand rule</b>, where thump finger represents 
!! the 1st vector and index finger represents the 2nd vector and middle finger
!! represents the normal direction. This right hand rule can be seen perfectly 
!! in the left plane in the figure below.
!! \image html plane_per.png
!! Above image shows the normal directions for two planes pointing outwards 
!! the fluid domain.\n
!! Normal direction of the plane can be changed by just swaping the vectors
!! defining planes.i.e order of vector as shown in image below.
!! \image html plane1.png
!! \li refinement level on both planes should be the same and there should not
!! be any level jump on the periodic planes as shown below.
!! \image html multilevel_periodic_incorrect.png
!! Correct plane definition should look like below
!! \image html multilevel_periodic_correct.png
!! This can be achieved by shifting the plane by half of element size of
!! maximum refinement level intersecting the periodic planes.\n
!!
!! <b> If any of the above condition is not satisfied then seeder will 
!! terminate an error message. </b>\n
!! 
!! In seeder, when the node intersect only with periodic planes then the 
!! boundary id for that direction is set to huge number. So any node 
!! with direction of boundary id greater than number of boundaries then
!! the following algorithm is used for that direction.\n
!!
!! \b Algorithm used to generate the periodic boundary
!! \li Find the barycenter of the current element with periodic neighbor
!! \li Find the intersected periodic plane id and its assosiated opposite
!! plane id in the growing array of periodic planes. (periodic
!! plane definition is needed to project coordinate point from one plane
!! to another and also the normal direction to get to the fluid domain)
!! \li Project the barycenter of current element on its neighbor periodic 
!!     plane
!! \li Translate the projected point to the opposite plane
!! \li Use the current search direction to move to the fluid node.
!! \li get the treeID of that node and set negative of this treeID to boundary id
!!     for current direction.
!! \li if the fluid node is intersected by a boundary then set boundary id
!!     to minimun of intersected boundary.
!! \li if the fluid node is again intersected by periodic boundary,
!!     then move in the negative normal direction of opposite periodic plane
!!     and do step 3 and 4.
!!     
!!  
!! In the solver, the periodic boundaries are identified with the boundary
!! id with negative values.\n
!!
!! \ref periodicPlaneExample "How to define periodic planes in lua file?"
!!
!! Example lua file to generate Channel with one element in z-direction
!! is available at
!! \link testsuite/periodic/seeder.lua
!! \example testsuite/periodic/seeder.lua 



