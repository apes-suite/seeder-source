! Copyright (c) 2012, 2022 Harald Klimach <harald.klimach@dlr.de>
! Copyright (c) 2012 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2012, 2016 Jiaxing Qi <jiaxing.qi@uni-siegen.de>
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
!> Module to describe geometrical objects in Seeder.
!!
!! The sdr_spatialObj_type is used to build up a list of all geometric objects
!! to be considered in the mesh generation.
!! They serve as a pointer to more detailed descriptions of the actual
!! geometrical representation.
module sdr_spatialObj_module
  use env_module, only: minLength, zeroLength

  implicit none

  private

  public :: grw_spatialObjArray_type, sdr_spatialObj_type
  public :: init, append, truncate, destroy, empty, placeat

  ! Identifiers for supported geometric primitives
  integer, parameter, public :: point = 1
  integer, parameter, public :: line = 2
  integer, parameter, public :: triangle = 3
  integer, parameter, public :: box = 4
  integer, parameter, public :: periodicPlane = 5
  integer, parameter, public :: sphere= 6
  integer, parameter, public :: cylinder = 7
  integer, parameter, public :: spacerInterwoven = 8
  integer, parameter, public :: ellipsoid= 9

  !> This data type describes a geometric object generically.
  !!
  !! It is used to build a list of all objects that are to be checked for
  !! intersection and provides a uniform access to them.
  !! In the tree nodes a single integer index is then sufficient to identify
  !! the intersected object.
  type sdr_spatialObj_type
    !> Position of the attribute in the list of attributes, this object should
    !! be attached to.
    integer :: attribute_position

    !> What kind of geometric primitive is this object?
    !!
    !! Supported are:
    !! 1. point
    !! 2. line
    !! 3. triangle
    !! 4. box
    integer :: geometry_primitive

    !> Position in the list of correspoding primitive.
    integer :: primitive_position
  end type sdr_spatialObj_type

?? copy :: GA_decltxt(spatialObj, type(sdr_spatialObj_type))

contains

?? copy :: GA_impltxt(spatialObj, type(sdr_spatialObj_type), type(sdr_spatialObj_type))

end module sdr_spatialObj_module
