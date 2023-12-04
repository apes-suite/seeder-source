! Copyright (c) 2012-2013, 2015 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2012-2014, 2017, 2022-2023 Harald Klimach <harald.klimach@dlr.de>
! Copyright (c) 2012, 2014, 2016 Jiaxing Qi <jiaxing.qi@uni-siegen.de>
! Copyright (c) 2016 Tobias Girresser <tobias.girresser@student.uni-siegen.de>
! Copyright (c) 2016 Peter Vitt <peter.vitt2@uni-siegen.de>
! Copyright (c) 2018 Daniel Fleischer <daniel.fleischer@student.uni-siegen.de>
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
?? include 'tem/source/arrayMacros.inc'
!> Module to describe attributes attached to geometrical objects.
!!
!! Attribute list will be a dynamic unique list. Thus, multiple definitions of
!! the same attribute in the configuration file does not result in multiple
!! storages.
module sdr_attribute_module
  use env_module,            only: minLength, labelLen, zeroLength, rk
  use tem_aux_module,        only: tem_abort
  use tem_dyn_array_module,  only: dyn_labelArray_type, init, append, &
    &                              PositionOfVal
  use tem_grow_array_module, only: grw_intArray_type, grw_logicalArray_type, &
    &                              init, append
  use tem_tools_module,      only: upper_to_lower
  use tem_logging_module,    only: logunit

  use flu_binding,        only: flu_State, flu_next
  use aotus_module,       only: aot_get_val, flu_State,                        &
    &                           aoterr_Fatal, aoterr_NonExistent,              &
    &                           aoterr_WrongType
  use aot_table_module,   only: aot_table_open, aot_table_close,               &
    &                           aot_table_length, aot_table_top, aot_table_first

  implicit none

  private

  public :: sdr_attribute_type
  public :: sdr_load_attribute
  public :: sdr_init_attribute
  public :: dyn_attributeArray_type
  public :: sdr_attrList_type
  public :: sdr_identify_bc_colors
  public :: sdr_identify_inv_colors
  public :: sdr_any_bc_subresolution
  public :: sdr_any_bc_distanceRefine
  public :: sdr_isPeriodicDefined

  public :: init, append, PositionOfVal, truncate, destroy, empty, placeAt
  public :: sortTruncate

  !> Maximal number of different object kinds to distinguish.
  !!
  !! This is used to allow allocations of arrays for all
  !! object kinds with direct access to each component
  !! by the sdr_*_object constants below.
  integer, parameter, public :: sdr_object_kinds_max = 5

  ! Some parameters to easily identify the different kinds
  ! objects, that are distinguished.
  ! They have to be in the range 1:sdr_object_max!

  !> Identifier for boundary objects
  integer, parameter, public :: sdr_Boundary_object = 1

  !> Identifier for seed objects
  integer, parameter, public :: sdr_Seed_object = 2

  !> Identifier for refinement objects
  integer, parameter, public :: sdr_Refinement_object = 3

  !> Identifier for solid objects, that can be fluidify in the solver
  integer, parameter, public :: sdr_Fluidifyable_object = 4

  !> Identifier for no Solidification objects
  integer, parameter, public :: sdr_noSolidification_object = 5

  !> This data type contains info for distance refinement defined
  !! in config file for each boundary attribute.
  type sdr_distanceRefine_type
    !> Defines the distance to which this boundary
    !! should be refined
    real(kind=rk) :: radius

    !> level to refine nodes with in given radius
    integer :: reach_level
  end type sdr_distanceRefine_type

  !> This data type describes type of attribute
  type sdr_attribute_type
    !> Kind of this attribute:
    !! - boundary
    !! - seed
    !! - refinement
    !! - fluidifyable
    !! - noSolidification
    integer :: kind

    !> Label to identify this attribute.
    character(len=labelLen) :: label

    !> Color to associate the object with.
    !!
    !! This is only relevant for seeds and boundaries.
    !! Refinements do not have colors.
    !! Color names are case-insensitive!
    !! Boundaries limit seeds of the same color, or all seeds, if they
    !! do not have a color ('none' or 'all').
    !! Only boundaries without a color will actually be written as boundary
    !! conditions all others are assumed to be covered internally.
    !! NOTICE, that this results in a somewhat special behavior, where seeds
    !! of color 'none' or 'all' are just usual colors (though none and all will
    !! be the same), while boundaries of this color will confine all colors,
    !! not just the seeds with 'none' color. This means, that 'none' colored
    !! seeds can not be confined on their own. Their boundaries will always
    !! bound all colors!
    !! Boundaries, with a color matching no seed will be ignored
    !! by the flooding but are still relevant for refinement.
    !! The default is 'none'.
    character(len=labelLen) :: color

    !> Refinement level to resolve this attribute with at least.
    integer :: level

    !> ID in the list of attributes of this kind.
    integer :: id = 0

    !> Uni-id of this attribute in the list of unique names for this kind.
    !! (labels for boundaries,
    !!  colors for seeds)
    integer :: uni_id = 0

    !> This object should be resolved further than the elements of the mesh.
    !!
    !! Only useful for boundary objects. And only effective, if this boundary
    !! has a specific color (not the none-color).
    logical :: subresolution = .false.

    !> Indicator whether to calculate link-wise distance
    logical :: calc_dist = .false.

    !> Indicator whether the wall normal should be stored in boundary
    !! elements
    logical :: store_normal = .false.

    !> Indicator of whether to flood diagonal node during
    !! flood_periphery
    logical :: flood_diagonal = .true.

    !> Uni-id to identify attribute with certain number of
    !! distance refinement
    integer :: distRefine_id = 0

    !> Start position of distance refine object of this attribute in
    !! growing array of distance refine
    integer :: distance_first = 0

    !> Last position of distance refine object of this attribute in
    !! growing array of distance refine
    integer :: distance_last = 0

  end type sdr_attribute_type


?? copy :: GA_decltxt( distanceRefine, type(sdr_distanceRefine_type))
?? copy :: DA_decltxt( attribute, type(sdr_attribute_type))


  type sdr_attrList_type
    !> Dynamic array of all unique attributes.
    !!
    !! Attributes are distinguished by kind, label, color and level.
    type(dyn_attributeArray_type) :: dynArray

    !> Unique list of used names for the various attribute kinds.
    !!
    !! (Labels for boundaries, colors for seeds, not relevant otherwise)
    !! This is especially needed to find the actual number of different colors.
    type(dyn_labelArray_type) :: uni_name(sdr_object_kinds_max)

    !> uni_id of the 'none' color.
    !!
    !! The none color is special in that it will not be stored to disk.
    !! However, it will be used just like any other color during construction
    !! of the prototree. To filter the color later before writing the colors
    !! to disk, we store the index of the 'none' color here.
    !! If there is no 'none' color, this id is -1.
    integer :: none_color_id = -1

    !> ID of unique colors for each kindpos(sdr_Boundary_object).
    !!
    !! Required to identify the colors to which a certain boundary should
    !! apply.
    !! 'none' and 'all' boundaries: negative value
    !! matching seed labels: positive uni_id of that color
    !! non-matching: 0 (affecting no flooding color at all)
    integer, allocatable :: bc_color_id(:)

    !> Flag for each unique color, wether it should be inverted.
    !!
    !! Inverted colors will change reverse their flooded flags after flooding.
    !! Only elements that are flooded by the none color are considered, and
    !! the none color itself can not be inverted.
    logical, allocatable :: color_inverted(:)

    !> Growing array with flags on whether to calculate distances for
    !! each unique boundary.
    !!
    !! There might be conflicting calc_dist settings for attributes with
    !! the same boundary label. The `bc_uni_calcdist` consolidates this.
    !! If any of the attributes with the same label has the calc_dist set
    !! to true, this will be true and all attributes with that label will
    !! be changed accordingly.
    type(grw_logicalArray_type) :: bc_uni_calcdist

    !> Growing array with flags on whether to store normals for
    !! each unique boundary.
    !!
    !! There might be conflicting store_normal settings for attributes with
    !! the same boundary label. The `bc_uni_storenormal` consolidates this.
    !! If any of the attributes with the same label has `store_normal` set
    !! to true, this will be true and all attributes with that label will
    !! be changed accordingly.
    type(grw_logicalArray_type) :: bc_uni_storenormal

    !> A list of all the attribute positions for each kind.
    !! This provides the mapping of the kind id to the attribute position.
    type(grw_intArray_type) :: kindpos(sdr_object_kinds_max)

    !> Growing array of distance refinement info for boundary attribute.
    !! Use dynArray%distRefine_id to create attribute with distance refine
    type(grw_distanceRefineArray_type) :: distRefine

  end type sdr_attrList_type

!HK: To work around Cray compiler bug, we might need to define the assignment
!HK  ourselves...
!HK!  interface assignment(=)
!HK!    module procedure Copy_var
!HK!  end interface


  interface operator(==)
    module procedure isEqual
  end interface

  interface operator(/=)
    module procedure isUnequal
  end interface

  interface operator(<)
    module procedure isSmaller
  end interface

  interface operator(<=)
    module procedure isSmallerOrEqual
  end interface

  interface operator(>)
    module procedure isGreater
  end interface

!  interface operator(>=)
!    module procedure isGreaterOrEqual
!  end interface


contains


  !> Subroutine to initialize the list for all attributes
  subroutine sdr_init_attribute(me)
    type(sdr_attrList_type), intent(out) :: me

    me%none_color_id = -1

  end subroutine sdr_init_attribute



  ! ****************************************************************************!
  !> This routine loads the attribute information from the config file.
  subroutine sdr_load_attribute( attrList, conf, thandle, subres_colors, &
    &                            attr_pos )
    ! --------------------------------------------------------------------------!
    !> dynamic array of attribute type
    type(sdr_attrList_type), intent(inout) :: attrList
    type(flu_State) :: conf !< lua state
    integer, intent(in) :: thandle !<  handle to load attribute from
    !> List of colors which should by default use subelement resolution for
    !! their boundaries.
    type(dyn_labelArray_type), intent(in) :: subres_colors
    integer, intent(out) :: attr_pos !< position of attribute in attribute array
    ! --------------------------------------------------------------------------!
    integer :: iError
    character(len=labelLen) :: attr_kind
    integer :: attr_handle
    logical :: wasAdded
    logical :: newuni
    type(sdr_attribute_type) :: attribute ! save attibute info temporarily
    integer :: uni_pos
    logical :: merged_cd
    logical :: merged_sn
    logical :: defsub
    integer :: iDistRefine
    integer :: iBC, iAttr
    type(grw_intArray_type) :: prev_bcs
    ! --------------------------------------------------------------------------!

    write(logunit(1),*) 'Loading attribute'
    ! Open the attribute table.
    call aot_table_open(L = conf, parent = thandle, thandle = attr_handle, &
      &                 key = 'attribute')

    ! Load attribute only if the table exists otherwise prompt an error and
    ! abort.
    if (attr_handle .gt. 0 ) then
      ! load KIND
      call aot_get_val(L = conf, thandle = attr_handle, key = 'kind', &
        &              val = attr_kind, ErrCode = iError )

      if (btest(iError, aoterr_Fatal)) then
        write(logUnit(0),*) &
          &  'FATAL Error occured, while retrieving attribute kind:'
        if (btest(iError, aoterr_NonExistent)) &
          &  write(logunit(0),*) 'Attribute kind not existent!'
        if (btest(iError, aoterr_WrongType)) &
          &  write(logunit(0),*)'Attribute kind has wrong type!'
        call tem_abort()
      end if

      select case(upper_to_lower(trim(attr_kind)))
        case('boundary','periodic')
          attribute%kind = sdr_Boundary_object

        case('seed')
          attribute%kind = sdr_Seed_object

        case('refinement')
          attribute%kind = sdr_Refinement_object

        case('fluidifyable')
          attribute%kind = sdr_Fluidifyable_object

        case('nosolidification')
          attribute%kind = sdr_noSolidification_object

        case default
          write(logunit(0),*) 'ERROR: Unknown attribute kind: '//trim(attr_kind)
          call tem_abort()

      end select

      ! Load LABEL of this attribute.
      call aot_get_val(L = conf, thandle = attr_handle, key = 'label', &
        &              val = attribute%label, ErrCode = iError, &
        &              default='sdr_nolabel' )

      if (btest(iError, aoterr_Fatal)) then
        write(logunit(0),*) &
          &  'FATAL Error occured, while retrieving attribute label:'
        if (btest(iError, aoterr_WrongType)) &
          &  write(logunit(0),*) 'Label has wrong type!'
        call tem_abort()
      end if

      ! Load COLOR of this attribute.
      call aot_get_val(L = conf, thandle = attr_handle, key = 'color', &
        &              val = attribute%color, ErrCode = iError, &
        &              default = 'none' )

      if (btest(iError, aoterr_Fatal)) then
        write(logunit(0),*) &
          &  'FATAL Error occured, while retrieving attribute label:'
        if (btest(iError, aoterr_WrongType)) &
          &  write(logunit(0),*) 'Label has wrong type!'
        call tem_abort()
      end if

      ! Color 'all' is to be the same as 'none'. Also, make this parameter case
      ! insensitive.
      attribute%color = upper_to_lower(trim(attribute%color))
      if (trim(attribute%color) == 'all') attribute%color = 'none'

      ! Load LEVEL associated with this attribute.
      ! The level describes the refinement level, that should be used at least
      ! to refine the objects with this attribute to.
      ! The default of 0 does not impose any refinement at all.
      call aot_get_val(L = conf, thandle = attr_handle, key = 'level', &
        &              val = attribute%level, ErrCode = iError, default = 0)
      if (btest(iError, aoterr_WrongType)) then
        write(logunit(0),*) &
          &  'FATAL Error occured, while retrieving attribute level:'
        write(logunit(0),*) 'Level has wrong type, should be an Integer!'
        call tem_abort()
      end if

      ! Loading boundary specific settings.
      bc_settings: if (attribute%kind == sdr_Boundary_object) then

        if (trim(attribute%color) /= 'none') then
          ! Set the default for the subresolution according to the color.
          defsub = (PositionOfVal(me=subres_colors, val=attribute%color) > 0)

          ! Should the objects with this attribute be resolved on a subelement
          ! level?
          !>\todo HK: subresolution should get its default from a color-specific
          !!          setting and only needs to be read, if
          !!          subelement_resolution > 0.
          call aot_get_val( L       = conf,                    &
            &               thandle = attr_handle,             &
            &               key     = 'subresolution',         &
            &               val     = attribute%subresolution, &
            &               ErrCode = iError,                  &
            &               default = defsub                   )

          if (btest(iError, aoterr_WrongType)) then
            write(logunit(0),*) &
              & 'FATAL Error occured, while retrieving attribute subresolution:'
            write(logunit(0),*) 'subresolution should be a logical!'
            call tem_abort()
          end if
        end if

        ! Load CALC_DIST indicator
        call aot_get_val( L       = conf,                &
          &               thandle = attr_handle,         &
          &               key     = 'calc_dist',         &
          &               val     = attribute%calc_dist, &
          &               ErrCode = iError,              &
          &               default = .false.              )

        if (btest(iError, aoterr_WrongType)) then
          write(logunit(0),*) &
            & 'FATAL Error occured, while retrieving attribute calc_dist:'
          write(logunit(0),*) 'calc_dist should be a logical!'
          call tem_abort()
        end if

        ! Load STORE_NORMAL indicator
        call aot_get_val( L       = conf,                   &
          &               thandle = attr_handle,            &
          &               key     = 'store_normal',         &
          &               val     = attribute%store_normal, &
          &               ErrCode = iError,                 &
          &               default = .false.                 )

        if (btest(iError, aoterr_WrongType)) then
          write(logunit(0),*) &
            & 'FATAL Error occured, while retrieving attribute store_normal:'
          write(logunit(0),*) 'store_normal should be a logical!'
          call tem_abort()
        end if

        if (attribute%calc_dist) then
          ! Load flood_diagonal indicator (only relevant if calc_dist is active)
          call aot_get_val( L       = conf,                     &
            &               thandle = attr_handle,              &
            &               key     = 'flood_diagonal',         &
            &               val     = attribute%flood_diagonal, &
            &               ErrCode = iError,                   &
            &               default = .true.                    )

          if (btest(iError, aoterr_WrongType)) then
            write(logunit(0),*)  'FATAL Error occured, ' &
              &                  // 'while retrieving attribute flood_diagonal:'
            write(logunit(0),*) 'flood_diagonal should be a logical!'
            call tem_abort()
          end if
        end if

        ! load distance refine table from attribute table only for
        ! boundary attribute and append to growing
        ! array of distance refine in attrList
        call load_distanceRefine(attrList, conf, attr_handle, attribute)

      end if bc_settings

    else

      write(logunit(0),*) 'ERROR loading attribute for spatial object: '
      write(logunit(0),*) '  attribute table not defined'
      call tem_abort()

    endif

    call aot_table_close( L = conf, thandle = attr_handle )

    if (attribute%kind == sdr_Boundary_object) then
      ! Check whether this boundary is already present.
      ! If we already have boundaries we need to make sure that all of
      ! them have consistent calc_dist and store_normal settings.
      ! (If any boundary with of the same label has calc_dist set, we
      !  need to set it for all boundaries of this label, same for store_normal).
      uni_pos =  PositionOfVal( me = attrList%uni_name(sdr_Boundary_object), &
        &                       val = trim(attribute%label)                  )
      foundbc: if (uni_pos > 0) then
        ! Found a boundary of the same name.
        merged_cd = attribute%calc_dist &
          &         .or. attrList%bc_uni_calcdist%val(uni_pos)
        if (merged_cd .neqv. attribute%calc_dist) then
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(logunit(1),*) 'WARNING you set calc_dist=false here for'
          write(logunit(1),*) 'boundary '//trim(attribute%label)
          write(logunit(1),*) 'But it has already been defined as true by'
          write(logunit(1),*) 'another boundary object with this label.'
          write(logunit(1),*) 'This attribute will assume calc_dist=true!'
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          attribute%calc_dist = merged_cd
        end if
        if (merged_cd .neqv. attrList%bc_uni_calcdist%val(uni_pos)) then
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(logunit(1),*) 'WARNING you set calc_dist=true here for'
          write(logunit(1),*) 'boundary '//trim(attribute%label)
          write(logunit(1),*) 'But it has already been defined as false by'
          write(logunit(1),*) 'another boundary object with this label.'
          prev_bcs = sdr_attr_of_uni( attribute   = attrList,            &
            &                         object_kind = sdr_boundary_object, &
            &                         id          = uni_pos              )
          write(logunit(1),*) 'ATTENTION: setting calc_dist to TRUE for the' &
            &                 // ' following attributes:'
          write(logunit(1),*) prev_bcs%val(:prev_bcs%nVals)
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          do iBC=1,prev_bcs%nVals
            iAttr = prev_bcs%val(iBC)
            attrList%dynArray%val(iAttr)%calc_dist = merged_cd
          end do
          attrList%bc_uni_calcdist%val(uni_pos) = merged_cd
        end if

        merged_sn = attribute%store_normal &
          &         .or. attrList%bc_uni_storenormal%val(uni_pos)
        if (merged_sn .neqv. attribute%store_normal) then
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(logunit(1),*) 'WARNING you set store_normal=false here for'
          write(logunit(1),*) 'boundary '//trim(attribute%label)
          write(logunit(1),*) 'But it has already been defined as true by'
          write(logunit(1),*) 'another boundary object with this label.'
          write(logunit(1),*) 'This attribute will assume store_normal=true!'
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          attribute%store_normal = merged_sn
        end if
        if (merged_sn .neqv. attrList%bc_uni_storenormal%val(uni_pos)) then
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          write(logunit(1),*) 'WARNING you set store_normal=true here for'
          write(logunit(1),*) 'boundary '//trim(attribute%label)
          write(logunit(1),*) 'But it has already been defined as false by'
          write(logunit(1),*) 'another boundary object with this label.'
          prev_bcs = sdr_attr_of_uni( attribute   = attrList,            &
            &                         object_kind = sdr_boundary_object, &
            &                         id          = uni_pos              )
          write(logunit(1),*) 'ATTENTION: setting calc_dist to TRUE for the' &
            &                 // ' following attributes:'
          write(logunit(1),*) prev_bcs%val(:prev_bcs%nVals)
          write(logunit(1),*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          do iBC=1,prev_bcs%nVals
            iAttr = prev_bcs%val(iBC)
            attrList%dynArray%val(iAttr)%store_normal = merged_sn
          end do
          attrList%bc_uni_storenormal%val(uni_pos) = merged_sn
        end if
      end if foundbc
    end if

    ! Append attribute to dynamic array
    call append( attrList%dynArray, attribute, &
      &          pos = attr_pos,               &
      &          wasAdded = wasAdded           )

    if (wasAdded) then

      write(logunit(1),*) 'This is a new attribute and was added at ', &
        &                 attr_pos
      write(logunit(1),"(A)") '           kind: '//trim(attr_kind)
      write(logunit(1),"(A)") '          label: '//trim(attribute%label)
      write(logunit(1),"(A)") '          color: '//trim(attribute%color)
      write(logunit(1),"(A,I0)") '          level: ', attribute%level
      if (attribute%kind == sdr_Boundary_object) then
        write(logunit(1),*) '  subresolution: ', attribute%subresolution
        write(logunit(1),*) '  calc distance: ', attribute%calc_dist
        write(logunit(1),*) '  store normal : ', attribute%store_normal
        if (attribute%calc_dist) then
          write(logunit(1),*) ' flood diagonal: ', attribute%flood_diagonal
        end if
        if (attribute%distRefine_id > 0) then
          write(logUnit(1),"(A,I0)") '  Nr. of distance_refine: ', &
            &   attribute%distance_last - attribute%distance_first + 1
          do iDistRefine = attribute%distance_first, attribute%distance_last
            write(logunit(1),"(A,I0)")    '    iDistRefine: ', iDistRefine
            write(logUnit(1),"(A,E13.6)") '         radius: ',      &
              &        attrList%distRefine%val(iDistRefine)%radius
            write(logUnit(1),"(A,I0)") '    reach_level: ', &
              &        attrList%distRefine%val(iDistRefine)%reach_level
          end do
        end if
      end if

      ! Update the counters for this specific kind of object.
      if (trim(attr_kind) /= 'periodic') then
        call append(attrList%kindpos(attribute%kind), attr_Pos)
        attrList%dynArray%val(attr_pos)%id &
          &  = attrList%kindpos(attribute%kind)%nVals

        select case(attribute%kind)
        case (sdr_Seed_object)
          call append(attrList%uni_name(attribute%kind), attribute%color, &
            &         pos = uni_pos)
          ! If this was the 'none' color, store its position.
          if (trim(attribute%color) == 'none') attrList%none_color_id = uni_pos

        case default
          call append(attrList%uni_name(attribute%kind), attribute%label, &
            &         pos = uni_pos, wasAdded = newuni)

          if (newuni) then
            write(logunit(1),*) 'Added a new unique label to the the list of' &
              &                 //' kind "'//trim(attr_kind)//'":'
            write(logunit(1),*) trim(attribute%label)

            if (attribute%kind == sdr_Boundary_object) then
              ! Add the unique calc_dist setting for this boundary.
              call append(attrList%bc_uni_calcdist, attribute%calc_dist)
              ! Add the unique store_normal setting for this boundary.
              call append(attrList%bc_uni_storenormal, attribute%store_normal)
            end if
          end if

        end select

        attrList%dynArray%val(attr_pos)%uni_id = uni_pos
        write(logunit(1),*) 'Unique identifier position of this attribute:', &
          &                 uni_pos

      else
        !do not count boundary Id counter in the attribute for periodic planes
        attrList%dynArray%val(attr_pos)%id = -1
      end if

    else
      write(logunit(1),*) 'This is an old attribute found at ', attr_pos
    end if

  end subroutine sdr_load_attribute
! ******************************************************************************!


  ! ****************************************************************************!
  !> This routine loads the distance refine table from attribute table and
  !! appends the loaded distance in radius and level to refine nodes within
  !! radius the given radius.
  !!
  !! to array of distance refine and store nVals in distance refine to
  !! distRefine_id in attribute to create unique list of attribute
  subroutine load_distanceRefine(attrList, conf, thandle, attribute)
    ! --------------------------------------------------------------------------!
    !> dynamic array of attribute type
    type(sdr_attrList_type), intent(inout) :: attrList
    type(flu_State) :: conf !< lua state
    integer, intent(in) :: thandle !<  current attribute handle
    !> save attibute info temporarily
    type(sdr_attribute_type), intent(inout) :: attribute
    ! --------------------------------------------------------------------------!
    integer :: dr_handle, dr_subhandle
    integer :: dr_length, iDistRefine
    ! --------------------------------------------------------------------------!
    write(logunit(1),*) 'Loading distance_refine'

    ! Open distance refine table
    call aot_table_open(L = conf, parent = thandle, thandle = dr_handle, &
      &                 key = 'distance_refine')

    ! load distance refine only if the table exists otherwise prompt
    ! warning message and set attribute%distRefine_id = 0
    if (dr_handle > 0) then
      ! set current attribute distance_refine start position in growing
      ! array of distance_refine
      attribute%distance_first = attrList%distRefine%nVals + 1

      ! Load distance refine sub handle to check between single table or multiple table
      call aot_table_open( L = conf, parent = dr_handle,   &
        &                  thandle = dr_subhandle, pos = 1 )
      if (dr_subhandle == 0) then
        ! distance refine is a single table
        call aot_table_close( L = conf, thandle = dr_subHandle )
        call load_distanceRefine_single(attrList  = attrList,  &
          &                             conf      = conf,      &
          &                             thandle   = dr_handle, &
          &                             attribute = attribute  )
      else
        ! distance refine is a multiple table
        call aot_table_close( L = conf, thandle = dr_subHandle )
        dr_length = aot_table_length(L = conf, thandle = dr_handle)
        do iDistRefine = 1, dr_length
          call aot_table_open( L = conf, parent = dr_handle,             &
            &                  thandle = dr_subhandle, pos = iDistRefine )
          call load_distanceRefine_single(attrList  = attrList,     &
            &                             conf      = conf,         &
            &                             thandle   = dr_subHandle, &
            &                             attribute = attribute     )
          call aot_table_close( L = conf, thandle = dr_subHandle )
        end do
      end if
      ! set nVals of distance_refine as distRefine_id to create
      ! unique list of attribute
      attribute%distRefine_id = attrList%distRefine%nVals

      ! set current attribute distance_refine last position in growing
      ! array of distance_refine
      attribute%distance_last = attrList%distRefine%nVals
    else
      write(logUnit(5),*) 'WARNING: No distance_refine defined for attribute'
      write(logUnit(5),*) '         boundary label: '//trim(attribute%label)

      ! no distance refine
      attribute%distRefine_id = 0
      attribute%distance_first = 0
      attribute%distance_last = 0
    end if

    call aot_table_close(L = conf, thandle = dr_handle)

  end subroutine load_distanceRefine
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> load single distance refine table
  subroutine load_distanceRefine_single(attrList, conf, thandle, attribute)
    ! --------------------------------------------------------------------------!
    !> dynamic array of attribute type
    type(sdr_attrList_type), intent(inout) :: attrList
    type(flu_State) :: conf !< lua state
    integer, intent(in) :: thandle !<  handle to load distance refine from
    !> save attibute info temporarily
    type(sdr_attribute_type), intent(inout) :: attribute
    ! --------------------------------------------------------------------------!
    integer :: iError
    type(sdr_distanceRefine_type) :: distRefine
    integer :: level_offset
    ! --------------------------------------------------------------------------!

    ! load radius which defines the distance to apply the level
    call aot_get_val(L = conf, thandle = thandle, key = 'radius', &
      &              val = distRefine%radius, ErrCode = iError )

    if (btest(iError, aoterr_Fatal)) then
      write(logUnit(0),*) &
        &  'FATAL Error occured, while retrieving distance_refine radius:'
      if (btest(iError, aoterr_NonExistent)) &
        &  write(logunit(0),*) 'distance_refine radius not existent!'
      if (btest(iError, aoterr_WrongType)) &
        &  write(logunit(0),*)'distance_refine has wrong type!'
      call tem_abort()
    end if

    ! load level to refine node within given radius.
    ! if level is not defined, set default to current boundary level
    call aot_get_val(L = conf, thandle = thandle, key = 'level_offset', &
      &              val = level_offset, ErrCode = iError,              &
      &              default = 0)
    if (btest(iError, aoterr_WrongType)) then
      write(logunit(0),*) &
        &  'FATAL Error occured, while retrieving distance_refine &
        &  level_offset:'
      write(logunit(0),*) 'level_offset has wrong type, should be an Integer!'
      call tem_abort()
    end if

    if (level_offset > 0) then
      write(logUnit(0),*) 'ERROR: level_offset is greater than zero'
      write(logUnit(0),*) 'SOLUTION: level_offset must be <= 0'
      call tem_abort()
    end if

    ! compute reach level from level_offset
    distRefine%reach_level = max(attribute%level + level_offset,0)

    if (distRefine%reach_level==0) then
      write(logUnit(1),*) 'WARNING: Distance refine level is negative'
      write(logUnit(1),*) '         i.e level+offset_level < 0'
      write(logUnit(1),*) '         This distance refine will be omitted'
    end if

    ! append distance refine
    call append(me = attrList%distRefine, val = distRefine)

  end subroutine load_distanceRefine_single
  ! ****************************************************************************!


! ******************************************************************************!
  !> Identify all unique boundary colors, after all boundary attributes and
  !! seeds are known.
  !!
  !! This has to be done separately from reading the attributes, due to the
  !! interdependence of the boundaries and seed objects. It has to be ensured
  !! that all seed objects are known at this point.
  !!
  !! The bc_colors will correspond to the unique color name list, or are 0
  !! if the bc color does not exist in that list.
  !! 'none' bc colors are marked negatively as they are to be applied to all
  !! seed colors.
  subroutine sdr_identify_bc_colors(attribute)
    type(sdr_attrList_type), intent(inout) :: attribute

    integer :: nBCs
    integer :: colorpos
    integer :: iBC, iAttr
    character(len=labelLen) :: bc_color

    nBCs = attribute%kindpos(sdr_Boundary_Object)%nVals
    allocate(attribute%bc_color_id(nBCs))

    do iBC=1,nBCs
      iAttr = attribute%kindpos(sdr_Boundary_Object)%val(iBC)
      bc_color = attribute%dynArray%val(iAttr)%color
      if (trim(bc_color) == 'none') then
        attribute%bc_color_id(iBC) = -1
      else
        ! colorpos is 0 if the bc_color is not found in the uni_name list,
        ! otherwise it is the corresponding index in the list of unique color
        ! names.
        colorpos = PositionOfVal(me  = attribute%uni_name(sdr_Seed_Object), &
          &                      val = bc_color)
        attribute%bc_color_id(iBC) = colorpos
      end if
    end do

  end subroutine sdr_identify_bc_colors
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> Look up colors that should be inverted and set their inversion flag
  !! accordingly.
  subroutine sdr_identify_inv_colors(attribute, invert_color)
    type(sdr_attrList_type), intent(inout) :: attribute
    type(dyn_labelArray_type), intent(in) :: invert_color

    integer :: nColors
    integer :: iColor
    character(len=labelLen) :: colname

    nColors = attribute%uni_name(sdr_seed_object)%nVals

    allocate(attribute%color_inverted(nColors))

    attribute%color_inverted = .false.

    do iColor=1,nColors
      colname = attribute%uni_name(sdr_seed_object)%val(iColor)
      if (trim(colname) /= 'none') then
        ! If the color is found in the list of colors to invert, mark it
        ! accordingly now.
        attribute%color_inverted(iColor) &
          &  = (PositionOfVal(invert_color, trim(colname)) > 0)
      end if
    end do

  end subroutine sdr_identify_inv_colors
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> Returns if any bc attribute has the subresolution option set.
  !!
  !! The subresolution option only plays a role for boundary objects and only
  !! for those with a specific color (not none). This routine iterates over all
  !! boundary definitions and checks the subresolution flag. If any flag is
  !! true, the result of the function will be true.
  !! Only colors, which do have a corresponding seed are considered here.
  !! Note, that the subresolution flag will only be set for specific colors,
  !! thus, we do not need to check for 'none' colors here separately.
  function sdr_any_bc_subresolution(attribute) result(any_subres)
    !> The attribute list to check th boundaries in.
    type(sdr_attrList_type), intent(in) :: attribute

    !> Returned value, true if any boundary object has the subresolution flag
    !! set.
    logical :: any_subres

    integer :: nBCs
    integer :: iBC, iAttr

    nBCs = attribute%kindpos(sdr_Boundary_Object)%nVals
    any_subres = .false.

    do iBC=1,nBCs
      ! Only colors that actually do have an active seed (bc_color_id>0),
      ! need to be considered.
      if (attribute%bc_color_id(iBC) > 0) then
        iAttr = attribute%kindpos(sdr_Boundary_Object)%val(iBC)
        any_subres &
          & = (any_subres .or. attribute%dynArray%val(iAttr)%subresolution)
      end if
    end do

  end function sdr_any_bc_subresolution
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> Returns if any bc attribute has the distance refine option with
  !! reach_level>0.
  !!
  !! The distance refine option only plays a role for boundary objects.
  !! This routine iterates over all
  !! boundary definitions and checks the distRefine_id>0 and
  !! distRefine%reach_level>0.
  function sdr_any_bc_distanceRefine(attribute) result(any_distRef)
    !> The attribute list to check th boundaries in.
    type(sdr_attrList_type), intent(in) :: attribute

    !> Returned value, true if any boundary object has the
    !! valid distance refine
    logical :: any_distRef

    integer :: nBCs
    integer :: iBC, iAttr, distRef_first, distRef_last

    nBCs = attribute%kindpos(sdr_Boundary_Object)%nVals
    any_distRef = .false.

    do iBC=1,nBCs
      iAttr = attribute%kindpos(sdr_Boundary_Object)%val(iBC)
      if (attribute%dynArray%val(iAttr)%distRefine_id > 0) then
        distRef_first = attribute%dynArray%val(iAttr)%distance_first
        distRef_last = attribute%dynArray%val(iAttr)%distance_last

        any_distRef = ( any_distRef .or.                             &
          & any(attribute%distRefine%val(distRef_first:distRef_last) &
          &                         %reach_level>0) )
      end if
    end do

  end function sdr_any_bc_distanceRefine
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> Returns if periodic bc attribute is defined
  function sdr_isPeriodicDefined(attribute) result(isPeriodic)
    !> The attribute list to check th boundaries in.
    type(sdr_attrList_type), intent(in) :: attribute

    !> Returned value, true if periodic attribute is defined
    logical :: isPeriodic

    integer :: nAttrs

    nAttrs = attribute%dynArray%nVals
    isPeriodic = any(attribute%dynArray%val(1:nAttrs)%id == -1)
  end function sdr_isPeriodicDefined
  ! ****************************************************************************!

  ! ------------------------------------------------------------------------ !
  !> Get all attributes of the given object kind and (unique) id from the
  !! list of attributes in "attribute".
  function sdr_attr_of_uni(attribute, object_kind, id) result(allattr)
    ! -------------------------------------------------------------------- !
    !> The list of attributes to look up the unique entries in.
    type(sdr_attrList_type), intent(in) :: attribute

    !> The kind of objects to look for (boundaries, seeds, refinements...)
    integer, intent(in) :: object_kind

    !> The unique id, to look for (all attributes with this id will be
    !! returned).
    integer, intent(in) :: id

    !> Resulting list of attributes with the same unique id.
    type(grw_IntArray_type) :: allattr
    ! -------------------------------------------------------------------- !
    integer :: iAttr
    ! -------------------------------------------------------------------- !

    call init(allattr)

    do iAttr=1,attribute%dynArray%nVals
      if (attribute%dynArray%val(iAttr)%kind == object_kind) then
        if (attribute%dynArray%val(iAttr)%uni_id == id) then
          call append(me = allattr, val = iAttr)
        end if
      end if
    end do

  end function sdr_attr_of_uni
  ! ------------------------------------------------------------------------ !
  ! ------------------------------------------------------------------------ !


?? copy :: GA_impltxt(distanceRefine, type(sdr_distanceRefine_type), type(sdr_distanceRefine_type))
?? copy :: DA_impltxt(attribute, type(sdr_attribute_type), type(sdr_attribute_type))

! ******************************************************************************!
  !> This function provides the test for equality of two attributes.
  !!
  !! Two attributes are considered to be equal if their kind, label, level,
  !! color, subresolution and distancerefine are all equal.
  function isEqual(left, right) result(equality)
    ! ---------------------------------------------------------------------------
    !> attribute to compare
    type(sdr_attribute_type), intent(in) :: left
    !> attribute to compare against
    type(sdr_attribute_type), intent(in) :: right
    !> is equal??
    logical :: equality
    ! ---------------------------------------------------------------------------

    equality = ( left%kind == right%kind ) &
      &      .and. ( trim(left%label) == trim(right%label) ) &
      &      .and. ( trim(left%color) == trim(right%color) ) &
      &      .and. ( left%level == right%level ) &
      &      .and. ( left%distRefine_id == right%distRefine_id ) &
      &      .and. ( left%subresolution .eqv. right%subresolution )

  end function isEqual
! ******************************************************************************!



! ******************************************************************************!
  !> This function provides the test for unequality of two attributes.
  !!
  !! Two attributes are considered unequal if any of their kind, label, level,
  !! color, subresolution or distancerefine are not equal.
  function isUnequal(left, right) result(unequality)
    ! ---------------------------------------------------------------------------
    !> attribute to compare
    type(sdr_attribute_type), intent(in) :: left
    !> attribute to compare against
    type(sdr_attribute_type), intent(in) :: right
    !> is unequal??
    logical :: unequality
    ! ---------------------------------------------------------------------------

    unequality = ( left%kind /= right%kind ) &
      &        .or. ( trim(left%label) /= trim(right%label) ) &
      &        .or. ( trim(left%color) /= trim(right%color) ) &
      &        .or. ( left%level /= right%level ) &
      &        .or. ( left%distRefine_id /= right%distRefine_id ) &
      &        .or. ( left%subresolution .neqv. right%subresolution )

  end function isUnequal
! ******************************************************************************!


! ******************************************************************************!
  !> This function provides a comparison of two attributes.
  !!
  !! Check if left attribute is smaller than right attribute
  !! by checking smaller for attribute kind, label color and level
  function isSmaller(left, right) result(small)
    ! ---------------------------------------------------------------------------
    !> attribute to compare
    type(sdr_attribute_type), intent(in) :: left
    !> attribute to compare against
    type(sdr_attribute_type), intent(in) :: right
    !> is smaller??
    logical :: small
    ! ---------------------------------------------------------------------------

    small = .false.
    if (left%kind < right%kind) then
      small = .true.
    else if (left%kind == right%kind) then
      if (trim(left%label) < trim(right%label)) then
        small = .true.
      else if (trim(left%label) == trim(right%label)) then
        if (trim(left%color) < trim(right%color)) then
          small = .true.
        else if (trim(left%color) == trim(right%color)) then
          if (left%level < right%level) then
            small = .true.
          else if (left%level == right%level) then
            if (left%distRefine_id < right%distRefine_id) then
              small = .true.
            else if (left%distRefine_id == right%distRefine_id) then
              small = ( (left%subresolution .neqv. right%subresolution) &
                &       .and. right%subresolution )
            end if
          end if
        end if
      end if
    end if

  end function isSmaller
! ******************************************************************************!


! ******************************************************************************!
  !> This function provides a comparison of two attributes.
  !!
  !! Check if left attribute is smaller or equal than right attribute
  !! by checking smaller or equal for attribute kind, label color and level
  function isSmallerOrEqual(left, right) result(small)
    ! ---------------------------------------------------------------------------
    !> attribute to compare
    type(sdr_attribute_type), intent(in) :: left
    !> attribute to compare against
    type(sdr_attribute_type), intent(in) :: right
    !> is smaller??
    logical :: small
    ! ---------------------------------------------------------------------------

    small = .false.
    if (left%kind < right%kind) then
      small = .true.
    else if (left%kind == right%kind) then
      if (trim(left%label) < trim(right%label)) then
        small = .true.
      else if (trim(left%label) == trim(right%label)) then
        if (trim(left%color) < trim(right%color)) then
          small = .true.
        else if (trim(left%color) == trim(right%color)) then
          if (left%level < right%level) then
            small = .true.
          else if (left%level == right%level) then
            if (left%distRefine_id < right%distRefine_id) then
              small = .true.
            else if (left%distRefine_id == right%distRefine_id) then
              small = ( (left%subresolution .eqv. right%subresolution) &
                &       .or. right%subresolution )
            end if
          end if
        end if
      end if
    end if

  end function isSmallerOrEqual
! ******************************************************************************!


! ******************************************************************************!
  !> This function provides a comparison of two attributes.
  !!
  !! Check if left attribute is greater than right attribute
  !! by checking greater for attribute kind, label, color and level
  function isGreater(left, right) result(great)
    ! ---------------------------------------------------------------------------
    !> attribute to compare
    type(sdr_attribute_type), intent(in) :: left
    !> attribute to compare against
    type(sdr_attribute_type), intent(in) :: right
    !> is greater??
    logical :: great
    ! ---------------------------------------------------------------------------

    great = .false.
    if (left%kind > right%kind) then
      great = .true.
    else if (left%kind == right%kind) then
      if (trim(left%label) > trim(right%label)) then
        great = .true.
      else if (trim(left%label) == trim(right%label)) then
        if (trim(left%color) > trim(right%color)) then
          great = .true.
        else if (trim(left%color) == trim(right%color)) then
          if (left%level > right%level) then
            great = .true.
          else if (left%level == right%level) then
            if (left%distRefine_id > right%distRefine_id) then
              great = .true.
            else if (left%distRefine_id == right%distRefine_id) then
              great = ( (left%subresolution .neqv. right%subresolution) &
                &       .and. left%subresolution )
            end if
          end if
        end if
      end if
    end if

  end function isGreater
! ******************************************************************************!


! ******************************************************************************!
  !> This function provides a comparison of two attributes.
  !!
  !! Check if left attribute is greater or equal than right attribute
  !! by checking greater for attribute kind, label, color and level
!  function isGreaterOrEqual(left, right) result(great)
!    ! ---------------------------------------------------------------------------
!    !> attribute to compare
!    type(sdr_attribute_type), intent(in) :: left
!    !> attribute to compare against
!    type(sdr_attribute_type), intent(in) :: right
!    !> is greater??
!    logical :: great
!    ! ---------------------------------------------------------------------------
!
!    great = .false.
!    if (left%kind > right%kind) then
!      great = .true.
!    else if (left%kind == right%kind) then
!      if (trim(left%label) > trim(right%label)) then
!        great = .true.
!      else if (trim(left%label) == trim(right%label)) then
!        if (trim(left%color) > trim(right%color)) then
!          great = .true.
!        else if (trim(left%color) == trim(right%color)) then
!          if (left%level > right%level) then
!            great = .true.
!          else if (left%level == right%level) then
!            if (left%distRefine_id > right%distRefine_id) then
!              great = .true.
!            else if (left%distRefine_id == right%distRefine_id) then
!              great = ( (left%subresolution .eqv. right%subresolution) &
!                &       .or. left%subresolution )
!            end if
!          end if
!        end if
!      end if
!    end if
!
!  end function isGreaterOrEqual
! ******************************************************************************!

end module sdr_attribute_module

!> \page attribute Attribute
!! Attribute defines the property for the geometries defined in the
!! spatial object. Following attribute kinds are supported,
!! - \ref boundary "Boundary"
!! - \ref seed "Seed"
!! - \ref refinement "Refinement".
!! \n
!! Attribute list is dynamic unique list. Thus, multiple definitions of
!! the same attribute in the configuration file do not result in multiple
!! storages.
