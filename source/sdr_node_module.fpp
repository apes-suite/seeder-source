! Copyright (c) 2012-2014, 2022 Harald Klimach <harald.klimach@dlr.de>
! Copyright (c) 2012-2013, 2015, 2019 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2012 Jiaxing Qi <jiaxing.qi@uni-siegen.de>
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
?? include 'arrayMacros.inc'
! ******************************************************************************!
!> This module implements the description of a node in the tree that is going to
!! be created by Seeder.
module sdr_node_module
  use env_module,            only: rk, long_k, minLength, zeroLength
  use tem_param_module,      only: qQQQ
  use tem_grow_array_module, only: grw_int2dArray_type, grw_longArray_type,    &
    &                              grw_intArray_type, &
    &                              init, append, placeAt, truncate
  use tem_dyn_array_module,  only: dyn_longArray_type, init, append, truncate
  use tem_color_prop_module, only: colors_per_char
  use tem_geometry_module,   only: tem_eligibleChildren
  use tem_mergesort_module,  only: mrgrnk

  use sdr_geometry_module,   only: sdr_geometry_type

  implicit none

  private

  public :: sdr_node_type

  public :: init, append, truncate, destroy, empty, placeAt
  public :: sdr_wetNeighborsFace
  public :: sdr_mark_floodNode
  public :: sdr_set_nodeProp_bit
  public :: sdr_clear_nodeProp_bit
  public :: sdr_nodeProp_btest
  public :: sdr_nodeColors
  public :: sdr_bitfieldColors
  public :: sdr_color_log2char
  public :: sdr_inHeritBnd_eligibleChildren
  public :: sdr_intersectObjPos_type
  public :: grw_intersectObjPosArray_type
  public :: sdr_append_childIntersectedObject

  ! Some parameters to encode node properties in the PropertyBits bitmask

  !> This bit is set, if the node is a leaf, that is there are no further
  !! descendants.
  integer, parameter, public :: isLeaf_bit = 1

  !> If this bit is set, the node intersects some computational domain boundary.
  integer, parameter, public :: IntersectsBoundary_bit = 2

  !> If any of 26 direct neighbor is intersected by boundary 
  integer, parameter, public :: hasBoundary_bit = 3

  !> This bit is set, if the node is a fluidifyable that can be 
  !! fluidify in solver
  integer, parameter, public :: isFluidifyable_bit = 4

  !> If the node is flooded by any color, this bit is set.
  !! It is useful to identify elements, that belong to the computational
  !! domain.
  integer, parameter, public :: isFlooded_bit = 5

  !> Indicating if the node is colored with an non-none color.
  !!
  !! This is used to filter none colors when transferring the information from
  !! the prototree to the treelm data structure.
  integer, parameter, public :: isColored_bit = 6

  !> Indicates if this node is one that is to be included in the final mesh.
  !!
  !! It is needed to distinguish such nodes, which are refined further for
  !! subelement resolution of boundaries, from ordinary virtual nodes.
  integer, parameter, public :: isTarget_bit = 7

  !> This bit is set, if solidification is not allowed for the node
  !!
  integer, parameter, public :: isNoSolidification_bit = 0

  !> This data type describes first and last intersected object
  !! of a node in growing array of intersected_objects.
  !!
  !! Childs intersected objects are filled in parents intersected
  !! objects position until childs objects fit. 
  !! Children are filled with sorted fashsion from child with
  !! max nObjects to least
  type sdr_intersectObjPos_type
    !> First position of intersected object in 1st list
    integer :: first

    !> Last position of intersected object in 1st list
    integer :: last
  end type sdr_intersectObjPos_type


?? copy :: GA_decltxt(intersectObjPos, type(sdr_intersectObjPos_type))
?? copy :: GA_decltxt(grwInt, type(grw_intArray_type))


  !> This data type describes a node in the tree to be created.
  type sdr_node_type

    !> Number of nodes in this list.
    integer :: nNodes = 0

    !> Number of integers to encode the properties.
    !! This depends on the number of colors and defines the size of the
    !! the first dimension (width) in the grw_int2dArray propertyBits.
    integer :: propLength = 1

    !> Auxilary value to provide the number of bytes available per integer
    !! in the propertybits.
    integer :: bytes_per_int

    !> Indicator for the starting bit of the last byte in the propertybits.
    integer :: lastbyte_pos

    !> Counter for the number of colors in the mesh.
    integer :: nColors

    !> Id of the 'none' color, this color will not be considered in the final
    !! color information for treelm.
    !!
    !! If there is no 'none' color this will be -1, otherwise it is the id
    !! of the color with the label 'none' in the unique list of color labels.
    integer :: none_color_id

    !> Number of characters needed to hold all colors.
    integer :: nColorChars

    !> Number of levels, that need to be further refined for colors with
    !! subelement resolution.
    !!
    !! If there are no colors with subelement resolution at all, this setting
    !! is 0.
    integer :: subelement_resolution

    !> True if distance refine objects are created for 
    !! boundary attribute.
    !!
    !! If there is no distance refine object then no need to append
    !! distance_first and distance_last for every node
    logical :: distanceRefine = .false.

    !> List of treeIDs for all the nodes in the built tree.
    !!
    !! We are exploiting here the fact, that new nodes are created in order
    !! level by level. Therefore, it is not necessary to use an
    !! additional index list to maintain the ranking of the entries, as it is
    !! done in the dyn_arrays.
    type(dyn_longArray_type) :: treeID

    !> The property bits can be used to attach various information to the
    !! element.
    !! They can span multiple integers (width of the 2d growing array) and all
    !! bytes except the very last one are used to encode colors (1 byte per
    !! color).
    !!
    !! In the last byte other properties are encoded (Bits 0-7) of last byte.
    !! (The last byte is given by
    !!  ibits(PropertyBits%val(propLength,iNode), pos=bit_size(int)-8, len=8))
    !! The following bits are used:
    !! Bit 0: NoSolidification is set, if solidification is not allowed for node
    !! Bit 1: isLeaf Bit is set, if there are no further descendants
    !! Bit 2: IntersectsBoundaries, is set, if the element intersects boundaries
    !! Bit 3: IntersectsAll Set, if all objects are seperate intersected_object
    !! Bit 4: isFluidifyable is set, if the node is fluidifyable.
    !! Bit 5: isFlooded is set, if node is flooded by any color.
    !! Bit 6: isColored is set, if there is another than the none color in the
    !!        node.
    !! Bit 7: isTarget is set, if this node should be in the final mesh, even
    !!        so it has further children.
    !!
    !! To encode a color, we need a representation for each of the 6 sides and
    !! one for the volume, resulting in a total of 7 bits for each color.
    !! Thus, a complete Byte is used for each color. The first color is stored
    !! in the first Byte of the first integer and the other follow
    !! consecutively.
    !! Ordering of the sides as given by the definition described for the
    !! linkPos component.
    !! We also need to store intersections on a per color basis, as we want to
    !! get the flooding stopped by differently colored boundaries.
    !! The layout of the bits for each color ar therefore:
    !! Bit   0: intersects a relevant geometry (same color or geom. color = -1)
    !! Bit   1: node is flooded with this color
    !! Bit 2-7: colored wetness of the 6 node sides
    type(grw_int2dArray_type) :: PropertyBits

    !> Store which of 26 directions of node has boundary.
    !! Uses first 26 bits of the integer with each bit
    !! corresponds to treelm direction given by qOffset in tem_param_module
    type(grw_intArray_type) :: hasBndBits

    !> Minimum boundary ID intersect by the node
    !!
    !! Used to set boundaryID based on thuming rule when a node
    !! is intersected by more than one boundary object
    type(grw_intArray_type) :: minBCID

    !> List of all objects intersecting this node.
    !!
    !! If children are created for this node, the number of children
    !! with intersected objects thats fits in to the memory of its parent
    !! starting from child with highest number of intersected object
    !! will be copied to the memory position of its parent to save memory.
    type(grw_intArray_type) :: intersected_object

    !> First and last position of this node's intersected objects in
    !! growing array of intersected_object
    type(grw_intersectObjPosArray_type) :: userObjPos

    !> List of all sphere_distance objects intersecting this node
    type(grw_intArray_type) :: intersected_distance

    !> First and last position of this node's intersected distance refine 
    !! objects in growing array of intersected_distance
    type(grw_intersectObjPosArray_type) :: distObjPos

    !> Memory unused by children from parent intersected object list
    integer :: memLeft_userObj

    !> Memory unused by children from parent intersected distance list
    integer :: memLeft_distObj

    !> minLevel for each node. 
    !! Used in refineleaf and inherit distance refine object.
    !! If intersected object level is <= minLevel then that object
    !! is not added to intersected object list for children.
    !! This should reduce memory to inHerit lot of intersected objects
    !! to children
    type(grw_intArray_type) :: minLevel

    !> This is used to store the location of the 6 neighbors for leaf nodes and
    !! the first child for virtual nodes at linkpos(1).
    !!
    !! 1: neighbor x-1 / first child node
    !! 2: neighbor x+1
    !! 3: neighbor y-1
    !! 4: neighbor y+1
    !! 5: neighbor z-1
    !! 6: neighbor z+1
    type(grw_intArray_type) :: linkpos(6)

    !> If subelement resolution is required, the sublevel keeps track of how
    !! deep the subelement resolution is below the target element.
    !!
    !! For nodes above isTarget nodes it is set to a negative value. In the
    !! target node it is set to the subelement_resolution.
    !! On each level this counter is decreased by 1, when passing it on to the
    !! children. Boundaries, that require subelement resolution will stop in
    !! refinement, when sublevel has reached 0, or if a total level of 20 is
    !! reached.
    !! If subelement_resolution is 0, this array is not used
    !! (and not allocated).
    type(grw_intArray_type) :: sublevel

  end type sdr_node_type


  interface sdr_wetNeighborsFace
    module procedure sdr_wetNeighborsFace_single
    module procedure sdr_wetNeighborsFace_all
  end interface

  interface init
    module procedure init_node
  end interface

  interface append
    module procedure append_newNode
  end interface

  interface truncate
    module procedure truncate_node
  end interface

  interface sdr_append_childIntersectedObject
    module procedure sdr_append_childIntersectedObjectAll
    module procedure sdr_append_childIntersectedObjectGTminLevel
  end interface sdr_append_childIntersectedObject

contains

?? copy :: GA_impltxt(intersectObjPos, type(sdr_intersectObjPos_type))
?? copy :: GA_impltxt(grwInt, type(grw_intArray_type))

  !> Initialize the node type.
  subroutine init_node(me, nColors, none_color_id, distanceRefine, nSublevels, &
    &                  length)
    !> The nodelist to initialize.
    type(sdr_node_type), intent(out) :: me

    !> Number of different colors that are to be used.
    integer, intent(in) :: nColors

    !> Id of the color with the 'none' label, this color will be ignored in the
    !! treelm color data. It has to be -1, if there is no none color.
    integer, intent(in) :: none_color_id

    !> Number of levels to refine beyond target element levels for boundaries,
    !! that require this. Has to be 0, if there are no such boundaries.
    integer, intent(in) :: nSublevels

    !> If any boundary attribute has distance refine
    logical, intent(in) :: distanceRefine

    !> Initial length for the list of nodes.
    integer, intent(in), optional :: length

    integer :: iSide
    integer :: nNonecolors

    me%nNodes = 0
    me%nColors = nColors
    me%none_color_id = none_color_id
    me%subelement_resolution = nSublevels
    me%distanceRefine = distanceRefine
    me%memLeft_userObj = 0
    me%memLeft_distObj = 0

    ! Number of none colors (0 or 1), can be found by evaluating none_color_id
    ! as it is -1 if there is none and positive otherwise:
    nNonecolors = min(none_color_id,0) + 1

    me%nColorChars = ceiling(real(nColors-nNonecolors)/real(colors_per_char))

    ! Number of bytes there are in a default integer
    me%bytes_per_int = bit_size(me%bytes_per_int)/8

    ! Start position of the last byte in default integers
    me%lastbyte_pos = (me%bytes_per_int-1)*8

    ! We need nColors + 1 bytes at least, thus the proplength is given by:
    me%propLength = ceiling(real(nColors+1,kind=rk)/me%bytes_per_int)

    call init(me%treeID, length=length)
    call init(me%PropertyBits, length = length, width = me%propLength)
    call init(me%hasBndBits, length = length)
    call init(me%minLevel, length)
    call init(me%intersected_object, length)
    call init(me%userObjPos, length)
    if (me%distanceRefine) then
      call init(me%intersected_distance, length)
      call init(me%distObjPos, length)
    end if  
    do iSide=1,6
      call init(me%linkpos(iSide), length)
    end do
    call init(me%minBCID, length)
    if (me%subelement_resolution > 0) call init(me%sublevel, length)

  end subroutine init_node


  !> Truncate the growing arrays in the node list to their actual size.
  subroutine truncate_node(me)
    !> The nodelist to initialize.
    type(sdr_node_type), intent(inout) :: me

    integer :: iSide

    !!call truncate(me%treeID)
    !!call truncate(me%PropertyBits)
    call truncate(me%hasBndBits)
    call truncate(me%minLevel)
    call truncate(me%intersected_object)
    call truncate(me%userObjPos)
    if (me%distanceRefine) then
      call truncate(me%intersected_distance)
      call truncate(me%distObjPos)
    end if
    do iSide=1,6
      call truncate(me%linkpos(iSide))
    end do

    call truncate(me%minBCID)
    if (me%subelement_resolution > 0) call truncate(me%sublevel)

  end subroutine truncate_node


  ! ****************************************************************************
  !> Append a new node to the protoTree.
  !!
  !! Please note that the chosen data structures exploit the fact, that the
  !! algorithm to build the tree is doing this ordered by treeIDs. Therfore,
  !! this routine appends a new node at the end of the list of existing nodes
  !! without any checks.
  subroutine append_newNode(me, treeID, PropertyBits, sublevel, minlevel,      &
    &                       pos, grwTreeID)
    ! --------------------------------------------------------------------------!
    !> The nodelist to which the node is to be appended.
    type(sdr_node_type), intent(inout) :: me

    !> The treeID of the node to append.
    integer(kind=long_k), intent(in) :: treeID

    !> The Bitmask of initial properties to set.
    integer, intent(in) :: PropertyBits(:)

    !> Sublevel indication, counts down after the target node. Has to be
    !! negative if not a subelement. Only relevant if subelement_resolution > 0.
    !!
    !! Refinement will be stopped, when sublevel 0 is reached.
    integer, intent(in) :: sublevel

    !> Minlevel to refine this node
    integer, intent(in) :: minLevel

    !> Position in the list, where the node was appended.
    integer, intent(out) :: pos

    !> growing array of treeID, append treeID to this list if present
    type(grw_longArray_type), intent(inout), optional :: grwTreeID
    ! --------------------------------------------------------------------------!
    integer :: iSide
    type(sdr_intersectObjPos_type) :: objPos
    ! --------------------------------------------------------------------------!

    if (present(grwTreeID)) then
      call append(grwTreeID, treeID)
      me%nNodes = me%nNodes + 1
      pos = me%nNodes
    else  
      call append(me%treeID, treeID, pos=pos)
      me%nNodes = me%treeID%nVals
    end if  

    call append(me%PropertyBits, PropertyBits)
    call append(me%hasBndBits, 0)
    do iSide=1,6
      call append(me%linkpos(iSide), 0)
    end do

    call append(me%minLevel, minLevel)

    !KM: No need to append empty intersected_object
    !call append(me%intersected_object, 0)
    objPos%first = 1
    objPos%last = 0
    call append(me%userObjPos, objPos)

    if (me%distanceRefine) then
      call append(me%distObjPos, objPos)
    end if

    call append(me%minBCID, 0)

    if (me%subelement_resolution > 0) call append(me%sublevel, sublevel)
 
  end subroutine append_newNode
  ! ****************************************************************************


  ! ****************************************************************************
  !> Set a bit in the last byte of the node properties in a given node.
  !!
  !! This routine can be used to set a property bit in the last byte of a node,
  !! which is used for the flags that are independent of the colors.
  subroutine sdr_set_nodeProp_bit(node, iNode, bit)
    type(sdr_node_type), intent(inout) :: node
    integer, intent(in) :: iNode
    integer, intent(in) :: bit

    integer :: nodeprop
    
    nodeprop = node%PropertyBits%val(node%propLength, iNode) 
    node%PropertyBits%val(node%proplength, iNode) &
      &  = ibSet(nodeprop, node%lastbyte_pos + bit)
  end subroutine sdr_set_nodeProp_bit
  ! ****************************************************************************


  ! ****************************************************************************
  !> Clear a bit in the last byte of the node properties in a given node.
  !!
  !! This routine can be used to clear a property bit in the last byte of a
  !! node, which is used for the flags that are independent of the colors.
  subroutine sdr_clear_nodeProp_bit(node, iNode, bit)
    type(sdr_node_type), intent(inout) :: node
    integer, intent(in) :: iNode
    integer, intent(in) :: bit

    integer :: nodeprop
    
    nodeprop = node%PropertyBits%val(node%propLength, iNode) 
    node%PropertyBits%val(node%proplength, iNode) &
      &  = ibClr(nodeprop, node%lastbyte_pos + bit)
  end subroutine sdr_clear_nodeProp_bit
  ! ****************************************************************************


  ! ****************************************************************************
  !> Set a bit in the last byte of the node properties in a given node.
  !!
  !! This routine can be used to set a property bit in the last byte of a node,
  !! which is used for the flags that are independent of the colors.
  pure function sdr_nodeProp_btest(node, iNode, bit) result(isSet)
    type(sdr_node_type), intent(in) :: node
    integer, intent(in) :: iNode
    integer, intent(in) :: bit
    logical :: isSet

    integer :: nodeprop
    
    nodeprop = node%PropertyBits%val(node%propLength, iNode) 
    isSet = btest(nodeprop, node%lastbyte_pos + bit)
  end function sdr_nodeProp_btest
  ! ****************************************************************************


  ! ****************************************************************************
  !> This routine will wet the neighbors sides of the neighbor node
  !!
  !! col_int is the integer in which the flags for this color are encoded.
  !! col_bit is the 0th bit of this color within the integer.
  !! Thus, for the first color it should be col_int = 1, col_bit = 0.
  !! iNode is the index of the node whose neighbors are to be marked as
  !! wet. Note, that this will be done unconditionally, that is regardless of
  !! the state in iNode itself.
  !! Be aware, that the indices of neighbor need to be available in the linkpos
  !! array when calling this routine!
  subroutine sdr_wetNeighborsFace_single(node, iNode, col_int, col_bit)
    ! --------------------------------------------------------------------------!
    !> List of all nodes in the tree
    type(sdr_node_type), intent(inout) :: node
    !> Current node
    integer, intent(in) :: iNode
    integer, intent(in) :: col_int
    integer, intent(in) :: col_bit
    ! --------------------------------------------------------------------------!
    integer :: iDir, iSide, dir_off, neighbor_pos, neighborside
    integer :: mySide
    integer :: neighborbit_pos
    ! --------------------------------------------------------------------------!

    dirLoop: do iDir=1,3
      dir_off = (iDir-1)*2
      sideLoop: do iSide=1,2
        mySide = dir_off + iSide
        neighbor_pos = node%linkpos(mySide)%val(iNode)

        ! Copy the color of the current node to the adjacent side of the
        ! neighboring element. This side can be found with the following
        ! modulo operation.
        ! CAUTION: neighborside is defined with 0,1 where mySide is 1,2.
        !          the modulo maps 1 to 0 and 2 to 1, resulting in the
        !          expected behavior.
        neighborside = dir_off + mod(iSide, 2)

        ! Bit 0 is used for the intersection status,
        ! Bit 1 is used for the flooding status.
        ! Wetness is stored from Bit 2 onwards,
        ! neighborside ranges from 0 to 5 and col_bit
        ! provides the index of Bit 0 for this color.
        ! The bit, that has to be set for this side is therefore given by:
        neighborbit_pos = col_bit + neighborside + 2

        node%PropertyBits%val(col_int, neighbor_pos) &
          &   = ibset(node%PropertyBits%val(col_int, neighbor_pos), &
          &           neighborbit_pos)

      end do sideLoop
    end do dirLoop

  end subroutine sdr_wetNeighborsFace_single
  ! ****************************************************************************


  ! ****************************************************************************
  !> This routine will wet the neighbors sides of the neighbor node in all
  !! colors, that are flooded.
  !!
  !! The neighbors have to be available in the linkpos array prior calling
  !! this routine!
  subroutine sdr_wetNeighborsFace_all(node, iNode)
    ! --------------------------------------------------------------------------!
    !> List of all nodes in the tree
    type(sdr_node_type), intent(inout) :: node
    !> Current node
    integer, intent(in) :: iNode
    ! --------------------------------------------------------------------------!
    integer :: iColor
    integer :: col_int
    integer :: col_bit
    logical :: isflooded
    ! --------------------------------------------------------------------------!

    ! Extract the color of the current node from the property bits.
    do iColor=1,node%nColors
      col_int = (iColor-1) / node%bytes_per_int + 1
      col_bit = mod(iColor-1, node%bytes_per_int)*8
      isFlooded = btest(node%PropertyBits%val(col_int,iNode), &
        &               col_bit+1)
      if (isFlooded) then
        call sdr_wetNeighborsFace_single(node, iNode, col_int, col_bit)
      end if
    end do

  end subroutine sdr_wetNeighborsFace_all
  ! ****************************************************************************


  ! ****************************************************************************
  !> This routine floods the node with the given color and increases
  !! nFloodedLeaves.
  subroutine sdr_mark_floodNode( node, iNode, nFloodedLeaves, color )
    ! --------------------------------------------------------------------------!
    !> Node object
    type(sdr_node_type), intent(inout) :: node
    !> Node to modify
    integer, intent(in) :: iNode
    !> Number of flooded leaves to increase
    integer, intent(inout) :: nFloodedLeaves
    !> Color to set in the given bitfield
    integer, intent(in) :: color
    ! --------------------------------------------------------------------------!
    integer :: col_int, col_bit
    integer :: nodeFlooded_bit

    nodeFlooded_bit = node%lastbyte_pos + isFlooded_bit
    ! If this node was not flooded before, increment the counter for flooded
    ! leaves, and mark the node as flooded now.
    if ( .not. btest(node%PropertyBits%val(node%proplength, iNode), &
      &              nodeFlooded_bit) ) then
      nFloodedLeaves = nFloodedLeaves + 1
      
      node%PropertyBits%val(node%proplength, iNode) &
        &  = ibset(node%PropertyBits%val(node%proplength, iNode), &
        &          nodeFlooded_bit)
    end if
    col_int = (Color-1) / node%bytes_per_int + 1
    col_bit = mod(Color-1, node%bytes_per_int)*8 + 1
    node%PropertyBits%val(col_int,iNode) &
      &   = ibset(node%PropertyBits%val(col_int,iNode), col_bit)
  end subroutine sdr_mark_floodNode
  ! ****************************************************************************


  ! ****************************************************************************
  !> If parent has hasBoundary_bit then this function will inherit
  !! this property to eligible childrens
  pure function sdr_inHeritBnd_eligibleChildren(node, iNode) &
    &  result(child_hasBnd)
    ! --------------------------------------------------------------------------!
    !> Description of the nodes in the prototree.
    type(sdr_node_type), intent(in) :: node
    !> Index of the node to evaluate.
    integer, intent(in) :: iNode
    !> result contains which child has boundary
    logical :: child_hasBnd(8)
    ! --------------------------------------------------------------------------!
    integer, allocatable :: eligible_childs(:)
    integer :: iDir, iChild
    ! --------------------------------------------------------------------------!
    ! set default
    child_hasBnd = .false.
    if ( sdr_nodeProp_btest(node  = node,             &
      &                     iNode = iNode,            &
      &                     bit   = hasBoundary_bit ) ) then

      ! Inherit hasBoundary bit only to eligible children 
      do iDir = 1, qQQQ
        if ( btest(node%hasBndBits%val(iNode), iDir) ) then
          call tem_eligibleChildren(eligible_childs, iDir)
          do iChild = 1, size(eligible_childs)
            child_hasBnd( eligible_childs(iChild) ) = .true.
          end do
          deallocate(eligible_childs)
        end if
      end do
    end if  

  end function sdr_inHeritBnd_eligibleChildren
  ! ****************************************************************************


  ! ****************************************************************************
  !> Return all colors of the given node encoded in an array of characters.
  !!
  !! Each character is supposed to hold an ASCII symbol and by setting the
  !! byte bits, we can store up to 7 colors in each character.
  function sdr_nodeColors(node, iNode) result(colchar)
    ! --------------------------------------------------------------------------!
    !> Description of the nodes in the prototree.
    type(sdr_node_type), intent(in) :: node
    !> Index of the node to evaluate.
    integer, intent(in) :: iNode
    !> Gathered color information in an array of characters.
    character :: colchar(node%nColorChars)
    ! --------------------------------------------------------------------------!

    colchar = sdr_bitfieldColors( node = node,                              &
      &                           bitfield =node%PropertyBits%val(:, iNode) )

  end function sdr_nodeColors
  ! ****************************************************************************


  ! ****************************************************************************
  !> Return all colors of the given bitfield encoded in an array of characters.
  !!
  !! The bitfield has to correspond with the color definition in nodes.
  !! Each character is supposed to hold an ASCII symbol and by setting the
  !! byte bits, we can store up to 7 colors in each character.
  function sdr_bitfieldColors(node, bitfield) result(colchar)
    ! --------------------------------------------------------------------------!
    !> Description of the nodes in the prototree.
    type(sdr_node_type), intent(in) :: node
    !> Index of the node to evaluate.
    integer, intent(in) :: bitfield(:)
    !> Gathered color information in an array of characters.
    character :: colchar(node%nColorChars)
    ! --------------------------------------------------------------------------!
    integer :: charcount, intcount
    integer :: charbit, intbit
    integer :: iColor, iNotNone
    integer :: tmpColor(node%nColorChars)
    ! --------------------------------------------------------------------------!

    tmpColor = 0

    iNotNone = 0

    do iColor=0,node%nColors-1

      ! Only process non-none colors.
      if (iColor+1 /= node%none_color_id) then

        ! Integer and bit in integer for this color (bit 1 decides flooding)
        intcount = iColor/node%bytes_per_int + 1
        intbit = mod(iColor, node%bytes_per_int)*8 + 1

        if ( btest(bitfield(intcount), intbit) ) then
          ! Character and bit in character for this color
          charcount = iNotNone/colors_per_char + 1
          charbit = mod(iNotNone, colors_per_char)

          tmpColor(charcount) = ibset(tmpColor(charcount), charbit)
        end if

        iNotNone = iNotNone + 1

      end if

    end do

    colchar = achar(tmpColor)

  end function sdr_bitfieldColors
  ! ****************************************************************************


  ! ****************************************************************************
  !> Return all colors encoded in an array of characters based on an array of
  !! logicals indicating for each color wether it should be set or not.
  !!
  !! Each character is supposed to hold an ASCII symbol and by setting the
  !! byte bits, we can store up to 7 colors in each character.
  function sdr_color_log2char(node, logicals) result(colchar)
    ! --------------------------------------------------------------------------!
    !> Description of the nodes in the prototree.
    type(sdr_node_type), intent(in) :: node

    !> Flag for each color wether it should be set or not.
    logical, intent(in) :: logicals(:)

    !> Gathered color information in an array of characters.
    character :: colchar(node%nColorChars)
    ! --------------------------------------------------------------------------!
    integer :: charcount
    integer :: charbit
    integer :: iColor, iNotNone
    integer :: tmpColor(node%nColorChars)
    ! --------------------------------------------------------------------------!

    tmpColor = 0

    iNotNone = 0

    do iColor=0,node%nColors-1

      ! Only process non-none colors.
      if (iColor+1 /= node%none_color_id) then

        if ( logicals(iColor+1) ) then
          ! Character and bit in character for this color
          charcount = iNotNone/colors_per_char + 1
          charbit = mod(iNotNone, colors_per_char)

          tmpColor(charcount) = ibset(tmpColor(charcount), charbit)
        end if

        iNotNone = iNotNone + 1

      end if

    end do

    colchar = achar(tmpColor)

  end function sdr_color_log2char
  ! ****************************************************************************


  ! ****************************************************************************
  !> This routine appends temporary child intersected object to actual growing
  !! array of intersected objects. To reduce memory usuage, the child with
  !! maximum number of intersected objects from parent is appended at same 
  !! position as its parent. Rest of the childrens intersected objects are
  !! appended to the end of growing array
  subroutine sdr_append_childIntersectedObjectAll(node, parent, testAll,       &
    & intersected_object, grwObjPos, child_nodePos, child_nObjects,            &
    & child_intersected_object, child_objPos, memLeft)
    ! --------------------------------------------------------------------------!
    !> contains information about all nodes in protoTree
    type(sdr_node_type), intent(in) :: node
    !> Position of parent node
    integer, intent(in) :: parent
    !> true for level 1 nodes
    logical, intent(in) :: testAll
    !> Growing array of intersected objects. 
    !! Could be user defined  or distance refine spatial objects
    type(grw_intArray_type), intent(inout) :: intersected_object
    !> First and last position of intersected object of all nodes in
    !! intersected_object list
    type(grw_intersectObjPosArray_type), intent(inout) :: grwObjPos
    !> 8 children node position in protoTree
    integer, intent(in) :: child_nodePos(8)
    !> number of intersected objets in 8 children
    integer, intent(in) :: child_nObjects(8)
    !> Temporary array of intersected objects for 8 children
    type(grw_intArray_type), intent(inout) :: child_intersected_object
    !> first and last index of intersected objects of 8 children in 
    !! child_intersected_object list
    type(sdr_intersectObjPos_type), intent(in) :: child_objPos(8)
    !> memory of parent intersected object unused by children
    integer, intent(out) :: memLeft
    ! --------------------------------------------------------------------------!
    integer :: iChild, childIDX, child_sorted(8)
    integer :: child_intersected_first_inParent
    logical :: parent_isTarget
    ! --------------------------------------------------------------------------!
    ! Append child with maximum number of intersected object
    ! in the position of parent to reduce memory only if parent
    ! is not a target bit.

    ! if parent is target bit do not write intersected object
    ! of children in parent position
    parent_isTarget = sdr_nodeProp_btest(node  = node,       &
        &                                iNode = parent,     &
        &                                bit   = isTarget_bit) 

    ! Find the sorted list
    call mrgrnk(child_nObjects, child_sorted)
 
    ! left space is number of objects we can still fit in parent position
    ! Initially, We could fill upto nObjects of parent 
    memLeft = grwObjPos%val(parent)%last - grwObjPos%val(parent)%first + 1

    ! Start position of a child in parent list
    child_intersected_first_inParent = grwObjPos%val(parent)%first

    ! Find the sorted list, mrgrnk sorted min to max so use 
    ! offset 8-iChild to append child with max nObjects first
    do iChild = 8, 1, -1
      ! append child according to sorted list
      childIDX = child_sorted(iChild)
      
      ! append child only when it has intersected objects
      ! if childs nObjects is <= left_space than fill this child
      ! in parent position and set intersected first and last accordingly.
      ! Also update left_space
      if (child_nObjects(childIDX) <= memLeft .and.&
        & .not. testAll .and. .not. parent_isTarget) then
        ! Set the first and last index of this childs intesected object 
        ! in the array
        grwObjPos%val(child_nodePos(childIDX))%first =             &
          &                       child_intersected_first_inParent
        grwObjPos%val(child_nodePos(childIDX))%last =              &
          &                       child_intersected_first_inParent &   
          &                     + child_nObjects(childIDX) - 1
  
        call placeAt( me  = intersected_object,               &
          &           val = child_intersected_object%val(     &
          &                    child_objPos(childIDX)%first : &
          &                    child_objPos(childIDX)%last ), &
          &           pos = child_intersected_first_inParent  ) 

        ! update left_space
        memLeft = memLeft - child_nObjects(childIDX)
        ! update next childs start position in parent list
        child_intersected_first_inParent =                &
          & grwObjPos%val(child_nodePos(childIDX))%last + 1

      else
        ! append to end of intersected object list

        ! Set the first and last index of this childs intesected object 
        ! in the array
        grwObjPos%val(child_nodePos(childIDX))%first =       &
          &                       intersected_object%nVals + 1

        grwObjPos%val(child_nodePos(childIDX))%last =     &
          &                      intersected_object%nVals &
          &                    + child_nObjects(childIDX)
        call append( me  = intersected_object,               &
          &          val = child_intersected_object%val(     &
          &                   child_objPos(childIDX)%first : &
          &                   child_objPos(childIDX)%last )  )

      end if
    end do

  end subroutine sdr_append_childIntersectedObjectAll
  ! ****************************************************************************


  ! ****************************************************************************
  !> This routine appends temporary child intersected object to actual growing
  !! array of intersected objects. To reduce memory usuage, the child with
  !! maximum number of intersected objects from parent is appended at same 
  !! position as its parent. Rest of the childrens intersected objects are
  !! appended to the end of growing array.
  !!
  !! We append only objects with level greater than children minLevel
  !! to reduce memory.
  subroutine sdr_append_childIntersectedObjectGTminLevel(geometry, node,       &
    & parent, testAll, intersected_object, grwObjPos, child_nodePos,           &
    & child_intersected_object, child_objPos, memLeft)
    ! --------------------------------------------------------------------------!
    !> type which contains all geometry object infos
    type(sdr_geometry_type), intent(in) :: geometry
    !> contains information about all nodes in protoTree
    type(sdr_node_type), intent(in) :: node
    !> Position of parent node
    integer, intent(in) :: parent
    !> true for level 1 nodes
    logical, intent(in) :: testAll
    !> Growing array of intersected objects. 
    !! Could be user defined  or distance refine spatial objects
    type(grw_intArray_type), intent(inout) :: intersected_object
    !> First and last position of intersected object of all nodes in
    !! intersected_object list
    type(grw_intersectObjPosArray_type), intent(inout) :: grwObjPos
    !> 8 children node position in protoTree
    integer, intent(in) :: child_nodePos(8)
    !> Temporary array of intersected objects for 8 children
    type(grw_intArray_type), intent(inout) :: child_intersected_object
    !> first and last index of intersected objects of 8 children in 
    !! child_intersected_object list
    type(sdr_intersectObjPos_type), intent(in) :: child_objPos(8)
    !> memory of parent intersected object unused by children
    integer, intent(out) :: memLeft
    ! --------------------------------------------------------------------------!
    integer :: iChild, childIDX, child_sorted(8)
    integer :: child_objPos_first_inParent, child_objPos_next_inParent
    integer :: child_nObjectsNew(8)
    logical :: parent_isTarget
    integer :: iObject, obj_pos, attr_pos, objLevel
    ! --------------------------------------------------------------------------!
    ! Append child with maximum number of intersected object
    ! in the position of parent to reduce memory only if parent
    ! is not a target bit.

    ! if parent is target bit do not write intersected object
    ! of children in parent position
    parent_isTarget = sdr_nodeProp_btest(node  = node,       &
        &                                iNode = parent,     &
        &                                bit   = isTarget_bit) 

    ! left space is number of objects we can still fit in parent position
    ! Initially, We could fill upto nObjects of parent 
    memLeft = grwObjPos%val(parent)%last - grwObjPos%val(parent)%first + 1

    ! Start position of a child in parent list
    child_objPos_first_inParent = grwObjPos%val(parent)%first
    child_objPos_next_inParent = child_objPos_first_inParent 

    child_nObjectsNew = 0!child_nObjects
    do iChild = 1, 8
      do iObject = child_objPos(iChild)%first, child_objPos(iChild)%last
        obj_pos = child_intersected_object%val(iObject)
        attr_pos = geometry%spatialObj%val(obj_pos)%attribute_position
        objLevel = geometry%attribute%dynArray%val(attr_pos)%level
       
        if (objLevel > node%minLevel%val(child_nodePos(iChild))) then
          ! number of objects with level > minLevel  
          child_nObjectsNew(iChild) = child_nObjectsNew(iChild) + 1
        end if !objlevel>minLevel
      end do ! iObject
    end do !iChild

    ! Find the sorted list
    call mrgrnk(child_nObjectsNew, child_sorted)
 
    ! Find the sorted list, mrgrnk sorted min to max so use 
    ! offset 8-iChild to append child with max nObjects first
    do iChild = 8, 1, -1
      ! append child according to sorted list
      childIDX = child_sorted(iChild)

      ! append child only when it has intersected objects
      ! if childs nObjects is <= left_space than fill this child
      ! in parent position and set intersected first and last accordingly.
      ! Also update left_space
      if (child_nObjectsNew(childIDX) <= memLeft .and.&
        & .not. testAll .and. .not. parent_isTarget) then

        ! append objects with level > child minlevel
        ! Count new number of objects per child which has level
        ! greater than childs minLevel and 
        do iObject = child_objPos(childIDX)%first, child_objPos(childIDX)%last
          obj_pos = child_intersected_object%val(iObject)
          attr_pos = geometry%spatialObj%val(obj_pos)%attribute_position
          objLevel = geometry%attribute%dynArray%val(attr_pos)%level
         
          if (objLevel > node%minLevel%val(child_nodePos(childIDX))) then
            call placeAt( me  = intersected_object,        &
              &           val = obj_pos,                   &
              &           pos = child_objPos_next_inParent )

            ! Position of next childObjPos in parent 
            child_objPos_next_inParent = child_objPos_next_inParent + 1
          end if !objlevel>minLevel
        end do ! iObject

        ! Set the first and last index of this childs intesected object 
        ! in the array
        grwObjPos%val(child_nodePos(childIDX))%first =             &
          &                       child_objPos_first_inParent
        grwObjPos%val(child_nodePos(childIDX))%last =           &
          &                       child_objPos_first_inParent   &   
          &                     + child_nObjectsNew(childIDX) - 1
  
        ! update left_space
        memLeft = memLeft - child_nObjectsNew(childIDX)
        ! update next childs start position in parent list
        child_objPos_first_inParent =                     &
          & grwObjPos%val(child_nodePos(childIDX))%last + 1

      else
        ! append to end of intersected object list

        ! Set the first and last index of this childs intesected object 
        ! in the array
        grwObjPos%val(child_nodePos(childIDX))%first =       &
          &                       intersected_object%nVals + 1
        ! append objects with level > child minlevel
        ! Count new number of objects per child which has level
        ! greater than childs minLevel and 
        do iObject = child_objPos(childIDX)%first, child_objPos(childIDX)%last
          obj_pos = child_intersected_object%val(iObject)
          attr_pos = geometry%spatialObj%val(obj_pos)%attribute_position
          objLevel = geometry%attribute%dynArray%val(attr_pos)%level
         
          if (objLevel > node%minLevel%val(child_nodePos(childIDX))) then
            call append( me  = intersected_object, val = obj_pos )
          end if !objlevel>minLevel
        end do ! iObject

        ! Set the last index of this childs intesected object 
        ! in the array
        grwObjPos%val(child_nodePos(childIDX))%last =      &
          &                      intersected_object%nVals

      end if
    end do

  end subroutine sdr_append_childIntersectedObjectGTminLevel
  ! ****************************************************************************
  

end module sdr_node_module
