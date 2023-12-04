! Copyright (c) 2015 Samuel Ziegenberg
! Copyright (c) 2015-2016 Kannan Masilamani <kannan.masilamani@uni-siegen.de>
! Copyright (c) 2016,2021 Harald Klimach <harald.klimach@uni-siegen.de>
! Copyright (c) 2016 Tobias Girresser <tobias.girresser@student.uni-siegen.de>
! Copyright (c) 2016 Jiaxing Qi <jiaxing.qi@uni-siegen.de>
! Copyright (c) 2016 Neda Ebrahimi Pour <neda.epour@uni-siegen.de>
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
!> author: Samuel Zukerberg
!! author: Kannan Masilamani
!! Module contains datatype contains protoData which is read from
!! seeder debug output in harvester(restart) format.
!! Also, routines to append protoData variables
!! to varSys, to access protoData array using get_element interface and routine
!! to read restart to fill protoData%val
!!
?? include 'treelm/source/deriveMacros.inc'

module sdr_protoData_module

  use, intrinsic :: iso_c_binding

  use env_module,            only: rk, io_buffer_size, labelLen

  use tem_logging_module,    only: logUnit
  use tem_time_module,       only: tem_time_type
  use tem_varsys_module, only: tem_varSys_proc_element, tem_varSys_proc_point, &
    &                          tem_varSys_proc_getParams, &
    &                          tem_varSys_proc_setParams, &
    &                          tem_varSys_proc_setupIndices, &
    &                          tem_varSys_proc_getValOfIndex, &
    &                          tem_varsys_append_statevar, &
    &                          tem_varSys_type, tem_varSys_op_type, &
    &                          tem_varSys_getParams_dummy, &
    &                          tem_varSys_setParams_dummy
  use tem_restart_module,    only: tem_restart_type, tem_restart_openRead,     &
    &                              tem_restart_readData, tem_restart_closeRead 
  
  use treelmesh_module,      only: treelmesh_type

  use sdr_attribute_module,  only: sdr_attrList_type, sdr_seed_object

  implicit none

  ! -----------------------------------------------------------------------------
  ! Data type to store binary information from restart
  type sdr_protoData_type
     real(kind=rk), allocatable :: val(:)
  end type sdr_protoData_type
  ! -----------------------------------------------------------------------------

  contains


  ! ****************************************************************************!
  subroutine  sdr_append_protoVar(protoVar, varSys, method_data, get_element)
    ! ---------------------------------------------------------------------------
    !> variable names in protoData
    character(len=labelLen), intent(in) ::  protoVar(:)
    !> global variable system to which protoData to be appended 
    type(tem_varSys_type), intent(inout)        :: varSys
    !> Data that is required by the methods to obtain the variable
    type(c_ptr), intent(in) :: method_data
    !> Procedure which allows the retrieval of the variable in an element.
    procedure(tem_varSys_proc_element), pointer :: get_element
    ! ---------------------------------------------------------------------------
    integer :: iVar, addedPos, nComponents
    logical :: wasAdded
    procedure(tem_varSys_proc_point), pointer :: get_point => NULL()
    procedure(tem_varSys_proc_setParams), pointer :: set_params => NULL()
    procedure(tem_varSys_proc_getParams), pointer :: get_params => NULL()
    procedure(tem_varSys_proc_setupIndices), pointer :: setup_indices => NULL()
    procedure(tem_varSys_proc_getValOfIndex), pointer :: get_valOfIndex &
      &                                                  => NULL()
    ! protoData info variables
    integer :: nProtoVars
    ! convert varName to small letters
    character(len=labelLen) :: varName
    ! ---------------------------------------------------------------------------
    write(logUnit(5),*) 'Append variables to varSys'

    nProtoVars = size(protoVar)
    get_params => tem_varSys_getparams_dummy
    set_params => tem_varSys_setparams_dummy
    nullify(get_point)
    nullify(setup_indices)
    nullify(get_valOfIndex)
 
    do iVar = 1, nProtoVars
      ! set nComponents depends on variable name
      select case(trim(protoVar(iVar)))
      case default
        nComponents = 1
      end select

      ! convert upper to lower case
      varName = protoVar(iVar)

      ! append variable to varSys
      call tem_varSys_append_stateVar( me             = varSys,         &
        &                              varName        = trim(varName),  &
        &                              nComponents    = nComponents,    &
        &                              method_data    = method_data,    &
        &                              set_params     = set_params,     &
        &                              get_point      = get_point,      &
        &                              get_element    = get_element,    &
        &                              get_params     = get_params,     &
        &                              setup_indices  = setup_indices,  &
        &                              get_valofindex = get_valofindex, &
        &                              pos            = addedPos,       &
        &                              wasAdded       = wasAdded        )

      if (wasAdded) then 
        write(logUnit(10),*) ' Appended variable:'//trim(protoVar(iVar))
      else if (addedpos < 1) then
        write(logUnit(1),*) 'Error: variable '//trim(protoVar(iVar))// &
          &                 ' is not added to variable system'
      end if

    end do ! iVar

  end subroutine sdr_append_protoVar
  ! ****************************************************************************!


  ! ****************************************************************************!
  !> Return the solver state variable for a given set of elements
  !!
  !! The interface has to comply to the abstract interface
  !! tem_varSys_module#tem_varSys_proc_element.
  recursive subroutine access_state( fun, varsys, elempos, time, tree, nElems, &
    &                                nDofs, res                                )
    !--------------------------------------------------------------------------!
    !> Description of the method to obtain the variables, here some preset
    !! values might be stored, like the space time function to use or the
    !! required variables.
    class(tem_varSys_op_type), intent(in) :: fun

    !> The variable system to obtain the variable from.
    type(tem_varSys_type), intent(in) :: varSys

    !> Position of the TreeID of the element to get the variable for in the
    !! global treeID list.
    integer, intent(in) :: elempos(:)

    !> Point in time at which to evaluate the variable.
    type(tem_time_type), intent(in)  :: time

    !> global treelm mesh info
    type(treelmesh_type), intent(in) :: tree

    !> Number of values to obtain for this variable (vectorized access).
    integer, intent(in) :: nElems

    !> Number of degrees of freedom within an element.
    integer, intent(in) :: nDofs

    !> Resulting values for the requested variable.
    !!
    !! Linearized array dimension:
    !! (n requested entries) x (nComponents of this variable)
    !! x (nDegrees of freedom)
    !! Access: (iElem-1)*fun%nComponents*nDofs +
    !!         (iDof-1)*fun%nComponents + iComp
    real(kind=rk), intent(out) :: res(:)
    ! ------------------------------------------------------------------------ !
    integer :: iElem, iComp
    type(sdr_protoData_type), pointer :: fPtr
    ! ------------------------------------------------------------------------ !
    call C_F_POINTER( fun%method_data, fPtr )

    res = 0.0_rk
    do iElem = 1, nElems
      do iComp = 1, fun%nComponents
        res( (iElem-1)*fun%nComponents+iComp ) = fPtr%val(                &
& ?IDXELEM?(fun%state_varPos(iComp), 1, elemPos(iElem), varSys%nScalars,1))
      end do
    end do !iElem

  end subroutine access_state
  ! ****************************************************************************!


! ******************************************************************************!
  !> Read the restart file into the state vectors
  !!
  subroutine sdr_readRestart(restart, tree, protoData)
      ! ------------------------------------------------------------------------- 
      !> restart information
      type(tem_restart_type), intent(inout)  :: restart
      !> mesh, provided in treelm format
      type(treelmesh_type), intent(in)       :: tree
      !> protoData read from restart file
      type(sdr_protoData_type), intent(out)  :: protoData
      ! -------------------------------------------------------------------------
      ! local variables
      real(kind=rk), allocatable :: chunk(:)
      integer :: iChunk, iElem, nScalars, iIndex, iVar
      integer :: elemOff ! Offset in the overall number of elements
      ! -------------------------------------------------------------------------
      write(logUnit(1),*)'Reading restart...'
      write(logUnit(5),*) 'nElems', tree%nElems
      nScalars = restart%varMap%nScalars
      write(logUnit(5),*) 'nScalars', nScalars
      allocate(protoData%val(tree%nElems*restart%varMap%nScalars))
      allocate(chunk( io_buffer_size ))
  
      ! open the file, read header and prepare buffering
      call tem_restart_openRead( me = restart )
  
      elemOff = 0
      ! loop over the chunks
      do iChunk = 1, restart%read_file%nChunks
        ! set the number of elements that are on the stack
        restart%nChunkElems = min(restart%read_file%chunkSize, tree%nElems-elemOff)
  
        call tem_restart_readData( restart, chunk )
  
        iIndex = 0
        do iElem = elemOff + 1, elemOff + restart%nChunkElems
          do iVar = 1, nScalars
            iIndex = iIndex + 1
            protoData%val((iElem-1)*nScalars + iVar) = chunk(iIndex)          
          end do
        end do
      
        elemOff = elemOff + restart%nChunkElems
      end do
   
      ! close the file
      call tem_restart_closeRead( me = restart )
!      write(*,*) 'nElems ', tree%nElems
!      do iElem = 1, tree%nElems
!        write(*,*) 'treeID ', tree%treeID(iElem)
!        write(*,*) 'protoData ', &
!& protoData%val((iElem-1)*nScalars+1:iElem*nScalars) 
!      end do
      deallocate( chunk )
  
      write(logUnit(1),*) 'Done reading restart.'
  
    end subroutine sdr_readRestart
! ******************************************************************************!

end module sdr_protoData_module
