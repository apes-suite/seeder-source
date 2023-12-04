module tem_load_ascii

  use env_module, only: newunit
  use tem_global_module, only: tem_global_type
  use tem_bc_prop_module, only: tem_BC_prop_type
  use tem_tools_module, only: tem_write
  
  implicit none
  
  contains
  !> A routine to load global informations from the header file in the given
  !! directory.
  subroutine load_tem_global_ascii(me, dirname, found)
    type(tem_global_type), intent(out) :: me !< Structure to store header in
  
    !> Directory containing the mesh informations
    character(len=*), intent(in) :: dirname
    !> if file exit
    logical, intent(out):: found
  
    character(len=300) :: headname
    integer :: inunit
    integer :: version
    integer :: iProp
    logical :: ex
  
    me%dirname = trim(adjustl(dirname))
    headname = trim(me%dirname)//'header.ascii'
  
    found = .true.
    inquire(file=trim(headname), exist=ex)
    if( .not. ex) then
      call tem_write('WARNING: File '//trim(headname)//' not found. Skipping.')
      found = .false.
      return
    endif
    !! Read the header only on the root process, broadcast to all others
    inunit = newunit()
    open(unit = inunit, file = trim(headname), &
      &  status = 'old', action = 'read')
    read(inunit,*) version
    read(inunit,'(a)') me%label
    read(inunit,'(a)') me%comment
    read(inunit,*) me%BoundingCubeLength
    read(inunit,*) me%Origin
    read(inunit,*) me%nElems
    read(inunit,*) me%minLevel
    read(inunit,*) me%maxLevel
    read(inunit,*) me%nProperties
  
    allocate(me%Property(me%nProperties))
  
    do iProp=1,me%nProperties
      read(inunit,'(a)') me%Property(iprop)%label
      read(inunit,*) me%Property(iprop)%bitpos
      read(inunit,*) me%Property(iprop)%nElems
    end do
    close(inunit)
  
  end subroutine load_tem_global_ascii

  subroutine load_tem_BC_prop_ascii(me, dirname, found)
    !> Boundary condition construct to load the data into
    type(tem_BC_prop_type), intent(out) :: me
    !> Directory containing the mesh informations
    character(len=*), intent(in) :: dirname
    !> if file exit
    logical, intent(out):: found

    integer :: i
    logical :: ex
    integer :: fUnit
    character(len=256) :: headerfile

    headerfile = trim(dirname)//'bnd.ascii'
    inquire(file=trim(headerfile), exist=ex)
    found = .true.
    if( .not. ex) then
      call tem_write('WARNING: File '//trim(headerfile)//' not found. Skipping.')
      found = .false.
      return
    endif
    !! Read the header only on the root process, broadcast to all others
    fUnit = newUnit()
    open(unit = fUnit, file = headerfile, action = 'read', &
        &  status = 'old')

    read(fUnit,*) me%nSides
    read(fUnit,*) me%nBCtypes
    allocate(me%BC_label(me%nBCtypes))

    do i=1,me%nBCtypes
      read(fUnit,'(a)') me%BC_label(i)
    end do

    close(fUnit)

  end subroutine load_tem_BC_prop_ascii
  
end module tem_load_ascii

!> main program
program convertascii

  use tem_global_module, only: tem_global_type, dump_tem_global
  use tem_load_ascii, only: load_tem_global_ascii, load_tem_BC_prop_ascii
  use tem_tools_module, only: tem_write
  use tem_bc_prop_module, only: tem_BC_prop_type, dump_tem_BC_propHeader

  implicit none

  type(tem_global_type) :: header
  type(tem_BC_prop_type) :: bcprop
  character(len=1024) :: dirname
  logical :: found_glob, found_bc

  call get_command_argument( 1, dirname )

  call tem_write('loading header.ascii')
  call load_tem_global_ascii( header, trim(dirname), found_glob )
  
  call tem_write('dumping header.lua')
  header%mypart = 0
  if(found_glob)  call dump_tem_global( header ) 

  call tem_write('loading bnd.ascii')
  call load_tem_BC_prop_ascii( bcprop, trim(dirname), found_bc )

  call tem_write('dumping bnd.lua')
  if (found_bc) call dump_tem_BC_propHeader( bcprop, trim(dirname)//'bnd.lua' )

endprogram convertascii


