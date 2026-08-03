!> Small helpers for parsing command line options.
!>
!> Options use the form "--name value". A flag with no value is not
!> supported. Unknown options are ignored.
module ht_cli
   implicit none
   private

   public :: cli_has
   public :: cli_value
   public :: cli_integer

contains

   !> Return true when the named option appears on the command line.
   logical function cli_has(name) result(found)
      character(len=*), intent(in) :: name
      character(len=:), allocatable :: arg
      integer :: i, n

      found = .false.
      do i = 1, command_argument_count()
         call get_command_argument(i, length=n)
         allocate (character(len=max(n, 1)) :: arg)
         call get_command_argument(i, arg)
         if (arg == name) then
            found = .true.
            deallocate (arg)
            return
         end if
         deallocate (arg)
      end do
   end function cli_has

   !> Return the value that follows a named option.
   !> Returns the default value when the option is absent.
   function cli_value(name, default) result(value)
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: value
      integer :: i, n
      character(len=:), allocatable :: arg

      value = default
      do i = 1, command_argument_count() - 1
         call get_command_argument(i, length=n)
         allocate (character(len=max(n, 1)) :: arg)
         call get_command_argument(i, arg)
         if (arg == name) then
            call get_command_argument(i + 1, length=n)
            deallocate (value)
            allocate (character(len=max(n, 1)) :: value)
            call get_command_argument(i + 1, value)
            deallocate (arg)
            return
         end if
         deallocate (arg)
      end do
   end function cli_value

   !> Read an integer option value. ierr is nonzero on parse failure.
   subroutine cli_integer(name, value, ierr)
      character(len=*), intent(in) :: name
      integer, intent(out) :: value
      integer, intent(out) :: ierr
      character(len=:), allocatable :: text

      text = cli_value(name, '')
      if (len_trim(text) == 0) then
         ierr = 1
         return
      end if
      read (text, *, iostat=ierr) value
   end subroutine cli_integer

end module ht_cli
