!> Harmonic constituents for one tide station.
!>
!> Each constituent carries an amplitude (metres), a phase lag (degrees),
!> and a speed in degrees per mean solar hour. The speeds follow the
!> standard astronomical angular speeds. A set of constituents describes
!> the tide at one station for any time.
module ht_constituents
   use ht_time, only: dp
   implicit none
   private

   public :: constituent_t
   public :: constituent_set_t
   public :: max_constituents
   public :: constituent_name_len
   public :: standard_speed
   public :: add_constituent
   public :: load_constituent_set
   public :: write_constituent_set

   integer, parameter :: max_constituents = 64
   integer, parameter :: constituent_name_len = 4
   integer, parameter :: line_len = 128
   character(len=*), parameter :: z0_token = 'z0'

   !> One harmonic constituent.
   type :: constituent_t
      character(len=constituent_name_len) :: name = ''
      real(dp) :: amplitude = 0.0_dp
      real(dp) :: phase_deg = 0.0_dp
      real(dp) :: speed_deg_per_hour = 0.0_dp
   end type constituent_t

   !> A set of constituents for one station.
   type :: constituent_set_t
      integer :: n = 0
      real(dp) :: z0 = 0.0_dp
      type(constituent_t) :: item(max_constituents)
   end type constituent_set_t

contains

   !> Return the standard speed in degrees per hour for a constituent name.
   !> Returns zero for an unknown name.
   pure real(dp) function standard_speed(name) result(speed)
      character(len=*), intent(in) :: name
      character(len=constituent_name_len) :: key

      key = lower(name)
      select case (key)
      case ('sa')
         speed = 0.0410686_dp
      case ('ssa')
         speed = 0.0821373_dp
      case ('mm')
         speed = 0.5443747_dp
      case ('mf')
         speed = 1.0980331_dp
      case ('q1')
         speed = 13.3986609_dp
      case ('o1')
         speed = 13.9430356_dp
      case ('p1')
         speed = 14.9589314_dp
      case ('k1')
         speed = 15.0410686_dp
      case ('n2')
         speed = 28.4397295_dp
      case ('m2')
         speed = 28.9841042_dp
      case ('s2')
         speed = 30.0000000_dp
      case ('k2')
         speed = 30.0821373_dp
      case ('m4')
         speed = 57.9682084_dp
      case ('ms4')
         speed = 58.9841042_dp
      case default
         speed = 0.0_dp
      end select
   end function standard_speed

   !> Return true when a name matches a known constituent.
   pure logical function is_known(name) result(known)
      character(len=*), intent(in) :: name
      known = standard_speed(name) > 0.0_dp
   end function is_known

   !> Append a constituent to a set. Returns nonzero when the set is full
   !> or the name is unknown.
   subroutine add_constituent(set, name, amplitude, phase_deg, ierr)
      type(constituent_set_t), intent(inout) :: set
      character(len=*), intent(in) :: name
      real(dp), intent(in) :: amplitude
      real(dp), intent(in) :: phase_deg
      integer, intent(out) :: ierr

      if (.not. is_known(name)) then
         ierr = 1
         return
      end if
      if (set%n >= max_constituents) then
         ierr = 2
         return
      end if
      set%n = set%n + 1
      set%item(set%n)%name = upper(name)
      set%item(set%n)%amplitude = amplitude
      set%item(set%n)%phase_deg = phase_deg
      set%item(set%n)%speed_deg_per_hour = standard_speed(name)
      ierr = 0
   end subroutine add_constituent

   !> Load a constituent set from a text file.
   !>
   !> The file contains one line per constituent. Lines that start with
   !> "#" are comments. A line that starts with the token "z0" sets the
   !> mean sea level. All other lines hold a name, an amplitude, and a
   !> phase. The phase is in degrees.
   subroutine load_constituent_set(filename, set, ierr, message)
      character(len=*), intent(in) :: filename
      type(constituent_set_t), intent(out) :: set
      integer, intent(out) :: ierr
      character(len=:), allocatable, intent(out) :: message
      integer :: unit
      character(len=line_len) :: line
      character(len=line_len) :: field1, field2, field3
      integer :: ios, k
      real(dp) :: value2, value3
      logical :: opened

      set = constituent_set_t()
      open (newunit=unit, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         ierr = 1
         message = 'cannot open file: '//trim(filename)
         return
      end if

      do
         read (unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         call split_fields(line, field1, field2, field3, k)
         if (k == 0) cycle
         if (same_text(trim(field1), z0_token)) then
            if (k < 2) cycle
            read (field2, *, iostat=ios) set%z0
            if (ios /= 0) cycle
         else
            if (k < 3) cycle
            if (.not. is_known(trim(field1))) then
               call close_unit(unit, opened)
               ierr = 2
               message = 'unknown constituent name: '//trim(field1)
               return
            end if
            read (field2, *, iostat=ios) value2
            if (ios /= 0) cycle
            read (field3, *, iostat=ios) value3
            if (ios /= 0) cycle
            call add_constituent(set, trim(field1), value2, value3, ierr)
            if (ierr /= 0) then
               call close_unit(unit, opened)
               message = 'too many constituents in file: '//trim(filename)
               return
            end if
         end if
      end do

      call close_unit(unit, opened)
      ierr = 0
      message = ''
   end subroutine load_constituent_set

   !> Write a constituent set to a text file in the same format.
   subroutine write_constituent_set(filename, set, ierr, message)
      character(len=*), intent(in) :: filename
      type(constituent_set_t), intent(in) :: set
      integer, intent(out) :: ierr
      character(len=:), allocatable, intent(out) :: message
      integer :: unit, i

      open (newunit=unit, file=filename, status='replace', action='write', iostat=ierr)
      if (ierr /= 0) then
         message = 'cannot write file: '//trim(filename)
         return
      end if
      write (unit, '(a,1x,f0.3)') z0_token, set%z0
      do i = 1, set%n
         write (unit, '(a,1x,f0.4,1x,f0.1)') set%item(i)%name, set%item(i)%amplitude, set%item(i)%phase_deg
      end do
      close (unit)
      message = ''
   end subroutine write_constituent_set

   !> Split a line into whitespace-delimited fields.
   !> A "#" starts a comment. Fields beyond the third are ignored.
   subroutine split_fields(line, field1, field2, field3, count)
      character(len=*), intent(in) :: line
      character(len=*), intent(out) :: field1
      character(len=*), intent(out) :: field2
      character(len=*), intent(out) :: field3
      integer, intent(out) :: count
      integer :: i, j
      integer :: fields

      field1 = ''
      field2 = ''
      field3 = ''
      fields = 0
      i = 1
      do while (i <= len(line))
         if (line(i:i) == '#') exit
         if (line(i:i) == ' ') then
            i = i + 1
            cycle
         end if
         j = i
         do while (j <= len(line) .and. line(j:j) /= ' ' .and. line(j:j) /= '#')
            j = j + 1
         end do
         fields = fields + 1
         select case (fields)
         case (1)
            field1 = line(i:j - 1)
         case (2)
            field2 = line(i:j - 1)
         case (3)
            field3 = line(i:j - 1)
         end select
         i = j
      end do
      count = fields
   end subroutine split_fields

   !> Case-insensitive text comparison.
   pure logical function same_text(a, b) result(same)
      character(len=*), intent(in) :: a
      character(len=*), intent(in) :: b
      same = lower(a) == lower(b)
   end function same_text

   !> Lowercase copy of a string.
   pure function lower(text) result(low)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: low
      integer :: i, c
      do i = 1, len(text)
         c = iachar(text(i:i))
         if (c >= iachar('A') .and. c <= iachar('Z')) c = c + 32
         low(i:i) = achar(c)
      end do
   end function lower

   !> Uppercase copy of a string.
   pure function upper(text) result(up)
      character(len=*), intent(in) :: text
      character(len=len(text)) :: up
      integer :: i, c
      do i = 1, len(text)
         c = iachar(text(i:i))
         if (c >= iachar('a') .and. c <= iachar('z')) c = c - 32
         up(i:i) = achar(c)
      end do
   end function upper

   !> Close a unit without error when it is not open.
   subroutine close_unit(unit, opened)
      integer, intent(in) :: unit
      logical, intent(out) :: opened
      inquire (unit=unit, opened=opened)
      if (opened) close (unit)
   end subroutine close_unit

end module ht_constituents
