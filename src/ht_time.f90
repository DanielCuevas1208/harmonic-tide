!> Date and time helpers for Harmonic Tide.
!>
!> Time is stored as an integer hour count since the reference epoch
!> 2000-01-01T00:00:00 UTC. The epoch hour count is zero at that instant.
!> Civil dates convert to and from this hour count.
module ht_time
   use, intrinsic :: iso_fortran_env, only: int64, real64
   implicit none
   private

   public :: time_stamp
   public :: dp
   public :: epoch_year, epoch_month, epoch_day
   public :: hours_per_day, minutes_per_hour
   public :: hours_since_epoch
   public :: time_from_hours
   public :: add_hours
   public :: days_from_civil
   public :: civil_from_days
   public :: is_leap_year
   public :: days_in_month
   public :: iso_format
   public :: clock_format
   public :: parse_iso_date
   public :: parse_iso_datetime

   integer, parameter :: dp = real64
   integer, parameter :: epoch_year = 2000
   integer, parameter :: epoch_month = 1
   integer, parameter :: epoch_day = 1
   integer, parameter :: hours_per_day = 24
   integer, parameter :: minutes_per_hour = 60

   !> A civil date and time in UTC.
   type :: time_stamp
      integer :: year = 2000
      integer :: month = 1
      integer :: day = 1
      integer :: hour = 0
   end type time_stamp

contains

   !> Return true when year is a leap year.
   pure logical function is_leap_year(year) result(leap)
      integer, intent(in) :: year
      leap = mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)
   end function is_leap_year

   !> Return the number of days in a month.
   pure integer function days_in_month(year, month) result(days)
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, parameter :: table(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      days = table(month)
      if (month == 2 .and. is_leap_year(year)) days = 29
   end function days_in_month

   !> Return the day count since 1970-01-01 for a civil date.
   !> Uses the algorithm by Howard Hinnant.
   pure integer(int64) function days_from_civil(year, month, day) result(days)
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, intent(in) :: day
      integer :: y, m
      integer(int64) :: era, yoe, doy, doe

      y = year
      m = month
      if (m <= 2) y = y - 1
      era = floor_div_i(int(y, int64), 400_int64)
      yoe = int(y - int(era)*400, int64)
      doy = (153*(m + merge(-3, 9, m > 2)) + 2)/5 + day - 1
      doe = yoe*365 + yoe/4 - yoe/100 + doy
      days = era*146097 + doe - 719468
   end function days_from_civil

   !> Return the civil date for a day count since 1970-01-01.
   !> Uses the algorithm by Howard Hinnant.
   pure function civil_from_days(days) result(ts)
      integer(int64), intent(in) :: days
      type(time_stamp) :: ts
      integer(int64) :: z, era, doe, yoe, doy, mp

      z = days + 719468
      era = floor_div_i(z, 146097_int64)
      doe = z - era*146097
      yoe = (doe - doe/1460 + doe/36524 - doe/146096)/365
      ts%year = int(yoe + era*400)
      doy = doe - (365*yoe + yoe/4 - yoe/100)
      mp = (5*doy + 2)/153
      ts%day = int(doy - (153*mp + 2)/5 + 1)
      ts%month = int(mp + merge(3, -9, mp < 10))
      if (ts%month <= 2) ts%year = ts%year + 1
   end function civil_from_days

   !> Return the hour count since the reference epoch for a time stamp.
   pure integer(int64) function hours_since_epoch(ts) result(hours)
      type(time_stamp), intent(in) :: ts
      hours = (days_from_civil(ts%year, ts%month, ts%day) - epoch_days())*hours_per_day + ts%hour
   end function hours_since_epoch

   !> Return the time stamp for an hour count since the reference epoch.
   pure function time_from_hours(hours) result(ts)
      integer(int64), intent(in) :: hours
      type(time_stamp) :: ts
      integer(int64) :: day_count, day, hour

      day = floor_div_i(hours, int(hours_per_day, int64))
      hour = hours - day*int(hours_per_day, int64)
      day_count = day + epoch_days()
      ts = civil_from_days(day_count)
      ts%hour = int(hour)
   end function time_from_hours

   !> Return the time stamp shifted by an integer number of hours.
   pure function add_hours(ts, hours) result(shifted)
      type(time_stamp), intent(in) :: ts
      integer, intent(in) :: hours
      type(time_stamp) :: shifted
      shifted = time_from_hours(hours_since_epoch(ts) + hours)
   end function add_hours

   !> Format a time stamp as YYYY-MM-DD, optionally with the hour.
   !> With include_time, the form is YYYY-MM-DDTHH:00.
   pure function iso_format(ts, include_time) result(text)
      type(time_stamp), intent(in) :: ts
      logical, intent(in) :: include_time
      character(len=:), allocatable :: text

      if (include_time) then
         text = pad_int(ts%year, 4)//'-'//pad_int(ts%month, 2)//'-'//pad_int(ts%day, 2)// &
                'T'//pad_int(ts%hour, 2)//':00'
      else
         text = pad_int(ts%year, 4)//'-'//pad_int(ts%month, 2)//'-'//pad_int(ts%day, 2)
      end if
   end function iso_format

   !> Format a time stamp as YYYY-MM-DD HH:00.
   pure function clock_format(ts) result(text)
      type(time_stamp), intent(in) :: ts
      character(len=:), allocatable :: text
      text = pad_int(ts%year, 4)//'-'//pad_int(ts%month, 2)//'-'//pad_int(ts%day, 2)// &
             ' '//pad_int(ts%hour, 2)//':00'
   end function clock_format

   !> Return the day count of the reference epoch since 1970-01-01.
   pure integer(int64) function epoch_days() result(days)
      days = 10957_int64
   end function epoch_days

   !> Parse a date in the form YYYY-MM-DD into a time stamp.
   !> ierr is nonzero when the text does not match the form.
   subroutine parse_iso_date(text, ts, ierr)
      character(len=*), intent(in) :: text
      type(time_stamp), intent(out) :: ts
      integer, intent(out) :: ierr

      if (len_trim(text) < 10) then
         ierr = 1
         return
      end if
      if (text(5:5) /= '-' .or. text(8:8) /= '-') then
         ierr = 1
         return
      end if
      read (text(1:4), '(i4)', iostat=ierr) ts%year
      if (ierr /= 0) return
      read (text(6:7), '(i2)', iostat=ierr) ts%month
      if (ierr /= 0) return
      read (text(9:10), '(i2)', iostat=ierr) ts%day
      if (ierr /= 0) return
      ts%hour = 0
   end subroutine parse_iso_date

   !> Parse a datetime in the form YYYY-MM-DDTHH:MM into a time stamp
   !> and the minute of the hour. ierr is nonzero when the text does not
   !> match the form.
   subroutine parse_iso_datetime(text, ts, minute, ierr)
      character(len=*), intent(in) :: text
      type(time_stamp), intent(out) :: ts
      integer, intent(out) :: minute
      integer, intent(out) :: ierr

      if (len_trim(text) < 16) then
         ierr = 1
         return
      end if
      if (text(11:11) /= 'T' .or. text(14:14) /= ':') then
         ierr = 1
         return
      end if
      call parse_iso_date(text(1:10), ts, ierr)
      if (ierr /= 0) return
      read (text(12:13), '(i2)', iostat=ierr) ts%hour
      if (ierr /= 0) return
      read (text(15:16), '(i2)', iostat=ierr) minute
      if (ierr /= 0) return
   end subroutine parse_iso_datetime

   !> Floor division for 64-bit integers.
   pure integer(int64) function floor_div_i(a, b) result(q)
      integer(int64), intent(in) :: a
      integer(int64), intent(in) :: b
      integer(int64) :: r
      q = a/b
      r = a - q*b
      if (r /= 0 .and. sign(1_int64, r) /= sign(1_int64, b)) q = q - 1
   end function floor_div_i

   !> Zero-padded integer text.
   pure function pad_int(value, width) result(text)
      integer, intent(in) :: value
      integer, intent(in) :: width
      character(len=:), allocatable :: text
      character(len=16) :: buffer
      write (buffer, '(i0)') value
      if (len_trim(buffer) < width) then
         text = repeat('0', width - len_trim(buffer))//trim(buffer)
      else
         text = trim(buffer)
      end if
   end function pad_int

end module ht_time
