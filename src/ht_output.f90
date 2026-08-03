!> Writers for the tide table and the plot data file.
module ht_output
   use ht_time, only: dp, time_stamp, time_from_hours, iso_format, hours_since_epoch
   use ht_constituents, only: constituent_set_t
   use ht_extrema, only: extrema_list_t
   implicit none
   private

   public :: write_tide_table
   public :: write_plot_csv

   character(len=*), parameter :: sample_label = 'sample'
   character(len=*), parameter :: high_label = 'high'
   character(len=*), parameter :: low_label = 'low'

contains

   !> Write a human-readable tide table.
   !>
   !> The table lists the hourly heights and the detected highs and lows.
   !> heights(j) belongs to the hour start_ts plus j-1 hours.
   subroutine write_tide_table(filename, station, set, start_ts, heights, extrema, ierr, message)
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: station
      type(constituent_set_t), intent(in) :: set
      type(time_stamp), intent(in) :: start_ts
      real(dp), intent(in) :: heights(:)
      type(extrema_list_t), intent(in) :: extrema
      integer, intent(out) :: ierr
      character(len=:), allocatable, intent(out) :: message
      integer :: unit, i
      type(time_stamp) :: ts
      character(len=:), allocatable :: names

      open (newunit=unit, file=filename, status='replace', action='write', iostat=ierr)
      if (ierr /= 0) then
         message = 'cannot write file: '//trim(filename)
         return
      end if

      ts = time_from_hours(hours_since_epoch(start_ts) + size(heights) - 1)
      names = constituent_names(set)
      write (unit, '(a)') 'Tide table'
      write (unit, '(a,1x,a)') 'Station:', trim(station)
      write (unit, '(a,1x,f0.3,a)') 'Mean sea level:', set%z0, ' m'
      write (unit, '(a,1x,a)') 'From:', iso_format(start_ts, .true.)
      write (unit, '(a,1x,a)') 'To:', iso_format(ts, .true.)
      write (unit, '(a,1x,i0,a)') 'Hours:', size(heights)
      write (unit, '(a,1x,a)') 'Constituents:', names
      write (unit, '(a)') ''
      write (unit, '(a)') 'Hourly height'
      do i = 1, size(heights)
         ts = time_from_hours(hours_since_epoch(start_ts) + i - 1)
         write (unit, '(a,1x,f7.3,a)') iso_format(ts, .true.), heights(i), ' m'
      end do
      write (unit, '(a)') ''
      write (unit, '(a)') 'Highs and lows'
      if (size(extrema%item) == 0) then
         write (unit, '(a)') '  none detected'
      end if
      do i = 1, size(extrema%item)
         if (extrema%item(i)%is_high) then
            write (unit, '(1x,a,1x,f7.3,a)') hour_text(extrema%item(i)%time_hours), &
               extrema%item(i)%height, ' m  HIGH'
         else
            write (unit, '(1x,a,1x,f7.3,a)') hour_text(extrema%item(i)%time_hours), &
               extrema%item(i)%height, ' m  LOW'
         end if
      end do

      close (unit)
      message = ''
   end subroutine write_tide_table

   !> Write a CSV file for plotting.
   !>
   !> Each hourly sample produces one row with the label "sample". Each
   !> detected extremum produces one row with the label "high" or "low".
   !> Columns are time, serial hours, height in metres, and event.
   subroutine write_plot_csv(filename, start_ts, heights, extrema, ierr, message)
      character(len=*), intent(in) :: filename
      type(time_stamp), intent(in) :: start_ts
      real(dp), intent(in) :: heights(:)
      type(extrema_list_t), intent(in) :: extrema
      integer, intent(out) :: ierr
      character(len=:), allocatable, intent(out) :: message
      integer :: unit, i
      real(dp) :: base_hours

      open (newunit=unit, file=filename, status='replace', action='write', iostat=ierr)
      if (ierr /= 0) then
         message = 'cannot write file: '//trim(filename)
         return
      end if

      base_hours = real(hours_since_epoch(start_ts), dp)
      write (unit, '(a)') 'time,serial_hours,height_m,event'
      do i = 1, size(heights)
         write (unit, '(a)') csv_row(base_hours + real(i - 1, dp), heights(i), sample_label)
      end do
      do i = 1, size(extrema%item)
         if (extrema%item(i)%is_high) then
            write (unit, '(a)') csv_row(extrema%item(i)%time_hours, extrema%item(i)%height, high_label)
         else
            write (unit, '(a)') csv_row(extrema%item(i)%time_hours, extrema%item(i)%height, low_label)
         end if
      end do

      close (unit)
      message = ''
   end subroutine write_plot_csv

   !> Compose one CSV row.
   function csv_row(hours, height, label) result(text)
      real(dp), intent(in) :: hours
      real(dp), intent(in) :: height
      character(len=*), intent(in) :: label
      character(len=:), allocatable :: text
      character(len=24) :: hours_text_buffer, height_buffer

      write (hours_text_buffer, '(f14.4)') hours
      write (height_buffer, '(f0.3)') height
      text = hour_text(hours)//','//trim(adjustl(hours_text_buffer))//','// &
             trim(adjustl(height_buffer))//','//trim(label)
   end function csv_row

   !> Format serial hours as YYYY-MM-DDTHH:MM.
   function hour_text(hours) result(text)
      use, intrinsic :: iso_fortran_env, only: int64
      real(dp), intent(in) :: hours
      character(len=:), allocatable :: text
      integer :: minute
      type(time_stamp) :: ts

      ts = time_from_hours(int(floor(hours), int64))
      minute = nint((hours - floor(hours))*60.0_dp)
      if (minute >= 60) minute = 59
      text = iso_format(ts, .true.)
      write (text(len(text) - 1:len(text)), '(i2.2)') minute
   end function hour_text

   !> Space-separated list of constituent names.
   function constituent_names(set) result(names)
      type(constituent_set_t), intent(in) :: set
      character(len=:), allocatable :: names
      integer :: i
      names = ''
      do i = 1, set%n
         names = names//' '//trim(set%item(i)%name)
      end do
      names = adjustl(names)
   end function constituent_names

end module ht_output
