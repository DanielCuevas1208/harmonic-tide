!> Build a tide table and a plot data file for one station.
program tide_predict
   use ht_time, only: dp, time_stamp, hours_since_epoch, iso_format, parse_iso_date
   use ht_constituents, only: constituent_set_t, load_constituent_set
   use ht_prediction, only: tide_series
   use ht_extrema, only: extrema_list_t, find_extrema
   use ht_output, only: write_tide_table, write_plot_csv
   use ht_cli, only: cli_has, cli_value, cli_integer
   implicit none

   type(constituent_set_t) :: set
   type(time_stamp) :: start_ts, end_ts
   real(dp), allocatable :: heights(:)
   type(extrema_list_t) :: extrema
   character(len=:), allocatable :: station_file, table_file, plot_file, start_text, end_text
   integer :: days, n_hours, ierr
   integer :: start_hours, end_hours
   character(len=:), allocatable :: message
   logical :: have_end

   if (cli_has('--help')) then
      call print_usage()
      stop 0
   end if

   station_file = cli_value('--station', 'data/wilson_cove.constituents')
   start_text = cli_value('--start', '2024-06-01')
   table_file = cli_value('--table', 'tide_table.txt')
   plot_file = cli_value('--plot', 'tide_plot.csv')
   have_end = cli_has('--end')

   call parse_iso_date(start_text, start_ts, ierr)
   if (ierr /= 0) then
      write (*, '(a)') 'error: --start must have the form YYYY-MM-DD'
      stop 1
   end if

   if (have_end) then
      end_text = cli_value('--end', '')
      call parse_iso_date(end_text, end_ts, ierr)
      if (ierr /= 0) then
         write (*, '(a)') 'error: --end must have the form YYYY-MM-DD'
         stop 1
      end if
      start_hours = int(hours_since_epoch(start_ts))
      end_hours = int(hours_since_epoch(end_ts))
      n_hours = end_hours - start_hours + 1
      if (n_hours <= 0) then
         write (*, '(a)') 'error: --end must not fall before --start'
         stop 1
      end if
   else
      call cli_integer('--days', days, ierr)
      if (ierr /= 0) days = 10
      n_hours = days*24
   end if

   call load_constituent_set(station_file, set, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if
   if (set%n == 0) then
      write (*, '(a)') 'error: the station file holds no constituents'
      stop 1
   end if

   allocate (heights(n_hours))
   call tide_series(set, real(hours_since_epoch(start_ts), dp), 1.0_dp, heights)
   call find_extrema(heights, real(hours_since_epoch(start_ts), dp), 1.0_dp, extrema)

   call write_tide_table(table_file, base_name(station_file), set, start_ts, heights, extrema, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if
   call write_plot_csv(plot_file, start_ts, heights, extrema, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if

   write (*, '(a)') 'Tide prediction complete'
   write (*, '(a,1x,a)') 'Station:', base_name(station_file)
   write (*, '(a,1x,a)') 'Range:', iso_format(start_ts, .true.)
   write (*, '(a,1x,i0,a)') 'Hours:', n_hours
   write (*, '(a,1x,i0,a)') 'Constituents:', set%n
   write (*, '(a,1x,i0,a)') 'Highs and lows:', size(extrema%item)
   write (*, '(a,1x,a)') 'Table:', table_file
   write (*, '(a,1x,a)') 'Plot data:', plot_file

contains

   !> Return the file name without its directory.
   function base_name(path) result(name)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: name
      integer :: i
      name = path
      do i = len(path), 1, -1
         if (path(i:i) == '/' .or. path(i:i) == '\') then
            name = path(i + 1:)
            exit
         end if
      end do
   end function base_name

   !> Print the usage text.
   subroutine print_usage()
      write (*, '(a)') 'tide_predict: build a tide table for one station'
      write (*, '(a)') ''
      write (*, '(a)') 'Options:'
      write (*, '(a)') '  --station FILE   constituent file (default data/wilson_cove.constituents)'
      write (*, '(a)') '  --start DATE     first hour, YYYY-MM-DD (default 2024-06-01)'
      write (*, '(a)') '  --days N         number of days (default 10)'
      write (*, '(a)') '  --end DATE       last day, YYYY-MM-DD (overrides --days)'
      write (*, '(a)') '  --table FILE     tide table output (default tide_table.txt)'
      write (*, '(a)') '  --plot FILE      plot data output (default tide_plot.csv)'
   end subroutine print_usage

end program tide_predict
