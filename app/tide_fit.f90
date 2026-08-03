!> Estimate harmonic constituents from measured water levels.
program tide_fit
   use ht_time, only: dp, time_stamp, hours_since_epoch, parse_iso_datetime
   use ht_constituents, only: constituent_set_t, constituent_t, load_constituent_set, write_constituent_set
   use ht_fit, only: fit_constituents, fit_result_t
   use ht_cli, only: cli_has, cli_value
   implicit none

   type(constituent_set_t) :: template
   type(fit_result_t) :: result
   real(dp), allocatable :: times(:), heights(:)
   character(len=:), allocatable :: station_file, data_file, output_file, message
   integer :: ierr, n, unit, i, minute, ios
   type(time_stamp) :: ts
   character(len=256) :: line
   character(len=:), allocatable :: time_text
   real(dp) :: height

   if (cli_has('--help')) then
      call print_usage()
      stop 0
   end if

   station_file = cli_value('--station', 'data/wilson_cove.constituents')
   data_file = cli_value('--data', 'data/wilson_cove_measured.csv')
   output_file = cli_value('--output', 'fitted_constituents.txt')

   call load_constituent_set(station_file, template, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if
   if (template%n == 0) then
      write (*, '(a)') 'error: the template file holds no constituents'
      stop 1
   end if

   call read_measurements(data_file, times, heights, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if

   call fit_constituents(template, times, heights, result, ierr)
   if (ierr /= 0) then
      write (*, '(a,i0)') 'error: the fit failed with code ', ierr
      stop 1
   end if

   call write_constituent_set(output_file, fitted_set(template, result), ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if

   call print_report(template, result, output_file)

contains

   !> Read a measured-level CSV file with a header row.
   !> Each data row holds an ISO datetime and a height in metres.
   subroutine read_measurements(filename, times, heights, ierr, message)
      character(len=*), intent(in) :: filename
      real(dp), allocatable, intent(out) :: times(:)
      real(dp), allocatable, intent(out) :: heights(:)
      integer, intent(out) :: ierr
      character(len=:), allocatable, intent(out) :: message
      integer :: unit, n_rows, i, pos
      real(dp), allocatable :: t(:), h(:)
      character(len=:), allocatable :: dt
      type(time_stamp) :: ts
      integer :: minute
      logical :: header

      open (newunit=unit, file=filename, status='old', action='read', iostat=ierr)
      if (ierr /= 0) then
         message = 'cannot open file: '//trim(filename)
         return
      end if

      n_rows = 0
      header = .true.
      do
         read (unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (header) then
            header = .false.
            cycle
         end if
         n_rows = n_rows + 1
      end do
      rewind (unit)

      allocate (t(max(n_rows, 1)), h(max(n_rows, 1)))
      n_rows = 0
      header = .true.
      do
         read (unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (header) then
            header = .false.
            cycle
         end if
         pos = index(line, ',')
         if (pos <= 1) cycle
         allocate (character(len=pos - 1) :: dt)
         dt = line(1:pos - 1)
         call parse_iso_datetime(dt, ts, minute, ierr)
         if (ierr == 0) read (line(pos + 1:), *, iostat=ierr) height
         deallocate (dt)
         if (ierr /= 0) cycle
         n_rows = n_rows + 1
         t(n_rows) = real(hours_since_epoch(ts), dp) + real(minute, dp)/60.0_dp
         h(n_rows) = height
      end do
      close (unit)

      if (n_rows == 0) then
         ierr = 1
         message = 'no usable rows in file: '//trim(filename)
         return
      end if
      allocate (times(n_rows), heights(n_rows))
      times = t(1:n_rows)
      heights = h(1:n_rows)
      ierr = 0
      message = ''
   end subroutine read_measurements

   !> Build a constituent set that holds only the fitted values.
   function fitted_set(template, result) result(set)
      type(constituent_set_t), intent(in) :: template
      type(fit_result_t), intent(in) :: result
      type(constituent_set_t) :: set
      set = template
      set%z0 = result%z0
      do i = 1, template%n
         set%item(i)%amplitude = result%item(i)%amplitude
         set%item(i)%phase_deg = result%item(i)%phase_deg
      end do
   end function fitted_set

   !> Print a summary of the fit.
   subroutine print_report(template, result, output_file)
      type(constituent_set_t), intent(in) :: template
      type(fit_result_t), intent(in) :: result
      character(len=*), intent(in) :: output_file
      integer :: k

      write (*, '(a)') 'Constituent fit complete'
      write (*, '(a,1x,i0)') 'Samples:', result%n_samples
      write (*, '(a,1x,i0)') 'Unknowns:', result%n_unknowns
      write (*, '(a,1x,f0.3,a)') 'Mean sea level (fitted):', result%z0, ' m'
      write (*, '(a,1x,f0.4,a)') 'RMS error:', result%rms_error, ' m'
      write (*, '(a,1x,f0.4,a)') 'Max error:', result%max_error, ' m'
      write (*, '(a)') ''
      write (*, '(a)') 'Constituent   amplitude(m)  phase(deg)   fitted amp  fitted phase'
      do k = 1, template%n
         write (*, '(1x,a,1x,f9.3,1x,f9.1,1x,f9.3,1x,f9.1)') &
            template%item(k)%name, template%item(k)%amplitude, template%item(k)%phase_deg, &
            result%item(k)%amplitude, result%item(k)%phase_deg
      end do
      write (*, '(a)') ''
      write (*, '(a,1x,a)') 'Fitted constituents:', output_file
   end subroutine print_report

   !> Print the usage text.
   subroutine print_usage()
      write (*, '(a)') 'tide_fit: estimate constituents from measured water levels'
      write (*, '(a)') ''
      write (*, '(a)') 'Options:'
      write (*, '(a)') '  --station FILE   template constituents (default data/wilson_cove.constituents)'
      write (*, '(a)') '  --data FILE      measured levels CSV (default data/wilson_cove_measured.csv)'
      write (*, '(a)') '  --output FILE    fitted constituents (default fitted_constituents.txt)'
   end subroutine print_usage

end program tide_fit
