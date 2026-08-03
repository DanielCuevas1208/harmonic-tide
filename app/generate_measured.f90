!> Generate the synthetic measured-level file for the fit demo.
!>
!> The program evaluates the station constituents at hourly steps and
!> adds a small deterministic measurement noise. The noise uses a
!> Park-Miller linear congruential generator with a fixed seed, so the
!> output is reproducible on any machine.
!>
!> Build:
!>   gfortran -I<build>/<...> src/ht_time.f90 src/ht_constituents.f90 \
!>            src/ht_prediction.f90 tools/generate_measured.f90 -o build/bin/generate_measured
!>
!> Run:
!>   generate_measured <station_file> <output.csv> <start YYYY-MM-DD> <days> <seed>
program generate_measured
   use, intrinsic :: iso_fortran_env, only: int64
   use ht_time, only: dp, time_stamp, hours_since_epoch, time_from_hours, iso_format, parse_iso_date
   use ht_constituents, only: constituent_set_t, load_constituent_set
   use ht_prediction, only: tide_height
   implicit none

   character(len=256) :: station_file, output_file, start_text, days_text, seed_text
   type(constituent_set_t) :: set
   type(time_stamp) :: ts
   integer :: days, i, ierr, seed
   integer(int64) :: lcg_state
   real(dp) :: noise_amplitude
   character(len=:), allocatable :: message
   real(dp) :: height, noise

   if (command_argument_count() /= 5) then
      write (*, '(a)') 'usage: generate_measured <station> <output.csv> <YYYY-MM-DD> <days> <seed>'
      stop 1
   end if
   call get_command_argument(1, station_file)
   call get_command_argument(2, output_file)
   call get_command_argument(3, start_text)
   call get_command_argument(4, days_text)
   call get_command_argument(5, seed_text)
   read (days_text, *) days
   read (seed_text, *) seed

   call parse_iso_date(trim(start_text), ts, ierr)
   if (ierr /= 0) then
      write (*, '(a)') 'error: bad start date'
      stop 1
   end if
   call load_constituent_set(trim(station_file), set, ierr, message)
   if (ierr /= 0) then
      write (*, '(a)') 'error: '//message
      stop 1
   end if

   noise_amplitude = 0.05_dp
   lcg_state = int(seed, int64)
   open (newunit=ierr, file=trim(output_file), status='replace', action='write')
   write (ierr, '(a)') 'time,height_m'
   do i = 0, days*24 - 1
      height = tide_height(set, real(hours_since_epoch(ts), dp) + real(i, dp))
      call next_uniform(lcg_state, noise)
      height = height + (noise - 0.5_dp)*2.0_dp*noise_amplitude
      write (ierr, '(a,a)') iso_format(time_from_hours(hours_since_epoch(ts) + i), .true.), trim(height_text(height))
   end do
   close (ierr)
   write (*, '(a,1x,a)') 'Wrote', trim(output_file)

contains

   !> Height formatted as text with a comma prefix for the CSV row.
   function height_text(height) result(text)
      real(dp), intent(in) :: height
      character(len=:), allocatable :: text
      character(len=24) :: buffer
      write (buffer, '(f0.3)') height
      text = ','//trim(adjustl(buffer))
   end function height_text

   !> Draw one value in (0, 1) from the Park-Miller generator.
   subroutine next_uniform(state, value)
      integer(int64), intent(inout) :: state
      real(dp), intent(out) :: value
      integer(int64) :: hi, lo, result
      hi = state/127773_int64
      lo = mod(state, 127773_int64)
      result = 16807_int64*lo - 2836_int64*hi
      if (result <= 0) result = result + 2147483647_int64
      state = result
      value = real(result, dp)/2147483647.0_dp
   end subroutine next_uniform

end program generate_measured
