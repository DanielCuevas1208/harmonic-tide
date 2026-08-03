!> Tests for date and time handling.
program test_time
   use, intrinsic :: iso_fortran_env, only: int64, error_unit
   use testdrive, only: new_unittest, unittest_type, new_testsuite, testsuite_type, run_testsuite, error_type
   use ht_time, only: dp, time_stamp, hours_since_epoch, time_from_hours, add_hours, &
                      is_leap_year, days_in_month, iso_format, parse_iso_date, parse_iso_datetime
   use test_util, only: check_close, check_condition
   implicit none

   type(testsuite_type), allocatable :: testsuites(:)
   integer :: stat, is

   stat = 0
   testsuites = [new_testsuite('time', collect_tests)]
   do is = 1, size(testsuites)
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do
   if (stat > 0) error stop 1

contains

   !> Build the test list.
   subroutine collect_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      tests = [ &
         new_unittest('epoch_is_zero', test_epoch_is_zero), &
         new_unittest('round_trip_positive', test_round_trip_positive), &
         new_unittest('round_trip_negative', test_round_trip_negative), &
         new_unittest('hour_arithmetic', test_hour_arithmetic), &
         new_unittest('leap_years', test_leap_years), &
         new_unittest('month_lengths', test_month_lengths), &
         new_unittest('iso_formatting', test_iso_formatting), &
         new_unittest('parsing', test_parsing)]
   end subroutine collect_tests

   !> The reference epoch maps to zero hours.
   subroutine test_epoch_is_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts
      integer :: ierr
      call parse_iso_date('2000-01-01', ts, ierr)
      call check_close(error, real(hours_since_epoch(ts), dp), 0.0_dp, 0.0_dp, 'epoch hours')
   end subroutine test_epoch_is_zero

   !> Civil dates convert to hours and back without loss.
   subroutine test_round_trip_positive(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts, back
      integer :: i
      integer, parameter :: years(5) = [2000, 2024, 2026, 2100, 2200]

      do i = 1, size(years)
         ts = time_stamp(years(i), 6, 15, 12)
         back = time_from_hours(hours_since_epoch(ts))
         call check_condition(error, ts%year == back%year .and. ts%month == back%month .and. &
                              ts%day == back%day .and. ts%hour == back%hour, 'round trip')
      end do
   end subroutine test_round_trip_positive

   !> Dates before the epoch also convert without loss.
   subroutine test_round_trip_negative(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts, back
      ts = time_stamp(1970, 1, 1, 0)
      back = time_from_hours(hours_since_epoch(ts))
      call check_condition(error, hours_since_epoch(ts) == -262968_int64, 'pre-epoch hours')
      call check_condition(error, ts%year == back%year .and. ts%month == back%month .and. &
                           ts%day == back%day .and. ts%hour == back%hour, 'pre-epoch round trip')
      ts = time_stamp(1999, 12, 31, 23)
      back = time_from_hours(hours_since_epoch(ts))
      call check_condition(error, ts%year == back%year .and. ts%month == back%month .and. &
                           ts%day == back%day .and. ts%hour == back%hour, 'year boundary round trip')
   end subroutine test_round_trip_negative

   !> Hour arithmetic crosses midnight and year boundaries.
   subroutine test_hour_arithmetic(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts

      ts = add_hours(time_stamp(2024, 1, 1, 0), 25)
      call check_condition(error, ts%day == 2 .and. ts%hour == 1, 'add hours into next day')
      ts = add_hours(time_stamp(2024, 2, 28, 23), 2)
      call check_condition(error, ts%month == 2 .and. ts%day == 29 .and. ts%hour == 1, 'add across leap day')
      ts = add_hours(time_stamp(2023, 12, 31, 23), 1)
      call check_condition(error, ts%year == 2024 .and. ts%month == 1 .and. ts%day == 1, 'add across year')
   end subroutine test_hour_arithmetic

   !> Leap year rules.
   subroutine test_leap_years(error)
      type(error_type), allocatable, intent(out) :: error
      call check_condition(error, is_leap_year(2000), '2000 is a leap year')
      call check_condition(error, is_leap_year(2024), '2024 is a leap year')
      call check_condition(error, .not. is_leap_year(1900), '1900 is not a leap year')
      call check_condition(error, .not. is_leap_year(2100), '2100 is not a leap year')
      call check_condition(error, .not. is_leap_year(2023), '2023 is not a leap year')
   end subroutine test_leap_years

   !> Month lengths respect leap years.
   subroutine test_month_lengths(error)
      type(error_type), allocatable, intent(out) :: error
      call check_condition(error, days_in_month(2024, 2) == 29, 'february 2024')
      call check_condition(error, days_in_month(2023, 2) == 28, 'february 2023')
      call check_condition(error, days_in_month(2024, 4) == 30, 'april')
      call check_condition(error, days_in_month(2024, 12) == 31, 'december')
   end subroutine test_month_lengths

   !> ISO formatting is zero padded.
   subroutine test_iso_formatting(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts
      ts = time_stamp(2024, 3, 7, 4)
      call check_condition(error, iso_format(ts, .true.) == '2024-03-07T04:00', 'iso with time')
      call check_condition(error, iso_format(ts, .false.) == '2024-03-07', 'iso date only')
   end subroutine test_iso_formatting

   !> Parsing accepts valid text and rejects invalid text.
   subroutine test_parsing(error)
      type(error_type), allocatable, intent(out) :: error
      type(time_stamp) :: ts
      integer :: minute, ierr

      call parse_iso_datetime('2024-06-01T03:24', ts, minute, ierr)
      call check_condition(error, ierr == 0 .and. ts%day == 1 .and. ts%hour == 3 .and. minute == 24, 'parse datetime')
      call parse_iso_date('2024-02-29', ts, ierr)
      call check_condition(error, ierr == 0 .and. ts%month == 2 .and. ts%day == 29, 'parse leap date')
      call parse_iso_date('nonsense', ts, ierr)
      call check_condition(error, ierr /= 0, 'reject bad date')
      call parse_iso_datetime('2024-06-01T25:00', ts, minute, ierr)
      call check_condition(error, ierr == 0, 'accept hour 25 as a value')
   end subroutine test_parsing

end program test_time
