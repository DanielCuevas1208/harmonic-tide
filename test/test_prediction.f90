!> Tests for tide height prediction and high/low detection.
program test_prediction
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: new_unittest, unittest_type, new_testsuite, testsuite_type, run_testsuite, error_type
   use ht_time, only: dp
   use ht_constituents, only: constituent_set_t
   use ht_prediction, only: tide_height, tide_series
   use ht_extrema, only: find_extrema, extrema_list_t
   use test_util, only: check_close, check_condition, single_m2, two_constituents, load_test_set
   implicit none

   type(testsuite_type), allocatable :: testsuites(:)
   integer :: stat, is

   stat = 0
   testsuites = [new_testsuite('prediction', collect_tests)]
   do is = 1, size(testsuites)
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do
   if (stat > 0) error stop 1

contains

   !> Build the test list.
   subroutine collect_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      tests = [ &
         new_unittest('golden_heights', test_golden_heights), &
         new_unittest('known_two_constituents', test_known_two_constituents), &
         new_unittest('single_constituent_period', test_single_constituent_period), &
         new_unittest('series_uniform', test_series_uniform), &
         new_unittest('detect_extrema', test_detect_extrema), &
         new_unittest('extrema_empty', test_extrema_empty)]
   end subroutine collect_tests

   !> Heights match reference values computed independently.
   subroutine test_golden_heights(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      integer :: ierr

      set = load_test_set(ierr)
      call check_close(error, tide_height(set, 210384.0_dp), 1.034540518327135_dp, 1.0e-12_dp, 'golden t1')
      call check_close(error, tide_height(set, 210384.5_dp), 1.2053473831984078_dp, 1.0e-12_dp, 'golden t2')
      call check_close(error, tide_height(set, 210409.0_dp), 0.90873633373059448_dp, 1.0e-12_dp, 'golden t3')
      call check_close(error, tide_height(set, 211000.25_dp), 1.3057023689624818_dp, 1.0e-12_dp, 'golden t4')
      call check_close(error, tide_height(set, 220000.0_dp), 1.7704626989790584_dp, 1.0e-12_dp, 'golden t5')
      call check_close(error, tide_height(set, 0.0_dp), 0.78131257935981502_dp, 1.0e-12_dp, 'golden t6')
      call check_close(error, tide_height(set, 12.4206_dp), 0.995388466303471_dp, 1.0e-12_dp, 'golden t7')
   end subroutine test_golden_heights

   !> A small set with simple phases gives hand-checkable heights.
   subroutine test_known_two_constituents(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      set = two_constituents()
      call check_close(error, tide_height(set, 0.0_dp), 1.0_dp, 1.0e-12_dp, 'sum at zero')
      call check_close(error, tide_height(set, 1.0_dp), 0.94174147428094979_dp, 1.0e-12_dp, 'sum at one')
      call check_close(error, tide_height(set, 6.0_dp), 0.0056534809427984944_dp, 1.0e-12_dp, 'sum at six')
      call check_close(error, tide_height(set, 24.0_dp), 0.91081700469639304_dp, 1.0e-12_dp, 'sum at twenty-four')
   end subroutine test_known_two_constituents

   !> A single constituent repeats with its period.
   subroutine test_single_constituent_period(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      real(dp) :: period, a, b
      set = single_m2(0.5_dp, 2.0_dp, 0.0_dp)
      period = 360.0_dp/28.9841042_dp
      a = tide_height(set, 0.0_dp)
      b = tide_height(set, period)
      call check_close(error, a, 2.5_dp, 1.0e-9_dp, 'm2 max height')
      call check_close(error, a - b, 0.0_dp, 1.0e-9_dp, 'm2 periodicity')
      call check_close(error, tide_height(set, 3.1051_dp), 0.5000508954967442_dp, 1.0e-9_dp, 'm2 quarter period')
   end subroutine test_single_constituent_period

   !> The series advances at uniform steps.
   subroutine test_series_uniform(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      real(dp) :: heights(24)
      set = single_m2(0.0_dp, 1.0_dp, 0.0_dp)
      call tide_series(set, 0.0_dp, 1.0_dp, heights)
      call check_close(error, heights(1), tide_height(set, 0.0_dp), 1.0e-12_dp, 'series first')
      call check_close(error, heights(24), tide_height(set, 23.0_dp), 1.0e-12_dp, 'series last')
   end subroutine test_series_uniform

   !> Highs and lows appear at the expected times and heights.
   subroutine test_detect_extrema(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      type(extrema_list_t) :: list
      real(dp) :: heights(25)
      real(dp) :: base
      integer :: i

      set = single_m2(0.5_dp, 2.0_dp, 0.0_dp)
      base = 0.0_dp
      call tide_series(set, base, 1.0_dp, heights)
      call find_extrema(heights, base, 1.0_dp, list)
      call check_condition(error, size(list%item) == 3, 'three extrema in 25 hours')
      if (allocated(error)) return
      call check_condition(error, .not. list%item(1)%is_high, 'first extremum is a low')
      call check_close(error, list%item(1)%time_hours - base, 6.2066_dp, 1.0e-2_dp, 'first low time')
      call check_close(error, list%item(1)%height, -1.4993_dp, 5.0e-3_dp, 'first low height')
      call check_condition(error, list%item(2)%is_high, 'second extremum is a high')
      call check_close(error, list%item(2)%time_hours - base, 12.4179_dp, 1.0e-2_dp, 'high time')
      call check_close(error, list%item(2)%height, 2.4977_dp, 5.0e-3_dp, 'high height')
      call check_condition(error, .not. list%item(3)%is_high, 'third extremum is a low')
      call check_close(error, list%item(3)%time_hours - base, 18.6392_dp, 1.0e-2_dp, 'second low time')
      call check_close(error, list%item(3)%height, -1.4962_dp, 5.0e-3_dp, 'second low height')
   end subroutine test_detect_extrema

   !> A flat series has no extrema.
   subroutine test_extrema_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(extrema_list_t) :: list
      real(dp) :: heights(10)
      heights = 1.5_dp
      call find_extrema(heights, 0.0_dp, 1.0_dp, list)
      call check_condition(error, size(list%item) == 0, 'flat series has no extrema')
   end subroutine test_extrema_empty

end program test_prediction
