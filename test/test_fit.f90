!> Tests for least-squares constituent fitting.
program test_fit
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: new_unittest, unittest_type, new_testsuite, testsuite_type, run_testsuite, error_type
   use ht_time, only: dp, time_stamp, hours_since_epoch
   use ht_constituents, only: constituent_set_t
   use ht_prediction, only: tide_series
   use ht_fit, only: fit_constituents, fit_result_t, estimated_z0
   use test_util, only: check_close, check_condition, fit_truth_set
   implicit none

   type(testsuite_type), allocatable :: testsuites(:)
   integer :: stat, is

   stat = 0
   testsuites = [new_testsuite('fit', collect_tests)]
   do is = 1, size(testsuites)
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do
   if (stat > 0) error stop 1

contains

   !> Build the test list.
   subroutine collect_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      tests = [ &
         new_unittest('recovers_truth', test_recovers_truth), &
         new_unittest('z0_recovery', test_z0_recovery), &
         new_unittest('rejects_short', test_rejects_short)]
   end subroutine collect_tests

   !> A noise-free series recovers the source constituents closely.
   subroutine test_recovers_truth(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: truth
      type(fit_result_t) :: result
      real(dp), allocatable :: heights(:), times(:)
      real(dp) :: base
      integer :: ierr, k

      truth = fit_truth_set()
      allocate (heights(120*24), times(120*24))
      base = real(hours_since_epoch(time_stamp(2024, 1, 1, 0)), dp)
      call tide_series(truth, base, 1.0_dp, heights)
      times = times_at(base, size(heights))
      call fit_constituents(truth, times, heights, result, ierr)
      call check_condition(error, ierr == 0, 'fit succeeds')
      if (allocated(error)) return

      call check_close(error, result%z0, truth%z0, 1.0e-9_dp, 'fitted z0')
      call check_close(error, result%rms_error, 0.0_dp, 1.0e-9_dp, 'rms error')
      do k = 1, truth%n
         call check_close(error, result%item(k)%amplitude, truth%item(k)%amplitude, 1.0e-6_dp, &
                          'amplitude '//trim(truth%item(k)%name))
         call check_close(error, result%item(k)%phase_deg, truth%item(k)%phase_deg, 1.0e-4_dp, &
                          'phase '//trim(truth%item(k)%name))
      end do
   end subroutine test_recovers_truth

   !> The estimated mean sea level matches the source value.
   subroutine test_z0_recovery(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: truth
      type(fit_result_t) :: result
      real(dp), allocatable :: heights(:), times(:)
      real(dp) :: base
      integer :: ierr

      truth = fit_truth_set()
      allocate (heights(60*24), times(60*24))
      base = real(hours_since_epoch(time_stamp(2023, 6, 1, 0)), dp)
      call tide_series(truth, base, 1.0_dp, heights)
      times = times_at(base, size(heights))
      call fit_constituents(truth, times, heights, result, ierr)
      call check_condition(error, ierr == 0, 'z0 fit succeeds')
      if (allocated(error)) return
      call check_close(error, estimated_z0(result), truth%z0, 1.0e-8_dp, 'z0 via accessor')
      call check_condition(error, result%item(1)%name == 'M2', 'constituent name kept')
   end subroutine test_z0_recovery

   !> A series shorter than the unknowns is rejected.
   subroutine test_rejects_short(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: truth
      type(fit_result_t) :: result
      real(dp) :: times(2), heights(2)
      integer :: ierr

      truth = fit_truth_set()
      times = [1.0_dp, 2.0_dp]
      heights = [0.0_dp, 0.0_dp]
      call fit_constituents(truth, times, heights, result, ierr)
      call check_condition(error, ierr /= 0, 'short series rejected')
   end subroutine test_rejects_short

   !> Hourly times for a series that starts at base.
   function times_at(base, count) result(times)
      real(dp), intent(in) :: base
      integer, intent(in) :: count
      real(dp) :: times(count)
      integer :: i
      times = base + real([(i, i = 0, count - 1)], dp)
   end function times_at

end program test_fit
