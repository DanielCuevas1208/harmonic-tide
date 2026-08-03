!> Tests for constituent handling.
program test_constituents
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: new_unittest, unittest_type, new_testsuite, testsuite_type, run_testsuite, error_type
   use ht_time, only: dp
   use ht_constituents, only: constituent_set_t, add_constituent, standard_speed, load_constituent_set
   use test_util, only: check_close, check_condition, load_test_set
   implicit none

   type(testsuite_type), allocatable :: testsuites(:)
   integer :: stat, is

   stat = 0
   testsuites = [new_testsuite('constituents', collect_tests)]
   do is = 1, size(testsuites)
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do
   if (stat > 0) error stop 1

contains

   !> Build the test list.
   subroutine collect_tests(tests)
      type(unittest_type), allocatable, intent(out) :: tests(:)
      tests = [ &
         new_unittest('standard_speeds', test_standard_speeds), &
         new_unittest('load_sample_file', test_load_sample_file), &
         new_unittest('reject_unknown', test_reject_unknown), &
         new_unittest('capacity', test_capacity)]
   end subroutine collect_tests

   !> Well-known constituent speeds match the standard values.
   subroutine test_standard_speeds(error)
      type(error_type), allocatable, intent(out) :: error

      call check_close(error, standard_speed('M2'), 28.9841042_dp, 1.0e-7_dp, 'M2 speed')
      call check_close(error, standard_speed('m2'), 28.9841042_dp, 1.0e-7_dp, 'M2 speed lowercase')
      call check_close(error, standard_speed('S2'), 30.0_dp, 1.0e-7_dp, 'S2 speed')
      call check_close(error, standard_speed('K1'), 15.0410686_dp, 1.0e-7_dp, 'K1 speed')
      call check_close(error, standard_speed('O1'), 13.9430356_dp, 1.0e-7_dp, 'O1 speed')
      call check_close(error, standard_speed('M4'), 57.9682084_dp, 1.0e-7_dp, 'M4 speed')
   end subroutine test_standard_speeds

   !> The sample file loads with the expected count and values.
   subroutine test_load_sample_file(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      integer :: ierr

      set = load_test_set(ierr)
      call check_condition(error, ierr == 0, 'sample file loads')
      if (allocated(error)) return
      call check_condition(error, set%n == 10, 'sample file has ten constituents')
      call check_close(error, set%z0, 1.852_dp, 1.0e-9_dp, 'sample mean sea level')
      call check_close(error, set%item(1)%amplitude, 1.120_dp, 1.0e-9_dp, 'first amplitude')
      call check_close(error, set%item(1)%phase_deg, 235.6_dp, 1.0e-9_dp, 'first phase')
      call check_close(error, set%item(2)%speed_deg_per_hour, 30.0_dp, 1.0e-9_dp, 'second speed')
   end subroutine test_load_sample_file

   !> A missing file is reported as an error.
   subroutine test_reject_unknown(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      integer :: ierr
      character(len=:), allocatable :: message

      call load_constituent_set('data/does_not_exist.constituents', set, ierr, message)
      call check_condition(error, ierr /= 0, 'missing file rejected')
   end subroutine test_reject_unknown

   !> The set accepts many constituents.
   subroutine test_capacity(error)
      type(error_type), allocatable, intent(out) :: error
      type(constituent_set_t) :: set
      integer :: ierr, i

      set = constituent_set_t()
      do i = 1, 8
         call add_constituent(set, 'M2', 1.0_dp, 10.0_dp, ierr)
         call check_condition(error, ierr == 0, 'add succeeds')
         if (allocated(error)) return
      end do
      call check_condition(error, set%n == 8, 'count after adds')
   end subroutine test_capacity

end program test_constituents
