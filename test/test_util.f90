!> Shared helpers for the Harmonic Tide test suite.
module test_util
   use ht_time, only: dp
   use ht_constituents, only: constituent_set_t, add_constituent, load_constituent_set
   use testdrive, only: error_type, check
   implicit none
   private

   public :: check_close
   public :: check_condition
   public :: single_m2
   public :: two_constituents
   public :: fit_truth_set
   public :: load_test_set

contains

   !> Fail when the actual value is not within tolerance of the expected.
   subroutine check_close(error, actual, expected, tolerance, label)
      type(error_type), allocatable, intent(inout) :: error
      real(dp), intent(in) :: actual
      real(dp), intent(in) :: expected
      real(dp), intent(in) :: tolerance
      character(len=*), intent(in) :: label

      if (abs(actual - expected) > tolerance) then
         call check(error, .false., message=label)
      end if
   end subroutine check_close

   !> Fail with a message when a condition is false.
   subroutine check_condition(error, condition, label)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: condition
      character(len=*), intent(in) :: label

      call check(error, condition, message=label)
   end subroutine check_condition

   !> A set with a single M2 constituent.
   function single_m2(z0, amplitude, phase_deg) result(set)
      real(dp), intent(in) :: z0
      real(dp), intent(in) :: amplitude
      real(dp), intent(in) :: phase_deg
      type(constituent_set_t) :: set
      integer :: ierr

      set = constituent_set_t()
      set%z0 = z0
      call add_constituent(set, 'M2', amplitude, phase_deg, ierr)
   end function single_m2

   !> A set with two constituents at known values.
   function two_constituents() result(set)
      type(constituent_set_t) :: set
      integer :: ierr

      set = constituent_set_t()
      set%z0 = 0.5_dp
      call add_constituent(set, 'M2', 1.0_dp, 0.0_dp, ierr)
      call add_constituent(set, 'S2', 0.5_dp, 180.0_dp, ierr)
   end function two_constituents

   !> The known set used by the fit test.
   function fit_truth_set() result(set)
      type(constituent_set_t) :: set
      integer :: ierr

      set = constituent_set_t()
      set%z0 = 1.852_dp
      call add_constituent(set, 'M2', 1.120_dp, 235.6_dp, ierr)
      call add_constituent(set, 'S2', 0.460_dp, 262.4_dp, ierr)
      call add_constituent(set, 'O1', 0.210_dp, 96.5_dp, ierr)
      call add_constituent(set, 'K1', 0.280_dp, 112.0_dp, ierr)
   end function fit_truth_set

   !> Load the sample station file.
   function load_test_set(ierr) result(set)
      type(constituent_set_t) :: set
      integer, intent(out) :: ierr
      character(len=:), allocatable :: message

      call load_constituent_set('data/wilson_cove.constituents', set, ierr, message)
   end function load_test_set

end module test_util
