!> Tide height prediction from harmonic constituents.
!>
!> The height at time t follows the harmonic form
!>
!>   h(t) = z0 + sum_k a_k * cos(w_k * t - p_k)
!>
!> where z0 is the mean sea level, a_k is the amplitude, w_k is the
!> speed in degrees per hour, p_k is the phase in degrees, and t is the
!> time in hours since the reference epoch 2000-01-01T00:00:00 UTC.
module ht_prediction
   use ht_time, only: dp
   use ht_constituents, only: constituent_set_t
   implicit none
   private

   public :: tide_height
   public :: tide_series

   real(dp), parameter :: deg2rad = atan(1.0_dp)/45.0_dp

contains

   !> Return the tide height at a given time in hours since the epoch.
   pure real(dp) function tide_height(set, time_hours) result(height)
      type(constituent_set_t), intent(in) :: set
      real(dp), intent(in) :: time_hours
      integer :: k

      height = set%z0
      do k = 1, set%n
         height = height + set%item(k)%amplitude* &
                  cos(deg2rad*(set%item(k)%speed_deg_per_hour*time_hours - set%item(k)%phase_deg))
      end do
   end function tide_height

   !> Fill an array of tide heights at uniform time steps.
   !> The first sample sits at time_hours and each later sample advances
   !> by step_hours.
   subroutine tide_series(set, time_hours, step_hours, heights)
      type(constituent_set_t), intent(in) :: set
      real(dp), intent(in) :: time_hours
      real(dp), intent(in) :: step_hours
      real(dp), intent(out) :: heights(:)
      integer :: i

      do i = 1, size(heights)
         heights(i) = tide_height(set, time_hours + step_hours*real(i - 1, dp))
      end do
   end subroutine tide_series

end module ht_prediction
