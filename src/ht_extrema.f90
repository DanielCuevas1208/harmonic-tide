!> Detection of high and low tide from a sampled height series.
!>
!> A sign change in the first difference marks a local extremum. The
!> detection refines the time and height with a parabolic fit to the
!> three neighbouring samples. Times are returned in hours since the
!> reference epoch.
module ht_extrema
   use ht_time, only: dp
   implicit none
   private

   public :: extrema_item_t
   public :: extrema_list_t
   public :: find_extrema

   !> One detected extremum.
   type :: extrema_item_t
      real(dp) :: time_hours = 0.0_dp
      real(dp) :: height = 0.0_dp
      logical :: is_high = .true.
   end type extrema_item_t

   !> A list of detected extrema in time order.
   type :: extrema_list_t
      type(extrema_item_t), allocatable :: item(:)
   end type extrema_list_t

contains

   !> Detect high and low tide in a uniformly sampled height series.
   !> The samples start at time_hours and advance by step_hours.
   !> The bounds of the series never become extrema.
   subroutine find_extrema(heights, time_hours, step_hours, list)
      real(dp), intent(in) :: heights(:)
      real(dp), intent(in) :: time_hours
      real(dp), intent(in) :: step_hours
      type(extrema_list_t), intent(out) :: list
      integer :: n, i, count
      type(extrema_item_t) :: found(2*size(heights))

      n = size(heights)
      count = 0
      do i = 2, n - 1
         if (heights(i - 1) < heights(i) .and. heights(i) > heights(i + 1)) then
            count = count + 1
            call refine(heights, i, time_hours, step_hours, .true., found(count))
         else if (heights(i - 1) > heights(i) .and. heights(i) < heights(i + 1)) then
            count = count + 1
            call refine(heights, i, time_hours, step_hours, .false., found(count))
         end if
      end do
      allocate (list%item(count))
      list%item = found(1:count)
   end subroutine find_extrema

   !> Fit a parabola through samples i-1, i, i+1 and store its vertex.
   subroutine refine(heights, index, time_hours, step_hours, is_high, result)
      real(dp), intent(in) :: heights(:)
      integer, intent(in) :: index
      real(dp), intent(in) :: time_hours
      real(dp), intent(in) :: step_hours
      logical, intent(in) :: is_high
      type(extrema_item_t), intent(out) :: result
      real(dp) :: left, centre, right, a, b, offset

      left = heights(index - 1)
      centre = heights(index)
      right = heights(index + 1)
      a = 0.5_dp*(left - 2.0_dp*centre + right)
      b = 0.5_dp*(right - left)
      if (abs(a) > tiny(1.0_dp)) then
         offset = -b/(2.0_dp*a)
         if (abs(offset) <= 0.5_dp) then
            result%time_hours = time_hours + step_hours*(real(index - 1, dp) + offset)
            result%height = centre - b*b/(4.0_dp*a)
            result%is_high = is_high
            return
         end if
      end if
      result%time_hours = time_hours + step_hours*real(index - 1, dp)
      result%height = centre
      result%is_high = is_high
   end subroutine refine

end module ht_extrema
