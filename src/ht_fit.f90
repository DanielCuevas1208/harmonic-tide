!> Least-squares estimation of harmonic constituents.
!>
!> The model is linear in its unknowns:
!>
!>   h(t) = z0 + sum_k [ c_k * cos(w_k * t) + s_k * sin(w_k * t) ]
!>
!> The fit solves for z0, c_k, and s_k with a QR decomposition. The
!> amplitude and phase follow from c_k and s_k. Times use hours since
!> the reference epoch and speeds use degrees per hour.
module ht_fit
   use ht_time, only: dp
   use ht_constituents, only: constituent_t, constituent_set_t, max_constituents, &
                              constituent_name_len
   implicit none
   private

   public :: fit_result_t
   public :: fit_constituents
   public :: estimated_z0

   !> The result of a constituent fit.
   type :: fit_result_t
      integer :: n_samples = 0
      integer :: n_unknowns = 0
      integer :: n_constituents = 0
      real(dp) :: z0 = 0.0_dp
      real(dp) :: rms_error = 0.0_dp
      real(dp) :: max_error = 0.0_dp
      type(constituent_t) :: item(max_constituents)
   end type fit_result_t

contains

   !> Fit constituents to measured water levels.
   !>
   !> The template supplies the constituent names. The fit uses only the
   !> name and speed of each template entry. times and heights hold the
   !> measured series. On entry, times hold hours since the epoch and
   !> heights hold metres. ierr is nonzero when the problem is empty or
   !> underdetermined.
   subroutine fit_constituents(template, times, heights, result, ierr)
      type(constituent_set_t), intent(in) :: template
      real(dp), intent(in) :: times(:)
      real(dp), intent(in) :: heights(:)
      type(fit_result_t), intent(out) :: result
      integer, intent(out) :: ierr
      integer :: n, m, k
      real(dp), allocatable :: design(:,:)
      real(dp), allocatable :: rhs(:)
      real(dp), allocatable :: solution(:)
      real(dp), allocatable :: fitted(:)
      real(dp) :: residual, sum_sq, max_residual

      n = size(times)
      m = 1 + 2*template%n
      result = fit_result_t()
      result%n_samples = n
      result%n_constituents = template%n
      result%n_unknowns = m
      if (n == 0 .or. m == 0) then
         ierr = 1
         return
      end if
      if (n < m) then
         ierr = 2
         return
      end if

      allocate (design(n, m), rhs(n), solution(m), fitted(n))
      call build_design(template, times, design)
      rhs = heights
      call solve_least_squares(design, rhs, solution, ierr)
      if (ierr /= 0) return

      result%z0 = solution(1)
      do k = 1, template%n
         call from_coefficients(solution(1 + 2*(k - 1) + 1), solution(1 + 2*(k - 1) + 2), &
                                template%item(k)%name, template%item(k)%speed_deg_per_hour, &
                                result%item(k))
      end do

      fitted = matmul(design, solution)
      sum_sq = 0.0_dp
      max_residual = 0.0_dp
      do k = 1, n
         residual = heights(k) - fitted(k)
         sum_sq = sum_sq + residual*residual
         max_residual = max(max_residual, abs(residual))
      end do
      if (n > 0) result%rms_error = sqrt(sum_sq/real(n, dp))
      result%max_error = max_residual
   end subroutine fit_constituents

   !> Return the estimated mean sea level from a fit result.
   pure real(dp) function estimated_z0(result) result(z0)
      type(fit_result_t), intent(in) :: result
      z0 = result%z0
   end function estimated_z0

   !> Build the least-squares design matrix.
   !> Column 1 holds ones. Each constituent adds two columns for the
   !> cosine and sine terms.
   subroutine build_design(template, times, design)
      type(constituent_set_t), intent(in) :: template
      real(dp), intent(in) :: times(:)
      real(dp), intent(out) :: design(:,:)
      integer :: k, i, col
      real(dp) :: angle

      design(:, 1) = 1.0_dp
      do k = 1, template%n
         col = 1 + 2*(k - 1)
         do i = 1, size(times)
            angle = deg2rad_value()*template%item(k)%speed_deg_per_hour*times(i)
            design(i, col + 1) = cos(angle)
            design(i, col + 2) = sin(angle)
         end do
      end do
   end subroutine build_design

   !> Convert c and s coefficients into an amplitude and a phase.
   !> The phase follows cos(w*t - p) = c*cos(w*t) + s*sin(w*t).
   subroutine from_coefficients(c, s, name, speed, item)
      real(dp), intent(in) :: c
      real(dp), intent(in) :: s
      character(len=*), intent(in) :: name
      real(dp), intent(in) :: speed
      type(constituent_t), intent(out) :: item
      item%name = name
      item%speed_deg_per_hour = speed
      item%amplitude = sqrt(c*c + s*s)
      item%phase_deg = mod(atan2(s, c)*180.0_dp/pi_value(), 360.0_dp)
      if (item%phase_deg < 0.0_dp) item%phase_deg = item%phase_deg + 360.0_dp
   end subroutine from_coefficients

   !> Solve the linear least-squares problem min ||A x - b|| with
   !> modified Gram-Schmidt QR decomposition.
   subroutine solve_least_squares(design, rhs, solution, ierr)
      real(dp), intent(inout) :: design(:,:)
      real(dp), intent(in) :: rhs(:)
      real(dp), intent(out) :: solution(:)
      integer, intent(out) :: ierr
      integer :: n, m, i, j, k
      real(dp), allocatable :: q(:,:), r(:,:), projected(:)
      real(dp) :: norm, scale

      n = size(design, 1)
      m = size(design, 2)
      allocate (q(n, m), r(m, m), projected(n))
      q = design
      r = 0.0_dp
      scale = maxval(sum(design*design, dim=1))
      if (scale <= 0.0_dp) then
         ierr = 3
         return
      end if
      scale = sqrt(scale)

      do j = 1, m
         r(j, j) = norm2(q(:, j))
         if (r(j, j) < 1.0e-10_dp*scale) then
            ierr = 3
            return
         end if
         q(:, j) = q(:, j)/r(j, j)
         do i = j + 1, m
            r(j, i) = dot_product(q(:, j), q(:, i))
            q(:, i) = q(:, i) - r(j, i)*q(:, j)
         end do
      end do

      projected = rhs
      do j = 1, m
         solution(j) = dot_product(q(:, j), projected)
         projected = projected - solution(j)*q(:, j)
      end do

      call back_substitute(r, solution, ierr)
   end subroutine solve_least_squares

   !> Solve an upper-triangular system in place.
   subroutine back_substitute(r, x, ierr)
      real(dp), intent(in) :: r(:,:)
      real(dp), intent(inout) :: x(:)
      integer, intent(out) :: ierr
      integer :: m, i, j

      m = size(x)
      ierr = 0
      do i = m, 1, -1
         if (abs(r(i, i)) < tiny(1.0_dp)) then
            ierr = 3
            return
         end if
         do j = i + 1, m
            x(i) = x(i) - r(i, j)*x(j)
         end do
         x(i) = x(i)/r(i, i)
      end do
   end subroutine back_substitute

   !> Pi to double precision.
   pure real(dp) function pi_value() result(pi)
      pi = 4.0_dp*atan(1.0_dp)
   end function pi_value

   !> Degrees to radians factor.
   pure real(dp) function deg2rad_value() result(factor)
      factor = atan(1.0_dp)/45.0_dp
   end function deg2rad_value

end module ht_fit
