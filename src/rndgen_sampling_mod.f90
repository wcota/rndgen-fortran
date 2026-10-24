! Numerical transformations shared by the scalar and array interfaces.
module rndgen_sampling_mod
  use iso_fortran_env, only: i4 => int32, i8 => int64
  use iso_fortran_env, only: sp => real32, dp => real64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private

  integer(i8), parameter :: mask32 = int(z'FFFFFFFF', i8)

  public :: integer_span, integer_add
  public :: interval_dp, interval_sp
  public :: validate_real_bounds, interpolate_dp, interpolate_sp

contains

  function integer_span(lower, upper) result(span)
    integer(i8), intent(in) :: lower, upper
    integer(i8) :: span, low, high

    if (upper < lower) error stop 'Integer bounds must be ordered'
    low = iand(upper, mask32) - iand(lower, mask32)
    high = shiftr(upper, 32) - shiftr(lower, 32)
    if (low < 0_i8) high = high - 1_i8
    span = ior(shiftl(iand(high, mask32), 32), iand(low, mask32))
  end function integer_span

  pure function integer_add(left, right) result(value)
    integer(i8), intent(in) :: left, right
    integer(i8) :: value, low, high

    low = iand(left, mask32) + iand(right, mask32)
    high = shiftr(left, 32) + shiftr(right, 32) + shiftr(low, 32)
    value = ior(shiftl(iand(high, mask32), 32), iand(low, mask32))
  end function integer_add

  subroutine validate_real_bounds(lower, upper)
    real(dp), intent(in) :: lower, upper
    if (.not. ieee_is_finite(lower) .or. &
      .not. ieee_is_finite(upper)) then
      error stop 'Real bounds must be finite'
    end if
    if (upper < lower) error stop 'Real bounds must be ordered'
  end subroutine validate_real_bounds

  function interval_dp(lower, upper, uniform) result(value)
    real(dp), intent(in) :: lower, upper, uniform
    real(dp) :: value

    call validate_real_bounds(lower, upper)
    value = interpolate_dp(lower, upper, uniform)
  end function interval_dp

  pure function interpolate_dp(lower, upper, uniform) result(value)
    real(dp), intent(in) :: lower, upper, uniform
    real(dp) :: value

    if (lower == upper) then
      value = lower
      return
    end if
    if (lower < 0.0_dp .and. upper > 0.0_dp) then
      value = (1.0_dp - uniform)*lower + uniform*upper
    else
      value = lower + (upper - lower)*uniform
    end if
    value = max(lower, min(value, nearest(upper, -1.0_dp)))
  end function interpolate_dp

  function interval_sp(lower, upper, uniform) result(value)
    real(sp), intent(in) :: lower, upper
    real(dp), intent(in) :: uniform
    real(sp) :: value

    call validate_real_bounds(real(lower, dp), real(upper, dp))
    value = interpolate_sp(lower, upper, uniform)
  end function interval_sp

  pure function interpolate_sp(lower, upper, uniform) result(value)
    real(sp), intent(in) :: lower, upper
    real(dp), intent(in) :: uniform
    real(sp) :: value

    value = real(interpolate_dp(real(lower, dp), real(upper, dp), &
      uniform), sp)
    if (lower < upper) then
      value = max(lower, min(value, nearest(upper, -1.0_sp)))
    end if
  end function interpolate_sp

end module rndgen_sampling_mod
