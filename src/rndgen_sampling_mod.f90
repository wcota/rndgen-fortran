! Numerical transformations shared by the scalar and array interfaces.
module rndgen_sampling_mod
  use iso_fortran_env, only: i4 => int32, i8 => int64
  use iso_fortran_env, only: sp => real32, dp => real64
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private

  integer(i8), parameter :: mask32 = int(z'FFFFFFFF', i8)

  ! The proposal consists of the mode's unit mass and a continuous envelope.
  ! No normalization sum or table proportional to the support is needed.
  type, public :: powerlaw_parameters
    private
    integer(i4) :: lower, upper
    real(dp) :: gamma, log_ratio, point_probability
    real(dp) :: continuous_probability, exponent, magnitude, tail, exp_delta
    logical :: near_one
    logical :: initialized = .false.
  contains
    procedure :: init => powerlaw_init
    procedure :: propose => powerlaw_propose
    procedure :: accept => powerlaw_accept
  end type powerlaw_parameters

  public :: integer_span, integer_add, interval_dp, interval_sp
  public :: validate_real_bounds, interpolate_dp, interpolate_sp

contains

  ! Return upper-lower as an unsigned bit pattern, including 2^64-1.
  function integer_span(lower, upper) result(span)
    integer(i8), intent(in) :: lower, upper
    integer(i8) :: span, low, high

    if (upper < lower) error stop 'Integer bounds must be ordered'
    low = iand(upper, mask32) - iand(lower, mask32)
    high = shiftr(upper, 32) - shiftr(lower, 32)
    if (low < 0_i8) high = high - 1_i8
    span = ior(shiftl(iand(high, mask32), 32), iand(low, mask32))
  end function integer_span

  ! Add bit patterns without relying on signed integer overflow.
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

  ! The caller validates the bounds once, including for empty arrays.
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

  ! Accurate near zero, without requiring nonstandard log1p/expm1 intrinsics.
  pure function log1p_small(value) result(answer)
    real(dp), intent(in) :: value
    real(dp) :: answer, rounded

    rounded = 1.0_dp + value
    if (rounded == 1.0_dp) then
      answer = value
    else
      answer = log(rounded)*(value/(rounded - 1.0_dp))
    end if
  end function log1p_small

  pure function expm1_small(value) result(answer)
    real(dp), intent(in) :: value
    real(dp) :: answer

    answer = 2.0_dp*sinh(0.5_dp*value)*exp(0.5_dp*value)
  end function expm1_small

  ! Integral of exp(-rate*t) over t in [0, log_ratio].
  pure function envelope_integral(rate, log_ratio) result(integral)
    real(dp), intent(in) :: rate, log_ratio
    real(dp) :: integral, argument

    if (rate == 0.0_dp) then
      integral = log_ratio
    else if (rate > 36.0_dp/log_ratio) then
      integral = 1.0_dp/rate
    else
      argument = -rate*log_ratio
      if (abs(argument) < 0.5_dp) then
        integral = log_ratio*(expm1_small(argument)/argument)
      else
        integral = (1.0_dp - exp(argument))/rate
      end if
    end if
  end function envelope_integral

  subroutine powerlaw_init(this, lower, upper, gamma)
    class(powerlaw_parameters), intent(inout) :: this
    integer(i4), intent(in) :: lower, upper
    real(dp), intent(in) :: gamma
    real(dp) :: integral

    if (.not. ieee_is_finite(gamma)) then
      error stop 'Power-law exponent must be finite'
    end if
    if (lower < 1 .or. upper < lower) then
      error stop 'Power-law bounds must satisfy 1 <= lower <= upper'
    end if
    this%lower = lower
    this%upper = upper
    this%gamma = gamma
    this%point_probability = 1.0_dp
    this%log_ratio = 0.0_dp
    this%initialized = .true.
    if (lower == upper) return

    this%log_ratio = log1p_small(real(upper - lower, dp)/real(lower, dp))
    if (gamma >= 0.0_dp) then
      integral = real(lower, dp)* &
        envelope_integral(gamma - 1.0_dp, this%log_ratio)
    else
      integral = real(upper, dp)* &
        envelope_integral(1.0_dp - gamma, this%log_ratio)
    end if
    this%point_probability = 1.0_dp/(1.0_dp + integral)
    this%continuous_probability = 1.0_dp - this%point_probability

    ! Cache only parameter-dependent terms. Keep the per-draw arithmetic
    ! order so this optimization preserves the corrected sample streams.
    this%exponent = 1.0_dp - gamma
    this%magnitude = abs(this%exponent)
    this%near_one = this%magnitude <= 0.5_dp/this%log_ratio
    this%exp_delta = 0.0_dp
    this%tail = 0.0_dp
    if (this%near_one) then
      this%exp_delta = expm1_small(this%exponent*this%log_ratio)
    else if (this%magnitude <= 700.0_dp/this%log_ratio) then
      this%tail = exp(-this%magnitude*this%log_ratio)
    end if
  end subroutine powerlaw_init

  subroutine powerlaw_propose(this, uniform, x, value)
    class(powerlaw_parameters), intent(in) :: this
    real(dp), intent(in) :: uniform
    real(dp), intent(out) :: x
    integer(i4), intent(out) :: value
    real(dp) :: u, delta, logarithm

    if (.not. this%initialized) error stop 'Initialize the power-law sampler'
    if (uniform < this%point_probability .or. &
      this%lower == this%upper) then
      value = this%lower
      if (this%gamma < 0.0_dp) value = this%upper
      x = real(value, dp)
      return
    end if

    u = (uniform - this%point_probability)/ &
      this%continuous_probability
    u = max(0.0_dp, min(u, nearest(1.0_dp, -1.0_dp)))

    if (this%near_one) then
      if (this%exponent*this%log_ratio == 0.0_dp) then
        logarithm = u*this%log_ratio
      else
        delta = u*this%exp_delta
        logarithm = log1p_small(delta)/this%exponent
      end if
      x = real(this%lower, dp)*exp(logarithm)
    else
      if (this%exponent < 0.0_dp) then
        x = real(this%lower, dp)*exp(-log(1.0_dp - u + u*this%tail)/ &
          this%magnitude)
      else if (u == 0.0_dp) then
        x = real(this%lower, dp)
      else
        x = real(this%upper, dp)*exp(log(u + (1.0_dp - u)*this%tail)/ &
          this%magnitude)
      end if
    end if

    x = max(real(this%lower, dp), min(x, real(this%upper, dp)))
    if (this%gamma >= 0.0_dp) then
      value = max(this%lower + 1, ceiling(x, kind=i4))
    else
      value = min(this%upper - 1, floor(x, kind=i4))
    end if
  end subroutine powerlaw_propose

  function powerlaw_accept(this, x, value, uniform) result(accepted)
    class(powerlaw_parameters), intent(in) :: this
    real(dp), intent(in) :: x, uniform
    integer(i4), intent(in) :: value
    logical :: accepted
    real(dp) :: magnitude, log_ratio, distance, scaled, lower_bound

    accepted = .true.
    if (uniform == 0.0_dp .or. this%gamma == 0.0_dp) return
    if (x == real(value, dp)) return
    magnitude = abs(this%gamma)
    if (this%gamma > 0.0_dp) then
      distance = (real(value, dp) - x)/real(value, dp)
    else
      distance = (x - real(value, dp))/x
    end if

    ! Bernoulli bounds squeeze (1-distance)**magnitude. Here distance is
    ! at most 1/2, so even a finite extreme exponent cannot overflow scaled.
    scaled = magnitude*distance
    lower_bound = 1.0_dp - distance
    if (magnitude >= 1.0_dp) lower_bound = max(0.0_dp, 1.0_dp - scaled)
    if (uniform < lower_bound) return
    if (uniform >= 1.0_dp/(1.0_dp + scaled)) then
      accepted = .false.
      return
    end if

    log_ratio = log1p_small(-distance)
    if (magnitude >= 1.0_dp) then
      accepted = log(uniform)/magnitude < log_ratio
    else
      accepted = log(uniform) < magnitude*log_ratio
    end if
  end function powerlaw_accept
end module rndgen_sampling_mod
