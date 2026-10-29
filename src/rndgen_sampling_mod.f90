! Numerical transformations shared by the scalar and array interfaces.
module rndgen_sampling_mod
  use iso_fortran_env, only: i4 => int32, i8 => int64
  implicit none
  private

  integer(i8), parameter :: mask32 = int(z'FFFFFFFF', i8)

  public :: integer_span, integer_add

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

end module rndgen_sampling_mod
