module rndgen_stats_powerlaw
    use iso_fortran_env, only : i4 => int32, dp => real64
    use rndgen_stats_mod, only : rndgen_stats_t
  use rndgen_sampling_mod, only: powerlaw_parameters
    implicit none
    private

    type, extends(rndgen_stats_t) :: rndgen_pl_t
    type(powerlaw_parameters), private :: parameters
        real(kind=dp) :: pl_gamma
        integer(kind=i4) :: pl_kmin, pl_kmax
    contains
        procedure, public :: init_powerlaw
        procedure, public :: rndPL => rndgen_pl_scalar
        procedure, public :: rndPL_array => rndgen_pl_array
    end type

    public :: rndgen_pl_t

contains

    subroutine init_powerlaw(this, kmin, kmax, gama)
        class(rndgen_pl_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: kmin, kmax
        real(kind=dp), intent(in) :: gama

    call this%parameters%init(kmin, kmax, gama)

        this%pl_kmin = kmin
        this%pl_kmax = kmax
        this%pl_gamma = gama

    end subroutine init_powerlaw

    function rndgen_pl_scalar(this) result(rnd_number)
        class(rndgen_pl_t), intent(inout) :: this
    real(kind=dp) :: x, uniform
        integer(kind=i4) :: j, rnd_number

    if (.not. allocated(this%rng)) then
      error stop 'Initialize the power-law RNG engine'
    end if
        do
      uniform = this%rng%rnd()
      call this%parameters%propose(uniform, x, j)
      uniform = this%rng%rnd()
      if (this%parameters%accept(x, j, uniform)) exit
        end do

        rnd_number = j
    end function rndgen_pl_scalar

    function rndgen_pl_array(this, n) result(arr)
        class(rndgen_pl_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        integer(kind=i4), allocatable :: arr(:)
        integer(kind=i4) :: i

        allocate(arr(n))
        do i = 1, n
            arr(i) = this%rndPL()
        end do
    end function rndgen_pl_array

end module rndgen_stats_powerlaw
