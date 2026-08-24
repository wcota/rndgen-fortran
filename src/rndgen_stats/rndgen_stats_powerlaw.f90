module rndgen_stats_powerlaw
    use iso_fortran_env, only : i4 => int32, dp => real64
    use rndgen_stats_mod, only : rndgen_stats_t
    implicit none
    private

    type, extends(rndgen_stats_t) :: rndgen_pl_t
        real(kind=dp), private :: pl_AA, pl_expo, pl_x0, pl_xc
        real(kind=dp) :: pl_gamma
        integer(kind=i4) :: pl_kmin, pl_kmax
        real(kind=dp), private, allocatable :: pl_prob(:)
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
        integer(kind=i4) :: j

        this%pl_kmin = kmin
        this%pl_kmax = kmax
        this%pl_gamma = gama

        if (allocated(this%pl_prob)) deallocate (this%pl_prob)
        allocate (this%pl_prob(kmin:kmax))

        this%pl_AA = 0.0_dp

        do j = kmin, kmax
            this%pl_AA = this%pl_AA + (1.0_dp * j)**(-gama)
            this%pl_prob(j) = (1.0_dp * j)**(-gama)
        end do

        this%pl_AA = 1.0_dp / this%pl_AA
        this%pl_prob = this%pl_AA * this%pl_prob

        this%pl_x0 = (1.0_dp * (kmin - 1))**(-gama + 1.0_dp)
        this%pl_xc = (1.0_dp * kmax)**(-gama + 1.0_dp)
        this%pl_expo = 1.0_dp / (1.0_dp - gama)
    end subroutine init_powerlaw

    function rndgen_pl_scalar(this) result(rnd_number)
        class(rndgen_pl_t), intent(inout) :: this
        real(kind=dp) :: z, x
        integer(kind=i4) :: j, rnd_number

        do
            j = this%pl_kmin - 1
            do while (j < this%pl_kmin)
                z = this%rng%rnd()
                x = (this%pl_x0 - z * (this%pl_x0 - this%pl_xc))**this%pl_expo
                j = ceiling(x)
            end do

            z = this%rng%rnd()

            if (.not. (z * this%pl_AA / (x**this%pl_gamma) >= this%pl_prob(j))) exit
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