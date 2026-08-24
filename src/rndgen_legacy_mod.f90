module rndgen_legacy_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_state_t
    implicit none
    private

    !> Fortran intrinsic random number generator wrapper
    type, extends(rndgen_base_t) :: rndgen_intrinsic_t
        private
        integer(kind=i4), public :: oseed
        integer(kind=i4), allocatable :: seed_array(:)
    contains
        procedure, public :: init_i4 => rndgen_intrinsic_init_i4
        procedure, public :: init_i8 => rndgen_intrinsic_init_i8
        procedure, public :: reset => rndgen_intrinsic_reset
        procedure, public :: next_integer => rndgen_intrinsic_next_integer
        procedure, public :: get_state => rndgen_intrinsic_get_state
        procedure, public :: set_state => rndgen_intrinsic_set_state
        procedure, public :: rnd_dp => rndgen_intrinsic_rnd_dp
    end type

    !> Numerical Recipes ran2 random number generator wrapper
    type, extends(rndgen_base_t) :: rndgen_ran2_t
        private
        integer(kind=i8), public :: oseed
        integer(kind=i4) :: idum
        integer(kind=i4) :: idum2
        integer(kind=i4) :: iy
        integer(kind=i4) :: iv(32)
        logical :: initialized = .false.
    contains
        procedure, public :: init_i4 => rndgen_ran2_init_i4
        procedure, public :: init_i8 => rndgen_ran2_init_i8
        procedure, public :: reset => rndgen_ran2_reset
        procedure, public :: next_integer => rndgen_ran2_next_integer
        procedure, public :: get_state => rndgen_ran2_get_state
        procedure, public :: set_state => rndgen_ran2_set_state
        procedure, public :: rnd_dp => rndgen_ran2_rnd_dp
    end type

    public :: rndgen_intrinsic_t, rndgen_ran2_t

contains

    !=========================================================================
    ! Fortran Intrinsic RNG
    !=========================================================================

    subroutine rndgen_intrinsic_init_i4(this, iseed)
        class(rndgen_intrinsic_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed
        integer :: n

        this%oseed = iseed

        call random_seed(size=n)

        if (allocated(this%seed_array)) then
            if (size(this%seed_array) /= n) then
                deallocate(this%seed_array)
            end if
        end if

        if (.not. allocated(this%seed_array)) then
            allocate(this%seed_array(n))
        end if

        this%seed_array = abs(iseed)

        if (this%seed_array(1) == 0_i4) then
            this%seed_array(1) = 1_i4
        end if

        ! Fill the remaining elements deterministically.
        if (n > 1) then
            this%seed_array(2:) = this%seed_array(1)
        end if

        call random_seed(put=this%seed_array)
    end subroutine rndgen_intrinsic_init_i4


    subroutine rndgen_intrinsic_init_i8(this, iseed)
        class(rndgen_intrinsic_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: iseed

        call this%init_i4(int(iseed, kind=i4))
    end subroutine rndgen_intrinsic_init_i8


    subroutine rndgen_intrinsic_reset(this)
        class(rndgen_intrinsic_t), intent(inout) :: this

        call this%init_i4(this%oseed)
    end subroutine rndgen_intrinsic_reset


    function rndgen_intrinsic_next_integer(this) result(val)
        class(rndgen_intrinsic_t), intent(inout) :: this
        integer(kind=i8) :: val
        real(kind=dp) :: r

        call random_number(r)

        val = int(r * real(huge(0_i8), kind=dp),kind=i8)
    end function rndgen_intrinsic_next_integer


    function rndgen_intrinsic_get_state(this) result(state)
        class(rndgen_intrinsic_t), intent(in) :: this
        type(rndgen_state_t) :: state

        error stop "rndgen_intrinsic_get_state: not implemented"
    end function rndgen_intrinsic_get_state


    subroutine rndgen_intrinsic_set_state(this, state)
        class(rndgen_intrinsic_t), intent(inout) :: this
        type(rndgen_state_t), intent(in) :: state

        error stop "rndgen_intrinsic_set_state: not implemented"
    end subroutine rndgen_intrinsic_set_state


    function rndgen_intrinsic_rnd_dp(this) result(rnd_number)
        class(rndgen_intrinsic_t), intent(inout) :: this
        real(kind=dp) :: rnd_number

        call random_number(rnd_number)
    end function rndgen_intrinsic_rnd_dp


    !=========================================================================
    ! Numerical Recipes ran2
    !=========================================================================

    subroutine rndgen_ran2_init_i4(this, iseed)
        class(rndgen_ran2_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed
        real(kind=dp) :: rdum

        error stop "rndgen_ran2_init_i4: not implemented due to licensing issues with Numerical Recipes code"
    end subroutine rndgen_ran2_init_i4


    subroutine rndgen_ran2_init_i8(this, iseed)
        class(rndgen_ran2_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: iseed

        call this%init_i4(int(iseed, kind=i4))
    end subroutine rndgen_ran2_init_i8


    subroutine rndgen_ran2_reset(this)
        class(rndgen_ran2_t), intent(inout) :: this

        call this%init_i8(this%oseed)
    end subroutine rndgen_ran2_reset


    function rndgen_ran2_next_integer(this) result(val)
        class(rndgen_ran2_t), intent(inout) :: this
        integer(kind=i8) :: val
        real(kind=dp) :: r

        r = this%rnd_dp()

        val = int(r * real(huge(0_i8), kind=dp), kind=i8)
    end function rndgen_ran2_next_integer


    function rndgen_ran2_get_state(this) result(state)
        class(rndgen_ran2_t), intent(in) :: this
        type(rndgen_state_t) :: state

        error stop "rndgen_ran2_get_state: not implemented"
    end function rndgen_ran2_get_state


    subroutine rndgen_ran2_set_state(this, state)
        class(rndgen_ran2_t), intent(inout) :: this
        type(rndgen_state_t), intent(in) :: state

        error stop "rndgen_ran2_set_state: not implemented"
    end subroutine rndgen_ran2_set_state


    !=========================================================================
    ! Numerical Recipes ran2
    !=========================================================================

    function rndgen_ran2_rnd_dp(this) result(ran2_val)
        class(rndgen_ran2_t), intent(inout) :: this
        real(kind=dp) :: ran2_val

        error stop "rndgen_ran2_rnd_dp: not implemented due to licensing issues with Numerical Recipes code"

    end function rndgen_ran2_rnd_dp

end module rndgen_legacy_mod