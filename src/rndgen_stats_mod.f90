module rndgen_stats_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_t
    implicit none
    private

    type :: rndgen_stats_t
        class(rndgen_base_t), allocatable :: rng
    contains
        procedure, private :: init_i4 => rndgen_stats_t_init_i4
        procedure, private :: init_i8 => rndgen_stats_t_init_i8
        generic, public :: init => init_i4, init_i8
    end type

    public :: rndgen_stats_t

contains

    !> Initializes the random number generator engine with a 64-bit integer seed
    !> If you want to use another RNG, allocate it before calling this subroutine and set the rng pointer to it.
    !> Otherwise, the default RNG will be used.
    subroutine rndgen_stats_t_init_i8(this, iseed)
        class(rndgen_stats_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: iseed

        ! Allocate the requested generator polymorphically
        if (.not. allocated(this%rng)) then
            allocate(rndgen_t :: this%rng) ! uses the default generator type
        end if

        call this%rng%init(iseed)
    end subroutine

    !> Initializes the random number generator engine with a 32-bit integer seed
    !> If you want to use another RNG, allocate it before calling this subroutine and set the rng pointer to it.
    !> Otherwise, the default RNG will be used.
    subroutine rndgen_stats_t_init_i4(this, iseed)
        class(rndgen_stats_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed

        ! calls the 64-bit version of the init subroutine
        call rndgen_stats_t_init_i8(this, int(iseed, kind=i8))
    end subroutine

end module rndgen_stats_mod