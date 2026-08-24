module rndgen_stats_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_xoshiro256_t
    implicit none
    private

    type :: rndgen_stats_t
        class(rndgen_base_t), allocatable :: rng
    contains
        procedure, public :: init => init_stats_engine
    end type

    public :: rndgen_stats_t

contains

    subroutine init_stats_engine(this, iseed)
        class(rndgen_stats_t), intent(inout) :: this
        integer(kind=i4), intent(in), optional :: iseed
        integer(kind=i8) :: seed_val

        if (.not. allocated(this%rng)) then
            allocate(rndgen_xoshiro256_t :: this%rng)
        end if

        seed_val = 1234_i8
        if (present(iseed)) seed_val = int(iseed, kind=i8)
        call this%rng%init(seed_val)
    end subroutine

end module rndgen_stats_mod