module rndgen_stats_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_xoshiro256_t, rndgen_kiss_t
    use rndgen_legacy_mod, only : rndgen_intrinsic_t, rndgen_ran2_t
    implicit none
    private

    type :: rndgen_stats_t
        class(rndgen_base_t), allocatable :: rng
    contains
        procedure, public :: init => init_stats_engine
    end type

    public :: rndgen_stats_t

contains

    subroutine init_stats_engine(this, iseed, gen_type)
        class(rndgen_stats_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed
        character(len=*), intent(in), optional :: gen_type

        character(len=32) :: selected_type

        ! Define default engine type
        selected_type = "xoshiro"
        if (present(gen_type)) selected_type = gen_type

        ! Allocate the requested generator polymorphically
        if (.not. allocated(this%rng)) then
            select case (trim(selected_type))
            case ("kiss")
                allocate(rndgen_kiss_t :: this%rng)
            case ("ran2")
                allocate(rndgen_ran2_t :: this%rng)
            case ("intrinsic")
                allocate(rndgen_intrinsic_t :: this%rng)
            case default
                allocate(rndgen_xoshiro256_t :: this%rng) ! default generator
            end select
        end if

        call this%rng%init(iseed)
    end subroutine

end module rndgen_stats_mod