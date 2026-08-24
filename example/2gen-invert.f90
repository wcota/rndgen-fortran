program example_invert_seeds
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only: rndgen_base_t, rndgen_kiss_t, rndgen_xoshiro256_t
    implicit none

    ! How to use:
    ! ./a.out seed1 seed2

    ! Declare an allocatable array of polymorphic generators
    class(rndgen_base_t), allocatable :: generators(:)
    integer(kind=i4) :: seed1, seed2
    character(len=64) :: caux

    ! Use modern Fortran standard for command line arguments
    if (command_argument_count() /= 2) stop 'Error: Please provide exactly two arguments (seed1 seed2)'

    ! Read first seed
    call get_command_argument(1, caux)
    read(caux, *) seed1

    ! Read second seed
    call get_command_argument(2, caux)
    read(caux, *) seed2

    ! ==========================================
    ! 1. Test seed inversion with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: generators(2))
    call run_invert_seeds_test(generators, "KISS", seed1, seed2)
    deallocate(generators)

    write(*,*) ""
    write(*,*) ""

    ! ==========================================
    ! 2. Test seed inversion with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: generators(2))
    call run_invert_seeds_test(generators, "Xoshiro256**", seed1, seed2)
    deallocate(generators)

contains

    !> Generic subroutine to test multiple streams and seed swapping
    subroutine run_invert_seeds_test(gens, gen_name, s1, s2)
        class(rndgen_base_t), intent(inout) :: gens(:)
        character(len=*), intent(in) :: gen_name
        integer(kind=i4), intent(in) :: s1, s2
        integer(kind=i4) :: i

        write(*,*) "=================================================="
        write(*,*) "  TESTING SEED INVERSION: ", gen_name
        write(*,*) "=================================================="

        ! Initialize each generator with its respective seed
        call gens(1)%init(s1)
        call gens(2)%init(s2)

        write(*,*) "Writing 10 numbers..."
        print*, "seed1:", s1
        do i = 1, 10
            print*, gens(1)%rnd()
        end do

        write(*,*) ""
        print*, "seed2:", s2
        do i = 1, 10
            print*, gens(2)%rnd()
        end do

        write(*,*) ""
        write(*,*) "Inverting the seeds and generating at the same time..."

        ! Re-initialize the generators swapping the original seeds
        call gens(1)%init(s2)
        call gens(2)%init(s1)

        ! Print the swapped seeds
        print*, "seed1 (now using s2):", s2
        print*, "seed2 (now using s1):", s1

        ! Print side-by-side to show the sequences have perfectly swapped
        do i = 1, 10
            print*, gens(1)%rnd(), gens(2)%rnd()
        end do

    end subroutine run_invert_seeds_test

end program example_invert_seeds