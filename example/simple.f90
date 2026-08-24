program example
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only: rndgen_base_t, rndgen_kiss_t, rndgen_xoshiro256_t
    implicit none

    class(rndgen_base_t), allocatable :: generator
    integer(kind=i4) :: seed = 294727492

    allocate(rndgen_kiss_t :: generator)
    call run_tests(generator, "KISS", seed)
    deallocate(generator)

    write (*, *) ""
    write (*, *) ""

    allocate(rndgen_xoshiro256_t :: generator)
    call run_tests(generator, "Xoshiro256**", seed)
    deallocate(generator)


contains

    subroutine run_tests(gen, gen_name, seed_val)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name
        integer(kind=i4), intent(in) :: seed_val
        integer(kind=i4) :: i

        write (*, *) "=================================================="
        write (*, *) "  TESTING GENERATOR: ", gen_name
        write (*, *) "=================================================="

        ! Initialize it with the seed
        call gen%init(seed_val)

        write (*, *) "10 random U(0,1):"
        do i = 1, 10
            write (*, *) gen%rnd()
        end do

        write (*, *) "10 random integers between 5 and 2587:"
        do i = 1, 10
            write (*, *) gen%int(5, 2587)
        end do

        write (*, *) "10 random real between -5.2 and 100.9:"
        do i = 1, 10
            write (*, *) gen%real(-5.2_dp, 100.9_dp)
        end do

        write (*, *) ""
        write (*, *) ""
        write (*, *) "Reset the generator and repeat"
        call gen%reset()

        write (*, *) "10 random U(0,1):"
        do i = 1, 10
            write (*, *) gen%rnd()
        end do

        write (*, *) "10 random integers between 5 and 2587:"
        do i = 1, 10
            write (*, *) gen%int(5, 2587)
        end do

        write (*, *) "10 random real between -5.2 and 100.9:"
        do i = 1, 10
            write (*, *) gen%real(-5.2_dp, 100.9_dp)
        end do

    end subroutine run_tests

end program
