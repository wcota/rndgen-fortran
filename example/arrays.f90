program example_arrays
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_kiss_t, rndgen_xoshiro256_t
    implicit none

    integer(kind=i4) :: seed = 294727492
    class(rndgen_base_t), allocatable :: generator

    ! ==========================================
    ! 1. Test array generation using KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: generator)
    call run_array_tests(generator, "KISS", seed)
    deallocate(generator)

    write (*, *) ""
    write (*, *) ""

    ! ==========================================
    ! 2. Test array generation using Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: generator)
    call run_array_tests(generator, "Xoshiro256**", seed)
    deallocate(generator)

contains

    !> Generic subroutine to test dynamic array allocation
    subroutine run_array_tests(gen, gen_name, seed_val)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name
        integer(kind=i4), intent(in) :: seed_val

        ! The format '(*(g0,x))' prints the text followed by the entire array on the same line
        character(len=*), parameter :: fmt = '(*(g0,x))'

        write (*, *) "=================================================="
        write (*, *) "  TESTING DYNAMIC ARRAYS: ", gen_name
        write (*, *) "=================================================="

        ! Initialize the generator with the provided seed
        call gen%init(seed_val)

        ! Note: We add the _i4 suffix to the size '10' and to the limits
        ! to ensure the compiler resolves the generic interface correctly
        write (*, fmt) "10 random U(0,1):", gen%rnd_array(10_i4)
        write (*, fmt) "10 random integers between 5 and 2587:", gen%rnd_array(10_i4, 5_i4, 2587_i4)
        write (*, fmt) "10 random real between -5.2 and 100.9:", gen%rnd_array(10_i4, -5.2_dp, 100.9_dp)
        write (*, fmt) "10 random boolean values:", gen%bool_array(10_i4)

        write (*, fmt) ""
        write (*, fmt) "--- Reset the generator and repeat ---"
        call gen%reset()

        write (*, fmt) "10 random U(0,1):", gen%rnd_array(10_i4)
        write (*, fmt) "10 random integers between 5 and 2587:", gen%rnd_array(10_i4, 5_i4, 2587_i4)
        write (*, fmt) "10 random real between -5.2 and 100.9:", gen%rnd_array(10_i4, -5.2_dp, 100.9_dp)
        write (*, fmt) "10 random boolean values:", gen%bool_array(10_i4)

    end subroutine run_array_tests

end program example_arrays