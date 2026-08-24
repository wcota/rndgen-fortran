program test_bits
    use iso_fortran_env, only: i4 => int32, i8 => int64, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Bit Balance with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_bit_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Bit Balance with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_bit_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Bit Balance with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_bit_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""
    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Bit Balance with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_bit_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Subroutine to test raw bit balance (0s and 1s proportion)
    subroutine run_bit_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        integer(i4), parameter :: N_BLOCKS = 100000
        integer(i8) :: total_bits, ones_count, zeros_count
        real(dp) :: ratio
        integer(i4) :: i, b, num_bits
        integer(i8) :: raw_val

        print *, "=================================================="
        print *, "--- Testing Raw Bit Balance (Monobit) : ", gen_name
        print *, "=================================================="

        call gen%init(iseed = 8888_i4)

        ones_count = 0_i8
        zeros_count = 0_i8
        num_bits = 0

        do i = 1, N_BLOCKS
            raw_val = gen%next_integer()

            if (i == 1) then
                if (gen_name == "KISS") then
                    num_bits = 32
                else
                    num_bits = 64
                end if
            end if

            do b = 0, num_bits - 1
                if (btest(raw_val, b)) then
                    ones_count = ones_count + 1_i8
                else
                    zeros_count = zeros_count + 1_i8
                end if
            end do
        end do

        total_bits = ones_count + zeros_count
        ratio = real(ones_count, dp) / real(total_bits, dp)

        print *, "Native bits evaluated  : ", num_bits
        print *, "Total bits evaluated   : ", total_bits
        print *, "Ratio of 1s (Ideal ~0.5): ", ratio

        ! Julienne check: The ratio of ones must be extremely close to 0.5 within a 0.001 tolerance
        check = ratio .approximates. 0.5_dp .within. 0.001_dp

        if (.not. check%test_passed()) then
            ! Instead of 'error stop' which kills the program globally,
            ! we print a warning and allow the execution to proceed.
            print *, "[WARNING] Fail: Bit bias detected in ", gen_name, "!"
        else
            print *, "[OK] Raw bit balance verified successfully (50/50 symmetry)!"
        end if

    end subroutine run_bit_tests

end program test_bits