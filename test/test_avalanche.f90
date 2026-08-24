program test_avalanche
    use iso_fortran_env, only: i4 => int32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng_a, rng_b
    type(test_diagnosis_t) :: check

    ! ==========================================
    ! 1. Test Avalanche Effect with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng_a)
    allocate(rndgen_kiss_t :: rng_b)
    call run_avalanche_test(rng_a, rng_b, "KISS")
    deallocate(rng_a, rng_b)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Avalanche Effect with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng_a)
    allocate(rndgen_xoshiro256_t :: rng_b)
    call run_avalanche_test(rng_a, rng_b, "Xoshiro256**")
    deallocate(rng_a, rng_b)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Avalanche Effect with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng_a)
    allocate(rndgen_intrinsic_t :: rng_b)
    call run_avalanche_test(rng_a, rng_b, "Fortran Intrinsic RNG")
    deallocate(rng_a, rng_b)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Avalanche Effect with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng_a)
    ! allocate(rndgen_ran2_t :: rng_b)
    ! call run_avalanche_test(rng_a, rng_b, "Numerical Recipes ran2 RNG")
    ! deallocate(rng_a, rng_b)

    ! write (*, *) ""

contains

    !> Subroutine to test the seed avalanche effect (1-bit seed difference test)
    subroutine run_avalanche_test(gen_a, gen_b, gen_name)
        class(rndgen_base_t), intent(inout) :: gen_a, gen_b
        character(len=*), intent(in) :: gen_name

        integer(i4), parameter :: N_SAMPLES = 10000000 ! 10 million samples for avalanche test
        real(dp), allocatable :: arr_a(:), arr_b(:)
        real(dp) :: mean_diff, max_diff
        integer(i4) :: i, different_count
        type(test_diagnosis_t) :: check

        print *, "=================================================="
        print *, "--- Testing Seed Avalanche Effect : ", gen_name
        print *, "=================================================="

        ! Initialize Gen A with seed 12345
        call gen_a%init(iseed = 12345_i4)

        ! Initialize Gen B with seed 12346 (only 1 bit changed!)
        call gen_b%init(iseed = 12346_i4)

        allocate(arr_a(N_SAMPLES))
        allocate(arr_b(N_SAMPLES))

        ! Generate sequences
        call gen_a%fill_array(arr_a)
        call gen_b%fill_array(arr_b)

        ! 1. Check that sequences are completely distinct right from the start
        different_count = 0
        max_diff = 0.0_dp
        do i = 1, N_SAMPLES
            if (arr_a(i) /= arr_b(i)) then
                different_count = different_count + 1
            end if
            max_diff = max(max_diff, abs(arr_a(i) - arr_b(i)))
        end do

        print *, "Total samples compared           : ", N_SAMPLES
        print *, "Sequences elements differing     : ", different_count
        print *, "Max absolute difference found    : ", max_diff

        ! Julienne check: 100% of the elements must be different (divergence rate = 1.0)
        check = (different_count .equalsExpected. N_SAMPLES)
        if (.not. check%test_passed()) then
            error stop "Fail: Seed change did not propagate! Sequences are overlapping."
        end if

        print *, "[OK] Perfect avalanche effect! Changing 1 bit in seed completely alters the output sequence."

        deallocate(arr_a, arr_b)
    end subroutine run_avalanche_test

end program test_avalanche