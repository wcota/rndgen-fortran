program test_statistics
    use iso_fortran_env, only: i4 => int32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Statistical Properties with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_statistics_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Statistical Properties with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_statistics_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Statistical Properties with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_statistics_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Statistical Properties with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_statistics_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Generic subroutine to test statistical properties (mean and variance)
    subroutine run_statistics_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        integer(i4), parameter :: N = 1000000
        real(dp), allocatable :: arr(:)
        real(dp) :: mean_val, variance_val

        ! Theoretical values for a Uniform(0,1) distribution
        real(dp), parameter :: EXPECTED_MEAN = 0.5_dp
        real(dp), parameter :: EXPECTED_VARIANCE = 1.0_dp / 12.0_dp

        print *, "=================================================="
        print *, "--- Testing Statistical Properties : ", gen_name
        print *, "=================================================="

        ! Initialize with a default seed
        call gen%init(iseed = 9999_i4)
        allocate(arr(N))

        ! Fill the array with 1 million numbers
        call gen%fill_array(arr)

        ! 1. Calculate the Mean
        mean_val = sum(arr) / real(N, dp)

        ! 2. Calculate the Variance (squared deviation from the mean)
        variance_val = sum((arr - mean_val)**2) / real(N, dp)

        print *, "Calculated Mean:     ", mean_val
        print *, "Calculated Variance: ", variance_val

        ! Tolerances for 1 million samples
        ! We use .approximates. .within. because statistics naturally fluctuate
        check = mean_val .approximates. EXPECTED_MEAN .within. 0.001_dp
        if (.not. check%test_passed()) error stop "Fail: Mean out of tolerance. Biased distribution!"

        check = variance_val .approximates. EXPECTED_VARIANCE .within. 0.001_dp
        if (.not. check%test_passed()) error stop "Fail: Variance out of tolerance. Biased distribution!"

        print *, "[OK] The distribution passed the Mean and Variance tests!"

        deallocate(arr)
    end subroutine run_statistics_tests

end program test_statistics