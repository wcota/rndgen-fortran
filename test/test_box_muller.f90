program test_box_muller
    use iso_fortran_env, only: i4 => int32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Box-Muller Normal Distribution with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_gaussian_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Box-Muller Normal Distribution with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_gaussian_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Box-Muller Normal Distribution with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_gaussian_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Box-Muller Normal Distribution with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_gaussian_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Local implementation of the Box-Muller transform for testing purposes
    function box_muller_transform(gen) result(res)
        class(rndgen_base_t), intent(inout) :: gen
        real(kind=dp) :: res
        real(kind=dp) :: u1, u2
        real(kind=dp), parameter :: pi = 3.14159265358979323846_dp

        ! Ensure u1 is strictly greater than 0 to avoid log(0)
        u1 = gen%rnd()
        do while (u1 == 0.0_dp)
            u1 = gen%rnd()
        end do

        u2 = gen%rnd()

        ! Box-Muller formula mapping Uniform(0,1) to Normal N(0,1)
        res = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * pi * u2)
    end function box_muller_transform

    !> Subroutine to test Gaussian mean (0.0) and variance (1.0)
    subroutine run_gaussian_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        integer(i4), parameter :: N_SAMPLES = 100000000
        real(dp), allocatable :: arr(:)
        real(dp) :: mean_val, variance_val
        integer(i4) :: i

        print *, "=================================================="
        print *, "--- Testing Gaussian N(0,1) Box-Muller : ", gen_name
        print *, "=================================================="

        call gen%init(iseed = 5555_i4)
        allocate(arr(N_SAMPLES))

        ! Generate 1 million Gaussian numbers using local Box-Muller
        do i = 1, N_SAMPLES
            arr(i) = box_muller_transform(gen)
        end do

        ! 1. Compute Mean (Expected: ~0.0)
        mean_val = sum(arr) / real(N_SAMPLES, dp)

        ! 2. Compute Variance (Expected: ~1.0)
        variance_val = sum((arr - mean_val)**2) / real(N_SAMPLES, dp)

        print *, "Calculated Gaussian Mean     : ", mean_val
        print *, "Expected Mean                :   0.000000"
        print *, "Calculated Gaussian Variance : ", variance_val
        print *, "Expected Variance            :   1.000000"

        ! Julienne checks with safe statistical tolerances for 1M samples
        check = mean_val .approximates. 0.0_dp .within. 0.005_dp
        if (.not. check%test_passed()) error stop "Fail: Gaussian mean out of bounds!"

        check = variance_val .approximates. 1.0_dp .within. 0.005_dp
        if (.not. check%test_passed()) error stop "Fail: Gaussian variance out of bounds!"

        print *, "[OK] Gaussian distribution successfully verified (Mean=0, Variance=1)!"

        deallocate(arr)
    end subroutine run_gaussian_tests

end program test_box_muller