program test_autocorrelation
    use iso_fortran_env, only: i4 => int32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng
    integer(i4), parameter :: N_SAMPLES = 200000000 ! 200 million samples for heavy stress

    ! ==========================================
    ! 1. Test Autocorrelation with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_autocorrelation_test(rng, "KISS", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Autocorrelation with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_autocorrelation_test(rng, "Xoshiro256**", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Autocorrelation with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_autocorrelation_test(rng, "Fortran Intrinsic RNG", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Autocorrelation with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_autocorrelation_test(rng, "Numerical Recipes ran2 RNG", N_SAMPLES)
    ! deallocate(rng)

contains

    !> Subroutine to calculate Lag-1 Autocorrelation and check for sequence independence
    subroutine run_autocorrelation_test(gen, gen_name, n)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name
        integer(i4), intent(in) :: n

        type(test_diagnosis_t) :: check
        real(dp), allocatable :: arr(:)
        real(dp) :: mean_val, variance_val, autocorr
        real(dp) :: sum_cov
        integer(i4) :: i

        print *, "=================================================="
        print *, "--- Testing Lag-1 Autocorrelation : ", gen_name
        print *, "--- Sample size: ", n
        print *, "=================================================="

        ! Initialize generator
        call gen%init(iseed = 7777_i4)
        allocate(arr(n))

        ! Generate bulk numbers in [0, 1)
        call gen%fill_array(arr)

        ! 1. Compute Mean
        mean_val = sum(arr) / real(n, dp)

        ! 2. Compute Variance
        variance_val = sum((arr - mean_val)**2) / real(n, dp)

        ! 3. Compute Covariance between x(i) and x(i+1) (Lag-1)
        sum_cov = 0.0_dp
        do i = 1, n - 1
            sum_cov = sum_cov + (arr(i) - mean_val) * (arr(i+1) - mean_val)
        end do

        ! Normalization to find the autocorrelation coefficient rho_1
        autocorr = sum_cov / (real(n - 1, dp) * variance_val)

        print *, "Calculated Mean          : ", mean_val
        print *, "Calculated Variance      : ", variance_val
        print *, "Lag-1 Autocorrelation    : ", autocorr
        print *, "Expected Value (Ideal)   :   0.000000"

        ! Julienne check: Autocorrelation must be extremely close to 0.0 within a 0.002 tolerance
        check = autocorr .approximates. 0.0_dp .within. 0.002_dp
        if (.not. check%test_passed()) then
            print *, check%diagnostics_string()
            error stop "Fail: High autocorrelation detected! Generator has sequential bias."
        end if

        print *, "[OK] Sequence passes the independence test (no autocorrelation)!"

        deallocate(arr)
    end subroutine run_autocorrelation_test

end program test_autocorrelation