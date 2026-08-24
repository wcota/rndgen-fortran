program test_advanced
    use iso_fortran_env, only: i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng
    integer(i4), parameter :: N_SAMPLES = 200000000 ! 200 million samples for heavy stress

    ! ==========================================
    ! 1. Run advanced tests on KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_advanced_metrics(rng, "KISS", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""
    write (*, *) ""

    ! ==========================================
    ! 2. Run advanced tests on Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_advanced_metrics(rng, "Xoshiro256**", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""
    write (*, *) ""

    ! ==========================================
    ! 3. Run advanced tests on Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_advanced_metrics(rng, "Fortran Intrinsic RNG", N_SAMPLES)
    deallocate(rng)

    write (*, *) ""
    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Run advanced tests on Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_advanced_metrics(rng, "Numerical Recipes ran2 RNG", N_SAMPLES)
    ! deallocate(rng)

contains

    subroutine run_advanced_metrics(gen, gen_name, n)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name
        integer(i4), intent(in) :: n

        ! Variables for Bounds
        integer(i4) :: i
        real(dp) :: r, min_val, max_val

        ! Variables for Entropy
        integer(i4) :: bins(256), idx
        real(dp) :: prob, entropy

        ! Variables for Benchmarking
        integer(i8) :: t_start, t_end, t_rate
        real(dp) :: dummy_dp ! Prevents aggressive compiler dead-code elimination
        integer(i4) :: dummy_i4
        real(dp), allocatable :: bulk_arr(:)

        print *, "=================================================="
        print *, "  ADVANCED METRICS & BENCHMARK : ", gen_name
        print *, "  Samples per test: ", n
        print *, "=================================================="

        call gen%init(42_i4)
        call system_clock(count_rate=t_rate)

        ! ---------------------------------------------------------
        ! PART 1: STRESS TEST BOUNDS [0, 1)
        ! ---------------------------------------------------------
        min_val = 1.0_dp
        max_val = 0.0_dp

        do i = 1, n
            r = gen%rnd()
            if (r < min_val) min_val = r
            if (r > max_val) max_val = r

            ! Absolute failure trigger
            if (r >= 1.0_dp) error stop "FATAL: Generated a number >= 1.0!"
            if (r < 0.0_dp) error stop "FATAL: Generated a number < 0.0!"
        end do

        print *, "--- 1. Bounds Verification ---"
        print *, "Lowest value generated : ", min_val
        print *, "Highest value generated: ", max_val
        print *, "Strictly inside [0, 1) : PASSED"
        print *, ""

        ! ---------------------------------------------------------
        ! PART 2: SHANNON ENTROPY (Uniformity Quality)
        ! ---------------------------------------------------------
        bins = 0
        do i = 1, n
            idx = gen%int(1_i4, 256_i4)
            bins(idx) = bins(idx) + 1_i4
        end do

        entropy = 0.0_dp
        do i = 1, 256
            prob = real(bins(i), dp) / real(n, dp)
            if (prob > 0.0_dp) then
                ! Shannon entropy formula: -sum(p * log2(p))
                entropy = entropy - prob * (log(prob) / log(2.0_dp))
            end if
        end do

        print *, "--- 2. Shannon Entropy ---"
        print *, "Calculated Entropy     : ", entropy, " bits"
        print *, "Theoretical Maximum    :  8.000000 bits"
        if (entropy > 7.999_dp) then
            print *, "Entropy Quality        : EXCELLENT"
        else
            print *, "Entropy Quality        : POOR / BIASED"
        end if
        print *, ""

        ! ---------------------------------------------------------
        ! PART 3: CPU BENCHMARKING (Speed)
        ! ---------------------------------------------------------
        print *, "--- 3. Performance Benchmark (Seconds) ---"
        dummy_dp = 0.0_dp
        dummy_i4 = 0_i4

        ! Task A: Scalar rnd() [0, 1)
        call system_clock(t_start)
        do i = 1, n
            dummy_dp = dummy_dp + gen%rnd()
        end do
        call system_clock(t_end)
        print *, "Scalar U(0,1) dp       : ", real(t_end - t_start, dp) / real(t_rate, dp), " s"

        ! Task B: Scalar Custom Real range
        call system_clock(t_start)
        do i = 1, n
            dummy_dp = dummy_dp + gen%real(-10.0_dp, 10.0_dp)
        end do
        call system_clock(t_end)
        print *, "Scalar Custom Real dp  : ", real(t_end - t_start, dp) / real(t_rate, dp), " s"

        ! Task C: Scalar Integer range
        call system_clock(t_start)
        do i = 1, n
            dummy_i4 = dummy_i4 + gen%int(1_i4, 1000_i4)
        end do
        call system_clock(t_end)
        print *, "Scalar Custom Int i4   : ", real(t_end - t_start, dp) / real(t_rate, dp), " s"

        ! Task D: Bulk Array Generation (fill_array)
        allocate(bulk_arr(n))
        call system_clock(t_start)
        call gen%fill_array(bulk_arr) ! Fill 20 million numbers in one call
        call system_clock(t_end)
        print *, "Bulk fill_array (dp)   : ", real(t_end - t_start, dp) / real(t_rate, dp), " s"

        ! Prevent compiler from optimizing away the loops
        if (dummy_dp == -999.0_dp .and. dummy_i4 == -999) print *, "Invisible state"
        deallocate(bulk_arr)

    end subroutine run_advanced_metrics

end program test_advanced