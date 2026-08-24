program test_benchmark_duel
    use iso_fortran_env, only: i4 => int32, i8 => int64, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng
    integer(i4), parameter :: N_ELEMENTS = 500000000 ! 500 million elements
    real(dp), allocatable :: buffer(:)

    allocate(buffer(N_ELEMENTS))

    print *, "=================================================="
    print *, "     PRNG HEAD-TO-HEAD PERFORMANCE DUEL (4 ENGINES)"
    print *, "     Array Size: 500,000,000 Doubles"
    print *, "=================================================="
    print *, ""

    ! ==========================================
    ! 1. KISS (32-bit legacy)
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_benchmark(rng, "KISS", buffer)
    deallocate(rng)
    print *, ""

    ! ==========================================
    ! 2. Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_benchmark(rng, "Xoshiro256**", buffer)
    deallocate(rng)
    print *, ""

    ! ==========================================
    ! 3. Fortran Intrinsic (random_number)
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_benchmark(rng, "Intrinsic", buffer)
    deallocate(rng)
    print *, ""

    ! ! ==========================================
    ! ! 4. ran2 (Numerical Recipes)
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_benchmark(rng, "ran2", buffer)
    ! deallocate(rng)
    ! print *, ""

    print *, "=================================================="
    print *, "SUMMARY:"
    print *, "- Xoshiro256**: State-of-the-art speed & bit purity."
    print *, "- KISS / ran2 : Fast classical 32-bit alternatives."
    print *, "- Intrinsic   : Dependent on compiler implementation."
    print *, "=================================================="

    deallocate(buffer)

contains

    !> Subroutine to measure execution time and throughput for any generator
    subroutine run_benchmark(gen, gen_name, buffer)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name
        real(dp), intent(out) :: buffer(:)

        integer(i8) :: t_start, t_end, t_rate
        real(dp) :: elapsed_time, speed_meps
        integer(i4) :: n_elements

        n_elements = size(buffer)
        call system_clock(count_rate=t_rate)

        ! Initialize seed
        call gen%init(iseed = 2026_i4)

        ! Benchmark bulk fill_array
        call system_clock(t_start)
        call gen%fill_array(buffer)
        call system_clock(t_end)

        elapsed_time = real(t_end - t_start, dp) / real(t_rate, dp)
        speed_meps = real(n_elements, dp) / (elapsed_time * 1.0_dp**6)

        print '(A, A15, A, F12.4, A)', '[', gen_name, '] Time elapsed : ', elapsed_time, ' seconds'
        print '(A, A15, A, F12.2, A)', '[', gen_name, '] Throughput   : ', speed_meps, ' M numbers/sec'

    end subroutine run_benchmark

end program test_benchmark_duel