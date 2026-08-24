program test_core
    use iso_fortran_env, only: i4 => int32, i8 => int64, sp => real32, dp => real64
    use julienne_m
    use rndgen_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Core functionalities with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_core_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Core functionalities with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_core_tests(rng, "Xoshiro256**")
    deallocate(rng)

contains

    !> Generic subroutine to test core features (bounds, reset, and state IO)
    subroutine run_core_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        type(rndgen_state_t) :: state_backup
        integer(i4) :: i
        real(dp) :: r1, r2
        real(sp) :: r_sp

        print *, "=================================================="
        print *, "--- Testing Core and rnd() [0, 1) : ", gen_name
        print *, "=================================================="

        ! Initialize with a default seed
        call gen%init(iseed = 42_i4)

        ! 1. Bounds Test [0, 1) with thousands of samples
        do i = 1, 10000
            r1 = gen%rnd()
            check = (r1 .isAtLeast. 0.0_dp) .also. (r1 .lessThan. 1.0_dp)
            if (.not. check%test_passed()) error stop "Fail: rnd() dp out of bounds!"

            r_sp = gen%rnd_sp()
            check = (r_sp .isAtLeast. 0.0_sp) .also. (r_sp .lessThan. 1.0_sp)
            if (.not. check%test_passed()) error stop "Fail: rnd_sp() out of bounds!"
        end do
        print *, "[OK] rnd_dp and rnd_sp strictly respect [0, 1)."

        ! 2. Reproducibility Test (Reset)
        call gen%init(123_i4)
        r1 = gen%rnd()
        call gen%reset()
        r2 = gen%rnd()

        ! We use an approximation check because of floating-point arithmetic precision
        check = r1 .approximates. r2 .within. 1e-12_dp
        if (.not. check%test_passed()) error stop "Fail: reset() diverged."
        print *, "[OK] reset() successfully preserves the sequence."

        ! 3. State Checkpoint Test (Save/Load)
        state_backup = gen%get_state()
        r1 = gen%rnd()
        call gen%set_state(state_backup)
        r2 = gen%rnd()

        check = r1 .approximates. r2 .within. 1e-12_dp
        if (.not. check%test_passed()) error stop "Fail: get_state/set_state diverged."
        print *, "[OK] get_state/set_state successfully preserve the sequence."

    end subroutine run_core_tests

end program test_core