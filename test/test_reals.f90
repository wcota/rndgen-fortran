program test_reals
    use iso_fortran_env, only: i4 => int32, sp => real32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Real and Boolean functionalities with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_real_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Real and Boolean functionalities with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_real_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Integer functionalities with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_real_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Integer functionalities with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_real_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Generic subroutine to test custom real ranges and boolean entropy
    subroutine run_real_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        integer(i4) :: i
        real(dp) :: v_dp
        real(sp) :: v_sp
        logical :: got_true, got_false

        print *, "=================================================="
        print *, "--- Testing Real Distributions [r1, r2) and Boolean : ", gen_name
        print *, "=================================================="

        ! Initialize with a default seed
        call gen%init(2002_i4)

        ! Reals (Double Precision) [10.0, 20.0)
        do i = 1, 10000
            v_dp = gen%real(10.0_dp, 20.0_dp)
            check = (v_dp .isAtLeast. 10.0_dp) .also. (v_dp .lessThan. 20.0_dp)
            if (.not. check%test_passed()) error stop "Fail: real_dp out of bounds."
        end do
        print *, "[OK] real_dp strictly respects custom bounds."

        ! Reals (Single Precision) [-5.0, 5.0)
        do i = 1, 10000
            v_sp = gen%real(-5.0_sp, 5.0_sp)
            check = (v_sp .isAtLeast. -5.0_sp) .also. (v_sp .lessThan. 5.0_sp)
            if (.not. check%test_passed()) error stop "Fail: real_sp out of bounds."
        end do
        print *, "[OK] real_sp strictly respects custom bounds."

        ! Boolean (Ensures the generator is not biased to a single state)
        got_true = .false.
        got_false = .false.
        do i = 1, 1000
            if (gen%bool()) then
                got_true = .true.
            else
                got_false = .true.
            end if
        end do
        check = (got_true .and. got_false) .equalsExpected. .true.
        if (.not. check%test_passed()) error stop "Fail: bool() is biased to a single state."
        print *, "[OK] bool() generates valid entropy (both states)."

    end subroutine run_real_tests

end program test_reals