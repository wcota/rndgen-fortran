program test_integers
    use iso_fortran_env, only: i4 => int32, i8 => int64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Integer functionalities with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_integer_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Integer functionalities with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_integer_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Integer functionalities with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_integer_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Integer functionalities with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_integer_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Generic subroutine to test integer distributions and overflow protection
    subroutine run_integer_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        integer(i4) :: i
        integer(i4) :: v_i4
        integer(i8) :: v_i8

        print *, "=================================================="
        print *, "--- Testing Integer Distributions [i1, i2] : ", gen_name
        print *, "=================================================="

        ! Initialize with a default seed
        call gen%init(1001_i4)

        ! Test i4: Standard Range
        do i = 1, 10000
            v_i4 = gen%int(1_i4, 10_i4)
            check = (v_i4 .isAtLeast. 1_i4) .also. (v_i4 .isAtMost. 10_i4)
            if (.not. check%test_passed()) error stop "Fail: int_i4 exceeded positive bounds."
        end do

        ! Test i4: Negative and mixed range
        do i = 1, 10000
            v_i4 = gen%int(-50_i4, 50_i4)
            check = (v_i4 .isAtLeast. -50_i4) .also. (v_i4 .isAtMost. 50_i4)
            if (.not. check%test_passed()) error stop "Fail: int_i4 exceeded negative/mixed bounds."
        end do
        print *, "[OK] int_i4 perfectly respects bounds."

        ! Test i8: Very large range (64 bits) to test overflow protection
        do i = 1, 10000
            v_i8 = gen%int(-5000000000_i8, 5000000000_i8)
            check = (v_i8 .isAtLeast. -5000000000_i8) .also. (v_i8 .isAtMost. 5000000000_i8)
            if (.not. check%test_passed()) error stop "Fail: int_i8 failed long range protection."
        end do
        print *, "[OK] int_i8 perfectly respects bounds."

    end subroutine run_integer_tests

end program test_integers