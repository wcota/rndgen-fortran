program test_arrays
    use iso_fortran_env, only: i4 => int32, dp => real64
    use julienne_m
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Block Operations with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_array_block_tests(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Block Operations with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_array_block_tests(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Test Block Operations with Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_array_block_tests(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Test Block Operations with Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_array_block_tests(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    !> Generic subroutine to test block operations (Arrays)
    subroutine run_array_block_tests(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        type(test_diagnosis_t) :: check
        real(dp), allocatable :: arr_dp(:)
        integer(i4), allocatable :: arr_i4(:)
        integer(i4), parameter :: n_size = 5000

        print *, "=================================================="
        print *, "--- Testing Block Operations (Arrays) : ", gen_name
        print *, "=================================================="

        ! Initialize with a default seed
        call gen%init(3003_i4)

        ! 1. Dynamic allocation test (rnd_array)
        arr_dp = gen%rnd_array(n_size)

        ! Array size is integer, so '.equalsExpected.' is safe.
        check = (size(arr_dp) .equalsExpected. n_size) .also. &
                (.all. (arr_dp .isAtLeast. 0.0_dp) .also. .all. (arr_dp .lessThan. 1.0_dp))

        if (.not. check%test_passed()) error stop "Fail: rnd_array failed allocation or bounds."
        print *, "[OK] Dynamic rnd_array correct."
        deallocate(arr_dp)

        ! 2. Buffer fill test (fill_array)
        allocate(arr_i4(n_size))
        call gen%fill_array(arr_i4, -100_i4, 100_i4)

        check = .all. (arr_i4 .isAtLeast. -100_i4) .also. .all. (arr_i4 .isAtMost. 100_i4)
        if (.not. check%test_passed()) error stop "Fail: fill_array(i4) failed mass bounds."
        print *, "[OK] fill_array (block integers) correct."
        deallocate(arr_i4)

    end subroutine run_array_block_tests

end program test_arrays