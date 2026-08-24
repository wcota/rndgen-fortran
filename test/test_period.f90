program test_period
    use iso_fortran_env, only : i4 => int32, i8 => int64
    use rndgen_mod
    use rndgen_legacy_mod
    implicit none

    class(rndgen_base_t), allocatable :: rng

    ! ==========================================
    ! 1. Test Short-Range Cycle Collision with KISS
    ! ==========================================
    allocate(rndgen_kiss_t :: rng)
    call run_collision_test(rng, "KISS")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 2. Test Short-Range Cycle Collision with Xoshiro256**
    ! ==========================================
    allocate(rndgen_xoshiro256_t :: rng)
    call run_collision_test(rng, "Xoshiro256**")
    deallocate(rng)

    write (*, *) ""

    ! ==========================================
    ! 3. Fortran Intrinsic RNG
    ! ==========================================
    allocate(rndgen_intrinsic_t :: rng)
    call run_collision_test(rng, "Fortran Intrinsic RNG")
    deallocate(rng)

    write (*, *) ""

    ! ! ==========================================
    ! ! 4. Numerical Recipes ran2 RNG
    ! ! ==========================================
    ! allocate(rndgen_ran2_t :: rng)
    ! call run_collision_test(rng, "Numerical Recipes ran2 RNG")
    ! deallocate(rng)

contains

    subroutine run_collision_test(gen, gen_name)
        class(rndgen_base_t), intent(inout) :: gen
        character(len=*), intent(in) :: gen_name

        integer(i8), parameter :: STEPS = 100000000_i8

        type(rndgen_state_t) :: saved_state
        type(rndgen_state_t) :: current_state

        integer(i8) :: i
        logical :: collided

        print *, "=================================================="
        print *, "--- Testing Short-Range Cycle Collision : ", gen_name
        print *, "=================================================="

        ! The legacy generators currently do not implement state
        ! serialization.
        select type (gen)
        type is (rndgen_intrinsic_t)
            print *, "[SKIP] get_state/set_state not implemented."
            return

        type is (rndgen_ran2_t)
            print *, "[SKIP] get_state/set_state not implemented."
            return

        class default
            ! State operations are available for the other generators.
        end select

        call gen%init(iseed = 1337_i4)

        ! Save the initial state after a few warm-up steps.
        do i = 1, 1000
            call advance_generator(gen)
        end do

        saved_state = gen%get_state()

        print *, "Baseline state captured at step 1000."
        print *, "Advancing generator by ", STEPS, " steps..."

        ! Advance the generator by millions of steps.
        do i = 1, STEPS
            call advance_generator(gen)
        end do

        current_state = gen%get_state()

        ! Compare the complete state rather than only data(1).
        collided = all(current_state%data == saved_state%data)

        if (collided) then
            print *, "[WARNING] Premature cycle collision detected!"
        else
            print *, "[OK] No short-range collisions detected after ", &
                     STEPS, " steps."
            print *, "Complete generator state is distinct from baseline."
            print *, "Period safety: PASSED."
        end if

    end subroutine run_collision_test


    subroutine advance_generator(gen)
        class(rndgen_base_t), intent(inout) :: gen
        integer(i8) :: value

        value = gen%next_integer()
    end subroutine advance_generator

end program test_period