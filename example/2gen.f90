program example_multiple_streams
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only: rndgen_base_t, rndgen_kiss_t, rndgen_xoshiro256_t
    implicit none

    ! How to use:
    ! ./a.out seed1 seed2

    class(rndgen_base_t), allocatable :: generators(:)
    integer(kind=i4) :: seed1, seed2
    character(len=64) :: caux

    if (command_argument_count() /= 2) stop 'give two arguments: seed1 seed2'

    call get_command_argument(1, caux)
    read(caux, *) seed1

    call get_command_argument(2, caux)
    read(caux, *) seed2

    ! Test the two streams with KISS and Xoshiro256**
    allocate(rndgen_kiss_t :: generators(2))
    call run_two_streams(generators, "KISS", seed1, seed2)
    deallocate(generators)

    write (*, *) ""
    write (*, *) ""

    allocate(rndgen_xoshiro256_t :: generators(2))
    call run_two_streams(generators, "Xoshiro256**", seed1, seed2)
    deallocate(generators)

contains

    !> Subrotina genérica que aceita um array de geradores polimórficos
    subroutine run_two_streams(gens, gen_name, s1, s2)
        class(rndgen_base_t), intent(inout) :: gens(:)
        character(len=*), intent(in) :: gen_name
        integer(kind=i4), intent(in) :: s1, s2
        integer(kind=i4) :: i

        write(*,*) "=================================================="
        write(*,*) "  TESTING TWO STREAMS: ", gen_name
        write(*,*) "=================================================="

        call gens(1)%init(s1)
        call gens(2)%init(s2)

        write(*,*) "Writing 10 numbers..."
        ! Usamos 's1' diretamente pois oseed é privado/específico da classe filha
        print*, "seed1:", s1
        do i = 1,10
            print*, gens(1)%rnd()
        enddo

        write(*,*) ""
        print*, "seed2:", s2
        do i = 1,10
            print*, gens(2)%rnd()
        enddo

        write(*,*) ""
        write(*,*) "Reseting each generator and using at the same time. Will give the same sequences:"
        call gens(1)%reset()
        call gens(2)%reset()
        do i = 1,10
            print*, gens(1)%rnd(), gens(2)%rnd()
        enddo
    end subroutine run_two_streams

end program example_multiple_streams