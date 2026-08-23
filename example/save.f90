program example
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_state_t, rndgen_kiss_t
    implicit none

    integer(kind=i4) :: i
    integer(kind=i4) :: seed
    real(kind=dp) :: x

    ! Declare the generator
    class(rndgen_base_t), allocatable :: generator

    ! Declare the object to IO operations of the seed
    type(rndgen_state_t) :: saved_state

    ! Allocate the generator as a KISS generator
    allocate(rndgen_kiss_t :: generator)

    seed = 294727492

    ! Initialize it with the seed
    call generator%init(seed)

    write(*,*) "10 random U(0,1):"
    do i = 1, 5
        write(*,*) generator%rnd()
    enddo
    write(*,*) "--- check this block ---"
    do i = 1,5
        write(*,*) generator%rnd()
    enddo
    write(*,*) "--- check this block ---"

    write(*,*) ""
    write(*,*) "Reset and save the state after the 5 first rnd"
    call generator%reset()
    do i = 1, 5
        x = generator%rnd()
    enddo
    saved_state = generator%get_state()
    call saved_state%save_state('example/example_save.state')

    write(*,*) ""
    write(*,*) "Now, use the generator normally, and then recover the saved state"
    do i = 1, 10
        write(*,*) generator%rnd()
    enddo

    write(*,*) ""
    write(*,*) "Now, read the seed from file."

    ! Two steps to read the seed from file and set the generator state
    call saved_state%read_state('example/example_save.state')
    call generator%set_state(saved_state)

    write(*,*) "--- check this block ---"
    do i = 1, 5
        write(*,*) generator%rnd()
    enddo
    write(*,*) "--- check this block ---"

end program