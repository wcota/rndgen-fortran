program example
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod
    implicit none

    integer(kind=i4) :: i
    integer(kind=i4) :: seed
    real(kind=dp) :: x
    
    ! Declare the generator
    type(rndgen) :: generator
    
    ! Declare the object to IO operations of the seed
    type(rndSeed) :: saved_seed

    seed = 294727492
    
    open(1, file='example_save.seed')

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
    call generator%save_seed(saved_seed, 1)
    
    write(*,*) ""
    write(*,*) "Now, use the generator normally, and then recover the saved state"
    do i = 1, 10
        write(*,*) generator%rnd()
    enddo
    
    
    write(*,*) ""
    write(*,*) "Now, read the seed from file."
    rewind(1) ! re-open the file
    call generator%read_seed(saved_seed, 1)
    write(*,*) "--- check this block ---"
    do i = 1, 5
        write(*,*) generator%rnd()
    enddo
    write(*,*) "--- check this block ---"
    
    close(1)


end program