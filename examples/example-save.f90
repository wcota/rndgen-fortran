include '../mod_rndgen.f90'

program example
use mod_rndgen
implicit none

    integer :: i, j
    integer :: seed = 294727492
    
    ! Declare the generator
    type(rndgen) :: generator
    
    ! Declare the object to IO operations of the seed
    type(rndSeed) :: saved_seed
    
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
        j = generator%rnd()
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