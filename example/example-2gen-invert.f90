program example
use rndgen_mod
implicit none

    ! How to use:
    ! ./a.out seed1 seed2

    type(rndgen) :: generators(2)    ! rnd list
    integer :: aux                  ! aux
    character*64 :: caux            ! aux
    integer :: i
    
    if (iargc() /= 2) stop 'give two arguments: seed1 seed2'

    call getarg(1,caux)
    read(caux,*) aux
    call generators(1)%init(aux) ! use the first seed
    
    call getarg(2,caux)
    read(caux,*) aux
    call generators(2)%init(aux) ! use the second seed
    
    
    write(*,*) "Writing 10 numbers..."
    print*, "seed1:", generators(1)%o_iseed
    do i = 1,10
        print*, generators(1)%rnd()
    enddo
    
    write(*,*) ""
    print*, "seed2:", generators(2)%o_iseed
    do i = 1,10
        print*, generators(2)%rnd()
    enddo
    
    write(*,*) ""
    write(*,*) "Inverting the seeds and generating at the same time..."
    aux = generators(1)%o_iseed
    call generators(1)%init(generators(2)%o_iseed)
    call generators(2)%init(aux)
    print*, "seed1:", generators(1)%o_iseed
    print*, "seed2:", generators(2)%o_iseed
    do i = 1,10
        print*, generators(1)%rnd(), generators(2)%rnd()
    enddo
    
end program
