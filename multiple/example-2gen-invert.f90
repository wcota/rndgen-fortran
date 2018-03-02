program teste
use mod_rndgen
implicit none

    ! How to use:
    ! ./a.out seed1 seed2

    type(rndgen) :: geradores(2)    ! rnd list
    integer :: aux                  ! aux
    character*64 :: caux            ! aux
    integer :: i

    call getarg(1,caux)
    read(caux,*) aux
    call geradores(1)%init(aux) ! use the first seed
    
    call getarg(2,caux)
    read(caux,*) aux
    call geradores(2)%init(aux) ! use the second seed
    
    
    write(*,*) "Writing 10 numbers..."
    print*, "seed1:", geradores(1)%o_iseed
    do i = 1,10
        print*, geradores(1)%rnd()
    enddo
    
    write(*,*) ""
    print*, "seed2:", geradores(2)%o_iseed
    do i = 1,10
        print*, geradores(2)%rnd()
    enddo
    
    write(*,*) ""
    write(*,*) "Inverting the seeds..."
    aux = geradores(1)%o_iseed
    call geradores(1)%init(geradores(2)%o_iseed)
    call geradores(2)%init(aux)
    print*, "seed1:", geradores(1)%o_iseed
    print*, "seed2:", geradores(2)%o_iseed
    do i = 1,10
        print*, geradores(1)%rnd(), geradores(2)%rnd()
    enddo
    
end program
