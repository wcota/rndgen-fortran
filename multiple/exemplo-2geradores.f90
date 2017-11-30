program teste
use mod_rndgen
implicit none

    ! Como usar:
    ! ./a.out semente1 semente2

    type(rndgen) :: geradores(2)    ! lista de geradores
    integer :: aux                  ! auxiliar de leitura
    character*64 :: caux            ! auxiliar de leitura
    integer :: i

    call getarg(1,caux)
    read(caux,*) aux
    call geradores(1)%init(aux)
    
    call getarg(2,caux)
    read(caux,*) aux
    print*, "Semente 2:", aux
    call geradores(2)%init(aux)
    
    
    write(*,*) "Escrevendo um de cada vez..."
    print*, "Semente 1:", geradores(1)%o_iseed
    do i = 1,10
        print*, geradores(1)%rnd()
    enddo
    
    write(*,*) ""
    print*, "Semente 2:", geradores(2)%o_iseed
    do i = 1,10
        print*, geradores(2)%rnd()
    enddo
    
    write(*,*) ""
    write(*,*) "Escrevendo ambos juntos após resetar. Deve dar a mesma sequência individualmente!"
    call geradores(1)%reset()
    call geradores(2)%reset()
    do i = 1,10
        print*, geradores(1)%rnd(), geradores(2)%rnd()
    enddo
    
end program
