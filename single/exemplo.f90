program exemplo
use mod_rndgen
implicit none

    integer :: i
    integer :: semente = 294727492

    ! Inicializa o gerador:
    call rnd_init(semente)
    
    ! Gera 10 números aleatórios U(0,1) a partir da semente:
    write(*,*) "Gera 10 números aleatórios U(0,1) a partir da semente:"
    do i = 1, 10
        write(*,*) rnd()
    enddo
    
    ! Gera 10 números inteiros entre 5 e 2587:
    write(*,*) "Gera 10 números inteiros entre 5 e 2587:"
    do i = 1, 10
        write(*,*) rnd_int(5,2587)
    enddo
    
    write(*,*) ""
    write(*,*) ""
    write(*,*) "Reseta o gerador e repete o feito anterior:"
    
    ! Reseta o gerador e repete o feito anterior:
    call rnd_reset()
    
    ! Gera 10 números aleatórios U(0,1) a partir da semente:
    write(*,*) "Gera 10 números aleatórios U(0,1) a partir da semente:"
    do i = 1, 10
        write(*,*) rnd()
    enddo
    
    ! Gera 10 números inteiros entre 5 e 2587:
    write(*,*) "Gera 10 números inteiros entre 5 e 2587:"
    do i = 1, 10
        write(*,*) rnd_int(5,2587)
    enddo

end program