program exemplo
use mod_rndgen
implicit none

    integer :: i, j, k
    integer :: semente = 294727492
    integer :: N = 1e7
    integer, allocatable :: pok(:)
    integer :: amostras = 4
    real*8 :: soma
    
    ! Inicializa o gerador PL:
    call rnd_PLinit(3,int(N**(1d0/2d0)),2.1d0,semente) ! k0, kc, gama, iseed
    
    allocate(pok(rndPL_k0:rndPL_kc))
    
    do j = 1, amostras
        ! Reseta o gerador para cada amostra
        call rnd_reset()
        ! esquenta o gerador para gerar a mesma sequencia para cada amostra:
        do i=1, 100000*j
            soma = rnd()
        enddo
    
        ! Agora gera as distribuicoes
        pok = 0
        do i=1,N
            k = rnd_PL()
            pok(k) = pok(k) + 1
        enddo
        
        soma = sum(pok)
        
        do i=rndPL_k0, rndPL_kc
            if (pok(i) > 0) write(j,*) i, 1d0*pok(i)/(1d0*soma)
        enddo
    enddo
    

end program