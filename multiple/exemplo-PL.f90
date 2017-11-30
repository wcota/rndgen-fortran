program exemplo
use mod_rndgen
implicit none

    integer :: i, j, k
    integer :: semente = 294727492
    integer :: N = 1e7
    integer, allocatable :: pok(:)
    integer :: amostras = 4
    real*8 :: soma
    type(rndgenPL) :: geradorPL

    ! Inicializa o gerador PL já com a semente inicial (último argumento)
    call geradorPL%initPL(3,int(N**(1d0/2d0)),2.1d0,semente) ! k0, kc, gam, iseed
    
    ! Aloca matriz de histograma
    allocate(pok(geradorPL%rndPL_k0:geradorPL%rndPL_kc))
    
    do j = 1, amostras
        ! Reseta o gerador para cada amostra
        call geradorPL%reset()
        ! esquenta o gerador para gerar a mesma sequencia para cada amostra:
        do i=1, 100000*j
            soma = geradorPL%rnd() ! soma is just a dummy variable...
        enddo
    
        ! Agora gera as distribuicoes
        pok = 0
        do i=1,N
            k = geradorPL%rndPL()
            pok(k) = pok(k) + 1
        enddo
        
        soma = sum(pok)
        
        do i=geradorPL%rndPL_k0, geradorPL%rndPL_kc
            if (pok(i) > 0) write(j,*) i, 1d0*pok(i)/(1d0*soma)
        enddo
    enddo
    

end program