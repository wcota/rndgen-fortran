include '../mod_rndgen.f90'

program example
use mod_rndgen
implicit none

    integer :: i, j, k
    integer :: seed = 294727492
    integer :: N = 1e7
    integer, allocatable :: pok(:)
    integer :: samples = 4
    real*8 :: sumc
    type(rndgenPL) :: generatorPL

    ! Initialize it with the seed with x0=3, xc = sqrt(N), \gamma = 2.1
    call generatorPL%initPL(3,int(N**(1d0/2d0)),2.1d0,seed) ! k0, kc, gam, iseed
    
    ! Histogram
    allocate(pok(generatorPL%rndPL_k0:generatorPL%rndPL_kc))
    
    do j = 1, samples
        ! Reset the generator for each sample
        call generatorPL%reset()
        ! generate a seed for each sample
        do i=1, 100000*j
            sumc = generatorPL%rnd() ! sumc is just a dummy variable...
        enddo
    
        ! Generate N power law random numbers
        pok = 0
        do i=1,N
            k = generatorPL%rndPL()
            pok(k) = pok(k) + 1
        enddo
        
        sumc = sum(pok)
        
        do i=generatorPL%rndPL_k0, generatorPL%rndPL_kc
            if (pok(i) > 0) write(j,*) i, 1d0*pok(i)/(1d0*sumc)
        enddo
    enddo
    
end program