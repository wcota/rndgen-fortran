program example
   use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
   use rndgen_stats_mod
   use rndgen_stats_powerlaw
   implicit none

   integer(kind=i4) :: i, j, k
   integer(kind=i4) :: seed
   integer(kind=i4) :: N = 10000000 ! 1e7
   integer(kind=i4), allocatable :: pok(:)
   integer(kind=i4) :: samples = 4
   real(kind=dp) :: sumc

   ! Usa o tipo estendido de lei de potência
   type(rndgen_pl_t) :: generatorPL

   seed = 294727492

   ! 1. Inicializa o motor de bits central
   call generatorPL%init(iseed = seed, gen_type = "kiss")

   ! 2. Inicializa os parâmetros da lei de potência
   call generatorPL%init_powerlaw(3, int(N**(1.0_dp/2.0_dp), kind=i4), 2.1_dp) ! kmin, kmax, gamma

   ! Histograma (acessando os campos com prefixo pl_)
   allocate (pok(generatorPL%pl_kmin:generatorPL%pl_kmax))

   do j = 1, samples
      ! Reset do gerador subjacente para cada amostra
      call generatorPL%rng%reset()

      ! Avança o gerador para criar dessincronização entre as amostras
      do i = 1, 100000*j
         sumc = generatorPL%rng%rnd() ! sumc é uma variável dummy
      end do

      ! Gera N números aleatórios com lei de potência
      pok = 0
      do i = 1, N
         k = generatorPL%rndPL()
         pok(k) = pok(k) + 1
      end do

      sumc = real(sum(pok), kind=dp)

      do i = generatorPL%pl_kmin, generatorPL%pl_kmax
         if (pok(i) > 0) write (j, *) i, 1.0_dp * real(pok(i), kind=dp) / sumc
      end do
   end do

end program