program example
   use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
   use rndgen_stats_mod
   use rndgen_stats_powerlaw
   implicit none

   integer(kind=i4) :: seed
   integer(kind=i4) :: N = 10000000 ! 1e7
   character(len=*), parameter :: fmt = '(*(g0,x))'

   ! Declare the generator using the new extended type
   type(rndgen_pl_t) :: generatorPL

   seed = 294727492

   ! Initialize the underlying engine and the power-law parameters
   call generatorPL%init(iseed = seed)
   call generatorPL%init_powerlaw(3, int(N**(1.0_dp/2.0_dp), kind=i4), 2.1_dp) ! kmin, kmax, gamma

   write (*, fmt) "10 random PL numbers:", generatorPL%rndPL_array(10)

   write (*, fmt) ""
   write (*, fmt) "Reset the generator and repeat"

   ! Reset the underlying bit generator to restore exact sequence
   call generatorPL%rng%reset()

   write (*, fmt) "10 random PL numbers:", generatorPL%rndPL_array(10)

end program