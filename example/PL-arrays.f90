program example
   use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
   use rndgenPL_mod
   implicit none

   integer(kind=i4) :: seed
   integer(kind=i4) :: N = 1e7
   character(len=*), parameter :: fmt = '(*(g0,x))'

   ! Declare the generator
   type(rndgenPL) :: generatorPL

   seed = 294727492

   ! Initialize it with the seed with x0=3, xc = sqrt(N), \gamma = 2.1
   call generatorPL%initPL(3, int(N**(1d0/2d0),kind=i4), 2.1_dp, seed) ! k0, kc, gam, iseed

   write (*, fmt) "10 random PL numbers:", generatorPL%rndPL_array(10) 

   write (*, fmt) ""
   write (*, fmt) "Reset the generator and repeat"
   call generatorPL%reset() 
   
   write (*, fmt) "10 random PL numbers:", generatorPL%rndPL_array(10) 

end program
