program example
   use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
   use rndgen_mod
   implicit none

   integer(kind=i4) :: i
   integer(kind=i4) :: seed = 294727492

   ! Declare the generator
   type(rndgen) :: generator

   ! Initialize it with the seed
   call generator%init(seed)

   write (*, *) "10 random U(0,1):"
   do i = 1, 10
      write (*, *) generator%rnd()
   end do

   write (*, *) "10 random integers between 5 and 2587:"
   do i = 1, 10
      write (*, *) generator%int(5, 2587)
   end do

   write (*, *) "10 random real between -5.2 and 100.9:"
   do i = 1, 10
      write (*, *) generator%real(-5.2_dp, 100.9_dp)
   end do

   write (*, *) ""
   write (*, *) ""
   write (*, *) "Reset the generator and repeat"
   call generator%reset()

   write (*, *) "10 random U(0,1):"
   do i = 1, 10
      write (*, *) generator%rnd()
   end do

   write (*, *) "10 random integers between 5 and 2587:"
   do i = 1, 10
    write (*, *) generator%int(5, 2587)
   end do

   write (*, *) "10 random real between -5.2 and 100.9:"
   do i = 1, 10
      write (*, *) generator%real(-5.2_dp, 100.9_dp)
   end do

end program
