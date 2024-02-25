program example
   use rndgen_mod
   implicit none

   integer, parameter :: dp = selected_real_kind(15) ! 8-byte reals
   integer, parameter :: i16 = selected_int_kind(16) ! 8-byte integer

   integer :: i
   integer :: seed = 294727492

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
      write (*, *) generator%int(5_i16, 2587_i16)
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
    write (*, *) generator%int(5_i16, 2587_i16)
   end do

   write (*, *) "10 random real between -5.2 and 100.9:"
   do i = 1, 10
      write (*, *) generator%real(-5.2_dp, 100.9_dp)
   end do

end program
