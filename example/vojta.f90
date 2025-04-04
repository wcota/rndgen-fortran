program example
   use rndgen_kinds_mod
   use rndgen_mod
   implicit none

   ! Adapted from <https://web.mst.edu/vojtat/class_5403/kiss05/rtest.f90>

   ! Declare the generator
   type(rndgen) :: generator

   integer :: seed = 1

   ! Initialize it with the seed
   call generator%init(seed)

   write (*, *) 'Now write(ing the first 10 random numbers'
   write (*, *) 'and the expected values (in brackets).'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.100223257)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.654553312)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.533925296)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.129070464)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.846589457)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.938662817)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.388126970)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.415468296)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.321098742)'
   write (*, '(F12.9,A)') generator%rnd(), '  (0.572245760)'

end program
