program example
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_kiss_t
    implicit none

    ! Adapted from <https://web.mst.edu/vojtat/class_5403/kiss05/rtest.f90>

    ! Declare the generator
    class(rndgen_base_t), allocatable :: generator

    integer :: seed = 1

    ! Allocate the generator
    allocate(rndgen_kiss_t :: generator)

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
