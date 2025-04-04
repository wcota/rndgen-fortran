program example
    use rndgen_kinds_mod
    use rndgen_mod
    implicit none
 
    integer(kind=i4) :: seed = 294727492
    character(len=*), parameter :: fmt = '(*(g0,x))'
 
    ! Declare the generator
    type(rndgen) :: generator
 
    ! Initialize it with the seed
    call generator%init(seed)
 
    write (*, fmt) "10 random U(0,1):", generator%rnd_array(10) 
    write (*, fmt) "10 random integers between 5 and 2587:", generator%rnd_array(10, 5, 2587) 
    write (*, fmt) "10 random real between -5.2 and 100.9:", generator%rnd_array(10, -5.2_dp, 100.9_dp)
 
    write (*, fmt) ""
    write (*, fmt) "Reset the generator and repeat"
    call generator%reset()
 
    write (*, fmt) "10 random U(0,1):", generator%rnd_array(10) 
    write (*, fmt) "10 random integers between 5 and 2587:", generator%rnd_array(10, 5, 2587) 
    write (*, fmt) "10 random real between -5.2 and 100.9:", generator%rnd_array(10, -5.2_dp, 100.9_dp)
 
 end program
 