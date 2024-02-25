program example
implicit none

    integer :: i
    integer :: seed = 294727492
    
    ! Declare the generator
    type(rndgen) :: generator

    ! Initialize it with the seed
    call generator%init(seed)
    
    write(*,*) "10 random U(0,1):"
    do i = 1, 10
        write(*,*) generator%rnd()
    enddo
    
    write(*,*) "10 random integers between 5 and 2587:"
    do i = 1, 10
        write(*,*) generator%int(5,2587)
    enddo
    
    write(*,*) ""
    write(*,*) ""
    write(*,*) "Reset the generator and repeat"
    call generator%reset()
    
    write(*,*) "10 random U(0,1):"
    do i = 1, 10
        write(*,*) generator%rnd()
    enddo
    
    write(*,*) "10 random integers between 5 and 2587:"
    do i = 1, 10
        write(*,*) generator%int(5,2587)
    enddo

end program   implicit none
