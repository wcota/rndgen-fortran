program example
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod
    implicit none

    ! How to use:
    ! ./a.out seed1 seed2

    type(rndgen) :: generators(2)   ! rnd list
    integer(kind=i4) :: aux                  ! aux
    character(len=64) :: caux       ! aux
    integer(kind=i4) :: i
    
    if (iargc() /= 2) stop 'give two arguments: seed1 seed2'

    call getarg(1,caux)
    read(caux,*) aux
    call generators(1)%init(aux)
    
    call getarg(2,caux)
    read(caux,*) aux
    call generators(2)%init(aux)
    
    
    write(*,*) "Writing 10 numbers..."
    print*, "seed1:", generators(1)%o_iseed
    do i = 1,10
        print*, generators(1)%rnd()
    enddo
    
    write(*,*) ""
    print*, "seed2:", generators(2)%o_iseed
    do i = 1,10
        print*, generators(2)%rnd()
    enddo
    
    write(*,*) ""
    write(*,*) "Reseting each generator and using at the same time. Will give the same sequences:"
    call generators(1)%reset()
    call generators(2)%reset()
    do i = 1,10
        print*, generators(1)%rnd(), generators(2)%rnd()
    enddo
    
end program
