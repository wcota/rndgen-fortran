! ## File: mod_random.f90
! ## - module: random number generator. This is just a module to be used in another program.
! ## See README.md for more information and use
!-----------------------------------------------------------------------------
! KISS random generator module, as unique: just one random generator
! IMPORTANT:
! THIS CODE WAS MODIFIED FROM http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90
! ! FORTRAN implementation by Thomas Vojta, vojta@mst.edu
! ! built on a module found at www.fortran.com
! 
! Copyright (C) 2017 Wesley Cota
! 
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------------
! Author    : Wesley Cota
! Email     : wesley.cota@ufv.br
! Date      : 29 Nov 2017
! Version   : 0.1
!-----------------------------------------------------------------------------

module mod_rndgen
implicit none

    public :: rnd, rnd_int, rnd_init, rnd_reset, rnd_o_iseed

    integer,parameter,private      :: r8b= SELECTED_REAL_KIND(P=14,R=99)   ! 8-byte reals
    integer,parameter,private      :: i4b= SELECTED_INT_KIND(8)            ! 4-byte integers 
    real(r8b),parameter,private    :: am=4.656612873077392578d-10       ! multiplier 1/2^31

    integer :: rnd_o_iseed                      ! original seed
    integer(i4b), private :: rnd_x,rnd_y,rnd_z,rnd_w        ! working variables for the four generators
    
    ! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    real*8, private :: rndPL_AA, rndPL_expo, rndPL_x0, rndPL_xc
    real*8, protected :: rndPL_gama
    integer, protected :: rndPL_k0, rndPL_kc
    real*8, private, allocatable :: rndPL_pk(:)
    ! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE

contains    

    function rnd()  ! KISS
        implicit none

        real(r8b)              :: rnd
        integer(i4b)           :: kiss

        rnd_x = 69069 * rnd_x + 1327217885
        rnd_y = ieor (rnd_y, ishft (rnd_y, 13)); rnd_y= ieor (rnd_y, ishft (rnd_y, -17)); 
        rnd_y= ieor (rnd_y, ishft (rnd_y, 5))
        rnd_z = 18000 * iand (rnd_z, 65535) + ishft (rnd_z, - 16)
        rnd_w = 30903 * iand (rnd_w, 65535) + ishft (rnd_w, - 16)
        kiss = ishft(rnd_x + rnd_y + ishft (rnd_z, 16) + rnd_w , -1)
        rnd=kiss*am
    end function
    
    subroutine rnd_reset()
        implicit none
        
        call rnd_init(rnd_o_iseed)
    end subroutine

    subroutine rnd_init(iseed)
        !use ifport
        implicit none

        integer(i4b)          :: idum,ia,im,iq,ir,iseed
        integer(i4b)          :: k,c1
        real(r8b)             :: rdum
        
        parameter (ia=16807,im=2147483647,iq=127773,ir=2836)
        
        iseed = abs(iseed) ! must be positive!
        
        rnd_o_iseed = iseed

        !!! Test integer representation !!!
        c1=-8
        c1=ishftc(c1,-3)
        !     print *,c1
        if (c1.ne.536870911) stop 'Nonstandard integer representation. Stopped.'

        idum=iseed
        idum= abs(1099087573 * idum)               ! 32-bit LCG to shuffle seeds
        if (idum.eq.0) idum=1
        if (idum.ge.IM) idum=IM-1

        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then
                rnd_x=idum+1
            else 
                rnd_x=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then 
                rnd_y=idum+1 
            else 
                rnd_y=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then
                rnd_z=idum+1 
        else 
                rnd_z=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then
                rnd_w=idum+1 
            else 
        rnd_w=idum
        endif

        rdum=rnd()

        return
    end subroutine
    
    function rnd_int(i1,i2)
        integer, intent(in) :: i1, i2
        integer             :: rnd_int
        
        rnd_int = min(int(rnd()*(i2+1-i1))+i1,i2)
    end function

    ! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    subroutine rnd_PLinit(k0,kc,gama)
        integer, intent(in) :: k0, kc
        real*8, intent(in) :: gama
        integer :: j
        
        rndPL_k0 = k0
        rndPL_kc = kc
        rndPL_gama = gama
        
        if (allocated(rndPL_pk)) deallocate(rndPL_pk)
        allocate(rndPL_pk(k0:kc))
        
        rndPL_AA = 0d0
        do j=k0,kc
            rndPL_AA=rndPL_AA + (1d0*j)**(-gama)
            rndPL_pk(j) = (1d0*j)**(-gama)
        enddo
        rndPL_AA = 1d0/rndPL_AA
        rndPL_pk = rndPL_AA * rndPL_pk
        
        rndPL_x0 = (1d0*(k0-1))**(-gama+1d0)
        rndPL_xc = (1d0*kc)**(-gama+1d0)
        rndPL_expo = 1d0/(1d0-gama)
        
    end subroutine
    
    function rnd_PL()
        real*8 :: z, x
        integer :: j, rnd_PL
        
        do
            z = rnd()
            x = (rndPL_x0 - z*(rndPL_x0 - rndPL_xc))**rndPL_expo
            j = ceiling(x)
            
            z = rnd()
            
            if (.not. z*rndPL_AA / (x**rndPL_gama) >= rndPL_pk(j)) exit
            
        enddo
        
        rnd_PL = j
        
    end function
    ! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    
end module
