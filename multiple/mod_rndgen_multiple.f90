! ## File: mod_random.f90
! ## - module: random number generator. This is just a module to be used in another program.
! ## See README.md for more information and use
!-----------------------------------------------------------------------------
! KISS random generator module, as object: can have multiple and independent generators!
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

    public :: rndgen

    integer,parameter,private      :: r8b= SELECTED_REAL_KIND(P=14,R=99)   ! 8-byte reals
    integer,parameter,private      :: i4b= SELECTED_INT_KIND(8)            ! 4-byte integers 
    real(r8b),parameter,private    :: am=4.656612873077392578d-10       ! multiplier 1/2^31
    

    type :: rndgen
        integer :: o_iseed                      ! original seed
        integer(i4b), private :: x,y,z,w        ! working variables for the four generators
        
        contains
            procedure :: rnd => p__rndgen_rnd
            procedure :: int => p__rndgen_rndint
            
            procedure :: init => p__rndgen_rndinit
            procedure :: reset => p__rndgen_rndreset
            
    end type
    
    ! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    ! Adapted from Silvio C. Ferreira code
    type, extends(rndgen) :: rndgenPL
        real*8, private :: rndPL_AA, rndPL_expo, rndPL_x0, rndPL_xc
        real*8 :: rndPL_gama
        integer :: rndPL_k0, rndPL_kc
        real*8, private, allocatable :: rndPL_pk(:)
        contains
            procedure :: rndPL => p__rndgenPL_rnd_PL
            procedure :: initPL => p__rndgenPL_rnd_PL_init
    end type
    ! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    
contains

    function p__rndgen_rnd(this) ! KISS
        implicit none
        
        class(rndgen) :: this

        real(r8b)              :: p__rndgen_rnd
        integer(i4b)           :: kiss

        this%x = 69069 * this%x + 1327217885
        this%y = ieor (this%y, ishft (this%y, 13)); this%y= ieor (this%y, ishft (this%y, -17)); 
        this%y= ieor (this%y, ishft (this%y, 5))
        this%z = 18000 * iand (this%z, 65535) + ishft (this%z, - 16)
        this%w = 30903 * iand (this%w, 65535) + ishft (this%w, - 16)
        kiss = ishft(this%x + this%y + ishft (this%z, 16) + this%w , -1)
        p__rndgen_rnd=kiss*am
    end function
    
    subroutine p__rndgen_rndreset(this)
        implicit none
        
        class(rndgen) :: this
        
        call this%init(this%o_iseed)
    end subroutine

    subroutine p__rndgen_rndinit(this,iseed)
        !use ifport
        implicit none
        
        class(rndgen) :: this

        integer(i4b)          :: idum,ia,im,iq,ir,iseed
        integer(i4b)          :: k,c1
        real(r8b)             :: rdum
        
        parameter (ia=16807,im=2147483647,iq=127773,ir=2836)
        
        iseed = abs(iseed) ! must be positive!
        
        this%o_iseed = iseed

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
                this%x=idum+1
            else 
                this%x=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then 
                this%y=idum+1 
            else 
                this%y=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then
                this%z=idum+1 
        else 
                this%z=idum
        endif
        k=(idum)/IQ
        idum=IA*(idum-k*IQ)-IR*k
        if (idum.lt.0) idum = idum + IM
        if (idum.lt.1) then
                this%w=idum+1 
            else 
        this%w=idum
        endif

        rdum=this%rnd()

        return
    end subroutine
    
    function p__rndgen_rndint(this,i1,i2)
        class(rndgen) :: this
        integer, intent(in) :: i1, i2
        integer             :: p__rndgen_rndint
        
        p__rndgen_rndint = min(int(this%rnd()*(i2+1-i1))+i1,i2)
    end function

    ! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    ! Adapted from Silvio C. Ferreira code
    subroutine p__rndgenPL_rnd_PL_init(this,k0,kc,gama,iseed)
        class(rndgenPL) :: this
        integer, intent(in) :: k0, kc
        real*8, intent(in) :: gama
        integer, intent(in), optional :: iseed
        integer :: j
        
        if (present(iseed)) call this%init(iseed)
        
        this%rndPL_k0 = k0
        this%rndPL_kc = kc
        this%rndPL_gama = gama
        
        if (allocated(this%rndPL_pk)) deallocate(this%rndPL_pk)
        allocate(this%rndPL_pk(k0:kc))
        
        this%rndPL_AA = 0d0
        do j=k0,kc
            this%rndPL_AA=this%rndPL_AA + (1d0*j)**(-gama)
            this%rndPL_pk(j) = (1d0*j)**(-gama)
        enddo
        this%rndPL_AA = 1d0/this%rndPL_AA
        this%rndPL_pk = this%rndPL_AA * this%rndPL_pk
        
        this%rndPL_x0 = (1d0*(k0-1))**(-gama+1d0)
        this%rndPL_xc = (1d0*kc)**(-gama+1d0)
        this%rndPL_expo = 1d0/(1d0-gama)
        
    end subroutine
    
    function p__rndgenPL_rnd_PL(this)
        class(rndgenPL) :: this
        real*8 :: z, x
        integer :: j, p__rndgenPL_rnd_PL
        
        do
            z = this%rnd()
            x = (this%rndPL_x0 - z*(this%rndPL_x0 - this%rndPL_xc))**this%rndPL_expo
            j = ceiling(x)
            
            z = this%rnd()
            
            if (.not. z*this%rndPL_AA / (x**this%rndPL_gama) >= this%rndPL_pk(j)) exit
            
        enddo
        
        p__rndgenPL_rnd_PL = j
        
    end function
    ! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE
    
end module
