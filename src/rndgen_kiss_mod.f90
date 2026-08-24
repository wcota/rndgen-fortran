! ## File: rndgen_kiss_mod.f90
! ## See README.md for more information and usage
!-----------------------------------------------------------------------------
! IMPORTANT:
! THIS CODE WAS ADAPTED FROM https://web.archive.org/web/20240225163957/http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90
!
! ORIGINAL COMMENTS:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Random number generator KISS05 after a suggestion by George Marsaglia
! in "Random numbers for C: The END?" posted on sci.crypt.random-numbers
! in 1999
!
! version as in "double precision RNGs" in  sci.math.num-analysis
! http://sci.tech-archive.net/Archive/sci.math.num-analysis/2005-11/msg00352.html
!
! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
! Overall period > 2^123
!
!
! A call to rkiss05() gives one random real in the interval [0,1),
! i.e., 0 <= rkiss05 < 1
!
! Before using rkiss05 call kissinit(seed) to initialize
! the generator by random integers produced by Park/Millers
! minimal standard LCG.
! Seed should be any positive integer.
!
! FORTRAN implementation by Thomas Vojta, vojta@mst.edu
! built on a module found at www.fortran.com
!
!
! History:
!        v0.9     Dec 11, 2010    first implementation
!        V0.91    Dec 11, 2010    inlined internal function for the SR component
!        v0.92    Dec 13, 2010    extra shuffle of seed in kissinit
!        v093     Aug 13, 2012    changed inter representation test to avoid data statements
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------------

module rndgen_kiss_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only : rndgen_base_t, rndgen_state_t
    implicit none
    private

    real(kind=dp), parameter :: kiss_am = 4.656612873077392578e-10_dp ! multiplier 1/2^31

    type, extends(rndgen_base_t) :: rndgen_kiss_t
        private
        integer(kind=i4), public :: oseed ! original seed used to initialize the random number generator
        integer(kind=i4) :: mseed(4) ! the 4 seeds used by the random number generator
    contains
        procedure, public :: init_i4 => rndgen_kiss_t_init_i4
        procedure, public :: init_i8 => rndgen_kiss_t_init_i8
        procedure, public :: reset => rndgen_kiss_t_reset
        procedure, public :: next_integer => rndgen_kiss_t_next_integer
        procedure, public :: get_state => rndgen_kiss_t_get_state
        procedure, public :: set_state => rndgen_kiss_t_set_state
        procedure, public :: rnd_dp => rndgen_kiss_t_rnd_dp
    end type

    public :: rndgen_kiss_t

contains

!> ==== KISS random number generator procedures ====

    !> Initializes the KISS random number generator with a 32-bit integer seed
    subroutine rndgen_kiss_t_init_i4(this, iseed)
        ! Adapted from <http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90> by Thomas Vojta

        class(rndgen_kiss_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed

        integer(kind=i4) :: idum, k, iseed_var
        real(kind=dp) :: rdum

        integer(kind=i4), parameter :: ia=16807, im=2147483647, iq=127773, ir=2836

        iseed_var = abs(iseed) ! must be positive!
        this%oseed = iseed_var ! save the original seed for later use in reset

        idum = iseed_var
        idum = abs(1099087573*idum)               ! 32-bit LCG to shuffle seeds
        if (idum == 0) idum = 1
        if (idum >= im) idum = im - 1

        k = idum / iq; idum = ia * (idum - k * iq) - ir * k
        if (idum < 0) idum = idum + im
        this%mseed(1) = merge(idum + 1, idum, idum < 1)

        k = idum / iq; idum = ia * (idum - k * iq) - ir * k
        if (idum < 0) idum = idum + im
        this%mseed(2) = merge(idum + 1, idum, idum < 1)

        k = idum / iq; idum = ia * (idum - k * iq) - ir * k
        if (idum < 0) idum = idum + im
        this%mseed(3) = merge(idum + 1, idum, idum < 1)

        k = idum / iq; idum = ia * (idum - k * iq) - ir * k
        if (idum < 0) idum = idum + im
        this%mseed(4) = merge(idum + 1, idum, idum < 1)

        ! warm up the generator with the first random number
        rdum = this%rnd_dp()
    end subroutine

    !> Wrapper for the KISS random number generator initialization with int64 seed
    subroutine rndgen_kiss_t_init_i8(this, iseed)
        class(rndgen_kiss_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: iseed

        ! perform a conversion from int64 to int32, since the KISS generator uses 32-bit integers
        call this%init_i4(int(iseed, kind=i4))
    end subroutine

    !> Reset the KISS random number generator to its original seed
    subroutine rndgen_kiss_t_reset(this)
        class(rndgen_kiss_t), intent(inout) :: this
        call this%init_i4(this%oseed)
    end subroutine

    !> Core function for KISS random number generation, returns a 31-bit integer
    function rndgen_kiss_t_next_integer(this) result(kiss_val)
        class(rndgen_kiss_t), intent(inout) :: this
        integer(kind=i8) :: kiss_val

        this%mseed(1) = 69069_i4*this%mseed(1) + 1327217885_i4
        this%mseed(2) = ieor(this%mseed(2), ishft(this%mseed(2), 13_i4));
        this%mseed(2) = ieor(this%mseed(2), ishft(this%mseed(2), -17_i4));
        this%mseed(2) = ieor(this%mseed(2), ishft(this%mseed(2), 5_i4))
        this%mseed(3) = 18000_i4*iand(this%mseed(3), 65535_i4) + ishft(this%mseed(3), -16_i4)
        this%mseed(4) = 30903_i4*iand(this%mseed(4), 65535_i4) + ishft(this%mseed(4), -16_i4)
        kiss_val = int(ishft(this%mseed(1) + this%mseed(2) + ishft(this%mseed(3), 16_i4) + this%mseed(4), -1_i4), kind=i8)

    end function

    !> Returns the current seed of the KISS random number generator
    function rndgen_kiss_t_get_state(this) result(state)
        class(rndgen_kiss_t), intent(in) :: this
        type(rndgen_state_t) :: state

        state%data = int(this%mseed, kind=i8) ! convert to int64 for storage
    end function

    !> Sets the current seed of the KISS random number generator
    subroutine rndgen_kiss_t_set_state(this, state)
        class(rndgen_kiss_t), intent(inout) :: this
        type(rndgen_state_t), intent(in) :: state

        this%mseed = int(state%data, kind=i4)
    end subroutine

    !> Generates a random number in the range [0, 1) using the KISS random number generator
    function rndgen_kiss_t_rnd_dp(this) result(rnd_number)
        class(rndgen_kiss_t), intent(inout) :: this
        real(kind=dp) :: rnd_number
        rnd_number = this%next_integer()*kiss_am ! returns in range [0, 1)
    end function

end module