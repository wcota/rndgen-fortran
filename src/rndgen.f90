! ## File: rndgen.f90
! ## - module: random number generator. This is just a module to be used in another program.
! ## See README.md for more information and usage
!-----------------------------------------------------------------------------
! Author    : Wesley Cota
! Email     : wesley@wcota.me
! Homepage  : http://wcota.me
! Date      : 23 Aug 2026
! Version   : 2.0.0
!-----------------------------------------------------------------------------

module rndgen_mod
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    implicit none
    private

    real(kind=dp), parameter :: kiss_am = 4.656612873077392578e-10_dp ! multiplier 1/2^31

    !> Container for seeds IO
    type :: rndgen_state_t
        integer(kind=i8) :: data(4)
    end type

    !> Abstract random generator type. This is the base type for all random number generators.
    type, abstract :: rndgen_base_t
        private ! protected members: only some procedures are public
    contains
        ! -- deferred procedures for initialization and reset
        procedure(rndgen_t_init_i4_iface), deferred, pass(this) :: init_i4
        procedure(rndgen_t_init_i8_iface), deferred, pass(this) :: init_i8
        generic, public :: init => init_i4, init_i8

        procedure(rndgen_t_reset_iface), deferred, pass(this) :: reset

        ! -- deferred procedure for generating the next random integer
        procedure(rndgen_t_next_integer_iface), deferred, pass(this) :: next_integer

        ! -- deferred procedures for getting and setting the seed
        procedure(rndgen_t_get_seed_iface), deferred, pass(this) :: get_seed
        procedure(rndgen_t_set_seed_iface), deferred, pass(this) :: set_seed

        ! -- main scalar random number generation procedure
        procedure(rndgen_t_rnd_dp_iface), deferred, pass(this) :: rnd_dp

        ! -- scalar random number generation procedures
        procedure, public :: rnd_sp => rndgen_t_rnd_sp  ! generates a random number in the range [0, 1)
        procedure, private :: int_i4 => rndgen_t_int_i4 ! generates a random integer number in the range [i1, i2]
        procedure, private :: int_i8 => rndgen_t_int_i8 ! generates a random integer number in the range [i1, i2]
        procedure, private :: real_sp => rndgen_t_real_sp ! generates a random real number in the range [r1, r2)
        procedure, private :: real_dp => rndgen_t_real_dp ! generates a random real number in the range [r1, r2)

        ! -- generic interfaces for random number generation
        generic, public :: rnd => rnd_dp ! default random number generator returns double precision
        generic, public :: int => int_i4, int_i8 ! will be resolved based on the integer kind of the arguments
        generic, public :: real => real_sp, real_dp ! will be resolved based on the real kind of the arguments
        procedure, public :: bool => rndgen_t_bool ! generates a random boolean value (true or false)

        ! -- fill array procedures
        procedure, private :: fill_rnd_dp => rndgen_t_fill_rnd_dp ! fills an array with random numbers in the range [0, 1)
        procedure, private :: fill_rnd_sp => rndgen_t_fill_rnd_sp ! fills an array with random numbers in the range [0, 1)
        procedure, private :: fill_int_i4 => rndgen_t_fill_int_i4 ! fills an array with random integer numbers in the range [i1, i2]
        procedure, private :: fill_int_i8 => rndgen_t_fill_int_i8 ! fills an array with random integer numbers in the range [i1, i2]
        procedure, private :: fill_real_sp => rndgen_t_fill_real_sp ! fills an array with random real numbers in the range [r1, r2)
        procedure, private :: fill_real_dp => rndgen_t_fill_real_dp ! fills an array with random real numbers in the range [r1, r2)
        procedure, private :: fill_bool => rndgen_t_fill_bool ! fills an array with random boolean values (true or false)

        ! -- generic interfaces for filling arrays
        generic, public :: fill_array => fill_rnd_dp, fill_int_i4, fill_int_i8, fill_real_sp, fill_real_dp, fill_bool
    end type

    !> Abstract interface for the random number generator type
    abstract interface
        subroutine rndgen_t_init_i4_iface(this, iseed)
            import :: rndgen_base_t, i4
            class(rndgen_base_t), intent(inout) :: this
            integer(kind=i4), intent(in) :: iseed
        end subroutine

        subroutine rndgen_t_init_i8_iface(this, iseed)
            import :: rndgen_base_t, i8
            class(rndgen_base_t), intent(inout) :: this
            integer(kind=i8), intent(in) :: iseed
        end subroutine

        subroutine rndgen_t_reset_iface(this)
            import :: rndgen_base_t
            class(rndgen_base_t), intent(inout) :: this
        end subroutine

        function rndgen_t_get_seed_iface(this) result(seed)
            import :: rndgen_base_t, rndgen_state_t
            class(rndgen_base_t), intent(in) :: this
            type(rndgen_state_t) :: seed
        end function

        subroutine rndgen_t_set_seed_iface(this, seed)
            import :: rndgen_base_t, rndgen_state_t
            class(rndgen_base_t), intent(inout) :: this
            type(rndgen_state_t), intent(in) :: seed
        end subroutine

        function rndgen_t_rnd_dp_iface(this) result(rnd_number)
            import :: rndgen_base_t, dp
            class(rndgen_base_t), intent(inout) :: this
            real(kind=dp) :: rnd_number
        end function

        function rndgen_t_next_integer_iface(this) result(val)
            import :: rndgen_base_t, i8
            class(rndgen_base_t), intent(inout) :: this
            integer(kind=i8) :: val
        end function
    end interface

    type, extends(rndgen_base_t) :: rndgen_kiss_t
        private
        integer(kind=i4) :: oseed ! original seed used to initialize the random number generator
        integer(kind=i4) :: mseed(4) ! the 4 seeds used by the random number generator
    contains
        procedure, public :: init_i4 => rndgen_kiss_t_init_i4
        procedure, public :: init_i8 => rndgen_kiss_t_init_i8
        procedure, public :: reset => rndgen_kiss_t_reset
        procedure, public :: next_integer => rndgen_kiss_t_next_integer
        procedure, public :: get_seed => rndgen_kiss_t_get_seed
        procedure, public :: set_seed => rndgen_kiss_t_set_seed
        procedure, public :: rnd_dp => rndgen_kiss_t_rnd_dp
    end type

    public :: rndgen_state_t, rndgen_base_t, rndgen_kiss_t

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
    function rndgen_kiss_t_get_seed(this) result(seed)
        class(rndgen_kiss_t), intent(in) :: this
        type(rndgen_state_t) :: seed

        seed%data = int(this%mseed, kind=i8) ! convert to int64 for storage
    end function

    !> Sets the current seed of the KISS random number generator
    subroutine rndgen_kiss_t_set_seed(this, seed)
        class(rndgen_kiss_t), intent(inout) :: this
        type(rndgen_state_t), intent(in) :: seed

        this%mseed = int(seed%data, kind=i4)
    end subroutine

    !> Generates a random number in the range [0, 1) using the KISS random number generator
    function rndgen_kiss_t_rnd_dp(this) result(rnd_number)
        class(rndgen_kiss_t), intent(inout) :: this
        real(kind=dp) :: rnd_number
        rnd_number = this%next_integer()*kiss_am ! returns in range [0, 1)
    end function

    !> ==== General procedures ====

    !> ===== Random number generation procedures =====

    !> rnd_sp: generates a random number in the range [0, 1) using single precision
    function rndgen_t_rnd_sp(this) result(rnd_number)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=sp) :: rnd_number
        rnd_number = real(this%rnd_dp(), kind=sp) ! returns in range [0, 1)
    end function

    !> int_i4: generates a random integer number in the range [i1, i2] using int32
    function rndgen_t_int_i4(this, i1, i2) result(rnd_number)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: i1, i2
        integer(kind=i4) :: rnd_number
        integer(kind=i8) :: range_8

        range_8 = int(i2, kind=i8) - int(i1, kind=i8) + 1_i8

        rnd_number = int(min(int(this%rnd_dp() * range_8, kind=i8) + int(i1, kind=i8), int(i2, kind=i8)), kind=i4) ! returns in range [i1, i2]
    end function

    !> int_i8: generates a random integer number in the range [i1, i2] using int64
    function rndgen_t_int_i8(this, i1, i2) result(rnd_number)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: i1, i2
        integer(kind=i8) :: rnd_number
        real(kind=dp) :: range_dp

        range_dp = real(i2, kind=dp) - real(i1, kind=dp) + 1.0_dp

        rnd_number = min(int(this%rnd_dp() * range_dp, kind=i8) + i1, i2) ! returns in range [i1, i2]
    end function

    !> real_sp: generates a random real number in the range [r1, r2) using single precision
    function rndgen_t_real_sp(this, r1, r2) result(rnd_number)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=sp), intent(in) :: r1, r2
        real(kind=sp) :: rnd_number
        rnd_number = real(this%real_dp(real(r1, kind=dp), real(r2, kind=dp)), kind=sp) ! returns in range [r1, r2)
    end function

    !> real_dp: generates a random real number in the range [r1, r2) using double precision
    function rndgen_t_real_dp(this, r1, r2) result(rnd_number)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=dp), intent(in) :: r1, r2
        real(kind=dp) :: rnd_number
        rnd_number = r1 + (r2 - r1)*this%rnd_dp() ! returns in range [r1, r2)
    end function

    !> bool: generates a random boolean value (true or false)
    function rndgen_t_bool(this) result(rnd_bool)
        class(rndgen_base_t), intent(inout) :: this
        logical :: rnd_bool
        rnd_bool = this%rnd_dp() < 0.5_dp ! returns true or false with equal probability
    end function

    !> ===== Fill array procedures =====

    !> fill_rnd_dp: fills an array with random numbers in the range [0, 1) using double precision
    subroutine rndgen_t_fill_rnd_dp(this, arr)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=dp), intent(out) :: arr(:)
        integer(kind=i4) :: i

        do i = 1, size(arr)
            arr(i) = this%rnd_dp()
        end do
    end subroutine

    !> fill_rnd_sp: fills an array with random numbers in the range [0, 1) using single precision
    subroutine rndgen_t_fill_rnd_sp(this, arr)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=sp), intent(out) :: arr(:)
        integer(kind=i4) :: i
        do i = 1, size(arr)
            arr(i) = this%rnd_sp()
        end do
    end subroutine

    !> fill_int_i4: fills an array with random integer numbers in the range [i1, i2] using int32
    subroutine rndgen_t_fill_int_i4(this, arr, i1, i2)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(out) :: arr(:)
        integer(kind=i4), intent(in) :: i1, i2
        integer(kind=i4) :: i
        integer(kind=i8) :: range_8, i1_8, i2_8

        i1_8 = int(i1, kind=i8)
        i2_8 = int(i2, kind=i8)
        range_8 = i2_8 - i1_8 + 1_i8

        do i = 1, size(arr)
            arr(i) = int(min(int(this%rnd_dp() * range_8, kind=i8) + i1_8, i2_8), kind=i4)
        end do
    end subroutine

    !> fill_int_i8: fills an array with random integer numbers in the range [i1, i2] using int64
    subroutine rndgen_t_fill_int_i8(this, arr, i1, i2)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i8), intent(out) :: arr(:)
        integer(kind=i8), intent(in) :: i1, i2
        integer(kind=i4) :: i
        real(kind=dp) :: range_dp

        range_dp = real(i2, kind=dp) - real(i1, kind=dp) + 1.0_dp

        do i = 1, size(arr)
            arr(i) = min(int(this%rnd_dp() * range_dp, kind=i8) + i1, i2)
        end do
    end subroutine

    !> fill_real_sp: fills an array with random real numbers in the range [r1, r2) using single precision
    subroutine rndgen_t_fill_real_sp(this, arr, r1, r2)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=sp), intent(out) :: arr(:)
        real(kind=sp), intent(in) :: r1, r2
        integer(kind=i4) :: i
        real(kind=sp) :: range_sp

        ! optimization to avoid repeated range calculation
        range_sp = r2 - r1

        do i = 1, size(arr)
            arr(i) = r1 + range_sp * this%rnd_sp()
        end do
    end subroutine

    !> fill_real_dp: fills an array with random real numbers in the range [r1, r2) using double precision
    subroutine rndgen_t_fill_real_dp(this, arr, r1, r2)
        class(rndgen_base_t), intent(inout) :: this
        real(kind=dp), intent(out) :: arr(:)
        real(kind=dp), intent(in) :: r1, r2
        integer(kind=i4) :: i
        real(kind=dp) :: range_dp

        ! optimization to avoid repeated range calculation
        range_dp = r2 - r1

        do i = 1, size(arr)
            arr(i) = r1 + range_dp * this%rnd_dp()
        end do
    end subroutine

    !> fill_bool: fills an array with random boolean values (true or false)
    subroutine rndgen_t_fill_bool(this, arr)
        class(rndgen_base_t), intent(inout) :: this
        logical, intent(out) :: arr(:)
        integer(kind=i4) :: i
        do i = 1, size(arr)
            arr(i) = this%rnd_dp() < 0.5_dp
        end do
    end subroutine

end module
