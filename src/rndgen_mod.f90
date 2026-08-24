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
    real(kind=dp), parameter :: xoshiro_am = 1.11022302462515654042e-16_dp ! 1/2^53

    !> Container for seeds IO
    type :: rndgen_state_t
        integer(kind=i8) :: data(4)
    contains
        procedure, private :: save_state_to_unit => rndgen_state_t_save_state_to_unit
        procedure, private :: read_state_from_unit => rndgen_state_t_read_state_from_unit
        procedure, private :: save_state_to_file => rndgen_state_t_save_state_to_file
        procedure, private :: read_state_from_file => rndgen_state_t_read_state_from_file
        generic, public :: save_state => save_state_to_unit, save_state_to_file
        generic, public :: read_state => read_state_from_unit, read_state_from_file
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
        procedure(rndgen_t_get_state_iface), deferred, pass(this) :: get_state
        procedure(rndgen_t_set_state_iface), deferred, pass(this) :: set_state

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

        ! -- generate arrays of random numbers
        procedure, private :: rnd_dp_array => rndgen_t_rnd_dp_array ! generates an array of random numbers in the range [0, 1)
        procedure, private :: int_i4_array => rndgen_t_int_i4_array ! generates an array of random integer numbers in the range [i1, i2]
        procedure, private :: int_i8_array => rndgen_t_int_i8_array ! generates an array of random integer numbers in the range [i1, i2]
        procedure, private :: real_sp_array => rndgen_t_real_sp_array ! generates an array of random real numbers in the range [r1, r2)
        procedure, private :: real_dp_array => rndgen_t_real_dp_array ! generates an array of random real numbers in the range [r1, r2)

        generic, public :: rnd_array => rnd_dp_array, real_sp_array, real_dp_array, int_i4_array, int_i8_array
        procedure, public :: bool_array => rndgen_t_bool_array ! generates an array of random boolean values (true or false)
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

        function rndgen_t_get_state_iface(this) result(seed)
            import :: rndgen_base_t, rndgen_state_t
            class(rndgen_base_t), intent(in) :: this
            type(rndgen_state_t) :: seed
        end function

        subroutine rndgen_t_set_state_iface(this, seed)
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

    type, extends(rndgen_base_t) :: rndgen_xoshiro256_t
        private
        integer(kind=i8), public :: oseed ! original seed used to initialize the random number generator
        integer(kind=i8) :: mseed(4) ! the 4 seeds used by the random number generator
    contains
        procedure, public :: init_i4 => rndgen_xoshiro256_t_init_i4
        procedure, public :: init_i8 => rndgen_xoshiro256_t_init_i8
        procedure, public :: reset => rndgen_xoshiro256_t_reset
        procedure, public :: next_integer => rndgen_xoshiro256_t_next_integer
        procedure, public :: get_state => rndgen_xoshiro256_t_get_state
        procedure, public :: set_state => rndgen_xoshiro256_t_set_state
        procedure, public :: rnd_dp => rndgen_xoshiro256_t_rnd_dp
    end type

    public :: rndgen_state_t, rndgen_base_t, rndgen_kiss_t, rndgen_xoshiro256_t

contains

    !> ==== xoshiro256 random number generator procedures ====

    !> Wrapper for the xoshiro initialization with int32 seed
    subroutine rndgen_xoshiro256_t_init_i4(this, iseed)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: iseed

        call this%init_i8(int(iseed, kind=i8))
    end subroutine

    !> Initializes the xoshiro random number generator with a 64-bit integer seed
    !> Adapted from https://github.com/fortran-lang/stdlib/blob/9a15c7772f1a76a6c497b9f3abb793841fc81f74/src/stats/stdlib_random.fypp
    subroutine rndgen_xoshiro256_t_init_i8(this, iseed)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        integer(kind=i8), intent(in) :: iseed
        real(kind=dp) :: rdum

        integer(kind=i8) :: sm_state, z
        integer(kind=i4) :: i

        ! Magic constants for SplitMix64 (from stdlib_random)
        integer(kind=i8), parameter :: SM_C1 = -7046029254386353131_i8
        integer(kind=i8), parameter :: SM_C2 = -4658895280553007687_i8
        integer(kind=i8), parameter :: SM_C3 = -7723592293110705685_i8

        this%oseed = iseed
        sm_state = iseed

        ! Warm up the SplitMix64 generator (discard 10 states as per stdlib)
        do i = 1, 10
            sm_state = sm_state + SM_C1
            z = sm_state
            z = ieor(z, shiftr(z, 30_i4)) * SM_C2
            z = ieor(z, shiftr(z, 27_i4)) * SM_C3
            z = ieor(z, shiftr(z, 31_i4))
        end do

        ! Fill the 4 states of xoshiro256**
        do i = 1, 4
            sm_state = sm_state + SM_C1
            z = sm_state
            z = ieor(z, shiftr(z, 30_i4)) * SM_C2
            z = ieor(z, shiftr(z, 27_i4)) * SM_C3
            z = ieor(z, shiftr(z, 31_i4))
            this%mseed(i) = z
        end do

        ! warm up the generator with the first random number
        rdum = this%rnd_dp()

    end subroutine

    !> Reset the xoshiro256** random number generator to its original seed
    subroutine rndgen_xoshiro256_t_reset(this)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        call this%init_i8(this%oseed)
    end subroutine

    !> Core function for xoshiro256** random number generation, returns a 64-bit integer
    !> Adapted from https://github.com/fortran-lang/stdlib/blob/9a15c7772f1a76a6c497b9f3abb793841fc81f74/src/stats/stdlib_random.fypp
    function rndgen_xoshiro256_t_next_integer(this) result(res)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        integer(kind=i8) :: res, t

        ! Calculate output: rotl(s1 * 5, 7) * 9
        res = ishftc(this%mseed(2) * 5_i8, 7_i4) * 9_i8

        t = shiftl(this%mseed(2), 17_i4)

        this%mseed(3) = ieor(this%mseed(3), this%mseed(1))
        this%mseed(4) = ieor(this%mseed(4), this%mseed(2))
        this%mseed(2) = ieor(this%mseed(2), this%mseed(3))
        this%mseed(1) = ieor(this%mseed(1), this%mseed(4))

        this%mseed(3) = ieor(this%mseed(3), t)

        this%mseed(4) = ishftc(this%mseed(4), 45_i4)
    end function

    !> Returns the current state of the xoshiro generator
    function rndgen_xoshiro256_t_get_state(this) result(seed)
        class(rndgen_xoshiro256_t), intent(in) :: this
        type(rndgen_state_t) :: seed

        ! Direct copy, no type conversion needed since xoshiro state is already i8
        seed%data = this%mseed
    end function

    !> Sets the current state of the xoshiro generator
    subroutine rndgen_xoshiro256_t_set_state(this, seed)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        type(rndgen_state_t), intent(in) :: seed

        this%mseed = seed%data
    end subroutine

    !> Generates a random number in the range [0, 1) using double precision (53 bits of entropy)
    function rndgen_xoshiro256_t_rnd_dp(this) result(rnd_number)
        class(rndgen_xoshiro256_t), intent(inout) :: this
        real(kind=dp) :: rnd_number

        ! Shift right by 11 bits to fit the 53-bit mantissa, then multiply by 1/2^53
        rnd_number = real(shiftr(this%next_integer(), 11_i4), kind=dp) * xoshiro_am
    end function

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
    function rndgen_kiss_t_get_state(this) result(seed)
        class(rndgen_kiss_t), intent(in) :: this
        type(rndgen_state_t) :: seed

        seed%data = int(this%mseed, kind=i8) ! convert to int64 for storage
    end function

    !> Sets the current seed of the KISS random number generator
    subroutine rndgen_kiss_t_set_state(this, seed)
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

    !> ===== Generate arrays of random numbers procedures =====

    !> rnd_sp_array: generates an array of random numbers in the range [0, 1) using single precision
    function rndgen_t_rnd_sp_array(this, n) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        real(kind=sp), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_rnd_sp(arr)
    end function

    !> rnd_dp_array: generates an array of random numbers in the range [0, 1) using double precision
    function rndgen_t_rnd_dp_array(this, n) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        real(kind=dp), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_rnd_dp(arr)
    end function

    !> int_i4_array: generates an array of random integer numbers in the range [i1, i2] using int32
    function rndgen_t_int_i4_array(this, n, i1, i2) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n, i1, i2
        integer(kind=i4), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_int_i4(arr, i1, i2)
    end function

    !> int_i8_array: generates an array of random integer numbers in the range [i1, i2] using int64
    function rndgen_t_int_i8_array(this, n, i1, i2) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        integer(kind=i8), intent(in) :: i1, i2
        integer(kind=i8), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_int_i8(arr, i1, i2)
    end function

    !> real_sp_array: generates an array of random real numbers in the range [r1, r2) using single precision
    function rndgen_t_real_sp_array(this, n, r1, r2) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        real(kind=sp), intent(in) :: r1, r2
        real(kind=sp), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_real_sp(arr, r1, r2)
    end function

    !> real_dp_array: generates an array of random real numbers in the range [r1, r2) using double precision
    function rndgen_t_real_dp_array(this, n, r1, r2) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        real(kind=dp), intent(in) :: r1, r2
        real(kind=dp), allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_real_dp(arr, r1, r2)
    end function

    !> bool_array: generates an array of random boolean values (true or false)
    function rndgen_t_bool_array(this, n) result(arr)
        class(rndgen_base_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        logical, allocatable :: arr(:)
        allocate(arr(n))
        call this%fill_bool(arr)
    end function

    !> ==== State saving and reading procedures ====

    !> Saves the state of the random number generator to a unit
    subroutine rndgen_state_t_save_state_to_unit(this, unit)
        class(rndgen_state_t), intent(in) :: this
        integer(kind=i4), intent(in) :: unit
        integer(kind=i4) :: i

        write(unit, *) (this%data(i), i=1, 4)
    end subroutine

    !> Reads the state of the random number generator from a unit
    subroutine rndgen_state_t_read_state_from_unit(this, unit)
        class(rndgen_state_t), intent(out) :: this
        integer(kind=i4), intent(in) :: unit
        integer(kind=i4) :: i

        read(unit, *) (this%data(i), i=1, 4)
    end subroutine

    !> Saves the state of the random number generator to a file
    subroutine rndgen_state_t_save_state_to_file(this, filename)
        class(rndgen_state_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        integer(kind=i4) :: unit, iostat

        open(newunit=unit, file=filename, status='replace', action='write', iostat=iostat)
        if (iostat /= 0) then
            error stop "Error opening file"
        end if
        call rndgen_state_t_save_state_to_unit(this, unit)
        close(unit)
    end subroutine

    !> Reads the state of the random number generator from a file
    subroutine rndgen_state_t_read_state_from_file(this, filename)
        class(rndgen_state_t), intent(out) :: this
        character(len=*), intent(in) :: filename
        integer(kind=i4) :: unit, iostat

        open(newunit=unit, file=filename, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            error stop "Error opening file"
        end if
        call rndgen_state_t_read_state_from_unit(this, unit)
        close(unit)
    end subroutine

end module
