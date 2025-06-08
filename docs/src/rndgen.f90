! ## File: rndgen.f90
! ## - module: random number generator. This is just a module to be used in another program.
! ## See README.md for more information and usage
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
! Email     : wesley@wcota.me
! Homepage  : http://wcota.me
! Date      : 08 Jun 2025
! Version   : 1.0.5
!-----------------------------------------------------------------------------

module rndgen_mod
   use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
   implicit none
   private

   real(kind=dp), parameter :: am = 4.656612873077392578d-10 ! multiplier 1/2^31

   !> Random seeds object
   type :: rndSeed
      integer(kind=i4), private :: mseed(4)
   contains
      procedure, private :: saveToFile_rndSeed
      procedure, private :: saveToFile_filename_rndSeed
      procedure, private :: readFromFile_rndSeed
      procedure, private :: readFromFile_filename_rndSeed
      generic :: saveToFile => saveToFile_rndSeed, saveToFile_filename_rndSeed
      generic :: readFromFile => readFromFile_rndSeed, readFromFile_filename_rndSeed
   end type

   !> Random number generator object with its procedures
   type :: rndgen
      integer(kind=i4) :: o_iseed ! seed used to generate the four new seeds
      type(rndSeed) :: seed

   contains

      procedure :: rnd => rnd_rndgen_dp  ! generates a random number in the range [0, 1)
      procedure :: int => int_rndgen_i4 ! generates a random integer number in the range [i1, i2]

      ! aliases:
      procedure :: int_i4 => int_rndgen_i4
      procedure :: int_i8 => int_rndgen_i8

      procedure :: real => real_rndgen_dp ! generates a random real number in the range [r1, r2)
      procedure :: real_sp => real_rndgen_sp ! generates a random real number in the range [r1, r2)

      procedure :: init => init_rndgen
      procedure :: reset => reset_rndgen

      procedure, private :: save_seed_rndgen
      procedure, private :: save_seed_filename_rndgen
      procedure, private :: read_seed_rndgen
      procedure, private :: read_seed_filename_rndgen
      generic :: save_seed => save_seed_rndgen, save_seed_filename_rndgen
      generic :: read_seed => read_seed_rndgen, read_seed_filename_rndgen

      generic :: rnd_array => rnd_array_rnd, rnd_array_real, rnd_array_int_i4
      procedure :: rnd_array_i4 => rnd_array_int_i4
      procedure :: rnd_array_i8 => rnd_array_int_i8
      procedure, private :: rnd_array_rnd
      procedure, private :: rnd_array_real
      procedure, private :: rnd_array_int_i4

   end type

   public :: rndgen, rndSeed

contains
   !> Generates a random number in the range [0, 1)
   function rnd_rndgen_dp(this) result(rnd_number) ! KISS
      ! Adapted from <http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90> by Thomas Vojta

      class(rndgen) :: this
      integer(kind=i8) :: kiss
      real(kind=dp) :: rnd_number

      this%seed%mseed(1) = 69069*this%seed%mseed(1) + 1327217885
      this%seed%mseed(2) = ieor(this%seed%mseed(2), ishft(this%seed%mseed(2), 13)); 
      this%seed%mseed(2) = ieor(this%seed%mseed(2), ishft(this%seed%mseed(2), -17)); 
      this%seed%mseed(2) = ieor(this%seed%mseed(2), ishft(this%seed%mseed(2), 5))
      this%seed%mseed(3) = 18000*iand(this%seed%mseed(3), 65535) + ishft(this%seed%mseed(3), -16)
      this%seed%mseed(4) = 30903*iand(this%seed%mseed(4), 65535) + ishft(this%seed%mseed(4), -16)
      kiss = ishft(this%seed%mseed(1) + this%seed%mseed(2) + ishft(this%seed%mseed(3), 16) + this%seed%mseed(4), -1)

      rnd_number = kiss*am ! returns in range [0, 1)
   end function

   !> Generates a random integer number in the range [i1, i2], int64
   function int_rndgen_i8(this, i1, i2) result(rnd_number)
      class(rndgen) :: this
      integer(kind=i8), intent(in) :: i1, i2
      integer(kind=i8) :: rnd_number

      rnd_number = min(int(this%rnd()*(i2 + 1 - i1)) + i1, i2) ! returns in range [i1, i2]
   end function

   !> Generates a random integer number in the range [i1, i2], int32
   function int_rndgen_i4(this, i1, i2) result(rnd_number)
      class(rndgen) :: this
      integer(kind=i4), intent(in) :: i1, i2
      integer(kind=i4) :: rnd_number

      rnd_number = min(int(this%rnd()*(i2 + 1 - i1)) + i1, i2) ! returns in range [i1, i2]
   end function

   !> Generates a random real number in the range [r1, r2), double
   function real_rndgen_dp(this, r1, r2) result(rnd_number)
      class(rndgen) :: this
      real(kind=dp), intent(in) :: r1, r2
      real(kind=dp) :: rnd_number

      rnd_number = r1 + (r2 - r1)*this%rnd() ! returns in range [r1, r2)
   end function

   !> Generates a random real number in the range [r1, r2), single
   function real_rndgen_sp(this, r1, r2) result(rnd_number)
      class(rndgen) :: this
      real(kind=sp), intent(in) :: r1, r2
      real(kind=sp) :: rnd_number

      rnd_number = real(this%real(real(r1, kind=dp), real(r2, kind=dp)), kind=sp) ! returns in range [r1, r2)
   end function

   !> Initializes the random number generator
   subroutine init_rndgen(this, iseed)
      ! Adapted from <http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90> by Thomas Vojta

      class(rndgen) :: this

      integer(kind=i4) :: idum, ia, im, iq, ir, iseed, iseed_var
      integer(kind=i4) :: k, c1
      real(kind=dp) :: rdum

      parameter(ia=16807, im=2147483647, iq=127773, ir=2836)

      iseed_var = abs(iseed) ! must be positive!

      this%o_iseed = iseed_var

      !!! Test integer representation !!!
      c1 = -8
      c1 = ishftc(c1, -3)
      !     print *,c1
      if (c1 /= 536870911) stop 'Nonstandard integer representation. Stopped.'

      idum = iseed_var
      idum = abs(1099087573*idum)               ! 32-bit LCG to shuffle seeds
      if (idum == 0) idum = 1
      if (idum >= IM) idum = IM - 1

      k = (idum)/IQ
      idum = IA*(idum - k*IQ) - IR*k
      if (idum < 0) idum = idum + IM
      if (idum < 1) then
         this%seed%mseed(1) = idum + 1
      else
         this%seed%mseed(1) = idum
      end if
      k = (idum)/IQ
      idum = IA*(idum - k*IQ) - IR*k
      if (idum < 0) idum = idum + IM
      if (idum < 1) then
         this%seed%mseed(2) = idum + 1
      else
         this%seed%mseed(2) = idum
      end if
      k = (idum)/IQ
      idum = IA*(idum - k*IQ) - IR*k
      if (idum < 0) idum = idum + IM
      if (idum < 1) then
         this%seed%mseed(3) = idum + 1
      else
         this%seed%mseed(3) = idum
      end if
      k = (idum)/IQ
      idum = IA*(idum - k*IQ) - IR*k
      if (idum < 0) idum = idum + IM
      if (idum < 1) then
         this%seed%mseed(4) = idum + 1
      else
         this%seed%mseed(4) = idum
      end if

      rdum = this%rnd() ! warm up the generator with the first random number
   end subroutine

   !> Resets the random number generator
   subroutine reset_rndgen(this)
      class(rndgen) :: this

      call this%init(this%o_iseed)
   end subroutine

   !> Save the current seeds to a seeds object and, optionally, to a file unit
   subroutine save_seed_rndgen(this, u_mseed, und)
      class(rndgen) :: this
      type(rndSeed), intent(out) :: u_mseed
      integer, intent(in), optional :: und

      u_mseed = this%seed

      if (present(und)) call u_mseed%saveToFile(und)

   end subroutine

   !> Save the current seeds to a seeds object and, optionally, to a file name
   subroutine save_seed_filename_rndgen(this, u_mseed, filename)
      class(rndgen) :: this
      type(rndSeed), intent(out) :: u_mseed
      character(len=*), intent(in) :: filename

      u_mseed = this%seed

      call u_mseed%saveToFile(filename)

   end subroutine

   !> Read the seeds from a seeds object or, optionally, from a file unit
   subroutine read_seed_rndgen(this, u_mseed, und)
      class(rndgen) :: this
      type(rndSeed), intent(in) :: u_mseed
      integer, intent(in), optional :: und

      if (present(und)) call u_mseed%readFromFile(und)

      this%seed = u_mseed

   end subroutine

   !> Read the seeds from a seeds object or, optionally, from a file name
   subroutine read_seed_filename_rndgen(this, u_mseed, filename)
      class(rndgen) :: this
      type(rndSeed), intent(in) :: u_mseed
      character(len=*), intent(in) :: filename

      call u_mseed%readFromFile(filename)

      this%seed = u_mseed

   end subroutine

   !> Save the seeds to a file unit
   subroutine saveToFile_rndSeed(this, und)
      implicit none
      class(rndSeed) :: this
      integer, intent(in) :: und
      integer :: i

      write (und, *) (this%mseed(i), i=1, 4)

   end subroutine

   !> Save the seeds to a file name
   subroutine saveToFile_filename_rndSeed(this, filename)
      implicit none
      class(rndSeed) :: this
      character(len=*), intent(in) :: filename
      integer :: und

      open(newunit=und, file=filename, status='replace', form='formatted', action='write')
      call this%saveToFile(und)
      close(und)

   end subroutine

   !> Read the seeds from a file unit
   subroutine readFromFile_rndSeed(this, und)
      class(rndSeed) :: this
      integer, intent(in) :: und
      integer :: i

      read (und, *) (this%mseed(i), i=1, 4)

   end subroutine

   !> Read the seeds from a file name
   subroutine readFromFile_filename_rndSeed(this, filename)
      class(rndSeed) :: this
      character(len=*), intent(in) :: filename
      integer :: und

      open(newunit=und, file=filename, status='old', form='formatted', action='read')
      call this%readFromFile(und)
      close(und)

   end subroutine

   !> Generates a random array of real numbers in the range [0, 1)
   function rnd_array_rnd(gen, n) result(arr)
      class(rndgen), intent(in) :: gen
      integer(kind=i4), intent(in) :: n
      integer(kind=i4) :: i
      real(kind=dp), allocatable :: arr(:)

      arr = [(gen%rnd(), i = 1, n)]

  end function rnd_array_rnd

  !> Generates a random array of real numbers in the range [r1, r2)
  function rnd_array_real(gen, n, r1, r2) result(arr)
     class(rndgen), intent(in) :: gen
     integer(kind=i4), intent(in) :: n
     real(kind=dp), intent(in) :: r1, r2
     integer(kind=i4) :: i
     real(kind=dp), allocatable :: arr(:)

     arr = [(gen%real(r1,r2), i = 1, n)]

 end function rnd_array_real

 function rnd_array_int_i4(gen, n, i1, i2) result(arr)
     class(rndgen), intent(in) :: gen
     integer(kind=i4), intent(in) :: n
     integer(kind=i4), intent(in) :: i1, i2
     integer(kind=i4) :: i
     integer(kind=i4), allocatable :: arr(:)

     arr = [(gen%int_i4(i1, i2), i = 1, n)]

 end function rnd_array_int_i4

 function rnd_array_int_i8(gen, n, i1, i2) result(arr)
     class(rndgen), intent(in) :: gen
     integer(kind=i4), intent(in) :: n
     integer(kind=i8), intent(in) :: i1, i2
     integer(kind=i4) :: i
     integer(kind=i8), allocatable :: arr(:)

     arr = [(gen%int_i8(i1, i2), i = 1, n)]

 end function rnd_array_int_i8

end module
