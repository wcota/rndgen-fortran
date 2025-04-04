module rndgenPL_mod
   use rndgen_kinds_mod
   use rndgen_mod
   implicit none
   private

   real(kind=dp), parameter :: am = 4.656612873077392578d-10 ! multiplier 1/2^31

   !> Random number generator object for power-law distribution, adapted from Silvio C. Ferreira code.
   !> The power-law distribution is given by P(k) = k^(-gamma), where k is an integer between kmin and kmax.
   type, extends(rndgen) :: rndgenPL
      ! auxiliar variables
      real(kind=dp), private :: AA, expo, x0, xc

      ! parameters
      real(kind=dp) :: gamma
      integer(kind=i4) :: kmin, kmax

      ! auxiliar distribution
      real(kind=dp), private, allocatable :: prob(:)
   contains
      procedure :: rndPL => rndPL_rndgenPL ! generates a random number following the power-law distribution
      procedure :: initPL => initPL_rndgenPL ! initializes the power-law random number generator
   end type

   public :: rndgenPL

contains

   !> Initializes the power-law random number generator
   subroutine initPL_rndgenPL(this, kmin, kmax, gama, iseed)
      class(rndgenPL) :: this
      integer(kind=i4), intent(in) :: kmin, kmax
      real(kind=dp), intent(in) :: gama
      integer, intent(in), optional :: iseed
      integer(kind=kind(kmax)) :: j

      if (present(iseed)) call this%init(iseed)

      this%kmin = kmin
      this%kmax = kmax
      this%gamma = gama

      if (allocated(this%prob)) deallocate (this%prob)
      allocate (this%prob(kmin:kmax))

      this%AA = 0d0

      do j = kmin, kmax
         this%AA = this%AA + (1.0_dp*j)**(-gama)
         this%prob(j) = (1.0_dp*j)**(-gama)
      end do

      this%AA = 1.0_dp/this%AA
      this%prob = this%AA*this%prob

      this%x0 = (1.0_dp*(kmin - 1))**(-gama + 1.0_dp)
      this%xc = (1.0_dp*kmax)**(-gama + 1.0_dp)
      this%expo = 1.0_dp/(1.0_dp - gama)

   end subroutine

   !> Generates a random number following the power-law distribution
   function rndPL_rndgenPL(this) result(rnd_number)
      class(rndgenPL) :: this
      real(kind=dp) :: z, x
      integer(kind=i4) :: j, rnd_number

      do
         z = this%rnd()
         x = (this%x0 - z*(this%x0 - this%xc))**this%expo
         j = ceiling(x)

         z = this%rnd()

         if (.not. z*this%AA/(x**this%gamma) >= this%prob(j)) exit

      end do

      rnd_number = j

   end function
end module
