module rndgenPL_mod
   implicit none
   private

   integer, parameter :: dp = selected_real_kind(15) ! 8-byte reals
   integer, parameter :: i8 = selected_int_kind(8) ! 4-byte integers
   real(kind=dp), parameter :: am = 4.656612873077392578d-10 ! multiplier 1/2^31

! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
   ! Adapted from Silvio C. Ferreira code
   type, extends(rndgen) :: rndgenPL
      real*8, private :: rndPL_AA, rndPL_expo, rndPL_x0, rndPL_xc
      real*8 :: rndPL_gama
      integer :: rndPL_k0, rndPL_kc
      real*8, private, allocatable :: rndPL_pk(:)
   contains
      procedure :: rndPL => rndPL_rndgenPL
      procedure :: initPL => initPL_rndgenPL
   end type
! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE

   public :: rndgenPL

contains

! SPECIFIC FOR PL: IF NOT USED, CAN DELETE
   ! Adapted from Silvio C. Ferreira code
   subroutine initPL_rndgenPL(this, k0, kc, gama, iseed)
      class(rndgenPL) :: this
      integer, intent(in) :: k0, kc
      real*8, intent(in) :: gama
      integer, intent(in), optional :: iseed
      integer :: j

      if (present(iseed)) call this%init(iseed)

      this%rndPL_k0 = k0
      this%rndPL_kc = kc
      this%rndPL_gama = gama

      if (allocated(this%rndPL_pk)) deallocate (this%rndPL_pk)
      allocate (this%rndPL_pk(k0:kc))

      this%rndPL_AA = 0d0
      do j = k0, kc
         this%rndPL_AA = this%rndPL_AA + (1d0*j)**(-gama)
         this%rndPL_pk(j) = (1d0*j)**(-gama)
      end do
      this%rndPL_AA = 1d0/this%rndPL_AA
      this%rndPL_pk = this%rndPL_AA*this%rndPL_pk

      this%rndPL_x0 = (1d0*(k0 - 1))**(-gama + 1d0)
      this%rndPL_xc = (1d0*kc)**(-gama + 1d0)
      this%rndPL_expo = 1d0/(1d0 - gama)

   end subroutine

   function rndPL_rndgenPL(this)
      class(rndgenPL) :: this
      real*8 :: z, x
      integer :: j, rndPL_rndgenPL

      do
         z = this%rnd()
         x = (this%rndPL_x0 - z*(this%rndPL_x0 - this%rndPL_xc))**this%rndPL_expo
         j = ceiling(x)

         z = this%rnd()

         if (.not. z*this%rndPL_AA/(x**this%rndPL_gama) >= this%rndPL_pk(j)) exit

      end do

      rndPL_rndgenPL = j

   end function
   ! / SPECIFIC FOR PL: IF NOT USED, CAN DELETE
end module
