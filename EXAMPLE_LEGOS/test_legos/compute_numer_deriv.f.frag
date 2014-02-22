subroutine compute_numer_deriv(atomnum,dir,pkim,DIM,N,coords,cutoff,cutpad,   &
                               boxSideLengths,NBC_Method,do_update_list,      &
                               coordsave,neighborList,RijList,deriv,deriv_err,&
                               ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!--Transferred variables
integer(c_int), intent(in)    :: atomnum
integer(c_int), intent(in)    :: dir
type(c_ptr),    intent(in)    :: pkim
integer(c_int), intent(in)    :: DIM
integer(c_int), intent(in)    :: N
real(c_double), intent(inout) :: coords(DIM,N)
real(c_double), intent(in)    :: cutoff
real(c_double), intent(in)    :: cutpad
real(c_double), intent(in)    :: boxSideLengths(DIM)
character(len=KIM_KEY_STRING_LENGTH), intent(in) :: NBC_Method
logical,        intent(inout) :: do_update_list
real(c_double), intent(inout) :: coordsave(DIM,N)
integer(c_int), intent(inout) :: neighborList(N+1,N)
real(c_double), intent(inout) :: RijList(DIM,N+1,N)
real(c_double), intent(out)   :: deriv
real(c_double), intent(out)   :: deriv_err
integer(c_int), intent(out)   :: ier

!-- Local variables
real(c_double), parameter :: eps_init = 1.e-6_cd
integer(c_int), parameter :: number_eps_levels = 15
real(c_double)  eps, deriv_last, deriv_err_last
integer(c_int)  i,idum
logical doing_neighbors

!-- KIM variables
real(c_double), pointer :: energy; type(c_ptr) :: penergy

! Initialize error flag
ier = KIM_STATUS_OK

deriv_last = 0.0_cd ! initialize

! Unpack data from KIM object
!
penergy = kim_api_get_data(pkim, "energy", ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                              "kim_api_get_data", ier)
   stop
endif
call c_f_pointer(penergy, energy)

! Figure out if neighbor list is being generated
!
doing_neighbors = .not.(index(NBC_Method,"CLUSTER").eq.1)

! Outer loop of Ridders' method for computing numerical derivative
!
eps = eps_init
deriv_err_last = huge(1.0_cd)
do i=1,number_eps_levels
   deriv = dfridr(eps,deriv_err)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "compute_numer_deriv",ier)
      stop
   endif
   if (deriv_err>deriv_err_last) then
      deriv  = deriv_last
      deriv_err = deriv_err_last
      exit
   endif
   eps = eps*10.0_cd
   deriv_last  = deriv
   deriv_err_last = deriv_err
enddo

return

contains

   !----------------------------------------------------------------------------
   !
   ! Compute numerical derivative using Ridders' method
   !
   ! Based on code from Numerical Recipes, Press et al., Second Ed., Cambridge,
   ! 1992
   !
   ! Ref: Ridders, C. J. F., "Two algorithms for the calculation of F'(x)=D",
   !      Advances in Engineering Software, Vol. 4, no. 2, pp. 75-76, 1982.
   !
   !
   ! Returns the gradient grad() of a KIM-compliant interatomic model at the 
   ! current configuration by Ridders' method of polynomial extrapolation.
   ! An estimate for the error in each component of the gradient is returned in
   ! grad_err().
   !
   !----------------------------------------------------------------------------
   real(c_double) function dfridr(h,err)
   implicit none

   !-- Transferred variables
   real(c_double), intent(inout) :: h
   real(c_double), intent(out)   :: err

   !-- Local variables
   integer(c_int), parameter :: NTAB=10     ! Maximum size of tableau
   real(c_double), parameter :: CON=1.4_cd  ! Stepsize incr. by CON at each iter
   real(c_double), parameter :: CON2=CON*CON
   real(c_double), parameter :: BIG=huge(1.0_cd)
   real(c_double), parameter :: SAFE=2.0_cd ! Returns when error is SAFE worse
                                            ! than the best so far
   integer(c_int) i,j
   integer(c_int) idum
   real(c_double) errt,fac,hh,a(NTAB,NTAB),fp,fm,coordorig

   dfridr = 0.0_cd ! initialize

   if (h.eq.0.0_cd) then
      ier = KIM_STATUS_FAIL
      return
   endif

   hh = h
   coordorig = coords(dir,atomnum)
   coords(dir,atomnum) = coordorig + hh
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighborList,RijList,ier)
   ier = kim_api_model_compute(pkim)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ier)
      stop
   endif
   fp = energy
   coords(dir,atomnum) = coordorig - hh
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighborList,RijList,ier)
   ier = kim_api_model_compute(pkim)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ier)
      stop
   endif
   fm = energy
   coords(dir,atomnum) = coordorig
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighborList,RijList,ier)
   a(1,1)=(fp-fm)/(2.0_cd*hh)
   err=BIG
   ! successive columns in the Neville tableau will go to smaller step sizes
   ! and higher orders of extrapolation
   do i=2,NTAB
      ! try new, smaller step size
      hh=hh/CON
      coords(dir,atomnum) = coordorig + hh
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighborList,RijList,ier)
      ier = kim_api_model_compute(pkim)
      if (ier.lt.KIM_STATUS_OK) then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ier)
         stop
      endif
      fp = energy
      coords(dir,atomnum) = coordorig - hh
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighborList,RijList,ier)
      ier = kim_api_model_compute(pkim)
      if (ier.lt.KIM_STATUS_OK) then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ier)
         stop
      endif
      fm = energy
      coords(dir,atomnum) = coordorig
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighborList,RijList,ier)
      a(1,i)=(fp-fm)/(2.0_cd*hh)
      fac=CON2
      ! compute extrapolations of various orders, requiring no new function
      ! evaluations
      do j=2,i
         a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.0_cd)
         fac=CON2*fac
         ! The error strategy is to compute each new extrapolation to one order
         ! lower, both at the present step size and the previous one.
         errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
         if (errt.le.err) then ! if error is decreased, save the improved answer
            err = errt
            dfridr=a(j,i)
         endif
      enddo
      if (abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err) return ! if higher order is worse
                                                     ! by significant factor
                                                     ! `SAFE', then quit early.
   enddo
   return
   end function dfridr

end subroutine compute_numer_deriv
