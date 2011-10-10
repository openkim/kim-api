subroutine compute_numer_deriv(atomnum,dir,pkim,DIM,N,coords,cutoff,cutpad,   &
                               boxlength,NBC_Method,do_update_list,coordsave, &
                               neighborList,RijList,deriv,deriv_err,ier)
use KIMservice
implicit none

!--Transferred variables
integer,                  intent(in)    :: atomnum
integer,                  intent(in)    :: dir
integer(kind=kim_intptr), intent(in)    :: pkim
integer,                  intent(in)    :: DIM
integer,                  intent(in)    :: N
real*8,                   intent(inout) :: coords(DIM,N)
real*8,                   intent(in)    :: cutoff
real*8,                   intent(in)    :: cutpad
real*8,                   intent(in)    :: boxlength(DIM)
character*64,             intent(in)    :: NBC_Method
logical,                  intent(inout) :: do_update_list
real*8,                   intent(inout) :: coordsave(DIM,N)
integer,                  intent(inout) :: neighborList(N+1,N)
real*8,                   intent(inout) :: RijList(DIM,N+1,N)
real*8,                   intent(out)   :: deriv
real*8,                   intent(out)   :: deriv_err
integer,                  intent(out)   :: ier

!-- Local variables
real*8,  parameter :: eps_init = 1.d-6
integer, parameter :: number_eps_levels = 15
real*8  eps, deriv_last, deriv_err_last
logical done
integer i,nn

!-- KIM variables
real*8 energy;         pointer(penergy,energy)

! Initialize error flag
ier = KIM_STATUS_OK

! Unpack data from KIM object
!
penergy = kim_api_get_data_f(pkim, "energy", ier)
if (ier.lt.KIM_STATUS_OK) then
   call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   stop
endif

! Outer loop of Ridders' method for computing numerical derivative
!
eps = eps_init
deriv_err_last = huge(1.d0)
do i=1,number_eps_levels
   deriv = dfridr(eps,deriv_err)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "compute_numer_deriv",ier)
      stop
   endif
   if (deriv_err>deriv_err_last) then
      deriv  = deriv_last
      deriv_err = deriv_err_last
      exit
   endif
   eps = eps*10.d0
   deriv_last  = deriv
   deriv_err_last = deriv_err
enddo

return

contains

   !--------------------------------------------------------------------------------
   !
   ! Compute numerical derivative using Ridders' method
   !
   ! Based on code from Numerical Recipes, Press et al., Second Ed., Cambridge, 1992
   !
   ! Ref: Ridders, C. J. F., "Two algorithms for the calculation of F'(x)=D",
   !      Advances in Engineering Software, Vol. 4, no. 2, pp. 75-76, 1982.
   !
   !
   ! Returns the gradient grad() of a KIM-compliant interatomic model at the current
   ! configuration by Ridders' method of polynomial extrapolation. An estimate
   ! for the error in each component of the gradient is returned in grad_err().
   !
   !--------------------------------------------------------------------------------
   real*8 function dfridr(h,err)
   implicit none
   
   !-- Transferred variables
   real*8, intent(inout) :: h
   real*8, intent(out)   :: err
   
   !-- Local variables
   integer, parameter :: NTAB=10     ! Maximum size of tableau
   real*8,  parameter :: CON=1.4d0   ! Stepsize increased by CON at each iter
   real*8,  parameter :: CON2=CON*CON
   real*8,  parameter :: BIG=huge(1.d0)
   real*8,  parameter :: SAFE=2.d0   ! Returns when error is SAFE worse than
                                     ! the best so far
   integer i,j
   real*8 errt,fac,hh,a(NTAB,NTAB),fp,fm,coordorig

   if (h.eq.0.d0) then
      ier = KIM_STATUS_FAIL
      return
   endif
   
   hh = h
   coordorig = coords(dir,atomnum)
   coords(dir,atomnum) = coordorig + hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                            do_update_list,coordsave,neighborList,RijList,ier)
   call kim_api_model_compute_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute",ier)
      stop
   endif
   fp = energy
   coords(dir,atomnum) = coordorig - hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                            do_update_list,coordsave,neighborList,RijList,ier)
   call kim_api_model_compute_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute",ier)
      stop
   endif
   fm = energy
   coords(dir,atomnum) = coordorig
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                            do_update_list,coordsave,neighborList,RijList,ier)
   a(1,1)=(fp-fm)/(2.d0*hh)
   err=BIG
   ! successive columns in the Neville tableau will go to smaller step sizes
   ! and higher orders of extrapolation
   do i=2,NTAB  
      ! try new, smaller step size
      hh=hh/CON 
      coords(dir,atomnum) = coordorig + hh
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                               do_update_list,coordsave,neighborList,RijList,ier)
      call kim_api_model_compute_f(pkim, ier)
      if (ier.lt.KIM_STATUS_OK) then
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute",ier)
         stop
      endif
      fp = energy
      coords(dir,atomnum) = coordorig - hh
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                               do_update_list,coordsave,neighborList,RijList,ier)
      call kim_api_model_compute_f(pkim, ier)
      if (ier.lt.KIM_STATUS_OK) then
         call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute",ier)
         stop
      endif
      fm = energy
      coords(dir,atomnum) = coordorig
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                               do_update_list,coordsave,neighborList,RijList,ier)
      a(1,i)=(fp-fm)/(2.d0*hh)
      fac=CON2
      ! compute extrapolations of various orders, requiring no new function
      ! evaluations
      do j=2,i
         a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.d0)
         fac=CON2*fac
         ! The error strategy is to compute each new extrapolation to one order
         ! lower, both at the present step size and the previous one.
         errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
         if (errt.le.err) then ! if error is decreased, save the improved answer
            err = errt
            dfridr=a(j,i)
         endif
      enddo
      if (abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err) return ! if higher order is worse by
                                                     ! significant factor `SAFE',
                                                     ! then quit early.
   enddo 
   return
   end function dfridr

end subroutine compute_numer_deriv
