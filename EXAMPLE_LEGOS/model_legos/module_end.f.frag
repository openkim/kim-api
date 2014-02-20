
  end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Pair potential: Lennard-Jones with smooth cutoff imposed by Ar^2 + Br + C
!
!-------------------------------------------------------------------------------
  subroutine pair(epsilon,sigma,A,B,C,r,phi,dphi,d2phi)
    implicit none

    !-- Transferred variables
    real(c_double), intent(in)  :: epsilon, sigma, A, B, C
    real(c_double), intent(in)  :: r
    real(c_double), intent(out) :: phi, dphi, d2phi

    !-- Local variables
    real(c_double) rsq,sor,sor6,sor12

    rsq  = r*r             !  r^2
    sor  = sigma/r         !  (sig/r)
    sor6 = sor*sor*sor     !
    sor6 = sor6*sor6       !  (sig/r)^6
    sor12= sor6*sor6       !  (sig/r)^12

    phi   =  4.0_cd*epsilon*(sor12-sor6) + A*rsq + B*r + C
    dphi  = 24.0_cd*epsilon*(-2.0_cd*sor12+sor6)/r  + 2.0_cd*A*r + B
    d2phi = 24.0_cd*epsilon*(26.0_cd*sor12-7.0_cd*sor6)/rsq + 2.0_cd*A

  end subroutine pair

!-------------------------------------------------------------------------------
!
! Model reinitialization routine
!
!-------------------------------------------------------------------------------
  integer(c_int) function ReInit(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in) :: pkim

    !-- Local variables
    real(c_double), pointer :: model_cutoff;  type(c_ptr) :: pcutoff
    real(c_double), pointer :: model_epsilon; type(c_ptr) :: pepsilon
    real(c_double), pointer :: model_sigma;   type(c_ptr) :: psigma
    real(c_double), pointer :: model_Pcutoff; type(c_ptr) :: pparamcut
    real(c_double), pointer :: model_cutnorm; type(c_ptr) :: pcutnorm
    real(c_double), pointer :: model_A;       type(c_ptr) :: pA
    real(c_double), pointer :: model_B;       type(c_ptr) :: pB
    real(c_double), pointer :: model_C;       type(c_ptr) :: pC
    real(c_double), pointer :: model_sigmasq; type(c_ptr) :: psigmasq
    real(c_double), pointer :: model_cutsq;   type(c_ptr) :: pcutsq
    integer idum

    ! Get (changed) parameters from KIM object ---------------------------------

    ! get stuff from KIM object
    call kim_api_getm_data(pkim, ReInit, &
         "PARAM_FREE_sigma",   psigma,    1, &
         "PARAM_FREE_epsilon", pepsilon,  1, &
         "PARAM_FREE_cutoff",  pparamcut, 1)
    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", ReInit)
       return
    endif

    call c_f_pointer(psigma,    model_sigma)
    call c_f_pointer(pepsilon,  model_epsilon)
    call c_f_pointer(pparamcut, model_Pcutoff)

    ! Set new values in KIM object ---------------------------------------------

    ! Set stuff in KIM object
    call kim_api_getm_data(pkim, ReInit, &
         "cutoff",              pcutoff,  1, &
         "PARAM_FIXED_cutnorm", pcutnorm, 1, &
         "PARAM_FIXED_A",       pA,       1, &
         "PARAM_FIXED_B",       pB,       1, &
         "PARAM_FIXED_C",       pC,       1, &
         "PARAM_FIXED_sigmasq", psigmasq, 1, &
         "PARAM_FIXED_cutsq",   pcutsq,   1)
    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", ReInit)
       return
    endif

    call c_f_pointer(pcutoff,  model_cutoff)
    call c_f_pointer(pcutnorm, model_cutnorm)
    call c_f_pointer(pA,       model_A)
    call c_f_pointer(pB,       model_B)
    call c_f_pointer(pC,       model_C)
    call c_f_pointer(psigmasq, model_sigmasq)
    call c_f_pointer(pcutsq,   model_cutsq)

    model_cutoff = model_Pcutoff
    model_cutnorm = model_cutoff/model_sigma
    model_A = 12.0_cd*model_epsilon*(-26.0_cd + 7.0_cd*model_cutnorm**6)/ &
         (model_cutnorm**14*model_sigma**2)
    model_B = 96.0_cd*model_epsilon*(7.0_cd-2.0_cd*model_cutnorm**6)/     &
         (model_cutnorm**13*model_sigma)
    model_C = 28.0_cd*model_epsilon*(-13.0_cd+4.0_cd*model_cutnorm**6)/   &
         (model_cutnorm**12)
    model_sigmasq = model_sigma**2
    model_cutsq = model_cutoff**2

    ReInit = KIM_STATUS_OK
    return

  end function ReInit

!-------------------------------------------------------------------------------
!
! Model destroy routine
!
!-------------------------------------------------------------------------------
  integer(c_int) function Destroy(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in) :: pkim

    !-- Local variables
    real(c_double), pointer :: model_epsilon; type(c_ptr) :: pepsilon
    real(c_double), pointer :: model_sigma;   type(c_ptr) :: psigma
    real(c_double), pointer :: model_Pcutoff; type(c_ptr) :: pparamcut
    real(c_double), pointer :: model_cutnorm; type(c_ptr) :: pcutnorm
    real(c_double), pointer :: model_A;       type(c_ptr) :: pA
    real(c_double), pointer :: model_B;       type(c_ptr) :: pB
    real(c_double), pointer :: model_C;       type(c_ptr) :: pC
    real(c_double), pointer :: model_sigmasq; type(c_ptr) :: psigmasq
    real(c_double), pointer :: model_cutsq;   type(c_ptr) :: pcutsq
    integer(c_int) idum


    call kim_api_getm_data(pkim, Destroy, &
         "PARAM_FREE_sigma",    psigma,    1, &
         "PARAM_FREE_epsilon",  pepsilon,  1, &
         "PARAM_FREE_cutoff",   pparamcut, 1, &
         "PARAM_FIXED_cutnorm", pcutnorm,  1, &
         "PARAM_FIXED_A",       pA,        1, &
         "PARAM_FIXED_B",       pB,        1, &
         "PARAM_FIXED_C",       pC,        1, &
         "PARAM_FIXED_sigmasq", psigmasq,  1, &
         "PARAM_FIXED_cutsq",   pcutsq,    1)
    if (Destroy.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Destroy)
       return
    endif

    call c_f_pointer(psigma,    model_sigma)
    call c_f_pointer(pepsilon,  model_epsilon)
    call c_f_pointer(pparamcut, model_Pcutoff)
    call c_f_pointer(pcutnorm, model_cutnorm)
    call c_f_pointer(pA,       model_A)
    call c_f_pointer(pB,       model_B)
    call c_f_pointer(pC,       model_C)
    call c_f_pointer(psigmasq, model_sigmasq)
    call c_f_pointer(pcutsq,   model_cutsq)

    deallocate( model_sigma   )
    deallocate( model_epsilon )
    deallocate( model_Pcutoff )
    deallocate( model_cutnorm )
    deallocate( model_A       )
    deallocate( model_B       )
    deallocate( model_C       )
    deallocate( model_sigmasq )
    deallocate( model_cutsq   )

    Destroy = KIM_STATUS_OK
    return

  end function Destroy

end module MODEL_NAME_STR


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer(c_int) function model_init(pkim) bind(c)
  use, intrinsic :: iso_c_binding
  use MODEL_NAME_STR
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr), intent(in) :: pkim

  !-- Local variables
  integer(c_int), parameter :: one=1
  real(c_double), pointer :: model_cutoff;  type(c_ptr) :: pcutoff
  real(c_double), pointer :: model_epsilon
  real(c_double), pointer :: model_sigma
  real(c_double), pointer :: model_Pcutoff
  real(c_double), pointer :: model_cutnorm
  real(c_double), pointer :: model_A
  real(c_double), pointer :: model_B
  real(c_double), pointer :: model_C
  real(c_double), pointer :: model_sigmasq
  real(c_double), pointer :: model_cutsq
  integer(c_int) ier, idum

  ! store pointers in KIM object
  call kim_api_setm_method(pkim, ier, &
       "compute", one, c_funloc(Compute_Energy_Forces), 1, &
       "reinit",  one, c_funloc(ReInit),                1, &
       "destroy", one, c_funloc(Destroy),               1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_method", ier)
     goto 42
  endif

  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     goto 42
  endif
  call c_f_pointer(pcutoff, model_cutoff)
  CUTOFF_VALUE_STR

  ! Allocate memory for parameters
  allocate( model_epsilon )
  allocate( model_sigma   )
  allocate( model_Pcutoff )
  allocate( model_cutnorm )
  allocate( model_A       )
  allocate( model_B       )
  allocate( model_C       )
  allocate( model_sigmasq )
  allocate( model_cutsq   )

  SIGMA_VALUE_STR

  EPSILON_VALUE_STR

  model_Pcutoff = model_cutoff

  model_cutnorm = model_cutoff/model_sigma

  model_A = 12.0_cd*model_epsilon*(-26.0_cd + 7.0_cd*model_cutnorm**6)/ &
       (model_cutnorm**14*model_sigma**2)

  model_B = 96.0_cd*model_epsilon*(7.0_cd-2.0_cd*model_cutnorm**6)/     &
       (model_cutnorm**13*model_sigma)

  model_C = 28.0_cd*model_epsilon*(-13.0_cd+4.0_cd*model_cutnorm**6)/   &
       (model_cutnorm**12)

  model_sigmasq = model_sigma**2

  model_cutsq = model_cutoff**2


  call kim_api_setm_data(pkim, ier, &
       "PARAM_FREE_sigma",    one, c_loc(model_sigma),    1, &
       "PARAM_FREE_epsilon",  one, c_loc(model_epsilon),  1, &
       "PARAM_FREE_cutoff",   one, c_loc(model_Pcutoff),  1, &
       "PARAM_FIXED_cutnorm", one, c_loc(model_cutnorm),  1, &
       "PARAM_FIXED_A",       one, c_loc(model_A),        1, &
       "PARAM_FIXED_B",       one, c_loc(model_B),        1, &
       "PARAM_FIXED_C",       one, c_loc(model_C),        1, &
       "PARAM_FIXED_sigmasq", one, c_loc(model_sigmasq),  1, &
       "PARAM_FIXED_cutsq",   one, c_loc(model_cutsq),    1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data", ier)
     goto 42
  endif

  ier = KIM_STATUS_OK
42 continue
  model_init = ier
  return

end function model_init
