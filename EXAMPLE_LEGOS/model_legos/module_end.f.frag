
  end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Pair potential: Lennard-Jones with smooth cutoff imposed by Ar^2 + Br + C
!
!-------------------------------------------------------------------------------
  subroutine pair(epsilon,sigma,A,B,C,r,phi,dphi,d2phi)
    implicit none

    !-- Transferred variables
    double precision, intent(in)  :: epsilon, sigma, A, B, C
    double precision, intent(in)  :: r
    double precision, intent(out) :: phi, dphi, d2phi

    !-- Local variables
    double precision rsq,sor,sor6,sor12

    rsq  = r*r             !  r^2
    sor  = sigma/r         !  (sig/r)
    sor6 = sor*sor*sor     !
    sor6 = sor6*sor6       !  (sig/r)^6
    sor12= sor6*sor6       !  (sig/r)^12

    phi   =  4.d0*epsilon*(sor12-sor6) + A*rsq + B*r + C
    dphi  = 24.d0*epsilon*(-2.d0*sor12+sor6)/r  + 2.d0*A*r + B
    d2phi = 24.d0*epsilon*(26.d0*sor12-7.d0*sor6)/rsq + 2.d0*A

  end subroutine pair

!-------------------------------------------------------------------------------
!
! Model reinitialization routine
!
!-------------------------------------------------------------------------------
  integer function ReInit(pkim)
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in) :: pkim

    !-- Local variables
    real*8 model_cutoff;  pointer(pcutoff,model_cutoff)
    real*8 model_epsilon; pointer(pepsilon,model_epsilon)
    real*8 model_sigma;   pointer(psigma,model_sigma)
    real*8 model_Pcutoff; pointer(pparamcut,model_Pcutoff)
    real*8 model_cutnorm; pointer(pcutnorm,model_cutnorm)
    real*8 model_A;       pointer(pA,model_A)
    real*8 model_B;       pointer(pB,model_B)
    real*8 model_C;       pointer(pC,model_C)
    real*8 model_sigmasq; pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;   pointer(pcutsq,model_cutsq)
    integer idum

    ! Get (changed) parameters from KIM object ---------------------------------

    ! get stuff from KIM object
    call kim_api_getm_data_f(pkim, ReInit, &
         "PARAM_FREE_sigma",   psigma,    1, &
         "PARAM_FREE_epsilon", pepsilon,  1, &
         "PARAM_FREE_cutoff",  pparamcut, 1)
    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_data_f", ReInit)
       return
    endif

    ! Set new values in KIM object ---------------------------------------------

    ! Set stuff in KIM object
    call kim_api_getm_data_f(pkim, ReInit, &
         "cutoff",              pcutoff,  1, &
         "PARAM_FIXED_cutnorm", pcutnorm, 1, &
         "PARAM_FIXED_A",       pA,       1, &
         "PARAM_FIXED_B",       pB,       1, &
         "PARAM_FIXED_C",       pC,       1, &
         "PARAM_FIXED_sigmasq", psigmasq, 1, &
         "PARAM_FIXED_cutsq",   pcutsq,   1)
    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_data_f", ReInit)
       return
    endif

    model_cutoff = model_Pcutoff
    model_cutnorm = model_cutoff/model_sigma
    model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
         (model_cutnorm**14*model_sigma**2)
    model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
         (model_cutnorm**13*model_sigma)
    model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
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
  integer function Destroy(pkim)
    use KIM_API
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in) :: pkim

    !-- Local variables
    real*8 model_epsilon; pointer(pepsilon,model_epsilon)
    real*8 model_sigma;   pointer(psigma,model_sigma)
    real*8 model_Pcutoff; pointer(pparamcut,model_Pcutoff)
    real*8 model_cutnorm; pointer(pcutnorm,model_cutnorm)
    real*8 model_A;       pointer(pA,model_A)
    real*8 model_B;       pointer(pB,model_B)
    real*8 model_C;       pointer(pC,model_C)
    real*8 model_sigmasq; pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;   pointer(pcutsq,model_cutsq)
    integer idum


    call kim_api_getm_data_f(pkim, Destroy, &
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
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_data_f", Destroy)
       return
    endif
    call free(psigma)
    call free(pepsilon)
    call free(pparamcut)
    call free(pcutnorm)
    call free(pA)
    call free(pB)
    call free(pC)
    call free(psigmasq)
    call free(pcutsq)

    Destroy = KIM_STATUS_OK
    return

  end function Destroy

end module MODEL_NAME_STR


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer function model_init(pkim)
  use MODEL_NAME_STR
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in) :: pkim

  !-- Local variables
  integer(kind=kim_intptr), parameter :: one=1
  real*8 model_cutoff;  pointer(pcutoff,model_cutoff)
  real*8 model_epsilon; pointer(pepsilon,model_epsilon)
  real*8 model_sigma;   pointer(psigma,model_sigma)
  real*8 model_Pcutoff; pointer(pparamcut,model_Pcutoff)
  real*8 model_cutnorm; pointer(pcutnorm,model_cutnorm)
  real*8 model_A;       pointer(pA,model_A)
  real*8 model_B;       pointer(pB,model_B)
  real*8 model_C;       pointer(pC,model_C)
  real*8 model_sigmasq; pointer(psigmasq,model_sigmasq)
  real*8 model_cutsq;   pointer(pcutsq,model_cutsq)
  integer ier, idum

  ! store pointers in KIM object
  call kim_api_setm_data_f(pkim, ier, &
       "compute", one, loc(Compute_Energy_Forces), 1, &
       "reinit",  one, loc(ReInit),                1, &
       "destroy", one, loc(Destroy),               1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_setm_data_f", ier)
     goto 42
  endif

  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_data", ier)
     goto 42
  endif
  CUTOFF_VALUE_STR

  ! Allocate memory for stuff and store values
  psigma = malloc(one*8) ! 8 is the size of a real*8
  SIGMA_VALUE_STR

  pepsilon = malloc(one*8) ! 8 is the size of a real*8
  EPSILON_VALUE_STR

  pparamcut = malloc(one*8) ! 8 is the size of a real*8
  model_Pcutoff = model_cutoff

  pcutnorm = malloc(one*8) ! 8 is the size of a real*8
  model_cutnorm = model_cutoff/model_sigma

  pA = malloc(one*8) ! 8 is the size of a real*8
  model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
       (model_cutnorm**14*model_sigma**2)

  pB = malloc(one*8) ! 8 is the size of a real*8
  model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
       (model_cutnorm**13*model_sigma)

  pC = malloc(one*8) ! 8 is the size of a real*8
  model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
       (model_cutnorm**12)

  psigmasq = malloc(one*8) ! 8 is the size of a real*8
  model_sigmasq = model_sigma**2

  pcutsq = malloc(one*8) ! 8 is the size of a real*8
  model_cutsq = model_cutoff**2


  call kim_api_setm_data_f(pkim, ier, &
       "PARAM_FREE_sigma",    one, psigma,    1, &
       "PARAM_FREE_epsilon",  one, pepsilon,  1, &
       "PARAM_FREE_cutoff",   one, pparamcut, 1, &
       "PARAM_FIXED_cutnorm", one, pcutnorm,  1, &
       "PARAM_FIXED_A",       one, pA,        1, &
       "PARAM_FIXED_B",       one, pB,        1, &
       "PARAM_FIXED_C",       one, pC,        1, &
       "PARAM_FIXED_sigmasq", one, psigmasq,  1, &
       "PARAM_FIXED_cutsq",   one, pcutsq,    1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_setm_data_f", ier)
     goto 42
  endif

  ier = KIM_STATUS_OK
42 continue
  model_init = ier
  return

end function model_init
