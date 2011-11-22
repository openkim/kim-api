    
    if (comp_virial.eq.1) virial = - virial/DIM                   ! definition of virial term
    if ((comp_enepot.eq.1) .and. (comp_energy.eq.1)) then
       energy = sum(ene_pot(1:numberOfAtoms))                     ! compute total energy
    endif
    
  end subroutine Compute_Energy_Forces
  
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
  subroutine ReInit(pkim)
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
    integer ier
    
    ! Get (changed) parameters from KIM object ---------------------------------

    ! get sigma from KIM object
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    
    ! get epsilon from KIM object
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    
    ! get cutoff parameter from KIM object
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    
    ! Set new values in KIM object ---------------------------------------------
    
    ! store model cutoff in KIM object
    pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_cutoff = model_Pcutoff
    
    ! store cutnorm in KIM object
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_cutnorm = model_cutoff/model_sigma
    
    ! store A in KIM object
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
         (model_cutnorm**14*model_sigma**2)
    
    ! store B in KIM object
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
         (model_cutnorm**13*model_sigma)
    
    ! store C in KIM object
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
         (model_cutnorm**12)
    
    ! store sigma^2 in KIM object
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_sigmasq = model_sigma**2
    
    ! store cutoff^2 in KIM object
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    model_cutsq = model_cutoff**2
    
  end subroutine ReInit

!-------------------------------------------------------------------------------
!
! Model destroy routine
!
!-------------------------------------------------------------------------------
  subroutine Destroy(pkim)
    use KIMservice
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
    integer ier
    
    ! get sigma from KIM object and free memory
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(psigma)
    
    ! get epsilon from KIM object and free memory
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pepsilon)
    
    ! get cutoff parameter from KIM object and free memory
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pparamcut)
    
    ! get cutnorm in KIM object and free memory
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pcutnorm)
    
    ! get A in KIM object and free memory
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pA)
    
    ! get B in KIM object and free memory
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pB)
    
    ! get C in KIM object and free memory
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pC)
    
    ! get sigma^2 in KIM object and free memory
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(psigmasq)

    ! get cutoff^2 in KIM object and free memory
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.lt.KIM_STATUS_OK) then
       call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
       stop
    endif
    call free(pcutsq)

  end subroutine Destroy
  
end module MODEL_NAME_STR


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine MODEL_NAME_STR_init(pkim)
  use MODEL_NAME_STR
  use KIMservice
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
  integer ier
  
  ! store pointer to compute function in KIM object
  if (kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces)).ne.1) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  
  ! store pointer to reinit function in KIM object
  if (kim_api_set_data_f(pkim,"reinit",one,loc(ReInit)).ne.1) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif

  ! store pointer to destroy function in KIM object
  if (kim_api_set_data_f(pkim,"destroy",one,loc(Destroy)).ne.1) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  
  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
     stop
  endif
  CUTOFF_VALUE_STR
  
  ! Allocate memory for sigma and store value
  psigma = malloc(one*8) ! 8 is the size of a real*8
  ! store sigma in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_sigma",one,psigma)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  SIGMA_VALUE_STR
  
  ! Allocate memory for epsilon and store value
  pepsilon = malloc(one*8) ! 8 is the size of a real*8
  ! store epsilon in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_epsilon",one,pepsilon)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  EPSILON_VALUE_STR

  ! Allocate memory for parameter cutoff and store value
  pparamcut = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff as parameter in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_cutoff",one,pparamcut)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_Pcutoff = model_cutoff
  
  ! Allocate memory for parameter cutnorm and store value
  pcutnorm = malloc(one*8) ! 8 is the size of a real*8
  ! store cutnorm in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutnorm",one,pcutnorm)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier);
     stop
  endif
  model_cutnorm = model_cutoff/model_sigma
  
  ! Allocate memory for parameter A and store value
  pA = malloc(one*8) ! 8 is the size of a real*8
  ! store A in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_A",one,pA)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
       (model_cutnorm**14*model_sigma**2)
  
  ! Allocate memory for parameter B and store value
  pB = malloc(one*8) ! 8 is the size of a real*8
  ! store B in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_B",one,pB)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
       (model_cutnorm**13*model_sigma)
  
  ! Allocate memory for parameter C and store value
  pC = malloc(one*8) ! 8 is the size of a real*8
  ! store C in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_C",one,pC)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
       (model_cutnorm**12)
  
  ! Allocate memory for parameter sigmasq and store value
  psigmasq = malloc(one*8) ! 8 is the size of a real*8
  ! store sigma^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_sigmasq",one,psigmasq)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_sigmasq = model_sigma**2
  
  ! Allocate memory for parameter cutsq and store value
  pcutsq = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutsq",one,pcutsq)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
     stop
  endif
  model_cutsq = model_cutoff**2
  
end subroutine MODEL_NAME_STR_init
