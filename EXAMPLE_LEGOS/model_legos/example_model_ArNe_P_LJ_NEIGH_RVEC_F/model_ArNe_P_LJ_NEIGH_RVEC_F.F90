!*******************************************************************************
!**
!**  MODULE model_ArNe_P_LJ_NEIGH_RVEC_F
!**
!**  Lennard-Jones P potential model for argon and neon
!**  (modified to have smooth cutoff)
!**  (uses parameters listed in Modeling Materials, by Tadmor and Miller
!**   and the Lorentz/Berthelot Mixing Rule)
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************

module model_ArNe_P_LJ_NEIGH_RVEC_F
  use KIMservice
  implicit none


  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy
  public report_error
  
  ! Species indices
  integer, parameter :: Ar = 1
  integer, parameter :: Ne = 2
  
contains
  
!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
  subroutine Compute_Energy_Forces(pkim,ier)
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in)  :: pkim
    integer,                  intent(out) :: ier
    
    !-- Local variables
    integer, parameter :: DIM=3
    double precision :: r,Rsqij,phi,dphi,d2phi
    double precision :: CurEpsilon, CurSigma, CurA, CurB, CurC
    integer :: i,jj,numnei,atom,atom_ret
    
    !-- KIM variables
    integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
    integer nAtomTypes;  pointer(pnAtomTypes,nAtomTypes)
    integer atomTypes(1);  pointer(patomTypes,atomTypes)
    

    real*8 model_cutoff;        pointer(pcutoff,model_cutoff)
    real*8 model_epsilon(1);    pointer(pepsilon,model_epsilon)
    real*8 model_sigma(1);      pointer(psigma,model_sigma)
    real*8 model_cutnorm(1);    pointer(pcutnorm,model_cutnorm)
    real*8 model_A(1);          pointer(pA,model_A)
    real*8 model_B(1);          pointer(pB,model_B)
    real*8 model_C(1);          pointer(pC,model_C)
    real*8 model_sigmasq(1);    pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;         pointer(pcutsq,model_cutsq)
    real*8 energy;              pointer(penergy,energy)
    real*8 coordum(DIM,1);      pointer(pcoor,coordum)
    real*8 forcedum(DIM,1);     pointer(pforce,forcedum)
    real*8 enepotdum(1);        pointer(penepot,enepotdum)
    real*8 virial;              pointer(pvirial,virial)
    real*8 Rij(3,1);            pointer(pRij,Rij)
    integer nei1atom(1);        pointer(pnei1atom,nei1atom)
    real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
    integer :: comp_force, comp_enepot, comp_virial
    integer N4     !@@@@@@@@@ NEEDS TO BE FIXED
    
    ! Unpack data from KIM object
    !
    pnAtoms = kim_api_get_data_f(pkim,"numberOfAtoms",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pnAtomTypes = kim_api_get_data_f(pkim,"numberAtomTypes",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    patomTypes = kim_api_get_data_f(pkim,"atomTypes",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pcutoff = kim_api_get_data_f(pkim,"cutoff",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    penergy = kim_api_get_data_f(pkim,"energy",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    pcoor = kim_api_get_data_f(pkim,"coordinates",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       return
    endif
    
    ! Check to see if we have been asked to compute the forces, energyperatom, and virial
    comp_force  = kim_api_isit_compute_f(pkim,"forces",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_isit_compute", ier);
       return
    endif
    comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_isit_compute", ier);
       return
    endif
    comp_virial = kim_api_isit_compute_f(pkim,"virial",ier);
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_isit_compute", ier);
       return
    endif
    
    ! Cast to F90 arrays
    N4=numberOfAtoms
    if (comp_force.eq.1) then 
       pforce  = kim_api_get_data_f(pkim,"forces",ier);        
       if (ier.le.0) then
          call report_error(__LINE__, "kim_api_get_data", ier);
          return
       endif
       call toRealArrayWithDescriptor2d(forcedum,force,DIM,N4)
    endif
    if (comp_enepot.eq.1) then 
       penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier); 
       if (ier.le.0) then
          call report_error(__LINE__, "kim_api_get_data", ier);
          return
       endif
       call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N4)
    endif
    if (comp_virial.eq.1) then
       pvirial = kim_api_get_data_f(pkim,"virial",ier);
       if (ier.le.0) then
          call report_error(__LINE__, "kim_api_get_data", ier);
          return
       endif
    endif
    call toRealArrayWithDescriptor2d(coordum,coor,DIM,N4)


    ! Check to be sure that the atom types are correct
    ier = 0 ! assume an error
    do i = 1,numberOfAtoms
       if (.not. ( (atomTypes(i).eq.Ar) .or. (atomTypes(i).eq.Ne) ) ) then
          call report_error(__LINE__, "Wrong Atom Type", i);
          return
       endif
    enddo
    ier = 1 ! everything is ok

    
    ! Initialize potential energies, forces, virial term
    !
    if (comp_enepot.eq.1) then
       ene_pot(1:numberOfAtoms) = 0.d0
    else
       energy = 0.d0
    endif
    if (comp_force.eq.1)  force(1:3,1:numberOfAtoms) = 0.d0
    if (comp_virial.eq.1) virial = 0.d0


    !  Compute energy and forces
    !
    do i = 1,numberOfAtoms
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       ier = kim_api_get_full_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij)
       if (ier.le.0) then
          call report_error(__LINE__, "kim_api_get_full_neigh", ier);
          return
       endif
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          Rsqij = dot_product(Rij(:,jj),Rij(:,jj))         ! compute square distance
          if ( Rsqij < model_cutsq ) then                  ! particles are interacting?
             r = sqrt(Rsqij)                               ! compute distance
             if ((atomTypes(i).eq.Ar).and.(atomTypes(nei1atom(jj)).eq.Ar)) then
                CurEpsilon = model_epsilon(Ar)
                CurSigma   = model_sigma(Ar)
                CurA       = model_A(Ar)
                CurB       = model_B(Ar)
                CurC       = model_C(Ar)
             else if ((atomTypes(i).eq.Ne).and.(atomTypes(nei1atom(jj)).eq.Ne)) then
                CurEpsilon = model_epsilon(Ne)
                CurSigma   = model_sigma(Ne)
                CurA       = model_A(Ne)
                CurB       = model_B(Ne)
                CurC       = model_C(Ne)
             else
                CurEpsilon = model_epsilon(Ar+Ne)
                CurSigma   = model_sigma(Ar+Ne)
                CurA       = model_A(Ar+Ne)
                CurB       = model_B(Ar+Ne)
                CurC       = model_C(Ar+Ne)
             endif
             call pair(CurEpsilon, CurSigma, CurA, CurB, CurC, &
                  r, phi, dphi, d2phi)                     ! compute pair potential
             if (comp_enepot.eq.1) then                    !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi        ! accumulate energy
             else                                          !
                energy = energy + 0.5d0*phi                ! full neigh case
             endif                                         !
             if (comp_virial.eq.1) then                    !
                virial = virial + 0.5d0*r*dphi             ! accumul. virial=sum r(dV/dr)
             endif                                         !
             if (comp_force.eq.1) then                     !
                force(:,i) = force(:,i) + dphi*Rij(:,jj)/r ! accumulate forces
             endif
          endif
       enddo
    enddo
    
    if (comp_virial.eq.1) virial = - virial/DIM                   ! definition of virial term
    if (comp_enepot.eq.1) energy = sum(ene_pot(1:numberOfAtoms))  ! compute total energy
    
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
    double precision :: rsq,sor,sor6,sor12
    
    rsq  = r*r             !  r^2
    sor  = sigma/r   !  (sig/r)
    sor6 = sor*sor*sor
    sor6 = sor6*sor6       !  (sig/r)^6
    sor12= sor6*sor6       !  (sig/r)^12
    
    phi   =  4.d0*epsilon*(sor12-sor6) + A*rsq + B*r + C
    dphi  = 24.d0*epsilon*(-2.d0*sor12+sor6)/r  + 2.d0*A*r + B
    d2phi = 24.d0*epsilon*(26.d0*sor12-7.d0*sor6)/rsq + 2.d0*A
    
  end subroutine pair
  
!-------------------------------------------------------------------------------
!
! Model reinitialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
  subroutine ReInit(pkim)
    implicit none
    
    !-- Transferred variables
    integer(kind=kim_intptr), intent(in) :: pkim
    
    !-- Local variables
    real*8 model_cutoff;     pointer(pcutoff,model_cutoff)
    real*8 model_epsilon(1); pointer(pepsilon,model_epsilon)
    real*8 model_sigma(1);   pointer(psigma,model_sigma)
    real*8 model_Pcutoff;    pointer(pparamcut,model_Pcutoff)
    real*8 model_cutnorm(1); pointer(pcutnorm,model_cutnorm)
    real*8 model_A(1);       pointer(pA,model_A)
    real*8 model_B(1);       pointer(pB,model_B)
    real*8 model_C(1);       pointer(pC,model_C)
    real*8 model_sigmasq(1); pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;      pointer(pcutsq,model_cutsq)
    integer ier
    
    ! Get (changed) parameters from KIM object ---------------------------------

    ! get sigma from KIM object
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    
    ! get epsilon from KIM object
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    
    ! get cutoff parameter from KIM object
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    
    ! Set new values in KIM object ---------------------------------------------
    
    ! store model cutoff in KIM object
    pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_cutoff = model_Pcutoff
    
    ! store cutnorm in KIM object
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
    
    ! store A in KIM object
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_A(1:3) = 12.d0*model_epsilon(1:3)*(-26.d0 + 7.d0*model_cutnorm(1:3)**6)/ &
         (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
    
    ! store B in KIM object
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_B(1:3) = 96.d0*model_epsilon(1:3)*(7.d0-2.d0*model_cutnorm(1:3)**6)/     &
         (model_cutnorm(1:3)**13*model_sigma(1:3))
    
    ! store C in KIM object
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_C(1:3) = 28.d0*model_epsilon(1:3)*(-13.d0+4.d0*model_cutnorm(1:3)**6)/   &
         (model_cutnorm(1:3)**12)
    
    ! store sigma^2 in KIM object
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_sigmasq(1:3) = model_sigma(1:3)**2
    
    ! store cutoff^2 in KIM object
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    model_cutsq = model_cutoff**2
    
  end subroutine ReInit

!-------------------------------------------------------------------------------
!
! Model reinitialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
  subroutine Destroy(pkim)
    use KIMservice
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in) :: pkim
    
    !-- Local variables
    real*8 model_cutoff;     pointer(pcutoff,model_cutoff)
    real*8 model_epsilon(1); pointer(pepsilon,model_epsilon)
    real*8 model_sigma(1);   pointer(psigma,model_sigma)
    real*8 model_Pcutoff;    pointer(pparamcut,model_Pcutoff)
    real*8 model_cutnorm(1); pointer(pcutnorm,model_cutnorm)
    real*8 model_A(1);       pointer(pA,model_A)
    real*8 model_B(1);       pointer(pB,model_B)
    real*8 model_C(1);       pointer(pC,model_C)
    real*8 model_sigmasq(1); pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;      pointer(pcutsq,model_cutsq)
    integer ier
    
    ! get sigma from KIM object and free memory
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(psigma)
    
    ! get epsilon from KIM object and free memory
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pepsilon)
    
    ! get cutoff parameter from KIM object and free memory
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pparamcut)
    
    ! get cutnorm in KIM object and free memory
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pcutnorm)
    
    ! get A in KIM object and free memory
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pA)
    
    ! get B in KIM object and free memory
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pB)
    
    ! get C in KIM object and free memory
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pC)
    
    ! get sigma^2 in KIM object and free memory
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(psigmasq)

    ! get cutoff^2 in KIM object and free memory
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.le.0) then
       call report_error(__LINE__, "kim_api_get_data", ier);
       stop
    endif
    call free(pcutsq)

  end subroutine Destroy

  subroutine report_error(line, str, status)
    implicit none

    !-- Transferred variables
    integer,   intent(in) :: line
    character(len=*), intent(in) :: str
    integer,   intent(in) :: status

    !-- Local variables
    character(len=10000), parameter :: file = __FILE__

    !-- print the error message
    print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status

  end subroutine report_error
  
end module model_ArNe_P_LJ_NEIGH_RVEC_F


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_ArNe_P_LJ_NEIGH_RVEC_F_init(pkim)
  use model_ArNe_P_LJ_NEIGH_RVEC_F
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in) :: pkim
  
  !-- Local variables
  integer(kind=kim_intptr), parameter :: one=1
  integer(kind=kim_intptr), parameter :: three=3
  real*8 model_cutoff;     pointer(pcutoff,model_cutoff)
  real*8 model_epsilon(1); pointer(pepsilon,model_epsilon)
  real*8 model_sigma(1);   pointer(psigma,model_sigma)
  real*8 model_Pcutoff;    pointer(pparamcut,model_Pcutoff)
  real*8 model_cutnorm(1); pointer(pcutnorm,model_cutnorm)
  real*8 model_A(1);       pointer(pA,model_A)
  real*8 model_B(1);       pointer(pB,model_B)
  real*8 model_C(1);       pointer(pC,model_C)
  real*8 model_sigmasq(1); pointer(psigmasq,model_sigmasq)
  real*8 model_cutsq;      pointer(pcutsq,model_cutsq)
  integer ier
  
  ! store pointer to compute function in KIM object
  if (kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces)).ne.1) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  
  ! store pointer to reinit function in KIM object
  if (kim_api_set_data_f(pkim,"reinit",one,loc(ReInit)).ne.1) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif

  ! store pointer to destroy function in KIM object
  if (kim_api_set_data_f(pkim,"destroy",one,loc(Destroy)).ne.1) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  
  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data", ier);
     stop
  endif
  model_cutoff = 8.15d0 ! cutoff distance in angstroms
  
  ! Allocate memory for sigma and store value
  psigma = malloc(three*8) ! 8 is the size of a real*8
  ! store sigma in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_sigma",three,psigma)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_sigma(1) = 3.40d0 ! LJ Argon sigma in angstroms
  model_sigma(2) = 2.74d0 ! LJ Neon  sigma in angstroms
  model_sigma(3) = 0.5d0*(model_sigma(1) + model_sigma(2)) ! Lorentz/Berthelot Mixing Rule
  
  ! Allocate memory for epsilon and store value
  pepsilon = malloc(three*8) ! 8 is the size of a real*8
  ! store epsilon in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_epsilon",three,pepsilon)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_epsilon(1) = 0.0104d0 ! LJ Argon epsilon in eV
  model_epsilon(2) = 0.0031d0 ! LJ Neon  epsilon in eV
  model_epsilon(3) = sqrt(model_epsilon(1)*model_epsilon(2)) ! Lorentz/Berthelot Mixing Rule

  ! Allocate memory for parameter cutoff and store value
  pparamcut = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff as parameter in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_cutoff",one,pparamcut)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_Pcutoff = model_cutoff
  
  ! Allocate memory for parameter cutnorm and store value
  pcutnorm = malloc(three*8) ! 8 is the size of a real*8
  ! store cutnorm in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutnorm",three,pcutnorm)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
  
  ! Allocate memory for parameter A and store value
  pA = malloc(three*8) ! 8 is the size of a real*8
  ! store A in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_A",three,pA)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_A(1:3) = 12.d0*model_epsilon(1:3)*(-26.d0 + 7.d0*model_cutnorm(1:3)**6)/ &
       (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
  
  ! Allocate memory for parameter B and store value
  pB = malloc(three*8) ! 8 is the size of a real*8
  ! store B in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_B",three,pB)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_B(1:3) = 96.d0*model_epsilon(1:3)*(7.d0-2.d0*model_cutnorm(1:3)**6)/     &
       (model_cutnorm(1:3)**13*model_sigma(1:3))
  
  ! Allocate memory for parameter C and store value
  pC = malloc(three*8) ! 8 is the size of a real*8
  ! store C in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_C",three,pC)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_C(1:3) = 28.d0*model_epsilon(1:3)*(-13.d0+4.d0*model_cutnorm(1:3)**6)/   &
       (model_cutnorm(1:3)**12)
  
  ! Allocate memory for parameter sigmasq and store value
  psigmasq = malloc(three*8) ! 8 is the size of a real*8
  ! store sigma^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_sigmasq",three,psigmasq)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_sigmasq(1:3) = model_sigma(1:3)**2
  
  ! Allocate memory for parameter cutsq and store value
  pcutsq = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutsq",one,pcutsq)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data", ier);
     stop
  endif
  model_cutsq = model_cutoff**2

end subroutine model_ArNe_P_LJ_NEIGH_RVEC_F_init
