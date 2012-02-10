!*******************************************************************************
!**
!**  MODULE model_ArNe_P_MLJ_NEIGH_RVEC_F
!**
!**  Modified Lennard-Jones P potential model for argon and neon
!**  (modified to have smooth cutoff)
!**  (uses parameters listed in Modeling Materials, by Tadmor and Miller
!**   and the Lorentz/Berthelot Mixing Rule)
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************

#include "KIMstatus.h"
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module model_ArNe_P_MLJ_NEIGH_RVEC_F
  use KIM_API
  implicit none


  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy
  
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
    double precision r,Rsqij,phi,dphi,d2phi,dEidr
    double precision CurEpsilon, CurSigma, CurA, CurB, CurC
    integer i,j,jj,numnei,atom,atom_ret
    
    !-- KIM variables
    integer numberOfParticles;         pointer(pnAtoms,numberOfParticles)
    integer nparticleTypes;            pointer(pnparticleTypes,nparticleTypes)
    integer atomTypes(1);          pointer(patomTypes,atomTypes)
    real*8 model_cutoff;           pointer(pcutoff,model_cutoff)
    real*8 model_epsilon(1);       pointer(pepsilon,model_epsilon)
    real*8 model_sigma(1);         pointer(psigma,model_sigma)
    real*8 model_cutnorm(1);       pointer(pcutnorm,model_cutnorm)
    real*8 model_A(1);             pointer(pA,model_A)
    real*8 model_B(1);             pointer(pB,model_B)
    real*8 model_C(1);             pointer(pC,model_C)
    real*8 model_sigmasq(1);       pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;            pointer(pcutsq,model_cutsq)
    real*8 energy;                 pointer(penergy,energy)
    real*8 coordum(DIM,1);         pointer(pcoor,coordum)
    real*8 forcedum(DIM,1);        pointer(pforce,forcedum)
    real*8 enepotdum(1);           pointer(penepot,enepotdum)
    real*8 virialdum(1);     pointer(pvirial,virialdum)
    real*8 Rij(3,1);               pointer(pRij,Rij)
    integer nei1atom(1);           pointer(pnei1atom,nei1atom)
    real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
    integer :: comp_force, comp_enepot, comp_virial, comp_energy
    integer :: idum

    ! Check to see if we have been asked to compute the forces, energyperatom, 
    ! energy and virial
    call kim_api_getm_compute_f(pkim, ier, &
         "energy",        comp_energy,  1, &
         "forces",        comp_force,   1, &
         "particleEnergy", comp_enepot,  1, &
         "virial",  comp_virial,  1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_compute_f", ier)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data_f(pkim, ier, &
         "numberOfParticles",       pnAtoms,       1,                           &
         "numberParticleTypes",     pnparticleTypes,   1,                           &
         "atomTypes",           patomTypes,    1,                           &
         "cutoff",              pcutoff,       1,                           &
         "coordinates",         pcoor,         1,                           &
         "energy",              penergy,       TRUEFALSE(comp_energy.eq.1), &
         "forces",              pforce,        TRUEFALSE(comp_force.eq.1),  &
         "particleEnergy",       penepot,       TRUEFALSE(comp_enepot.eq.1), &
         "virial",        pvirial, TRUEFALSE(comp_virial.eq.1))
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
       return
    endif

    ! Cast to F90 arrays
    call KIM_to_F90_real_array_2d(coordum,coor,DIM,numberOfParticles)
    if (comp_force.eq.1)  call KIM_to_F90_real_array_2d(forcedum,force,DIM,numberOfParticles)
    if (comp_enepot.eq.1) call KIM_to_F90_real_array_1d(enepotdum,ene_pot,numberOfParticles)
    if (comp_virial.eq.1) call KIM_to_F90_real_array_1d(virialdum,virial_global,6)

    ! Unpack parameters from KIM object
    call kim_api_getm_data_f(pkim, ier, &
         "PARAM_FREE_epsilon",  pepsilon, 1, &
         "PARAM_FREE_sigma",    psigma,   1, &
         "PARAM_FIXED_cutnorm", pcutnorm, 1, &
         "PARAM_FIXED_A",       pA,       1, &
         "PARAM_FIXED_B",       pB,       1, &
         "PARAM_FIXED_C",       pC,       1, &
         "PARAM_FIXED_sigmasq", psigmasq, 1, &
         "PARAM_FIXED_cutsq",   pcutsq,   1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
       return
    endif

    ! Check to be sure that the atom types are correct
    ier = KIM_STATUS_FAIL ! assume an error
    do i = 1,numberOfParticles
       if (.not. ( (atomTypes(i).eq.Ar) .or. (atomTypes(i).eq.Ne) ) ) then
          idum = kim_api_report_error_f(__LINE__, __FILE__, "Wrong Atom Type", ier)
          return
       endif
    enddo
    ier = KIM_STATUS_OK ! everything is ok

    
    ! Initialize potential energies, forces, virial term
    !
    if (comp_enepot.eq.1) ene_pot(1:numberOfParticles)   = 0.d0
    if (comp_energy.eq.1) energy                     = 0.d0
    if (comp_force.eq.1)  force(1:3,1:numberOfParticles) = 0.d0
    if (comp_virial.eq.1) virial_global              = 0.d0


    !  Compute energy and forces
    !
    do i = 1,numberOfParticles
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       ier = kim_api_get_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij)
       if (ier.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_f_f", ier)
          return
       endif
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
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
                  r, phi, dphi, d2phi)                      ! compute pair potential
             dEidr = 0.5d0*dphi                             ! compute dEidr
             if (comp_enepot.eq.1) then                     !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi         ! accumulate energy
             elseif (comp_energy.eq.1) then                 !
                energy = energy + 0.5d0*phi                 ! full neigh case
             endif                                          !
             if (comp_virial.eq.1) then                     ! accumul. virial
                virial_global(1) = virial_global(1) + Rij(1,jj)*Rij(1,jj)*dEidr/r
                virial_global(2) = virial_global(2) + Rij(2,jj)*Rij(2,jj)*dEidr/r
                virial_global(3) = virial_global(3) + Rij(3,jj)*Rij(3,jj)*dEidr/r
                virial_global(4) = virial_global(4) + Rij(2,jj)*Rij(3,jj)*dEidr/r
                virial_global(5) = virial_global(5) + Rij(1,jj)*Rij(3,jj)*dEidr/r
                virial_global(6) = virial_global(6) + Rij(1,jj)*Rij(2,jj)*dEidr/r
             endif                                          !
             if (comp_force.eq.1) then                      !
                force(:,i) = force(:,i) + dEidr*Rij(:,jj)/r ! accumulate force on atom i
                force(:,j) = force(:,j) - dEidr*Rij(:,jj)/r ! accumulate force on atom j
             endif
          endif
       enddo
    enddo
    
    if (comp_enepot.eq.1 .and. comp_energy.eq.1) &
       energy = sum(ene_pot(1:numberOfParticles))                     ! compute total energy
    
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
    integer ier, idum
    
    ! Get (changed) parameters from KIM object ---------------------------------
    call kim_api_getm_data_f(pkim, ier, &
         "PARAM_FREE_sigma",   psigma,    1, &
         "PARAM_FREE_epsilon", pepsilon,  1, &
         "PARAM_FREE_cutoff",  pparamcut, 1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
       stop
    endif
    
    ! Set new values in KIM object ---------------------------------------------
    call kim_api_getm_data_f(pkim, ier, &
         "cutoff",              pcutoff,  1, &
         "PARAM_FIXED_cutnorm", pcutnorm, 1, &
         "PARAM_FIXED_A",       pA,       1, &
         "PARAM_FIXED_B",       pB,       1, &
         "PARAM_FIXED_C",       pC,       1, &
         "PARAM_FIXED_sigmasq", psigmasq, 1, &
         "PARAM_FIXED_cutsq",   pcutsq,   1)

    model_cutoff = model_Pcutoff
    model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
    model_A(1:3) = 12.d0*model_epsilon(1:3)*(-26.d0 + 7.d0*model_cutnorm(1:3)**6)/ &
         (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
    model_B(1:3) = 96.d0*model_epsilon(1:3)*(7.d0-2.d0*model_cutnorm(1:3)**6)/     &
         (model_cutnorm(1:3)**13*model_sigma(1:3))
    model_C(1:3) = 28.d0*model_epsilon(1:3)*(-13.d0+4.d0*model_cutnorm(1:3)**6)/   &
         (model_cutnorm(1:3)**12)
    model_sigmasq(1:3) = model_sigma(1:3)**2
    model_cutsq = model_cutoff**2
    
  end subroutine ReInit

!-------------------------------------------------------------------------------
!
! Model destory routine (REQUIRED)
!
!-------------------------------------------------------------------------------
  subroutine Destroy(pkim)
    use KIM_API
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
    integer ier, idum
    
    ! get parameters from KIM object
    call kim_api_getm_data_f(pkim, ier, &
         "PARAM_FREE_sigma",     psigma,    1, &
         "PARAM_FREE_epsilon",   pepsilon,  1, &
         "PARAM_FREE_cutoff",    pparamcut, 1, &
         "PARAM_FIXED_cutnorm",  pcutnorm,  1, &
         "PARAM_FIXED_A",        pA,        1, &
         "PARAM_FIXED_B",        pB,        1, &
         "PARAM_FIXED_C",        pC,        1, &
         "PARAM_FIXED_sigmasq",  psigmasq,  1, &
         "PARAM_FIXED_cutsq",    pcutsq,    1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
       stop
    endif

    ! free memory
    call free(psigma)
    call free(pepsilon)
    call free(pparamcut)
    call free(pcutnorm)
    call free(pA)
    call free(pB)
    call free(pC)
    call free(psigmasq)
    call free(pcutsq)

  end subroutine Destroy

end module model_ArNe_P_MLJ_NEIGH_RVEC_F


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_ArNe_P_MLJ_NEIGH_RVEC_F_init(pkim)
  use model_ArNe_P_MLJ_NEIGH_RVEC_F
  use KIM_API
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
  integer ier, idum
  
  ! store function pointers in KIM object
  call kim_api_setm_data_f(pkim, ier, &
       "compute", one, loc(Compute_Energy_Forces), 1, &
       "reinit",  one, loc(ReInit),                1, &
       "destroy", one, loc(Destroy),               1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_setm_data_f", ier)
     stop
  endif
  
  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  model_cutoff = 8.15d0 ! cutoff distance in angstroms
  
  ! Allocate memory for sigma and store value
  psigma = malloc(three*8) ! 8 is the size of a real*8
  if (psigma.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pepsilon = malloc(three*8) ! 8 is the size of a real*8
  if (pepsilon.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pparamcut = malloc(one*8) ! 8 is the size of a real*8
  if (pparamcut.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pcutnorm = malloc(three*8) ! 8 is the size of a real*8
  if (pcutnorm.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pA = malloc(three*8) ! 8 is the size of a real*8
  if (pA.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pB = malloc(three*8) ! 8 is the size of a real*8
  if (pB.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pC = malloc(three*8) ! 8 is the size of a real*8
  if (pC.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  psigmasq = malloc(three*8) ! 8 is the size of a real*8
  if (psigmasq.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif
  pcutsq = malloc(one*8) ! 8 is the size of a real*8
  if (pcutsq.eq.0) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL)
     stop
  endif

  ! store parameters in KIM object
  call kim_api_setm_data_f(pkim, ier, &
       "PARAM_FREE_sigma",    three,  psigma,    1, &
       "PARAM_FREE_epsilon",  three,  pepsilon,  1, &
       "PARAM_FREE_cutoff",   one,    pparamcut, 1, &
       "PARAM_FIXED_cutnorm", three,  pcutnorm,  1, &
       "PARAM_FIXED_A",       three,  pA,        1, &
       "PARAM_FIXED_B",       three,  pB,        1, &
       "PARAM_FIXED_C",       three,  pC,        1, &
       "PARAM_FIXED_sigmasq", three,  psigmasq,  1, &
       "PARAM_FIXED_cutsq",   one,    pcutsq,    1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_setm_data_f", ier)
     stop
  endif

  model_sigma(1) = 3.40d0 ! LJ Argon sigma in angstroms
  model_sigma(2) = 2.74d0 ! LJ Neon  sigma in angstroms
  model_sigma(3) = 0.5d0*(model_sigma(1) + model_sigma(2)) ! Lorentz/Berthelot Mixing Rule
  model_epsilon(1) = 0.0104d0 ! LJ Argon epsilon in eV
  model_epsilon(2) = 0.0031d0 ! LJ Neon  epsilon in eV
  model_epsilon(3) = sqrt(model_epsilon(1)*model_epsilon(2)) ! Lorentz/Berthelot Mixing Rule
  model_Pcutoff = model_cutoff
  model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
  model_A(1:3) = 12.d0*model_epsilon(1:3)*(-26.d0 + 7.d0*model_cutnorm(1:3)**6)/ &
       (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
  model_B(1:3) = 96.d0*model_epsilon(1:3)*(7.d0-2.d0*model_cutnorm(1:3)**6)/     &
       (model_cutnorm(1:3)**13*model_sigma(1:3))
  model_C(1:3) = 28.d0*model_epsilon(1:3)*(-13.d0+4.d0*model_cutnorm(1:3)**6)/   &
       (model_cutnorm(1:3)**12)
  model_sigmasq(1:3) = model_sigma(1:3)**2
  model_cutsq = model_cutoff**2

end subroutine model_ArNe_P_MLJ_NEIGH_RVEC_F_init
