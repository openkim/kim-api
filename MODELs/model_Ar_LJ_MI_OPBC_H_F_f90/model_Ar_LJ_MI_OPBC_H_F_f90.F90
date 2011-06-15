!*******************************************************************************
!**
!**  MODULE model_Ar_LJ_MI_OPBC_H_F_f90
!**
!**  Lennard-Jones pair potential model for argon 
!**  (modified to have smooth cutoff)
!**
!**  Author: Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************

module model_Ar_LJ_MI_OPBC_H_F_f90
  use KIMservice
  implicit none


  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy
  
  ! Species indices
  integer, parameter :: Ar = 1
  
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
    double precision, dimension(DIM) :: Rij
    double precision :: r,Rsqij,phi,dphi,d2phi
    integer :: i,j,jj,numnei,atom,atom_ret
    character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
    integer :: nbc                               ! 0 - half, 1 - full
    
    !-- KIM variables
    integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
    integer nAtomTypes;  pointer(pnAtomTypes,nAtomTypes)
    integer atomTypes(1);  pointer(patomTypes,atomTypes)
    

    real*8 model_cutoff;     pointer(pcutoff,model_cutoff)
    real*8 model_epsilon;    pointer(pepsilon,model_epsilon)
    real*8 model_sigma;      pointer(psigma,model_sigma)
    real*8 model_cutnorm;    pointer(pcutnorm,model_cutnorm)
    real*8 model_A;          pointer(pA,model_A)
    real*8 model_B;          pointer(pB,model_B)
    real*8 model_C;          pointer(pC,model_C)
    real*8 model_sigmasq;    pointer(psigmasq,model_sigmasq)
    real*8 model_cutsq;      pointer(pcutsq,model_cutsq)
    real*8 energy;           pointer(penergy,energy)
    real*8 coordum(DIM,1);   pointer(pcoor,coordum)
    real*8 forcedum(DIM,1);  pointer(pforce,forcedum)
    real*8 enepotdum(1);     pointer(penepot,enepotdum)
    real*8 boxlength(3);     pointer(pboxlength,boxlength)
    real*8 virial;           pointer(pvirial,virial)
    real*8 Rij_dummy(3,1);   pointer(pRij_dummy,Rij_dummy)
    integer nei1atom(1);     pointer(pnei1atom,nei1atom)
    real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
    integer :: comp_force, comp_enepot, comp_virial
    integer N4     !@@@@@@@@@ NEEDS TO BE FIXED
    
    ! Unpack data from KIM object
    !
    pnAtoms     = kim_api_get_data_f(pkim,"numberOfAtoms",ier);       if (ier.le.0) return
    pnAtomTypes = kim_api_get_data_f(pkim,"numberAtomTypes",ier);     if (ier.le.0) return
    patomTypes  = kim_api_get_data_f(pkim,"atomTypes",ier);           if (ier.le.0) return
    pcutoff     = kim_api_get_data_f(pkim,"cutoff",ier);              if (ier.le.0) return
    pepsilon    = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier);  if (ier.le.0) return
    psigma      = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier);    if (ier.le.0) return
    pcutnorm    = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier); if (ier.le.0) return
    pA          = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier);       if (ier.le.0) return
    pB          = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier);       if (ier.le.0) return
    pC          = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier);       if (ier.le.0) return
    psigmasq    = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier); if (ier.le.0) return
    pcutsq      = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier);   if (ier.le.0) return
    penergy     = kim_api_get_data_f(pkim,"energy",ier);              if (ier.le.0) return
    pcoor       = kim_api_get_data_f(pkim,"coordinates",ier);         if (ier.le.0) return
    pboxlength  = kim_api_get_data_f(pkim,"boxlength",ier);           if (ier.le.0) return
    
    ! Check to see if we have been asked to compute the forces, energyperatom, and virial
    comp_force  = kim_api_isit_compute_f(pkim,"forces",ier);         if (ier.le.0) return
    comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier);  if (ier.le.0) return
    comp_virial = kim_api_isit_compute_f(pkim,"virial",ier);         if (ier.le.0) return
    
    ! Cast to F90 arrays
    N4=numberOfAtoms
    if (comp_force.eq.1) then 
       pforce  = kim_api_get_data_f(pkim,"forces",ier);        
       if (ier.le.0) return
       call toRealArrayWithDescriptor2d(forcedum,force,DIM,N4)
    endif
    if (comp_enepot.eq.1) then 
       penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier); 
       if (ier.le.0) return
       call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N4)
    endif
    if (comp_virial.eq.1) then
       pvirial = kim_api_get_data_f(pkim,"virial",ier);
       if (ier.le.0) return
    endif
    call toRealArrayWithDescriptor2d(coordum,coor,DIM,N4)


    ! Check to be sure that the atom types are correct
    ier = 0 ! assume an error
    do i = 1,numberOfAtoms
       if (atomTypes(i).ne.Ar) return
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


    ! determine which NBC scenerio to use
    pNBC_Method = kim_api_get_nbc_method(pkim, ier); if (ier.le.0) return
    if (index(NBC_Method,"MI-OPBC-H").eq.1) then
       nbc = 0
    elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
       nbc = 1
    else
       ier = 0
       return
    endif
    call free(pNBC_Method) ! don't forget to release the memory...

    !  Compute energy and forces
    !
    do i = 1,numberOfAtoms
       
       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       
       if (nbc.eq.0) then
          ier = kim_api_get_half_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       else
          ier = kim_api_get_full_neigh(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_dummy)
       endif
       if (ier.le.0) return
       
       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rij = coor(:,i) - coor(:,j)                   ! distance vector between i j
          where ( abs(Rij) > 0.5d0*boxlength )          ! periodic boundary conditions
             Rij = Rij - sign(boxlength,Rij)            ! applied where needed.
          end where                                     ! 
          Rsqij = dot_product(Rij,Rij)                  ! compute square distance
          if ( Rsqij < model_cutsq ) then               ! particles are interacting?
             r = sqrt(Rsqij)                            ! compute distance
             call pair(model_epsilon,model_sigma,model_A,model_B, model_C, &
                  r,phi,dphi,d2phi)                     ! compute pair potential
             if (comp_enepot.eq.1) then                 !
                ene_pot(i) = ene_pot(i) + 0.5d0*phi     ! accumulate energy
                if (nbc.eq.0) then                      !
                   ene_pot(j) = ene_pot(j) + 0.5d0*phi  ! (i and j share it)
                endif                                   !
             else                                       !
                if (nbc.eq.0) then                      !
                   energy = energy + phi                ! half neigh case
                else                                    !
                   energy = energy + 0.5d0*phi          ! full neigh case
                endif                                      !
             endif
             if (comp_virial.eq.1) then                 !
                if (nbc.eq.0) then                      !
                   virial = virial + r*dphi             ! accumul. virial=sum r(dV/dr)
                else                                    !
                   virial = virial + 0.5d0*r*dphi       !
                endif                                   !
             endif                                      !
             if (comp_force.eq.1) then                  !
                force(:,i) = force(:,i) - dphi*Rij/r    ! accumulate forces
                if (nbc.eq.0) then                      !
                   force(:,j) = force(:,j) + dphi*Rij/r ! (Fji = -Fij)
                endif                                   !
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
    if (ier.le.0) stop '* ERROR: PARAM_FREE_sigma not found in KIM object.'
    
    ! get epsilon from KIM object
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FREE_epsilon not found in KIM object.'
    
    ! get cutoff parameter from KIM object
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FREE_cutoff not found in KIM object.'
    
    ! Set new values in KIM object ---------------------------------------------
    
    ! store model cutoff in KIM object
    pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
    if (ier.le.0) stop '* ERROR: cutoff not found in KIM object.'
    model_cutoff = model_Pcutoff
    
    ! store cutnorm in KIM object
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutnorm not found in KIM object.'
    model_cutnorm = model_cutoff/model_sigma
    
    ! store A in KIM object
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_A not found in KIM object.'
    model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
         (model_cutnorm**14*model_sigma**2)
    
    ! store B in KIM object
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_B not found in KIM object.'
    model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
         (model_cutnorm**13*model_sigma)
    
    ! store C in KIM object
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_C not found in KIM object.'
    model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
         (model_cutnorm**12)
    
    ! store sigma^2 in KIM object
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_sigmasq not found in KIM object.'
    model_sigmasq = model_sigma**2
    
    ! store cutoff^2 in KIM object
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutsq not found in KIM object.'
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
    
    ! get sigma from KIM object and free memory
    psigma = kim_api_get_data_f(pkim,"PARAM_FREE_sigma",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FREE_sigma not found in KIM object.'
    call free(psigma)
    
    ! get epsilon from KIM object and free memory
    pepsilon = kim_api_get_data_f(pkim,"PARAM_FREE_epsilon",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FREE_epsilon not found in KIM object.'
    call free(pepsilon)
    
    ! get cutoff parameter from KIM object and free memory
    pparamcut = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FREE_cutoff not found in KIM object.'
    call free(pparamcut)
    
    ! get cutnorm in KIM object and free memory
    pcutnorm = kim_api_get_data_f(pkim,"PARAM_FIXED_cutnorm",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutnorm not found in KIM object.'
    call free(pcutnorm)
    
    ! get A in KIM object and free memory
    pA = kim_api_get_data_f(pkim,"PARAM_FIXED_A",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_A not found in KIM object.'
    call free(pA)
    
    ! get B in KIM object and free memory
    pB = kim_api_get_data_f(pkim,"PARAM_FIXED_B",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_B not found in KIM object.'
    call free(pB)
    
    ! get C in KIM object and free memory
    pC = kim_api_get_data_f(pkim,"PARAM_FIXED_C",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_C not found in KIM object.'
    call free(pC)
    
    ! get sigma^2 in KIM object and free memory
    psigmasq = kim_api_get_data_f(pkim,"PARAM_FIXED_sigmasq",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_sigmasq not found in KIM object.'
    call free(psigmasq)

    ! get cutoff^2 in KIM object and free memory
    pcutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
    if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutsq not found in KIM object.'
    call free(pcutsq)

  end subroutine Destroy
  
end module model_Ar_LJ_MI_OPBC_H_F_f90


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_Ar_LJ_MI_OPBC_H_F_f90_init(pkim)
  use model_Ar_LJ_MI_OPBC_H_F_f90
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
  if (kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces)).ne.1) &
       stop '* ERROR: compute not found in KIM object.'
  
  ! store pointer to reinit function in KIM object
  if (kim_api_set_data_f(pkim,"reinit",one,loc(ReInit)).ne.1) &
       stop '* ERROR: reinit not found in KIM object.'

  ! store pointer to destroy function in KIM object
  if (kim_api_set_data_f(pkim,"destroy",one,loc(Destroy)).ne.1) &
       stop '* ERROR: destroy not found in KIM object.'
  
  ! store model cutoff in KIM object
  pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.le.0) stop '* ERROR: cutoff not found in KIM object.'
  model_cutoff = 8.15d0 ! cutoff distance in angstroms
  
  ! Allocate memory for sigma and store value
  psigma = malloc(one*8) ! 8 is the size of a real*8
  ! store sigma in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_sigma",one,psigma)
  if (ier.le.0) stop '* ERROR: PARAM_FREE_sigma not found in KIM object.'
  model_sigma = 3.4d0 ! LJ sigma in angstroms
  
  ! Allocate memory for epsilon and store value
  pepsilon = malloc(one*8) ! 8 is the size of a real*8
  ! store epsilon in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_epsilon",one,pepsilon)
  if (ier.le.0) stop '* ERROR: PARAM_FREE_epsilon not found in KIM object.'
  model_epsilon = 0.0104d0 ! LJ epsilon in eV
  

  ! Allocate memory for parameter cutoff and store value
  pparamcut = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff as parameter in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FREE_cutoff",one,pparamcut)
  if (ier.le.0) stop '* ERROR: PARAM_FREE_cutoff not found in KIM object.'
  model_Pcutoff = model_cutoff
  
  ! Allocate memory for parameter cutnorm and store value
  pcutnorm = malloc(one*8) ! 8 is the size of a real*8
  ! store cutnorm in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutnorm",one,pcutnorm)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutnorm not found in KIM object.'
  model_cutnorm = model_cutoff/model_sigma
  
  ! Allocate memory for parameter A and store value
  pA = malloc(one*8) ! 8 is the size of a real*8
  ! store A in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_A",one,pA)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_A not found in KIM object.'
  model_A = 12.d0*model_epsilon*(-26.d0 + 7.d0*model_cutnorm**6)/ &
       (model_cutnorm**14*model_sigma**2)
  
  ! Allocate memory for parameter B and store value
  pB = malloc(one*8) ! 8 is the size of a real*8
  ! store B in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_B",one,pB)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_B not found in KIM object.'
  model_B = 96.d0*model_epsilon*(7.d0-2.d0*model_cutnorm**6)/     &
       (model_cutnorm**13*model_sigma)
  
  ! Allocate memory for parameter C and store value
  pC = malloc(one*8) ! 8 is the size of a real*8
  ! store C in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_C",one,pC)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_C not found in KIM object.'
  model_C = 28.d0*model_epsilon*(-13.d0+4.d0*model_cutnorm**6)/   &
       (model_cutnorm**12)
  
  ! Allocate memory for parameter sigmasq and store value
  psigmasq = malloc(one*8) ! 8 is the size of a real*8
  ! store sigma^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_sigmasq",one,psigmasq)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_sigmasq not found in KIM object.'
  model_sigmasq = model_sigma**2
  
  ! Allocate memory for parameter cutsq and store value
  pcutsq = malloc(one*8) ! 8 is the size of a real*8
  ! store cutoff^2 in KIM object
  ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutsq",one,pcutsq)
  if (ier.le.0) stop '* ERROR: PARAM_FIXED_cutsq not found in KIM object.'
  model_cutsq = model_cutoff**2
  
end subroutine model_Ar_LJ_MI_OPBC_H_F_f90_init
