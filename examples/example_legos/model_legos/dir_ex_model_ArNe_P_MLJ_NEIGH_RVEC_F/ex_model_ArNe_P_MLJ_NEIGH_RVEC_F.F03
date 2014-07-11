!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2013--2014, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!

!*******************************************************************************
!**
!**  MODULE ex_model_ArNe_P_MLJ_NEIGH_RVEC_F
!**
!**  Modified Lennard-Jones P potential model for argon and neon
!**  (modified to have smooth cutoff)
!**  (uses parameters listed in Modeling Materials, by Tadmor and Miller
!**   and the Lorentz/Berthelot Mixing Rule)
!**
!**  Release: This file is part of the kim-api.git repository.
!**
!*******************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_ArNe_P_MLJ_NEIGH_RVEC_F
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none


  save
  private
  public Compute_Energy_Forces
  public ReInit
  public Destroy

  ! Species indices
  integer(c_int), parameter :: cd = c_double ! used for literal constants
  integer(c_int), parameter :: Ar = 1
  integer(c_int), parameter :: Ne = 2

contains

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
  integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in)  :: pkim

    !-- Local variables
    integer(c_int), parameter :: DIM=3
    real(c_double) r,Rsqij,phi,dphi,d2phi,dEidr
    real(c_double) CurEpsilon, CurSigma, CurA, CurB, CurC
    integer(c_int) i,j,jj,numnei,atom,atom_ret

    !-- KIM variables
    integer(c_int), pointer :: numberOfParticles; 
    type(c_ptr) :: pnumberOfParticles
    integer(c_int), pointer :: nOfSpecies;        type(c_ptr) :: pnOfSpecies
    integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies
    real(c_double), pointer :: model_cutoff;      type(c_ptr) :: pmodel_cutoff
    real(c_double), pointer :: model_epsilon(:);  type(c_ptr) :: pmodel_epsilon
    real(c_double), pointer :: model_sigma(:);    type(c_ptr) :: pmodel_sigma
    real(c_double), pointer :: model_cutnorm(:);  type(c_ptr) :: pmodel_cutnorm
    real(c_double), pointer :: model_A(:);        type(c_ptr) :: pmodel_A
    real(c_double), pointer :: model_B(:);        type(c_ptr) :: pmodel_B
    real(c_double), pointer :: model_C(:);        type(c_ptr) :: pmodel_C
    real(c_double), pointer :: model_sigmasq(:);  type(c_ptr) :: pmodel_sigmasq
    real(c_double), pointer :: model_cutsq;       type(c_ptr) :: pmodel_cutsq
    real(c_double), pointer :: energy;            type(c_ptr) :: penergy
    real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
    real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
    real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
    real(c_double), pointer :: virial(:);         type(c_ptr) :: pvirial
    real(c_double), pointer :: Rij(:,:);          type(c_ptr) :: pRij
    integer(c_int), pointer :: nei1atom(:);       type(c_ptr) :: pnei1atom
    integer(c_int) :: comp_force, comp_enepot, comp_virial, comp_energy
    integer(c_int) :: idum

    ! Check to see if we have been asked to compute the forces, energyperatom,
    ! energy and virial
    call kim_api_getm_compute(pkim, Compute_Energy_Forces,  &
         "energy",         comp_energy,  1, &
         "forces",         comp_force,   1, &
         "particleEnergy", comp_enepot,  1, &
         "virial",         comp_virial,  1)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_compute",   &
                                   Compute_Energy_Forces)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data(pkim,  Compute_Energy_Forces, &
         "numberOfParticles",     pnumberOfParticles,   1,                     &
         "numberOfSpecies",       pnOfSpecies,          1,                     &
         "particleSpecies",       pparticleSpecies,     1,                     &
         "cutoff",                pmodel_cutoff,        1,                     &
         "coordinates",           pcoor,                1,                     &
         "energy",                penergy,        TRUEFALSE(comp_energy.eq.1), &
         "forces",                pforce,         TRUEFALSE(comp_force.eq.1),  &
         "particleEnergy",        penepot,        TRUEFALSE(comp_enepot.eq.1), &
         "virial",                pvirial,        TRUEFALSE(comp_virial.eq.1))
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Compute_Energy_Forces)
       return
    endif

    call c_f_pointer(pnumberOfParticles, numberOfParticles)
    call c_f_pointer(pnOfSpecies,        nOfSpecies)
    call c_f_pointer(pparticleSpecies,   particleSpecies, [numberOfParticles])
    call c_f_pointer(pmodel_cutoff,      model_cutoff)
    call c_f_pointer(pcoor,              coor,            [DIM,numberOfParticles])
    if (comp_energy.eq.1) call c_f_pointer(penergy, energy)
    if (comp_force.eq.1)  then
      call c_f_pointer(pforce, force, [DIM,numberOfParticles])
    endif
    if (comp_enepot.eq.1) call c_f_pointer(penepot, enepot, [numberOfParticles])
    if (comp_virial.eq.1) call c_f_pointer(pvirial, virial, [6])

    ! Unpack parameters from KIM object
    call kim_api_getm_data(pkim, Compute_Energy_Forces, &
         "PARAM_FREE_epsilon",  pmodel_epsilon, 1, &
         "PARAM_FREE_sigma",    pmodel_sigma,   1, &
         "PARAM_FIXED_cutnorm", pmodel_cutnorm, 1, &
         "PARAM_FIXED_A",       pmodel_A,       1, &
         "PARAM_FIXED_B",       pmodel_B,       1, &
         "PARAM_FIXED_C",       pmodel_C,       1, &
         "PARAM_FIXED_sigmasq", pmodel_sigmasq, 1, &
         "PARAM_FIXED_cutsq",   pmodel_cutsq,   1)
    if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Compute_Energy_Forces)
       return
    endif

    call c_f_pointer(pmodel_epsilon, model_epsilon, [3])
    call c_f_pointer(pmodel_sigma,   model_sigma,   [3])
    call c_f_pointer(pmodel_cutnorm, model_cutnorm, [3])
    call c_f_pointer(pmodel_A,       model_A,       [3])
    call c_f_pointer(pmodel_B,       model_B,       [3])
    call c_f_pointer(pmodel_C,       model_C,       [3])
    call c_f_pointer(pmodel_sigmasq, model_sigmasq, [3])
    call c_f_pointer(pmodel_cutsq,   model_cutsq)

    ! Check to be sure that the atom species are correct
    Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
    do i = 1,numberOfParticles
       if (.not. ( (particleSpecies(i).eq.Ar) .or. (particleSpecies(i).eq.Ne) ) ) &
         then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "Wrong Atom Specie", Compute_Energy_Forces)
          return
       endif
    enddo
    Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok


    ! Initialize potential energies, forces, virial term
    !
    if (comp_enepot.eq.1) enepot = 0.0_cd
    if (comp_energy.eq.1) energy = 0.0_cd
    if (comp_force.eq.1)  force  = 0.0_cd
    if (comp_virial.eq.1) virial = 0.0_cd


    !  Compute energy and forces
    !
    do i = 1,numberOfParticles

       ! Get neighbors for atom i
       !
       atom = i ! request neighbors for atom i
       Compute_Energy_Forces = kim_api_get_neigh(pkim,1,atom,atom_ret,numnei, &
                                                 pnei1atom,pRij)
       if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_neigh_f",    &
                                      Compute_Energy_Forces)
          return
       endif

       call c_f_pointer(pnei1atom, nei1atom, [numnei])
       call c_f_pointer(pRij,      Rij,      [DIM,numnei])

       ! Loop over the neighbors of atom i
       !
       do jj = 1, numnei
          j = nei1atom(jj)
          Rsqij = dot_product(Rij(:,jj),Rij(:,jj)) ! compute square distance
          if ( Rsqij < model_cutsq ) then          ! particles are interacting?
             r = sqrt(Rsqij)                       ! compute distance
             if ((particleSpecies(i).eq.Ar).and. &
                 (particleSpecies(nei1atom(jj)).eq.Ar)) then
                CurEpsilon = model_epsilon(Ar)
                CurSigma   = model_sigma(Ar)
                CurA       = model_A(Ar)
                CurB       = model_B(Ar)
                CurC       = model_C(Ar)
             else if ((particleSpecies(i).eq.Ne).and. &
                      (particleSpecies(nei1atom(jj)).eq.Ne)) then
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
                  r, phi, dphi, d2phi)            ! compute pair potential
             dEidr = 0.5_cd*dphi                  ! compute dEidr
             if (comp_enepot.eq.1) then
                enepot(i) = enepot(i) + 0.5_cd*phi! accumulate energy
             endif
             if (comp_energy.eq.1) then
                energy = energy + 0.5_cd*phi      ! full neigh case
             endif
             if (comp_virial.eq.1) then           ! accumul. virial
                virial(1) = virial(1) + Rij(1,jj)*Rij(1,jj)*dEidr/r
                virial(2) = virial(2) + Rij(2,jj)*Rij(2,jj)*dEidr/r
                virial(3) = virial(3) + Rij(3,jj)*Rij(3,jj)*dEidr/r
                virial(4) = virial(4) + Rij(2,jj)*Rij(3,jj)*dEidr/r
                virial(5) = virial(5) + Rij(1,jj)*Rij(3,jj)*dEidr/r
                virial(6) = virial(6) + Rij(1,jj)*Rij(2,jj)*dEidr/r
             endif
             if (comp_force.eq.1) then
                ! accumulate force on atom i
                force(:,i) = force(:,i) + dEidr*Rij(:,jj)/r
                ! accumulate force on atom j
                force(:,j) = force(:,j) - dEidr*Rij(:,jj)/r
             endif
          endif
       enddo
    enddo

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
    real(c_double) :: rsq,sor,sor6,sor12

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
! Model reinitialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
  integer(c_int) function ReInit(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in) :: pkim

    !-- Local variables
    real(c_double), pointer :: model_cutoff;     type(c_ptr) :: pmodel_cutoff
    real(c_double), pointer :: model_epsilon(:); type(c_ptr) :: pmodel_epsilon
    real(c_double), pointer :: model_sigma(:);   type(c_ptr) :: pmodel_sigma
    real(c_double), pointer :: model_Pcutoff;    type(c_ptr) :: pmodel_Pcutoff
    real(c_double), pointer :: model_cutnorm(:); type(c_ptr) :: pmodel_cutnorm
    real(c_double), pointer :: model_A(:);       type(c_ptr) :: pmodel_A
    real(c_double), pointer :: model_B(:);       type(c_ptr) :: pmodel_B
    real(c_double), pointer :: model_C(:);       type(c_ptr) :: pmodel_C
    real(c_double), pointer :: model_sigmasq(:); type(c_ptr) :: pmodel_sigmasq
    real(c_double), pointer :: model_cutsq;      type(c_ptr) :: pmodel_cutsq
    integer(c_int) idum

    ! Get (changed) parameters from KIM object ---------------------------------
    call kim_api_getm_data(pkim, ReInit, &
         "PARAM_FREE_sigma",   pmodel_sigma,    1, &
         "PARAM_FREE_epsilon", pmodel_epsilon,  1, &
         "PARAM_FREE_cutoff",  pmodel_Pcutoff,  1)
    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_data", ReInit)
       return
    endif

    call c_f_pointer(pmodel_sigma,   model_sigma,   [3])
    call c_f_pointer(pmodel_epsilon, model_epsilon, [3])
    call c_f_pointer(pmodel_Pcutoff, model_Pcutoff)

    ! Set new values in KIM object ---------------------------------------------
    call kim_api_getm_data(pkim, ReInit, &
         "cutoff",              pmodel_cutoff,  1, &
         "PARAM_FIXED_cutnorm", pmodel_cutnorm, 1, &
         "PARAM_FIXED_A",       pmodel_A,       1, &
         "PARAM_FIXED_B",       pmodel_B,       1, &
         "PARAM_FIXED_C",       pmodel_C,       1, &
         "PARAM_FIXED_sigmasq", pmodel_sigmasq, 1, &
         "PARAM_FIXED_cutsq",   pmodel_cutsq,   1)

    if (ReInit.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", ReInit)
       return
    endif

    call c_f_pointer(pmodel_cutoff,  model_cutoff)
    call c_f_pointer(pmodel_cutnorm, model_cutnorm, [3])
    call c_f_pointer(pmodel_A,       model_A,       [3])
    call c_f_pointer(pmodel_B,       model_B,       [3])
    call c_f_pointer(pmodel_C,       model_C,       [3])
    call c_f_pointer(pmodel_sigmasq, model_sigmasq, [3])
    call c_f_pointer(pmodel_cutsq,   model_cutsq)

    model_cutoff = model_Pcutoff
    model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
    model_A(1:3) = 12.0_cd*model_epsilon(1:3)   &
      *(-26.0_cd + 7.0_cd*model_cutnorm(1:3)**6)/ &
      (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
    model_B(1:3) = 96.0_cd*model_epsilon(1:3)   &
      *(7.0_cd-2.0_cd*model_cutnorm(1:3)**6)/     &
      (model_cutnorm(1:3)**13*model_sigma(1:3))
    model_C(1:3) = 28.0_cd*model_epsilon(1:3)   &
      *(-13.0_cd+4.0_cd*model_cutnorm(1:3)**6)/   &
      (model_cutnorm(1:3)**12)
    model_sigmasq(1:3) = model_sigma(1:3)**2
    model_cutsq = model_cutoff**2

    ReInit = KIM_STATUS_OK
    return

  end function ReInit

!-------------------------------------------------------------------------------
!
! Model destory routine (REQUIRED)
!
!-------------------------------------------------------------------------------
  integer(c_int) function Destroy(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in) :: pkim

    !-- Local variables
    real(c_double), pointer :: model_epsilon(:); type(c_ptr) :: pmodel_epsilon
    real(c_double), pointer :: model_sigma(:);   type(c_ptr) :: pmodel_sigma
    real(c_double), pointer :: model_Pcutoff;    type(c_ptr) :: pmodel_Pcutoff
    real(c_double), pointer :: model_cutnorm(:); type(c_ptr) :: pmodel_cutnorm
    real(c_double), pointer :: model_A(:);       type(c_ptr) :: pmodel_A
    real(c_double), pointer :: model_B(:);       type(c_ptr) :: pmodel_B
    real(c_double), pointer :: model_C(:);       type(c_ptr) :: pmodel_C
    real(c_double), pointer :: model_sigmasq(:); type(c_ptr) :: pmodel_sigmasq
    real(c_double), pointer :: model_cutsq;      type(c_ptr) :: pmodel_cutsq
    integer(c_int) idum

    ! get parameters from KIM object
    call kim_api_getm_data(pkim, Destroy, &
         "PARAM_FREE_sigma",     pmodel_sigma,    1, &
         "PARAM_FREE_epsilon",   pmodel_epsilon,  1, &
         "PARAM_FREE_cutoff",    pmodel_Pcutoff,  1, &
         "PARAM_FIXED_cutnorm",  pmodel_cutnorm,  1, &
         "PARAM_FIXED_A",        pmodel_A,        1, &
         "PARAM_FIXED_B",        pmodel_B,        1, &
         "PARAM_FIXED_C",        pmodel_C,        1, &
         "PARAM_FIXED_sigmasq",  pmodel_sigmasq,  1, &
         "PARAM_FIXED_cutsq",    pmodel_cutsq,    1)
    if (Destroy.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", Destroy)
       return
    endif

    call c_f_pointer(pmodel_epsilon, model_epsilon, [3])
    call c_f_pointer(pmodel_sigma,   model_sigma,   [3])
    call c_f_pointer(pmodel_Pcutoff, model_Pcutoff)
    call c_f_pointer(pmodel_cutnorm, model_cutnorm, [3])
    call c_f_pointer(pmodel_A,       model_A,       [3])
    call c_f_pointer(pmodel_B,       model_B,       [3])
    call c_f_pointer(pmodel_C,       model_C,       [3])
    call c_f_pointer(pmodel_sigmasq, model_sigmasq, [3])
    call c_f_pointer(pmodel_cutsq,   model_cutsq)

    ! free memory
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

end module ex_model_ArNe_P_MLJ_NEIGH_RVEC_F


!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer(c_int) function model_init(pkim) bind(c)
  use, intrinsic :: iso_c_binding
  use ex_model_ArNe_P_MLJ_NEIGH_RVEC_F
  use KIM_API_F03
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  type(c_ptr), intent(in) :: pkim

  !-- Local variables
  integer(c_int), parameter :: one=1
  integer(c_int), parameter :: three=3
  real(c_double), pointer :: model_cutoff;     type(c_ptr) :: pmodel_cutoff
  real(c_double), pointer :: model_epsilon(:)
  real(c_double), pointer :: model_sigma(:)
  real(c_double), pointer :: model_Pcutoff
  real(c_double), pointer :: model_cutnorm(:)
  real(c_double), pointer :: model_A(:)
  real(c_double), pointer :: model_B(:)
  real(c_double), pointer :: model_C(:)
  real(c_double), pointer :: model_sigmasq(:)
  real(c_double), pointer :: model_cutsq
  integer(c_int) ier, idum

  ! store function pointers in KIM object
  call kim_api_setm_method(pkim, ier, &
       "compute", one, c_funloc(Compute_Energy_Forces), 1, &
       "reinit",  one, c_funloc(ReInit),                1, &
       "destroy", one, c_funloc(Destroy),               1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data", ier)
     goto 42
  endif

  ! store model cutoff in KIM object
  pmodel_cutoff =  kim_api_get_data(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     goto 42
  endif
  call c_f_pointer(pmodel_cutoff, model_cutoff)
  model_cutoff = 8.15_cd ! cutoff distance in angstroms

  ! Allocate memory for parameters
  allocate( model_epsilon(three) )
  allocate( model_sigma(three)   )
  allocate( model_Pcutoff        )
  allocate( model_cutnorm(three) )
  allocate( model_A(three)       )
  allocate( model_B(three)       )
  allocate( model_C(three)       )
  allocate( model_sigmasq(three) )
  allocate( model_cutsq          )

  ! Set Parameter values
  !
  model_sigma(1) = 3.40_cd ! LJ Argon sigma in angstroms
  model_sigma(2) = 2.74_cd ! LJ Neon  sigma in angstroms
  ! Lorentz/Berthelot Mixing Rule
  model_sigma(3) = 0.5_cd*(model_sigma(1) + model_sigma(2))
  model_epsilon(1) = 0.0104_cd ! LJ Argon epsilon in eV
  model_epsilon(2) = 0.0031_cd ! LJ Neon  epsilon in eV
  ! Lorentz/Berthelot Mixing Rule
  model_epsilon(3) = sqrt(model_epsilon(1)*model_epsilon(2))
  model_Pcutoff = model_cutoff
  model_cutnorm(1:3) = model_cutoff/model_sigma(1:3)
  model_A(1:3) = 12.0_cd*model_epsilon(1:3)   &
    *(-26.0_cd + 7.0_cd*model_cutnorm(1:3)**6)/ &
    (model_cutnorm(1:3)**14*model_sigma(1:3)**2)
  model_B(1:3) = 96.0_cd*model_epsilon(1:3)   &
    *(7.0_cd-2.0_cd*model_cutnorm(1:3)**6)/     &
    (model_cutnorm(1:3)**13*model_sigma(1:3))
  model_C(1:3) = 28.0_cd*model_epsilon(1:3)   &
    *(-13.0_cd+4.0_cd*model_cutnorm(1:3)**6)/   &
    (model_cutnorm(1:3)**12)
  model_sigmasq(1:3) = model_sigma(1:3)**2
  model_cutsq = model_cutoff**2

  ! store parameters in KIM object
  call kim_api_setm_data(pkim, ier, &
       "PARAM_FREE_sigma",    three,  c_loc(model_sigma(1)),    1, &
       "PARAM_FREE_epsilon",  three,  c_loc(model_epsilon(1)),  1, &
       "PARAM_FREE_cutoff",   one,    c_loc(model_Pcutoff),     1, &
       "PARAM_FIXED_cutnorm", three,  c_loc(model_cutnorm(1)),  1, &
       "PARAM_FIXED_A",       three,  c_loc(model_A(1)),        1, &
       "PARAM_FIXED_B",       three,  c_loc(model_B(1)),        1, &
       "PARAM_FIXED_C",       three,  c_loc(model_C(1)),        1, &
       "PARAM_FIXED_sigmasq", three,  c_loc(model_sigmasq(1)),  1, &
       "PARAM_FIXED_cutsq",   one,    c_loc(model_cutsq),       1)
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
