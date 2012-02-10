!
! Release: This file is part of the openkim-api.git repository.
!
! Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
! All rights reserved.
!
! Authors: Valeriu Smirichinski, Ryan S. Elliot, Ellad B. Tadmor
!

#include "KIMstatus.h"
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module model_Ne_P_LJ_NEIGH_PURE_H
  use  KIM_API
  implicit none
  
  save
  private
  public model_cutoff
  public calculate_wrap_f77

  !-- LJ parameters
  real*8, parameter :: model_cutoff  = 8.1500d0
  real*8, parameter :: sigma         = 2.7400d0
  real*8, parameter :: epsilon       = 0.0031d0

contains
  
  !-----------------------------------------------------------------------------
  !
  ! Computes energy and forces on atoms from the positions.
  ! (f90 wrapper that calls the actual f77 routine)
  !
  !-----------------------------------------------------------------------------
  subroutine calculate_wrap_f77(pkim,ier) ! compute routine with KIM interface
    use KIM_API
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in)  :: pkim
    integer,                  intent(out) :: ier

    !-- Local variables
    real*8 x(3,1);           pointer(px,x)                 ! position
    real*8 f(3,1);           pointer(pf,f)                 ! force
    real*8 ea(1);            pointer(pea,ea)               ! energy per atom
    real*8 potenergy;        pointer(ppotenergy,potenergy) ! total energy
    integer attypes(1);      pointer(pattypes,attypes)     ! atom types
    integer numberofatoms; pointer(pnumberofatoms,numberofatoms)
    integer numContrib; pointer(pnumContrib,numContrib)
    integer i, e_flag, f_flag, eper_flag, idum
    external calculate

    ! Check to see if we have been asked to compute the forces and energyperatom
    !
    call kim_api_getm_compute_f(pkim, ier, &
         "energy",        e_flag,    1, &
         "forces",        f_flag,    1, &
         "particleEnergy", eper_flag, 1)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_compute_f", ier)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data_f(pkim, ier, &
         "numberOfParticles",           pnumberofatoms, 1,                         &
         "numberContributingParticles", pnumContrib,    1,                         &
         "atomTypes",               pattypes,       1,                         &
         "coordinates",             px,             1,                         &
         "forces",                  pf,             TRUEFALSE(f_flag.eq.1),    &
         "energy",                  ppotenergy,     TRUEFALSE(e_flag.eq.1),    &
         "particleEnergy",           pea,            TRUEFALSE(eper_flag.eq.1))
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
       return
    endif

    do i=1,numberofatoms
       if (attypes(i).ne.1) then ! check for correct atom types Ne=1
          idum = kim_api_report_error_f(__LINE__, __FILE__, "Wrong Atom Type", KIM_STATUS_FAIL)
          return
       endif
    enddo

    ! Call FORTRAN 77 code that does actual calculation
    !
    call calculate(model_cutoff,sigma,epsilon,pkim,x,f,ea,numberofatoms,numContrib, &
                   potenergy,e_flag,f_flag,eper_flag,kim_api_get_neigh_f,ier)

  end subroutine calculate_wrap_f77
  
end module model_Ne_P_LJ_NEIGH_PURE_H


!  Model Initiation routine
subroutine model_Ne_P_LJ_NEIGH_PURE_H_init(pkim)
  use model_Ne_P_LJ_NEIGH_PURE_H
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in) :: pkim

  !-- Local variables
  integer ier, idum
  integer(kind=kim_intptr) one

  !-- KIM variables
  real*8 cutoff;  pointer(pcutoff,cutoff)  ! cutoff radius

  ! store pointer to compute function in KIM object
  one=1
  ier = kim_api_set_data_f(pkim,"compute",one,loc(calculate_wrap_f77))
  if (ier.lt.KIM_STATUS_OK)  then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
     stop
  endif

  ! store model cutoff in KIM object
  pcutoff = kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  cutoff = model_cutoff

end subroutine model_Ne_P_LJ_NEIGH_PURE_H_init
