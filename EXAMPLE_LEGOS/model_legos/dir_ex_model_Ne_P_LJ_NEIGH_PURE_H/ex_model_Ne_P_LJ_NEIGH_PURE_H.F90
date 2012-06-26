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
! Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
!
! Contributors:
!    Valeriu Smirichinski
!

!
! Release: This file is part of the openkim-api.git repository.
!


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_Ne_P_LJ_NEIGH_PURE_H
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
  integer function calculate_wrap_f77(pkim) ! compute routine with KIM interface
    use KIM_API
    implicit none

    !-- Transferred variables
    integer(kind=kim_intptr), intent(in)  :: pkim

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
    call kim_api_getm_compute_f(pkim, calculate_wrap_f77, &
         "energy",         e_flag,    1,   &
         "forces",         f_flag,    1,   &
         "particleEnergy", eper_flag, 1)
    if (calculate_wrap_f77.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_compute_f", calculate_wrap_f77)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data_f(pkim, calculate_wrap_f77, &
         "numberOfParticles",           pnumberofatoms, 1,                         &
         "numberContributingParticles", pnumContrib,    1,                         &
         "particleTypes",               pattypes,       1,                         &
         "coordinates",                 px,             1,                         &
         "forces",                      pf,             TRUEFALSE(f_flag.eq.1),    &
         "energy",                      ppotenergy,     TRUEFALSE(e_flag.eq.1),    &
         "particleEnergy",              pea,            TRUEFALSE(eper_flag.eq.1))
    if (calculate_wrap_f77.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_getm_data_f", calculate_wrap_f77)
       return
    endif

    do i=1,numberofatoms
       if (attypes(i).ne.1) then ! check for correct atom types Ne=1
          calculate_wrap_f77 = KIM_STATUS_FAIL
          idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                        "Wrong Atom Type", calculate_wrap_f77)
          return
       endif
    enddo

    ! Call FORTRAN 77 code that does actual calculation
    !
    call calculate(model_cutoff,sigma,epsilon,pkim,x,f,ea,numberofatoms,numContrib, &
                   potenergy,e_flag,f_flag,eper_flag,kim_api_get_neigh_f,calculate_wrap_f77)

    return
  end function calculate_wrap_f77

end module ex_model_Ne_P_LJ_NEIGH_PURE_H


!  Model Initiation routine
integer function ex_model_Ne_P_LJ_NEIGH_PURE_H_init(pkim)
  use ex_model_Ne_P_LJ_NEIGH_PURE_H
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
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_set_data_f", ier)
     goto 42
  endif

  ! store model cutoff in KIM object
  pcutoff = kim_api_get_data_f(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_data_f", ier)
     goto 42
  endif
  cutoff = model_cutoff

  ier = KIM_STATUS_OK
42 continue
  ex_model_Ne_P_LJ_NEIGH_PURE_H_init = ier
  return

end function ex_model_Ne_P_LJ_NEIGH_PURE_H_init
