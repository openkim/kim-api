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
! Copyright (c) 2013--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Valeriu Smirichinski
!

!
! Release: This file is part of the kim-api.git repository.
!


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_Ne_P_LJ_NEIGH_PURE_H
  use, intrinsic :: iso_c_binding
  use  KIM_API_F03
  implicit none

  save
  private
  public model_cutoff
  public calculate_wrap_f77

  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- LJ parameters
  real(c_double), parameter :: model_cutoff  = 8.1500_cd
  real(c_double), parameter :: sigma         = 2.7400_cd
  real(c_double), parameter :: epsilon       = 0.0031_cd

contains

  !-----------------------------------------------------------------------------
  !
  ! Computes energy and forces on particles from the positions.
  ! (f03 wrapper that calls the actual f77 routine)
  !
  !-----------------------------------------------------------------------------
  integer(c_int) function calculate_wrap_f77(pkim) bind(c)
    implicit none

    !-- Transferred variables
    type(c_ptr), intent(in)  :: pkim

    !-- Local variables
    real(c_double), pointer :: x(:,:);     type(c_ptr) :: px  ! position
    real(c_double), pointer :: f(:,:);     type(c_ptr) :: pf  ! force
    real(c_double), pointer :: ea(:);      type(c_ptr) :: pea ! energy per particle
    real(c_double), pointer :: potenergy;  type(c_ptr) :: ppotenergy ! total en.
    integer(c_int), pointer :: atspecies(:); type(c_ptr) :: patspecies ! species
    integer(c_int), pointer :: numberofparts; type(c_ptr) :: pnumberofparts
    integer(c_int), pointer :: numContrib;    type(c_ptr) :: pnumContrib
    integer(c_int) i, e_flag, f_flag, eper_flag, idum
    external calculate

    ! Check to see if we have been asked to compute the forces and energyperpart
    !
    call kim_api_getm_compute(pkim, calculate_wrap_f77, &
         "energy",         e_flag,    1,   &
         "forces",         f_flag,    1,   &
         "particleEnergy", eper_flag, 1)
    if (calculate_wrap_f77.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_compute", calculate_wrap_f77)
       return
    endif

    ! Unpack data from KIM object
    !
    call kim_api_getm_data(pkim, calculate_wrap_f77, &
     "numberOfParticles",           pnumberofparts, 1,                         &
     "numberContributingParticles", pnumContrib,    1,                         &
     "particleSpecies",             patspecies,     1,                         &
     "coordinates",                 px,             1,                         &
     "forces",                      pf,             TRUEFALSE(f_flag.eq.1),    &
     "energy",                      ppotenergy,     TRUEFALSE(e_flag.eq.1),    &
     "particleEnergy",              pea,            TRUEFALSE(eper_flag.eq.1))
    if (calculate_wrap_f77.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data", calculate_wrap_f77)
       return
    endif

    call c_f_pointer(pnumberofparts, numberofparts)
    call c_f_pointer(pnumContrib,    numContrib)
    call c_f_pointer(patspecies,     atspecies,    [numberofparts])
    call c_f_pointer(px,             x,            [3,numberofparts])
    if (f_flag.eq.1)    call c_f_pointer(pf,   f,  [3,numberofparts])
    if (eper_flag.eq.1) call c_f_pointer(pea, ea,  [numberofparts])
    if (e_flag.eq.1)    call c_f_pointer(ppotenergy, potenergy)

    do i=1,numberofparts
       if (atspecies(i).ne.1) then ! check for correct species Ne=1
          calculate_wrap_f77 = KIM_STATUS_FAIL
          idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                      "Wrong Species", calculate_wrap_f77)
          return
       endif
    enddo

    ! Call FORTRAN 77 code that does actual calculation
    !
    call calculate(model_cutoff,sigma,epsilon,pkim,x,f,ea,numberofparts, &
                   numContrib,potenergy,e_flag,f_flag,eper_flag,         &
                   kim_api_get_neigh,calculate_wrap_f77)

    return
  end function calculate_wrap_f77

end module ex_model_Ne_P_LJ_NEIGH_PURE_H


!  Model Initiation routine
integer(c_int) function model_init(pkim) bind(c)
  use, intrinsic :: iso_c_binding
  use ex_model_Ne_P_LJ_NEIGH_PURE_H
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr), intent(in) :: pkim

  !-- Local variables
  integer(c_int) ier, idum
  integer(c_int), parameter :: one = 1

  !-- KIM variables
  real(c_double), pointer :: cutoff;  type(c_ptr) :: pcutoff ! cutoff radius

  ! store pointer to compute function in KIM object
  ier = kim_api_set_method(pkim,"compute",one,c_funloc(calculate_wrap_f77))
  if (ier.lt.KIM_STATUS_OK)  then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_data", ier)
     goto 42
  endif

  ! store model cutoff in KIM object
  pcutoff = kim_api_get_data(pkim,"cutoff",ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     goto 42
  endif
  call c_f_pointer(pcutoff, cutoff)
  cutoff = model_cutoff

  ier = KIM_STATUS_OK
42 continue
  model_init = ier
  return

end function model_init
