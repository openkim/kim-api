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
! Copyright (c) 2012, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!


!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an
!**  isolated cluster of SPECIES_NAME_STR atoms
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!*******************************************************************************

#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program TEST_NAME_STR
  use KIM_API
  implicit none

  double precision, parameter :: FCCspacing     = FCC_SPACING_STR
  integer,          parameter :: nCellsPerSide  = 2
  integer,          parameter :: DIM            = 3
  integer,          parameter :: ATypes         = 1
  integer,          parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(kind=kim_intptr), parameter :: one    = 1
  integer(kind=kim_intptr), parameter :: NN     = N
  integer(kind=kim_intptr), parameter :: DIMN   = DIM*N
  integer(kind=kim_intptr), parameter :: six    = 6

  !
  ! KIM variables
  !
  character*80              :: testname     = "TEST_NAME_STR"
  character*80              :: modelname
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier, idum
  integer numberOfParticles;
  integer numberParticleTypes;
  integer particleTypes(N);

  real*8 cutoff;
  real*8 energy;
  real*8 virialglob(6);
  real*8 coor(DIM,N);
  real*8 forces(DIM,N);
  integer I
  integer middleDum


  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_init_f", ier)
     stop
  endif
  ! register memory with KIM object
  call kim_api_setm_data_f(pkim, ier, &
       "numberOfParticles",   DIMN, loc(numberOfParticles),   TRUEFALSE(.true.), &
       "numberParticleTypes", one,  loc(numberParticleTypes), TRUEFALSE(.true.), &
       "particleTypes",       NN,   loc(particleTypes),       TRUEFALSE(.true.), &
       "coordinates",         DIMN, loc(coor),                TRUEFALSE(.true.), &
       "cutoff",              one,  loc(cutoff),              TRUEFALSE(.true.), &
       "energy",              one,  loc(energy),              TRUEFALSE(.true.), &
       "forces",              DIMN, loc(forces),              TRUEFALSE(.true.), &
       "virial",              six,  loc(virialglob),          TRUEFALSE(.true.))

  ! call model's init routine
  ier = kim_api_model_init_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_init", ier)
     stop
  endif

  ! Set values
  numberOfParticles   = N
  numberParticleTypes = ATypes
  particleTypes(:)    = kim_api_get_partcl_type_code_f(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_partcl_type_code_f", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coor, middleDum)

  ! Call model compute
  ier = kim_api_model_compute_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_compute", ier)
     stop
  endif

  ! print results to screen
  print '(80(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname
  print '("Forces:")'
  print '("Atom     ' // &
  'X                        ' // &
  'Y                        ' // &
  'Z                        ")'
  print '(I2,"   ",3ES25.15)', (I,forces(:,I),I=1,N)
  print *
  print '("Energy        = ",ES25.15)', energy
  print '("Global Virial = ",3ES25.15)', (virialglob(I),I=1,3)
  print '("                ",3ES25.15)', (virialglob(I),I=4,6)

  ! don't forget to destroy and deallocate
  ier = kim_api_model_destroy_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_destroy", ier)
     stop
  endif
  call kim_api_free(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_free", ier)
     stop
  endif

  stop
end program TEST_NAME_STR
