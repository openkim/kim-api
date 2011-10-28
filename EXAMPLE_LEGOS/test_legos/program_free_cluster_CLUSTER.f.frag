!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an 
!**  isolated cluster of SPECIES_NAME_STR atoms
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

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program TEST_NAME_STR
  use KIMservice
  implicit none

  double precision, parameter :: FCCspacing     = FCC_SPACING_STR
  integer,          parameter :: nCellsPerSide  = 2
  integer,          parameter :: DIM            = 3
  integer,          parameter :: ATypes         = 1
  integer,          parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  !
  ! KIM variables
  !
  character*80              :: testname     = "TEST_NAME_STR"
  character*80              :: modelname
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier
  integer numberOfAtoms;   pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes; pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1); pointer(patomTypesdum,atomTypesdum)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 virialglobdum(1); pointer(pvirialglob,virialglobdum)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1); pointer(pforces,forcesdum)
  real*8 virialdum(DIM,1); pointer(pvirial,virialdum)
  integer I
  real*8, pointer  :: coords(:,:), forces(:,:), virialperatom(:,:), virialglobal(:)
  integer, pointer :: atomTypes(:)
  integer middleDum

  
  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_init_f", ier)
     stop
  endif
  ! Allocate memory via the KIM system
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine
  ier = kim_api_model_init_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_init", ier)
     stop
  endif


  ! Unpack data from KIM object
  !
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier);
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  pnAtomTypes = kim_api_get_data_f(pkim, "numberAtomTypes", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  patomTypesdum = kim_api_get_data_f(pkim, "atomTypes", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N)

  pcoor = kim_api_get_data_f(pkim, "coordinates", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  pvirialglob = kim_api_get_data_f(pkim, "virialGlobal", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor1d(virialglobdum, virialglobal, 6)

  pforces = kim_api_get_data_f(pkim, "forces", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(forcesdum, forces, DIM, N)

  pvirial = kim_api_get_data_f(pkim, "virialPerAtom", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(virialdum, virialperatom, 6, N)

  ! Set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, middleDum)

  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute", ier)
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
  'Z                        ' // &
  'V11                      ' // &
  'V22                      ' // &
  'V33                      ' // &
  'V23                      ' // &
  'V31                      ' // &
  'V12                      ")'
  print '(I2,"   ",3ES25.15,6ES25.15)', (I,forces(:,I),virialperatom(:,I),I=1,N)
  print *
  print '("Energy = ",ES25.15,"                              Global Virial = ",' // &
        '6ES25.15)', energy, virialglobal

  ! don't forget to destroy and deallocate
  call kim_api_model_destroy_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_destroy", ier)
     stop
  endif
  call kim_api_free(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_free", ier)
     stop
  endif

  stop
end program TEST_NAME_STR
