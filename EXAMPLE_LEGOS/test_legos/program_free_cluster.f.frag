!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an
!**  isolated cluster of SPECIES_NAME_STR atoms
!**
!**  Works with the following NBC methods:
!**        MI_OPBC_H
!**        MI_OPBC_F
!**        NEIGH_PURE_H
!**        NEIGH_PURE_F
!**        NEIGH_RVEC_F
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************

#include "KIM_API_status.h"
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program TEST_NAME_STR
  use KIM_API
  implicit none

  integer,          external  :: get_neigh_no_Rij
  integer,          external  :: get_neigh_Rij
  double precision, parameter :: FCCspacing     = FCC_SPACING_STR
  integer,          parameter :: nCellsPerSide  = 2
  integer,          parameter :: DIM            = 3
  integer,          parameter :: ATypes         = 1
  integer,          parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(kind=kim_intptr), parameter :: SizeOne        = 1

  ! neighbor list
  integer,                  allocatable :: neighborList(:,:)
  integer(kind=kim_intptr), allocatable :: NLRvecLocs(:)
  double precision,         allocatable :: RijList(:,:,:)
  double precision                      :: cutpad = CUTOFF_PADDING_STR

  !
  ! KIM variables
  !
  character*80              :: testname     = "TEST_NAME_STR"
  character*80              :: modelname
  character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer nbc  ! 0- MI_OPBC_H, 1- MI_OPBC_F, 2- NEIGH_PURE_H, 3- NEIGH_PURE_F, 4- NEIGH-RVCE-F
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier, idum
  integer numberOfParticles;   pointer(pnAtoms,numberOfParticles)
  integer numContrib;          pointer(pnumContrib,numContrib)
  integer numberParticleTypes; pointer(pnparticleTypes,numberParticleTypes)
  integer particleTypesdum(1); pointer(pparticleTypesdum,particleTypesdum)

  real*8 cutoff;               pointer(pcutoff,cutoff)
  real*8 energy;               pointer(penergy,energy)
  real*8 virialglobdum(1);     pointer(pvirialglob,virialglobdum)
  real*8 coordum(DIM,1);       pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1);     pointer(pforces,forcesdum)
  real*8 boxSideLengths(DIM);  pointer(pboxSideLengths,boxSideLengths)
  integer I
  real*8, pointer  :: coords(:,:), forces(:,:), virial_global(:)
  integer, pointer :: particleTypes(:)
  integer middleDum

  
  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_init_f", ier)
     stop
  endif

  ! determine which NBC scenerio to use
  pNBC_Method = kim_api_get_nbc_method_f(pkim, ier) ! don't forget to free
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"MI_OPBC_H").eq.1) then
     nbc = 0
  elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
     nbc = 1
  elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
     nbc = 2
  elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
     nbc = 3
  elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
     nbc = 4
  else
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error_f(__LINE__, __FILE__, "Unknown NBC method", ier)
     stop
  endif

  ! Allocate memory via the KIM system
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! Allocate and store pointers to neighbor list object and access function
  allocate(neighborList(N+1, N))
  if (nbc.eq.4) then
     allocate(RijList(DIM,N+1, N))
  endif
  !
  if (nbc.le.3) then
     ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(neighborList))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  else
     allocate(NLRvecLocs(3))
     NLRvecLocs(1) = loc(neighborList)
     NLRvecLocs(2) = loc(RijList)
     NLRvecLocs(3) = N
     ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  endif

  if (nbc.eq.0) then
     ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.1) then
     ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.2) then
     ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.3) then
     ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.4) then
     ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  endif

  ! call model's init routine
  ier = kim_api_model_init_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_init", ier)
     stop
  endif


  ! Unpack data from KIM object
  !
  call kim_api_getm_data_f(pkim, ier, &
       "numberOfParticles",           pnAtoms,          1,                                   &
       "numberContributingParticles", pnumContrib,      TRUEFALSE((nbc.eq.0).or.(nbc.eq.2)), &
       "numberParticleTypes",         pnparticleTypes,  1,                                   &
       "particleTypes",               pparticleTypesdum,1,                                   &
       "coordinates",                 pcoor,            1,                                   &
       "cutoff",                      pcutoff,          1,                                   &
       "boxSideLengths",              pboxSideLengths,  TRUEFALSE(nbc.le.1),                 &
       "energy",                      penergy,          1,                                   &
       "virial",                      pvirialglob,      1,                                   &
       "forces",                      pforces,          1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
     stop
  endif

  call KIM_to_F90_int_array_1d(particleTypesdum, particleTypes, N)
  call KIM_to_F90_real_array_2d(coordum, coords, DIM, N)
  call KIM_to_F90_real_array_1d(virialglobdum, virial_global, 6)
  call KIM_to_F90_real_array_2d(forcesdum, forces, DIM, N)

  ! Set values
  numberOfParticles = N
  if ((nbc.eq.0).or.(nbc.eq.2)) numContrib = N
  numberParticleTypes = ATypes
  particleTypes(:)    = kim_api_get_partcl_type_code_f(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_partcl_type_code_f", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, middleDum)
  if (nbc.le.1) boxSideLengths(:)  = 600.d0 ! large enough to make the cluster isolated

  ! compute neighbor lists
  if (nbc.eq.0) then
     call MI_OPBC_cluster_neighborlist(.true., N, coords, (cutoff+cutpad), boxSideLengths, neighborList)
  elseif (nbc.eq.1) then
     call MI_OPBC_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), boxSideLengths, neighborList)
  elseif (nbc.eq.2) then
     call NEIGH_PURE_cluster_neighborlist(.true., N, coords, (cutoff+cutpad), neighborList)
  elseif (nbc.eq.3) then
     call NEIGH_PURE_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), neighborList)
  elseif (nbc.eq.4) then
     call NEIGH_RVEC_F_cluster_neighborlist(N, coords, (cutoff+cutpad), N, neighborList, RijList)
  endif

  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute", ier)
     stop
  endif

  ! print results to screen
  print '(80(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname
  print '("Using NBC: ",A)', NBC_Method(1:(index(NBC_Method,char(0))-1))
  print '("Forces:")'
  print '("Atom     ' // &
  'X                        ' // &
  'Y                        ' // &
  'Z                        ")'
  print '(I2,"   ",3ES25.15)', (I,forces(:,I),I=1,N)
  print *
  print '("Energy        = ",ES25.15)', energy
  print '("Global Virial = ",3ES25.15)', (virial_global(I),I=1,3)
  print '("                ",3ES25.15)', (virial_global(I),I=4,6)

  ! Don't forget to free and/or deallocate
  call free(pNBC_Method) 
  deallocate(neighborList)
  if (nbc.eq.4) then
     deallocate(NLRvecLocs)
     deallocate(RijList)
  endif

  call kim_api_model_destroy_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_destroy", ier)
     stop
  endif
  call kim_api_free(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_free", ier)
     stop
  endif

  stop
end program TEST_NAME_STR
