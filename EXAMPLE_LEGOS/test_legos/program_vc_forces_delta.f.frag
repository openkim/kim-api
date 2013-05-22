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
!    Ellad B. Tadmor
!    Toon Verstraelen
!    Stephen M. Whalen
!


!*******************************************************************************
!**
!**  PROGRAM vc_forces_delta
!**
!**  KIM compliant program to perform numerical derivative check on a model
!**
!**  Works with the following NBC methods:
!**        NEIGH_RVEC_H
!**        NEIGH_PURE_H
!**        NEIGH_RVEC_F
!**        NEIGH_PURE_F
!**        MI_OPBC_H
!**        MI_OPBC_F
!**        CLUSTER
!**
!**  Release: This file is part of the openkim-api.git package.
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
program vc_forces_delta
  use KIM_API
  implicit none

  integer, external  :: get_neigh_no_Rij
  integer, external  :: get_neigh_Rij
  integer, parameter :: nCellsPerSide  = 2
  integer, parameter :: DIM            = 3
  real*8,  parameter :: cutpad         = 0.75d0
  integer, parameter :: max_types      = 30     ! most species a Model can support
  integer, parameter :: max_NBCs       = 20     ! maximum number of NBC methods
  real*8,  parameter :: eps_prec       = epsilon(1.d0)
  integer, parameter :: ndisp          = 100    ! number of displacements in one test
  real*8   FCCspacing

  integer, parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(kind=kim_intptr), parameter  :: SizeOne = 1

  real*8, allocatable                  :: forces_old(:,:)
  character(len=3)                     :: model_types(max_types)
  character(len=KIM_KEY_STRING_LENGTH) :: model_NBCs(max_NBCs)
  integer                              :: num_types
  integer                              :: num_NBCs
  integer                              :: nfail
  character(len=4)                     :: passfail
  real*8                               :: energy_old
  real*8                               :: abs_mean_delta
  real*8                               :: error
  real*8                               :: abs_mean_error
  real*8,dimension(100)                :: deltas, deltas_estimated
  real*8,                  allocatable :: cluster_coords(:,:)
  real*8,                  allocatable :: cluster_disps(:,:)
  character(len=3),        allocatable :: cluster_types(:)
  integer I,J,type,idisp

  ! neighbor list
  integer,                  allocatable :: neighborList(:,:)
  integer(kind=kim_intptr), allocatable :: NLRvecLocs(:)
  double precision,         allocatable :: RijList(:,:,:)
  double precision,         allocatable :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character*80              :: testname     = "vc_forces_delta"
  character*80              :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer nbc  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
               ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier, idum, inbc
  integer numberOfParticles;   pointer(pnAtoms,numberOfParticles)
  integer numContrib;          pointer(pnumContrib,numContrib)
  integer numberParticleTypes; pointer(pnparticleTypes,numberParticleTypes)
  integer particleTypesdum(1); pointer(pparticleTypesdum,particleTypesdum)

  real*8 cutoff;               pointer(pcutoff,cutoff)
  real*8 energy;               pointer(penergy,energy)
  real*8 coordum(DIM,1);       pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1);     pointer(pforces,forcesdum)
  real*8 boxSideLengths(DIM);  pointer(pboxSideLengths,boxSideLengths)
  real*8, pointer  :: coords(:,:), forces(:,:)
  integer, pointer :: particleTypes(:)
  integer middleDum
  character(len=10000) :: test_descriptor_string
  real*8 rnd

  ! Initialize error flag
  ier = KIM_STATUS_OK

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Get list of particle types supported by the model
  !
  call Get_Model_Supported_Types(modelname, max_types, model_types, num_types, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "Get_Model_Supported_Types", ier)
     stop
  endif

  ! Get list of NBCs supported by the model
  !
  call Get_Model_NBC_methods(modelname, max_NBCs, model_NBCs, num_NBCs, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "Get_Model_NBC_methods", ier)
     stop
  endif

  ! Setup random cluster
  !
  allocate(cluster_coords(3,N),cluster_disps(3,N),cluster_types(N))
  do i=1,N
     call random_number(rnd)  ! return random number between 0 and 1
     type = 1 + int(rnd*num_types)
     cluster_types(i) = model_types(type)
  enddo
  FCCspacing = 1.d0  ! initially generate an FCC cluster with lattice
                     ! spacing equal to one. This is scaled below based
                     ! on the cutoff radius of the model.
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., &
                                cluster_coords, middleDum)
  ! Generate random displacements for all atoms
  !
  do I=1,N
     do J=1,DIM
        call random_number(rnd)  ! return random number between 0 and 1
        cluster_disps(J,I) = 0.1d0*(rnd-0.5d0)
     enddo
  enddo

  ! Print output header
  !
  print *
  print *,'VERIFICATION CHECK: DELTA DERIVATIVE VERIFICATION OF FORCES'
  print *
  print '(120(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname

  ! Loop over all NBCs and perform numerical derivative check for each one
  !
  do inbc = 1, num_NBCs

     ! Write out KIM descriptor string for Test for current NBC
     !
     call Write_KIM_descriptor(model_NBCs(inbc), max_types, model_types, num_types, &
                               test_descriptor_string, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "Write_KIM_descriptor", ier)
        stop
     endif

     ! Create empty KIM object conforming to fields in the KIM descriptor files
     ! of the Test and Model
     !
     ier = kim_api_string_init_f(pkim,trim(test_descriptor_string)//char(0),modelname)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_string_init_f", ier)
        stop
     endif

     ! Double check that the NBC method being used is what we think it is
     !
     pNBC_Method = kim_api_get_nbc_method_f(pkim, ier) ! don't forget to free
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_nbc_method", ier)
        stop
     endif
     if (index(NBC_Method,trim(model_NBCs(inbc))).ne.1) then
        ier = KIM_STATUS_FAIL
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
              "Internal Error: Selected NBC method different from requested value", ier)
        stop
     endif

     ! Set NBC code based on selected NBC method
     !
     if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then
        nbc = 0
     elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
        nbc = 1
     elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
        nbc = 2
     elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
        nbc = 3
     elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
        nbc = 4
     elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
        nbc = 5
     elseif (index(NBC_Method,"CLUSTER").eq.1) then
        nbc = 6
     else
        ier = KIM_STATUS_FAIL
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "Unknown NBC method", ier)
        stop
     endif

     ! Allocate memory via the KIM system
     !
     call kim_api_allocate_f(pkim, N, num_types, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_allocate_f", ier)
        stop
     endif

     ! Allocate storage for neighbor lists and
     ! store pointers to neighbor list object and access function
     !
     if (nbc.le.5) then
        allocate(neighborList(N+1,N))
        if (nbc.eq.0.or.nbc.eq.2) then
           allocate(RijList(DIM,N+1,N), NLRvecLocs(3))
           NLRvecLocs(1) = loc(neighborList)
           NLRvecLocs(2) = loc(RijList)
           NLRvecLocs(3) = N
           ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                            "kim_api_set_data_f", ier)
              stop
           endif
        else
           ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(neighborList))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                            "kim_api_set_data_f", ier)
              stop
           endif
        endif
     endif

     ! Set pointer in KIM object to neighbor list routine
     !
     if (nbc.eq.0) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     elseif (nbc.eq.1) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     elseif (nbc.eq.2) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     elseif (nbc.eq.3) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     elseif (nbc.eq.4) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     elseif (nbc.eq.5) then
        ier = kim_api_set_data_f(pkim, "get_neigh", SizeOne, loc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_set_data_f", ier)
           stop
        endif
     endif

     ! Initialize Model
     !
     ier = kim_api_model_init_f(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_model_init", ier)
        stop
     endif

     ! Unpack data from KIM object
     !
     call kim_api_getm_data_f(pkim, ier, &
          "numberOfParticles",           pnAtoms,           1,                               &
          "numberContributingParticles", pnumContrib,       TRUEFALSE(nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4), &
          "numberParticleTypes",         pnparticleTypes,   1,                               &
          "particleTypes",               pparticleTypesdum, 1,                               &
          "coordinates",                 pcoor,             1,                               &
          "cutoff",                      pcutoff,           1,                               &
          "boxSideLengths",              pboxSideLengths,   TRUEFALSE(nbc.eq.4.or.nbc.eq.5), &
          "energy",                      penergy,           1,                               &
          "forces",                      pforces,           1)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_getm_data_f", ier)
        stop
     endif
     call KIM_to_F90_int_array_1d(particleTypesdum, particleTypes, N)
     call KIM_to_F90_real_array_2d(coordum, coords, DIM, N)
     call KIM_to_F90_real_array_2d(forcesdum, forces, DIM, N)

     ! Scale reference FCC configuration based on cutoff radius.
     ! (This is only done once.)
     if (inbc.eq.1) then
        FCCspacing = 0.75d0*cutoff ! set the FCC spacing to a fraction
                                   ! of the cutoff radius
        do i=1,N
           cluster_coords(:,i) = FCCspacing*cluster_coords(:,i)
        enddo
        print '("Using FCC lattice parameter: ",f12.5)', FCCspacing
     endif

     ! Set values in KIM object
     !
     numberOfParticles   = N
     if (nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4) numContrib = N
     numberParticleTypes = num_types
     do i=1,N
        particleTypes(i) = kim_api_get_partcl_type_code_f(pkim,trim(cluster_types(i)),ier)
     enddo
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_get_partcl_type_code_f", ier)
        stop
     endif
     do i=1,N
        coords(:,i) = cluster_coords(:,i) + cluster_disps(:,i)
     enddo
     if (nbc.eq.4.or.nbc.eq.5) boxSideLengths(:) = 600.d0 ! large enough to make the cluster isolated


     do_update_list = .true.
     allocate(forces_old(DIM,N))
     allocate(coordsave(DIM,N))
     do idisp=1,ndisp
        ! Compute neighbor lists
        !
        if (nbc.le.5) then
           call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths,NBC_Method,  &
                                    do_update_list,coordsave,neighborList,RijList,ier)
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                            "update_neighborlist", ier)
              stop
           endif
        endif


        ! Call model compute to get forces (gradient)
        !
        ier = kim_api_model_compute_f(pkim)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_model_compute", ier)
           stop
        endif
        ! Copy forces and energy to a place where the model can not change them
        energy_old = energy ! may fail with gfortran cray pointer bug
        forces_old(:,:) = forces

        ! Generate random displacements for all atoms. The displacements are not
        ! just meant to distort the system, as above. Instead, these are actually
        ! used to test how well the change in energy deu to this displacement can
        ! be reproduced by a first-order approximation.
        !
        do I=1,N
           do J=1,DIM
              call random_number(rnd)  ! return random number between 0 and 1
              cluster_disps(J,I) = 1d-4*(rnd-0.5d0)
              coords(J,I) = coords(J,I) + cluster_disps(J,I)
           enddo
        enddo

        ! Call model compute again to get forces (gradient)
        !
        if (nbc.le.5) then
           call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths,NBC_Method,  &
                                    do_update_list,coordsave,neighborList,RijList,ier)
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                            "update_neighborlist", ier)
              stop
           endif
        endif
        ier = kim_api_model_compute_f(pkim)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                         "kim_api_model_compute", ier)
           stop
        endif

        ! Store real delta and estimated delta in array for later analysis
        !
        deltas(idisp) = energy_old - energy
        deltas_estimated(idisp) = sum((forces+forces_old)*cluster_disps/2)
     enddo

     ! Print output of statistical test
     !
     print '(41(''=''))'
     print '("NBC Method = ",A28)', NBC_Method(1:(index(NBC_Method,char(0))-1))
     print '(41(''=''))'
     print *
     print '(A5,2X,A15,2X,A15,2X,A15)', "IDisp", "Delta", "Delta Est", "Error"
     abs_mean_delta = sum(abs(deltas))/ndisp
     nfail = 0
     abs_mean_error = 0.0
     do idisp=1,ndisp
        error = abs(deltas(idisp)-deltas_estimated(idisp))
        abs_mean_error = abs_mean_error + error
        if (error > abs_mean_delta) then
            passfail = "FAIL"
            nfail = nfail + 1
        else
            passfail = ""
        endif
        print '(I5,2X,ES15.7,2X,ES15.7,2X,ES15.7,2X,A4)', &
               idisp, deltas(idisp), deltas_estimated(idisp), &
               error, passfail
     enddo
     abs_mean_error = abs_mean_error/ndisp
     print *
     print '(A30,ES15.7)', "Mean absolute Error:", abs_mean_error
     print '(A30,ES15.7)', "Mean absolute Delta:", abs_mean_delta
     print '(A30,ES15.7)', "Ratio:", abs_mean_error/abs_mean_delta
     print '(A30,I15)', "Number of epic failures:", nfail
     print *

     ! Free temporary storage
     !
     call free(pNBC_Method)
     deallocate(forces_old)
     deallocate(coordsave)
     if (nbc.le.5) then ! deallocate neighbor list storage
        deallocate(neighborList)
        if (nbc.eq.0.or.nbc.eq.2) then
           deallocate(NLRvecLocs)
           deallocate(RijList)
        endif
     endif
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

  enddo ! loop over NBC methods

  ! Print output footer
  !
  print *
  print '(120(''-''))'

  ! Free cluster storage
  !
  deallocate(cluster_coords,cluster_disps,cluster_types)

  stop

end program vc_forces_delta
