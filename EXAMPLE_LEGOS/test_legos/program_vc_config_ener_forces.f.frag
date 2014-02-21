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
!    Ellad B. Tadmor
!    Stephen M. Whalen
!


!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy and forces for an
!**  isolated cluster configuration read from file.
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
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none

  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  real(c_double), parameter :: cutpad         = 0.75_cd
  integer(c_int), parameter :: max_types      = 30     ! most species allowed in a config
  integer(c_int), parameter :: max_NBCs       = 20     ! maximum number of NBC methods
  real(c_double), parameter :: eps_prec       = epsilon(1.0_cd)
  integer(c_int)            :: in
  integer(c_int)            :: N
  real(c_double)            :: max_force_component
  real(c_double)            :: scaled_eps_prec
  integer(c_int)            :: SizeOne = 1
  integer(c_int)            :: num_types_in_config
  integer(c_int)            :: num_NBCs
  logical                   :: found
  integer(c_int)            :: i,j
  real(c_double)            :: force_err(DIM),ave_force_error
  character(len=KIM_KEY_STRING_LENGTH) :: types_in_config(max_types)
  character(len=KIM_KEY_STRING_LENGTH) :: model_NBCs(max_NBCs)

  !
  ! neighbor list
  !
  type(neighObject_type), target :: NLRvecLocs
  integer(c_int), allocatable, target :: neighborList(:,:)
  real(c_double), allocatable, target :: RijList(:,:,:)
  real(c_double), allocatable         :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname     = "TEST_NAME_STR"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: configfile
  character(len=KIM_KEY_STRING_LENGTH), pointer &
                          :: conf_types(:)    ! configuration atom types (element symbols)
  real(c_double), pointer :: conf_coors(:,:)  ! configuration coordinates
  real(c_double), pointer :: conf_forces(:,:) ! configuration forces
  real(c_double)          :: conf_energy      ! configuration energy

  character(len=KIM_KEY_STRING_LENGTH), pointer :: NBC_Method; type(c_ptr) :: pNBC_Method
  integer(c_int) nbc  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
               ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  type(c_ptr)             :: pkim
  integer(c_int)          :: ier, idum, inbc
  character(len=10000)    :: test_descriptor_string
  integer(c_int), pointer :: numberOfParticles;    type(c_ptr) pnAtoms
  integer(c_int), pointer :: numContrib;           type(c_ptr) pnumContrib
  integer(c_int), pointer :: numberParticleTypes;  type(c_ptr) pnparticleTypes
  integer(c_int), pointer :: particleTypes(:);     type(c_ptr) pparticleTypes
  real(c_double), pointer :: cutoff;               type(c_ptr) pcutoff
  real(c_double), pointer :: energy;               type(c_ptr) penergy
  real(c_double), pointer :: coords(:,:);          type(c_ptr) pcoor
  real(c_double), pointer :: forces(:,:);          type(c_ptr) pforces
  real(c_double), pointer :: boxSideLengths(:);    type(c_ptr) pboxSideLengths

  ! Initialize error flag
  ier = KIM_STATUS_OK

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Get filename of file containing configuration
  print '("Please enter the configuration file name: ")'
  read(*,*) configfile

  ! Read in configuration file
  in = 20
  open(unit=in,file=trim(configfile))
  read(in,*,err=10) N
  if (N<1) then
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
           "Error: Number of atoms is less than 1", ier)
     stop
  endif
  allocate(conf_types(N),conf_coors(DIM,N),conf_forces(DIM,N))  ! dynamically allocate memory
!@--- IN FUTURE WILL BE CHANGED TO READ IN PERIODIC BOX INFO
!@--- read(in,*,err=20) conf_boxsize(:)
  do i=1,N
     read(in,*,err=30) conf_types(i), conf_coors(:,i)
  enddo
  read(in,*,err=40) conf_energy
  max_force_component = 0.0_cd
  do i=1,N
     read(in,*,err=50) conf_forces(:,i)
     max_force_component = max(max_force_component, &
                               abs(conf_forces(1,i)), &
                               abs(conf_forces(2,i)), &
                               abs(conf_forces(3,i)))
  enddo
  scaled_eps_prec = max_force_component*eps_prec
  goto 100

  ! Error handing on input
  !

10  continue
    ier = KIM_STATUS_FAIL
    idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
           "Error reading in the number of atoms N in the configuration", ier)
    stop

!20 continue
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
          "Error reading in the periodic box size in the configuration", ier)
   stop

30 continue
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
          "Error reading in a data line (species + coors) in configuration", ier)
   stop

40 continue
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
          "Error reading in the configuration energy", ier)
   stop

50 continue
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
          "Error reading in forces in configuration", ier)
   stop



  ! No errors -- continue from here

100 continue

  ! Generate list of species appearing in config
  !
  num_types_in_config = 1
  types_in_config(1) = conf_types(1)
  do i=2,N
     found = .false.
     do j=1,num_types_in_config
        found = (trim(conf_types(i))==trim(types_in_config(j)))
        if (found) exit
     enddo
     if (.not.found) then
        num_types_in_config = num_types_in_config + 1
        types_in_config(num_types_in_config) = conf_types(i)
     endif
  enddo

  ! Get list of NBCs supported by the model
  !
  call Get_Model_NBC_methods(modelname, max_NBCs, model_NBCs, num_NBCs, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Get_Model_NBC_methods", ier)
     stop
  endif

  ! Print output header
  !
  print *
  print *,'VERIFICATION CHECK: COMPUTE ENERGY AND FORCES FOR CONFIGURATION'
  print *
  print '(120(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)
  print *
  print *,'*** Read-in Configuration ***'
  print *
  print '(a,f20.10)', "Energy = ",conf_energy
  print *
  print '(A6,2X,A4,2X,A)',"Atom","Type","Coordinates"
  do i=1,N
     print '(I6,2X,A4,2X,3ES25.15)',i,conf_types(i), conf_coors(:,i)
  enddo
  print *
  print '(A6,2X,A4,2X,A)',"Atom","Type","Force"
  do i=1,N
     print '(I6,2X,A4,2X,3ES25.15)',i,conf_types(i),conf_forces(:,i)
  enddo
  print *

  ! Loop over all NBCs and compute energy and forces for each one
  !
  do inbc = 1, num_NBCs

     ! Write out KIM descriptor string for Test for current NBC
     !
     call Write_KIM_descriptor(model_NBCs(inbc), max_types, types_in_config, &
                               num_types_in_config, test_descriptor_string, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Write_KIM_descriptor", ier)
        stop
     endif

     ! Create empty KIM object conforming to fields in the KIM descriptor files
     ! of the Test and Model
     !
     ier = kim_api_string_init(pkim,trim(test_descriptor_string)//char(0),modelname)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_string_init", ier)
        stop
     endif

     ! Double check that the NBC method being used is what we think it is
     !
     pNBC_Method = kim_api_get_nbc_method(pkim, ier) ! don't forget to free
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_nbc_method", ier)
        stop
     endif
     call c_f_pointer(pNBC_Method, NBC_Method)
     if (index(NBC_Method,trim(model_NBCs(inbc))).ne.1) then
        ier = KIM_STATUS_FAIL
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
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
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Unknown NBC method", ier)
        stop
     endif

     ! Allocate memory via the KIM system
     !
     call kim_api_allocate(pkim, N, num_types_in_config, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_allocate", ier)
        stop
     endif

     ! Allocate storage for neighbor lists and
     ! store pointers to neighbor list object and access function
     !
     if (nbc.le.5) then
        allocate(neighborList(N+1,N))
        if (nbc.eq.0 .or. nbc.eq.2) then
           allocate(RijList(DIM,N+1,N))
           NLRvecLocs%pneighborList = c_loc(neighborList)
           NLRvecLocs%pRijList = c_loc(RijList)
           NLRvecLocs%NNeighbors = N
           ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(NLRvecLocs))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "kim_api_set_data", ier)
              stop
           endif
        else
           ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(neighborList))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "kim_api_set_data", ier)
              stop
           endif
        endif
     endif

     ! Set pointer in KIM object to neighbor list routine
     !
     if (nbc.eq.0) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_method", ier)
           stop
        endif
     elseif (nbc.eq.1) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_method", ier)
           stop
        endif
     elseif (nbc.eq.2) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_method", ier)
           stop
        endif
     elseif (nbc.eq.3) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_method", ier)
           stop
        endif
     elseif (nbc.eq.4) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_multiple_data", ier)
           stop
        endif
     elseif (nbc.eq.5) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh_no_Rij))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_multiple_data", ier)
           stop
        endif
     endif

     ! Initialize Model
     !
     ier = kim_api_model_init(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_init", ier)
        stop
     endif

     ! Unpack data from KIM object
     !
     call kim_api_getm_data(pkim, ier, &
          "numberOfParticles",           pnAtoms,           1,                               &
          "numberContributingParticles", pnumContrib,       TRUEFALSE(nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4), &
          "numberParticleTypes",         pnparticleTypes,   1,                               &
          "particleTypes",               pparticleTypes,    1,                               &
          "coordinates",                 pcoor,             1,                               &
          "cutoff",                      pcutoff,           1,                               &
          "boxSideLengths",              pboxSideLengths,   TRUEFALSE(nbc.eq.4.or.nbc.eq.5), &
          "energy",                      penergy,           1,                               &
          "forces",                      pforces,           1)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_getm_data", ier)
        stop
     endif
     call c_f_pointer(pnAtoms, numberOfParticles)
     if ((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)) call c_f_pointer(pnumContrib, numContrib)
     call c_f_pointer(pnparticleTypes, numberParticleTypes)
     call c_f_pointer(pparticleTypes,  particleTypes, [N])
     call c_f_pointer(pcoor, coords, [DIM,N])
     call c_f_pointer(pcutoff, cutoff)
     if ((nbc.eq.4).or.(nbc.eq.5)) call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])
     call c_f_pointer(penergy, energy)
     call c_f_pointer(pforces, forces, [DIM,N])

     ! Set values in KIM object
     !
     numberOfParticles   = N
     if (nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4) numContrib = N
     numberParticleTypes = num_types_in_config
     do i=1,N
        particleTypes(i) = kim_api_get_partcl_type_code(pkim,trim(conf_types(i)),ier)
     enddo
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_partcl_type_code", ier)
        stop
     endif
     do i=1,N
        coords(:,i) = conf_coors(:,i)
     enddo
     if (nbc.eq.4.or.nbc.eq.5) boxSideLengths(:) = 600.0_cd ! large enough to make the cluster isolated

     ! Compute neighbor lists
     !
     if (nbc.le.5) then
        do_update_list = .true.
        allocate(coordsave(DIM,N))
        call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths,NBC_Method,  &
                                 do_update_list,coordsave,neighborList,RijList,ier)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "update_neighborlist", ier)
           stop
        endif
     endif

     ! Call model compute to get energy and forces
     !
     ier = kim_api_model_compute(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_compute", ier)
        stop
     endif

     ! print results to screen
     !
     print '(41(''=''))'
     print '("NBC Method = ",A28)', NBC_Method(1:(index(NBC_Method,char(0))-1))
     print '(41(''=''))'
     print *
     print '(a,f20.10)', "Energy = ",energy
     print *
     print '(A6,2X,A4,2X,A)',"Atom","Type","Computed Force"
     do i=1,N
        print '(I6,2X,A4,2X,3ES25.15)',i,conf_types(i),forces(:,i)
     enddo
     print *
     print *,'*** Energy and Forces Agreement ***'
     print *
     print '(A6,2X,A4,2X,A)',"Atom","Type","Force Error"
     ave_force_error = 0.0_cd
     do i=1,N
        do j=1,DIM
           force_err(j) = abs(forces(j,i)-conf_forces(j,i))/ &
                          max(abs(conf_forces(j,i)),scaled_eps_prec)
        enddo
        print '(I6,2X,A4,2X,3ES25.15)',i,conf_types(i),force_err(:)
        ave_force_error = ave_force_error + dot_product(force_err,force_err)
     enddo
     ave_force_error = sqrt(ave_force_error)/dble(DIM*N)
     print *
     print '(a,ES25.15)', "Average force error = ",ave_force_error
     print *
     print '(a,ES25.15)', "Energy error        = ",abs(energy-conf_energy)/ &
                                                   max(abs(conf_energy),eps_prec)
     print *

     ! Free temporary storage
     !
     call KIM_API_c_free(pNBC_Method); NBC_Method => null()  ! free the memory
     if (nbc.le.5) then ! deallocate neighbor list storage
        deallocate(neighborList)
        deallocate(coordsave)
        if (nbc.eq.0.or.nbc.eq.2) then
           deallocate(RijList)
        endif
     endif
     ier = kim_api_model_destroy(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_destroy", ier)
        stop
     endif
     call kim_api_free(pkim, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_free", ier)
        stop
     endif

  enddo ! loop over NBC methods

  ! Print output footer
  !
  print *
  print '(120(''-''))'

  ! Free configuration storage
  !
  deallocate(conf_types,conf_coors,conf_forces)
  stop

end program TEST_NAME_STR
