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
! Copyright (c) 2013--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
!

!
! Release: This file is part of the kim-api.git package.
!


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

!-------------------------------------------------------------------------------
!
! module mod_neighborlist :
!
!    Module contains type and routines related to neighbor list calculation
!
!-------------------------------------------------------------------------------

module mod_neighborlist

  use, intrinsic :: iso_c_binding
  use KIM_API_F03

  public setup_neighborlist_KIM_access, get_neigh

  type neighObject_type
     integer(c_int), pointer :: neighborList(:,:)
     real(c_double), pointer :: RijList(:,:,:)
  end type neighObject_type

contains

!-------------------------------------------------------------------------------
!
! setup_neighborlist_KIM_access :
!
!    Store necessary pointers in KIM API object to access the neighbor list
!    data and methods.
!
!-------------------------------------------------------------------------------
subroutine setup_neighborlist_KIM_access(pkim, neighObject)
  implicit none

  !-- Transferred variables
  type(c_ptr),                    intent(in) :: pkim
  type(neighObject_type), target, intent(in) :: neighObject

  !-- Local variables
  integer(c_int), parameter :: SizeOne = 1
  integer(c_int)            :: ier, idum

  ! store location of neighObject variable
  !
  ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(neighObject))
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_data", ier)
     stop
  endif
  ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh))
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_method", ier)
     stop
  endif

  return

end subroutine setup_neighborlist_KIM_access

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
integer(c_int) function get_neigh(pkim,mode,request,part,numnei,pnei1part, &
                                  pRij) bind(c)
  implicit none

  !-- Transferred variables
  type(c_ptr),    intent(in)  :: pkim
  integer(c_int), intent(in)  :: mode
  integer(c_int), intent(in)  :: request
  integer(c_int), intent(out) :: part
  integer(c_int), intent(out) :: numnei
  type(c_ptr),    intent(out) :: pnei1part
  type(c_ptr),    intent(out) :: pRij

  !-- Local variables
  integer(c_int), parameter :: DIM = 3
  integer(c_int), save :: iterVal = 0
  integer(c_int)  N
  integer(c_int)  partToReturn
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  type(neighObject_type), pointer :: neighObject; type(c_ptr) :: pneighObject
  integer(c_int)  ier, idum

  ! unpack number of particles
  pnParts = kim_api_get_data(pkim, "numberOfParticles", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pnParts, numberOfParticles)

  ! unpack neighbor list object
  pneighObject = kim_api_get_data(pkim, "neighObject", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pneighObject, neighObject)

  N = size(neighObject%neighborList, 2)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_neigh = KIM_STATUS_NEIGH_ITER_INIT_OK
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_neigh = KIM_STATUS_NEIGH_ITER_PAST_END
           return
        else
           partToReturn = iterVal
        endif
     else
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Invalid request in get_neigh", &
                                    KIM_STATUS_NEIGH_INVALID_REQUEST)
        get_neigh = KIM_STATUS_NEIGH_INVALID_REQUEST
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Invalid part ID in get_neigh", &
                                    KIM_STATUS_PARTICLE_INVALID_ID)
        get_neigh = KIM_STATUS_PARTICLE_INVALID_ID
        return
     else
        partToReturn = request
     endif
  else ! not iterator or locator mode
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Invalid mode in get_neigh", &
                                 KIM_STATUS_NEIGH_INVALID_MODE)
     get_neigh = KIM_STATUS_NEIGH_INVALID_MODE
     return
  endif

  ! set the returned part
  part = partToReturn

  ! set the returned number of neighbors for the returned part
  numnei = neighObject%neighborList(1,part)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighObject%neighborList(2,part))

  ! set pointer to Rij to appropriate value
  if (associated(neighObject%RijList)) then
    pRij = c_loc(neighObject%RijList(1,1,part))
  else
    pRij = c_null_ptr
  endif

  get_neigh = KIM_STATUS_OK
  return
end function get_neigh

end module mod_neighborlist

!*******************************************************************************
!**
!**  PROGRAM vc_forces_numer_deriv
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
!*******************************************************************************

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program vc_forces_numer_deriv
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  real(c_double), parameter :: cutpad         = 0.75_cd
  integer(c_int), parameter :: max_species      = 200 ! most species supported
  integer(c_int), parameter :: max_NBCs       = 20 ! maximum number of NBCs
  real(c_double), parameter :: eps_prec       = epsilon(1.0_cd)
  real(c_double)  FCCspacing

  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter            :: SizeOne = 1
  real(c_double), allocatable          :: forces_num(:,:)
  real(c_double), allocatable          :: forces_num_err(:,:)
  character(len=KIM_KEY_STRING_LENGTH) :: model_species(max_species)
  character(len=KIM_KEY_STRING_LENGTH) :: model_NBCs(max_NBCs)
  integer(c_int)                       :: num_species
  integer(c_int)                       :: num_NBCs
  character(len=5)                     :: passfail
  real(c_double)                       :: forcediff
  real(c_double)                       :: forcediff_sumsq
  real(c_double)                       :: weight
  real(c_double)                       :: weight_sum
  real(c_double)                       :: alpha
  real(c_double)                       :: term
  real(c_double)                       :: term_max
  real(c_double), allocatable          :: cluster_coords(:,:)
  real(c_double), allocatable          :: cluster_disps(:,:)
  character(len=KIM_KEY_STRING_LENGTH), allocatable :: cluster_species(:)
  integer(c_int) I,J,Imax,Jmax,species

  !
  ! neighbor list
  !
  type(neighObject_type), target :: neighObject
  real(c_double), allocatable :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname     = "vc_forces_numer_deriv"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
  ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  integer(c_int) nbc
  type(c_ptr) pkim
  integer(c_int) ier, idum, inbc
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  integer(c_int), pointer :: numContrib;          type(c_ptr) :: pnumContrib
  integer(c_int), pointer :: numberOfSpecies;     type(c_ptr) :: pnOfSpecies
  integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) :: pparticleSpecies
  real(c_double), pointer :: cutoff;              type(c_ptr) :: pcutoff
  real(c_double), pointer :: energy;              type(c_ptr) :: penergy
  real(c_double), pointer :: coords(:,:);         type(c_ptr) :: pcoor
  real(c_double), pointer :: forces(:,:);         type(c_ptr) :: pforces
  real(c_double), pointer :: boxSideLengths(:);   type(c_ptr) :: pboxSideLengths
  integer(c_int) middleDum
  character(len=10000) :: test_descriptor_string
  real(c_double) rnd, deriv, deriv_err

  term_max = 0.0_cd ! initialize

  ! Initialize error flag
  ier = KIM_STATUS_OK

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Get list of particle species supported by the model
  !
  call Get_Model_Supported_Species(modelname, max_species, model_species, num_species, &
                                 ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Get_Model_Supported_Species", ier)
     stop
  endif

  ! Get list of NBCs supported by the model
  !
  call Get_Model_NBC_methods(modelname, max_NBCs, model_NBCs, num_NBCs, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Get_Model_NBC_methods", ier)
     stop
  endif

  ! Setup random cluster
  !
  allocate(cluster_coords(3,N),cluster_disps(3,N),cluster_species(N))
  do i=1,N
     call random_number(rnd)  ! return random number between 0 and 1
     species = 1 + int(rnd*num_species)
     cluster_species(i) = model_species(species)
  enddo
  FCCspacing = 1.0_cd  ! initially generate an FCC cluster with lattice
                     ! spacing equal to one. This is scaled below based
                     ! on the cutoff radius of the model.
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., &
                                cluster_coords, middleDum)
  ! Generate random displacements for all particles
  !
  do I=1,N
     do J=1,DIM
        call random_number(rnd)  ! return random number between 0 and 1
        cluster_disps(J,I) = 0.1_cd*(rnd-0.5_cd)
     enddo
  enddo

  ! Print output header
  !
  print *
  print *,'VERIFICATION CHECK: NUMERICAL DERIVATIVE VERIFICATION OF FORCES'
  print *
  print '(120(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! Loop over all NBCs and perform numerical derivative check for each one
  !
  do inbc = 1, num_NBCs

     ! Write out KIM descriptor string for Test for current NBC
     !
     call Write_KIM_descriptor(model_NBCs(inbc), max_species, model_species, &
                               num_species, test_descriptor_string, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Write_KIM_descriptor", ier)
        stop
     endif

     ! Create empty KIM object conforming to fields in the KIM descriptor files
     ! of the Test and Model
     !
     ier = kim_api_string_init(pkim,trim(test_descriptor_string),modelname)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_string_init", ier)
        stop
     endif

     ! Double check that the NBC method being used is what we think it is
     !
     ier = kim_api_get_nbc_method(pkim, NBC_Method)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_nbc_method", ier)
        stop
     endif
     if (index(NBC_Method,trim(model_NBCs(inbc))).ne.1) then
        ier = KIM_STATUS_FAIL
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
         "Internal Error: Selected NBC method different from requested value", &
         ier)
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
     call kim_api_allocate(pkim, N, num_species, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_allocate", ier)
        stop
     endif

     ! Allocate storage for neighbor lists and
     ! store pointers to neighbor list object and access function
     !
     if (nbc.le.5) then
       allocate(neighObject%neighborList(N+1,N))
       if (nbc.eq.0.or.nbc.eq.2) then
         allocate(neighObject%RijList(DIM,N+1,N))
       endif
       ier = kim_api_set_data(pkim, "neighObject", SizeOne, &
                              c_loc(neighObject))
       if (ier.lt.KIM_STATUS_OK) then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
           "kim_api_set_data", ier)
         stop
       endif
     endif

     ! Set pointer in KIM object to neighbor list routine
     !
     if (nbc.ne.6) then
        ier = kim_api_set_method(pkim, "get_neigh", SizeOne, &
                                 c_funloc(get_neigh))
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_set_method", ier)
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
          "numberOfParticles",           pnParts,           1,                               &
          "numberContributingParticles", pnumContrib,       TRUEFALSE(nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4), &
          "numberOfSpecies",             pnOfSpecies,       1,                               &
          "particleSpecies",             pparticleSpecies,  1,                               &
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
     call c_f_pointer(pnParts,          numberOfParticles)
     call c_f_pointer(pnOfSpecies,      numberOfSpecies)
     call c_f_pointer(pparticleSpecies, particleSpecies, [N])
     call c_f_pointer(pcoor,            coords,          [DIM,N])
     call c_f_pointer(pcutoff,          cutoff)
     call c_f_pointer(penergy,          energy)
     call c_f_pointer(pforces,          forces,          [DIM,N])
     if (nbc.eq.0.or.nbc.eq.1.or.nbc.eq.4) call c_f_pointer(pnumContrib, &
                                                            numContrib)
     if (nbc.eq.4.or.nbc.eq.5) call c_f_pointer(pboxSideLengths, &
                                                boxSideLengths, [DIM])

     ! Scale reference FCC configuration based on cutoff radius.
     ! (This is only done once.)
     if (inbc.eq.1) then
        FCCspacing = 0.75_cd*cutoff ! set the FCC spacing to a fraction
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
     numberOfSpecies = num_species
     do i=1,N
        particleSpecies(i) = kim_api_get_species_code(pkim, &
                                                      trim(cluster_species(i)),&
                                                      ier)
     enddo
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_species_code", ier)
        stop
     endif
     do i=1,N
        coords(:,i) = cluster_coords(:,i) + cluster_disps(:,i)
     enddo
     ! set boxSideLengths large enough to make the cluster isolated
     if (nbc.eq.4.or.nbc.eq.5) boxSideLengths(:) = 600.0_cd

     ! Compute neighbor lists
     !
     if (nbc.le.5) then
        do_update_list = .true.
        allocate(coordsave(DIM,N))
        call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                 NBC_Method,do_update_list,coordsave, &
                                 neighObject,ier)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "update_neighborlist", ier)
           stop
        endif
     endif

     ! Call model compute to get forces (gradient)
     !
     ier = kim_api_model_compute(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_compute", ier)
        stop
     endif

     ! Print results to screen
     !
     print '(41(''=''))'
     print '("NBC Method = ",A28,"  Energy = ",ES25.15)', trim(NBC_Method), &
                                                          energy
     print '(41(''=''))'
     print *

     ! Turn off force computation
     !
     call kim_api_set_compute(pkim, "forces", KIM_COMPUTE_FALSE, ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_set_compute", ier)
        stop
     endif

     ! Compute gradient using numerical differentiation
     !
     allocate(forces_num(DIM,N),forces_num_err(DIM,N))
     do I=1,N
        do J=1,DIM
           call compute_numer_deriv(I,J,pkim,DIM,N,coords,cutoff,cutpad,   &
                                    boxSideLengths,NBC_Method,do_update_list, &
                                    coordsave,neighObject,deriv,deriv_err,ier)
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "compute_numer_deriv", ier)
              stop
           endif
           forces_num(J,I) = -deriv
           forces_num_err(J,I) = deriv_err
        enddo
     enddo

     ! Continue printing results to screen
     !
     print '(A6,2X,A4,2X,A3,2X,2A25,3A15,2X,A4)',"Part","Spec","Dir", &
           "Force_model", "Force_numer", "Force diff", "pred error", "weight", &
           "stat"
     forcediff_sumsq = 0.0_cd
     weight_sum = 0.0_cd
     do I=1,N
        do J=1,DIM
           forcediff = abs(forces(J,I)-forces_num(J,I))
           if (forcediff<forces_num_err(J,I)) then
              passfail = "ideal"
           else
              passfail = "     "
           endif
           weight = max(abs(forces_num(J,I)),eps_prec)/ &
                    max(abs(forces_num_err(J,I)),eps_prec)
           term = weight*forcediff**2
           if (term.gt.term_max) then
              term_max = term
              Imax = I
              Jmax = J
           endif
           forcediff_sumsq = forcediff_sumsq + term
           weight_sum = weight_sum + weight
           if (J.eq.1) then
              print '(I6,2X,I4,2X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
                     I,particleSpecies(I),J,forces(J,I),forces_num(J,I), &
                     forcediff,forces_num_err(J,I),weight,passfail
           else
              print '(14X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
                     J,forces(J,I),forces_num(J,I), &
                     forcediff,forces_num_err(J,I),weight,passfail
           endif
        enddo
        print *
     enddo
     alpha = sqrt(forcediff_sumsq/weight_sum)/dble(DIM*N)
     print *
     print '("alpha = |Force_model - Force_numer|_w/(DIM*N) = ",ES15.5," (units of force)")', &
           alpha
     print *
     print '(''Maximum term obtained for Part = '',I6,'', Dir = '',I1,' // &
        ''', forcediff = '',ES15.5, '', forcediff/force_model = '',ES15.5)', &
        Imax,Jmax,abs(forces(Jmax,Imax)-forces_num(Jmax,Imax)),           &
        abs(forces(Jmax,Imax)-forces_num(Jmax,Imax))/abs(forces(Jmax,Imax))

     ! Free temporary storage
     !
     deallocate(forces_num)
     deallocate(forces_num_err)
     if (nbc.le.5) then ! deallocate neighbor list storage
        deallocate(neighObject%neighborList)
        deallocate(coordsave)
        if (nbc.eq.0.or.nbc.eq.2) then
           deallocate(neighObject%RijList)
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

  ! Free cluster storage
  !
  deallocate(cluster_coords,cluster_disps,cluster_species)

  stop

end program vc_forces_numer_deriv

!-------------------------------------------------------------------------------
!
!  Get number and identities of particle species supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Species(modelname, max_species, model_species, &
                                       num_species, ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!-- Transferred variables
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: modelname
integer(c_int),                       intent(in)   :: max_species
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_species(max_species)
integer(c_int),                       intent(out)  :: num_species
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) :: i
integer(c_int) :: maxStringLength

!-- KIM variables
character(len=KIM_KEY_STRING_LENGTH) :: species
type(c_ptr) pkim

! Initialize error flag
ier = KIM_STATUS_FAIL

! Generate empty KIM object containing generic model info
ier = kim_api_model_info(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Get species supported by the model
ier = kim_api_get_num_model_species(pkim, num_species, maxStringLength)
if (ier.lt.KIM_STATUS_OK) return
if (num_species.gt.max_species) then
  ier = KIM_STATUS_FAIL
  return
endif
do i=1,num_species
   ier = kim_api_get_model_species(pkim, i, species)
   if (ier.lt.KIM_STATUS_OK) return
   model_species(i) = trim(species)
enddo

ier = KIM_STATUS_OK
return

end subroutine Get_Model_Supported_Species

!-------------------------------------------------------------------------------
!
!  Get the number and identities of NBCs supported by the Model.
!
!-------------------------------------------------------------------------------
subroutine Get_Model_NBC_methods(modelname, max_NBCs, model_NBCs, num_NBCs, ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!-- Transferred variables
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: modelname
integer(c_int),                       intent(in)   :: max_NBCs
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_NBCs(max_NBCs)
integer(c_int),                       intent(out)  :: num_NBCs
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) :: index

!-- KIM variables
type(c_ptr) pkim

! Initialize error flag
ier = KIM_STATUS_OK

! Generate empty KIM object containing generic model info
ier = kim_api_model_info(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Identify supported NBCs by seeking index to each of the NBCs in the KIM model
num_NBCs = 0

! NEIGH_RVEC_H
index = kim_api_get_index(pkim, "NEIGH_RVEC_H", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_RVEC_H"
endif

! NEIGH_PURE_H
index = kim_api_get_index(pkim, "NEIGH_PURE_H", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_PURE_H"
endif

! NEIGH_RVEC_F
index = kim_api_get_index(pkim, "NEIGH_RVEC_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_RVEC_F"
endif

! NEIGH_PURE_F
index = kim_api_get_index(pkim, "NEIGH_PURE_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "NEIGH_PURE_F"
endif

! MI_OPBC_H
index = kim_api_get_index(pkim, "MI_OPBC_H", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "MI_OPBC_H"
endif

! MI_OPBC_F
index = kim_api_get_index(pkim, "MI_OPBC_F", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "MI_OPBC_F"
endif

! CLUSTER
index = kim_api_get_index(pkim, "CLUSTER", ier)
if (index.ge.0) then
   num_NBCs = num_NBCs + 1
   if (num_NBCs>max_NBCs) then
      ier = KIM_STATUS_FAIL
      return
   endif
   model_NBCs(num_NBCs) = "CLUSTER"
endif

! free temporary storage
call kim_api_free(pkim,ier)

return

end subroutine Get_Model_NBC_methods

!-------------------------------------------------------------------------------
!
!  Write KIM descriptor file for MiniMol for given NBC and set of
!  supported species
!
!-------------------------------------------------------------------------------
subroutine Write_KIM_descriptor(NBC_method, max_species, model_species, num_species, &
                                kim_descriptor, ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!-- Transferred variables
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: NBC_method
integer(c_int),                       intent(in)   :: max_species
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: model_species(max_species)
integer(c_int),                       intent(in)   :: num_species
character(len=10000),                 intent(out)  :: kim_descriptor
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) :: i
character(len=103)  :: divider
character(len=1)    :: cr
character(len=1024) :: species_line
character(len=32)   :: nbcline

! Initialize error flag
ier = KIM_STATUS_OK

! Define frequently used variables
!
cr = char(10)
divider = '#######################################################################################################'

! Write Minimol descriptor file into string kim_descriptor
!
kim_descriptor = &
   divider                                                                      // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER START'                                                        // cr // &
   '#'                                                                          // cr // &
   '# The contents of this file are subject to the terms of the Common Development' // cr // &
   '# and Distribution License Version 1.0 (the "License").'                    // cr // &
   '#'                                                                          // cr // &
   '# You can obtain a copy of the license at'                                  // cr // &
   '# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the'    // cr // &
   '# specific language governing permissions and limitations under the License.' // cr // &
   '#'                                                                          // cr // &
   '# When distributing Covered Code, include this CDDL HEADER in each file and'// cr // &
   '# include the License file in a prominent location with the name LICENSE.CDDL.' // cr // &
   '# If applicable, add the following below this CDDL HEADER, with the fields' // cr // &
   '# enclosed by brackets "[]" replaced with your own identifying information:'// cr // &
   '#'                                                                          // cr // &
   '# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.' // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER END'                                                          // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# Copyright (c) 2013--2017, Regents of the University of Minnesota.'        // cr // &
   '# All rights reserved.'                                                     // cr // &
   '#'                                                                          // cr // &
   '# Contributors:'                                                            // cr // &
   '#    Automatically generated by calling Test'                               // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# See src/standard.kim for documentation about this file'                   // cr // &
   '#'                                                                          // cr // &
   divider                                                                      // cr // &
                                                                                   cr // &
   'KIM_API_Version := 1.6.0'                                                   // cr // &
                                                                                   cr // &
   'Unit_length      := A'                                                      // cr // &
   'Unit_energy      := eV'                                                     // cr // &
   'Unit_charge      := e'                                                      // cr // &
   'Unit_temperature := K'                                                      // cr // &
   'Unit_time        := ps'                                                     // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'PARTICLE_SPECIES:'                                                          // cr // &
   '# Symbol/name               Type                    code'                   // cr

   do i = 1,num_species
      write(species_line,'(a,'' '',''spec'',20x,i4)') trim(model_species(i)),0
      kim_descriptor = trim(kim_descriptor) // trim(species_line) // cr
   enddo

   nbcline = NBC_method(1:28) // 'flag'

   kim_descriptor = trim(kim_descriptor) // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'CONVENTIONS:'                                                               // cr // &
   '# Name                      Type'                                           // cr // &
                                                                                   cr // &
   'OneBasedLists               flag'                                           // cr // &
                                                                                   cr // &
   'Neigh_BothAccess            flag'                                           // cr // &
                                                                                   cr // &
   nbcline                                                                      // cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_INPUT:'                                                               // cr // &
   '# Name                      Type         Unit       Shape              requirements' // cr // &
   'numberOfParticles           integer      none       []'                     // cr // &
                                                                                   cr // &
   'numberOfSpecies             integer      none       []'                     // cr // &
                                                                                   cr // &
   'particleSpecies             integer      none       [numberOfParticles]'    // cr // &
                                                                                   cr // &
   'coordinates                 double       length     [numberOfParticles,3]'  // cr // &
                                                                                   cr // &
   'get_neigh                   method       none       []'                     // cr // &
                                                                                   cr // &
   'neighObject                 pointer      none       []'                     // cr // &
                                                                                   cr // &
   'numberContributingParticles integer      none       []'                     // cr // &
                                                                                   cr // &
   'boxSideLengths              double       length     [3]'                    // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_OUTPUT:'                                                              // cr // &
   '# Name                      Type         Unit       Shape              requirements' // cr // &
                                                                                   cr // &
   'destroy                     method       none       []'                     // cr // &
                                                                                   cr // &
   'compute                     method       none       []'                     // cr // &
                                                                                   cr // &
   'cutoff                      double       length     []'                     // cr // &
                                                                                   cr // &
   'energy                      double       energy     []'                     // cr // &
                                                                                   cr // &
   'forces                      double       force      [numberOfParticles,3]'  // cr // &
                                                                                   cr // &
   divider                                                                            // cr

return

end subroutine Write_KIM_descriptor

subroutine update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave, &
                               neighObject,ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
use mod_neighborlist
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!-- Transferred variables
integer(c_int),         intent(in)    :: DIM
integer(c_int),         intent(in)    :: N
real(c_double),         intent(in)    :: coords(DIM,N)
real(c_double),         intent(in)    :: cutoff
real(c_double),         intent(in)    :: cutpad
real(c_double),         intent(in)    :: boxSideLengths(DIM)
character(len=64),      intent(in)    :: NBC_Method
logical,                intent(inout) :: do_update_list
real(c_double),         intent(inout) :: coordsave(DIM,N)
type(neighObject_type), intent(inout) :: neighObject
integer(c_int),         intent(out)   :: ier

!-- Local variables
! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
! 4- MI_OPBC_H, 5- MI_OPBC_F 
integer(c_int) nbc
real(c_double) disp, disp1, disp2, cutrange, dispvec(DIM)
integer(c_int) i, idum

! Initialize error code
!
ier = KIM_STATUS_OK

! Determine which NBC scenario to use
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
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "Unknown NBC method", ier)
   stop
endif

! Update neighbor lists if necessary, if not just update Rij vectors if these
! are used
!
if (.not.do_update_list) then   ! if update not requested

   ! check whether a neighbor list update is necessary even if it hasn't been
   ! requested using the "two max sum" criterion
   disp1 = 0.0_cd
   disp2 = 0.0_cd
   do i=1,N
      dispvec(1:DIM) = coords(1:DIM,i) - coordsave(1:DIM,i)
      disp = sqrt( dot_product(dispvec,dispvec) )
      if (disp >= disp1) then        !  1st position taken
         disp2 = disp1               !  push current 1st into 2nd place
         disp1 = disp                !  and put this one into current 1st
      else if (disp >= disp2) then   !  2nd position taken
         disp2 = disp
      endif
   enddo
   do_update_list = ( disp1 + disp2 > cutpad )

endif

if (do_update_list) then

   ! save current coordinates
   coordsave(1:DIM,1:N) = coords(1:DIM,1:N)

   ! compute neighbor lists
   cutrange = cutoff + cutpad
   if (nbc.eq.0) then
      call NEIGH_RVEC_cluster_neighborlist(.true., N, coords, cutrange, &
                                           neighObject)
   elseif (nbc.eq.1) then
      call NEIGH_PURE_cluster_neighborlist(.true., N, coords, cutrange, &
                                           neighObject)
   elseif (nbc.eq.2) then
      call NEIGH_RVEC_cluster_neighborlist(.false., N, coords, cutrange, &
                                           neighObject)
   elseif (nbc.eq.3) then
      call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutrange, &
                                           neighObject)
   elseif (nbc.eq.4) then
      call MI_OPBC_cluster_neighborlist(.true., N, coords, cutrange, &
                                        boxSideLengths, neighObject)
   elseif (nbc.eq.5) then
      call MI_OPBC_cluster_neighborlist(.false., N, coords, cutrange, &
                                        boxSideLengths, neighObject)
   endif

   ! neighbor list uptodate, no need to compute again for now
   do_update_list = .false.

else

   ! Even though neighbor lists are uptodate Rij vectors still need to be
   ! computed
   if (nbc.eq.0.or.nbc.eq.2) &
      call NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, neighObject)

endif

return

end subroutine update_neighborlist



!-------------------------------------------------------------------------------
!
! MI_OPBC_cluster_neighborlist : construct a half or full neighbor list using
!                                the particle coordinates in coords()
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_cluster_neighborlist(half, numberOfParticles, coords, rcut, &
                                        boxSideLengths, neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  logical,        intent(in)                  :: half
  integer(c_int), intent(in)                  :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)                  :: coords
  real(c_double), intent(in)                  :: rcut
  real(c_double), dimension(3), intent(in)    :: boxSideLengths
  type(neighObject_type),       intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) rcut2

  rcut2 = rcut**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        where (abs(dx) > 0.5_cd*boxSideLengths)  ! apply PBC
           dx = dx - sign(boxSideLengths,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.rcut2) then
           if (i.ne.j) then
              if ( (j .gt. i) .or. ((.not. half) .AND. (i.ne.j)) ) then
                  ! part j is a neighbor of part i
                  a = a+1
                  neighObject%neighborList(a,i) = j
              endif
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine MI_OPBC_cluster_neighborlist

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_cluster_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_cluster_neighborlist(half, numberOfParticles, coords, &
                                           cutoff, neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,        intent(in)            :: half
  integer(c_int), intent(in)            :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)            :: coords
  real(c_double), intent(in)            :: cutoff
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! part j is a neighbor of part i
           if ( (j .gt. i) .OR. ((.not. half) .AND. (i.ne.j)) ) then
               a = a+1
               neighObject%neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_cluster_neighborlist

!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_cluster_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_cluster_neighborlist(half, numberOfParticles, coords, &
                                           cutoff, neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,        intent(in)            :: half
  integer(c_int), intent(in)            :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)            :: coords
  real(c_double), intent(in)            :: cutoff
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           if ((half .and. i.lt.j) .or. (.not.half .and. i.ne.j)) then
              ! part j is a neighbor of part i
              a = a+1
              neighObject%neighborList(a,i) = j
              neighObject%RijList(:,a-1,i) = dx
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_RVEC_cluster_neighborlist

!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_update_Rij_vectors
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, neighObject)
  use, intrinsic :: iso_c_binding
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  integer(c_int), intent(in)  :: DIM
  integer(c_int), intent(in)  :: N
  real(c_double), intent(in)  :: coords(DIM,N)
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, jj, NN
  real(c_double) dx(DIM)

  do i=1,N
     NN = neighObject%neighborList(1,i)
     do jj=1,NN
        j = neighObject%neighborList(jj+1,i)
        dx(:) = coords(:, j) - coords(:, i)
        neighObject%RijList(:,jj,i) = dx(:)
     enddo
  enddo

  return

end subroutine NEIGH_RVEC_update_Rij_vectors

!-------------------------------------------------------------------------------
!
! create_FCC_configuration subroutine
!
!  creates a cubic configuration of FCC particles with lattice spacing
!  `FCCspacing' and `nCellsPerSide' cells along each direction.
!
!  With periodic==.true. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
!
!  With periodic==.false. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3
!
!  Returns the Id of the particle situated in the middle of the configuration
!  (this particle will have the most neighbors.)
!
!-------------------------------------------------------------------------------
subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, periodic, &
                                    coords, MiddlePartId)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  real(c_double), intent(in)  :: FCCspacing
  integer(c_int), intent(in)  :: nCellsPerSide
  logical,        intent(in)  :: periodic
  real(c_double), intent(out) :: coords(3,*)
  integer(c_int), intent(out) :: MiddlePartId
  !
  ! cluster setup variables
  !
  real(c_double) FCCshifts(3,4)
  real(c_double) latVec(3)
  integer(c_int) a, i, j, k, m

  ! Create a cubic FCC cluster
  !
  FCCshifts(1,1) = 0.0_cd
  FCCshifts(2,1) = 0.0_cd
  FCCshifts(3,1) = 0.0_cd
  FCCshifts(1,2) = 0.5_cd*FCCspacing
  FCCshifts(2,2) = 0.5_cd*FCCspacing
  FCCshifts(3,2) = 0.0_cd
  FCCshifts(1,3) = 0.5_cd*FCCspacing
  FCCshifts(2,3) = 0.0_cd
  FCCshifts(3,3) = 0.5_cd*FCCspacing
  FCCshifts(1,4) = 0.0_cd
  FCCshifts(2,4) = 0.5_cd*FCCspacing
 FCCshifts(3,4) = 0.5_cd*FCCspacing

  MiddlePartID = 1 ! Always put middle particle as #1
  a = 1            ! leave space for middle particle as particle #1
  do i=1,nCellsPerSide
     latVec(1) = (i-1)*FCCspacing
     do j=1,nCellsPerSide
        latVec(2) = (j-1)*FCCspacing
        do k=1,nCellsPerSide
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
              if ((i.eq.nCellsPerside/2+1).and.(j.eq.nCellsPerSide/2+1) .and. &
                   (k.eq.nCellsPerSide/2+1) .and. (m.eq.1)) then
                 coords(:,1) = latVec + FCCshifts(:,m) ! put middle particle as #1
                 a = a - 1
              endif
           enddo
        enddo
        if (.not. periodic) then
            ! Add in the remaining three faces
            ! pos-x face
            latVec(1) = nCellsPerSide*FCCspacing
            latVec(2) = (i-1)*FCCspacing
            latVec(3) = (j-1)*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,4)
            ! pos-y face
            latVec(1) = (i-1)*FCCspacing
            latVec(2) = nCellsPerSide*FCCspacing
            latVec(3) = (j-1)*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,3)
            ! pos-z face
            latVec(1) = (i-1)*FCCspacing
            latVec(2) = (j-1)*FCCspacing
            latVec(3) = nCellsPerSide*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,2)
         endif
     enddo
     if (.not. periodic) then
         ! Add in the remaining three edges
         latVec(1) = (i-1)*FCCspacing
         latVec(2) = nCellsPerSide*FCCspacing
         latVec(3) = nCellsPerSide*FCCspacing
         a = a+1; coords(:,a) = latVec
         latVec(1) = nCellsPerSide*FCCspacing
         latVec(2) = (i-1)*FCCspacing
         latVec(3) = nCellsPerSide*FCCspacing
         a = a+1; coords(:,a) = latVec
         latVec(1) = nCellsPerSide*FCCspacing
         latVec(2) = nCellsPerSide*FCCspacing
         latVec(3) = (i-1)*FCCspacing
         a = a+1; coords(:,a) = latVec
      endif
  enddo
  if (.not. periodic) then
      ! Add in the remaining corner
      a = a+1; coords(:,a) = nCellsPerSide*FCCspacing
  endif

  return

end subroutine create_FCC_configuration

subroutine compute_numer_deriv(partnum,dir,pkim,DIM,N,coords,cutoff,cutpad,   &
                               boxSideLengths,NBC_Method,do_update_list,      &
                               coordsave,neighObject,deriv,deriv_err,ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
use mod_neighborlist
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!--Transferred variables
integer(c_int),         intent(in)    :: partnum
integer(c_int),         intent(in)    :: dir
type(c_ptr),            intent(in)    :: pkim
integer(c_int),         intent(in)    :: DIM
integer(c_int),         intent(in)    :: N
real(c_double),         intent(inout) :: coords(DIM,N)
real(c_double),         intent(in)    :: cutoff
real(c_double),         intent(in)    :: cutpad
real(c_double),         intent(in)    :: boxSideLengths(DIM)
character(len=KIM_KEY_STRING_LENGTH), intent(in) :: NBC_Method
logical,                intent(inout) :: do_update_list
real(c_double),         intent(inout) :: coordsave(DIM,N)
type(neighObject_type), intent(inout) :: neighObject
real(c_double),         intent(out)   :: deriv
real(c_double),         intent(out)   :: deriv_err
integer(c_int),         intent(out)   :: ier

!-- Local variables
real(c_double), parameter :: eps_init = 1.e-6_cd
integer(c_int), parameter :: number_eps_levels = 15
real(c_double)  eps, deriv_last, deriv_err_last
integer(c_int)  i,idum
logical doing_neighbors

!-- KIM variables
real(c_double), pointer :: energy; type(c_ptr) :: penergy

! Initialize error flag
ier = KIM_STATUS_OK

deriv_last = 0.0_cd ! initialize

! Unpack data from KIM object
!
penergy = kim_api_get_data(pkim, "energy", ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                              "kim_api_get_data", ier)
   stop
endif
call c_f_pointer(penergy, energy)

! Figure out if neighbor list is being generated
!
doing_neighbors = .not.(index(NBC_Method,"CLUSTER").eq.1)

! Outer loop of Ridders' method for computing numerical derivative
!
eps = eps_init
deriv_err_last = huge(1.0_cd)
do i=1,number_eps_levels
   deriv = dfridr(eps,deriv_err)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "compute_numer_deriv",ier)
      stop
   endif
   if (deriv_err>deriv_err_last) then
      deriv  = deriv_last
      deriv_err = deriv_err_last
      exit
   endif
   eps = eps*10.0_cd
   deriv_last  = deriv
   deriv_err_last = deriv_err
enddo

return

contains

   !----------------------------------------------------------------------------
   !
   ! Compute numerical derivative using Ridders' method
   !
   ! Based on code from Numerical Recipes, Press et al., Second Ed., Cambridge,
   ! 1992
   !
   ! Ref: Ridders, C. J. F., "Two algorithms for the calculation of F'(x)=D",
   !      Advances in Engineering Software, Vol. 4, no. 2, pp. 75-76, 1982.
   !
   !
   ! Returns the gradient grad() of a KIM-compliant interatomic model at the 
   ! current configuration by Ridders' method of polynomial extrapolation.
   ! An estimate for the error in each component of the gradient is returned in
   ! grad_err().
   !
   !----------------------------------------------------------------------------
   real(c_double) function dfridr(h,err)
   implicit none

   !-- Transferred variables
   real(c_double), intent(inout) :: h
   real(c_double), intent(out)   :: err

   !-- Local variables
   integer(c_int), parameter :: NTAB=10     ! Maximum size of tableau
   real(c_double), parameter :: CON=1.4_cd  ! Stepsize incr. by CON at each iter
   real(c_double), parameter :: CON2=CON*CON
   real(c_double), parameter :: BIG=huge(1.0_cd)
   real(c_double), parameter :: SAFE=2.0_cd ! Returns when error is SAFE worse
                                            ! than the best so far
   integer(c_int) i,j
   integer(c_int) idum
   real(c_double) errt,fac,hh,a(NTAB,NTAB),fp,fm,coordorig

   dfridr = 0.0_cd ! initialize

   if (h.eq.0.0_cd) then
      ier = KIM_STATUS_FAIL
      return
   endif

   hh = h
   coordorig = coords(dir,partnum)
   coords(dir,partnum) = coordorig + hh
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighObject,ier)
   ier = kim_api_model_compute(pkim)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ier)
      stop
   endif
   fp = energy
   coords(dir,partnum) = coordorig - hh
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighObject,ier)
   ier = kim_api_model_compute(pkim)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ier)
      stop
   endif
   fm = energy
   coords(dir,partnum) = coordorig
   if (doing_neighbors) &
      call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave,       &
                               neighObject,ier)
   a(1,1)=(fp-fm)/(2.0_cd*hh)
   err=BIG
   ! successive columns in the Neville tableau will go to smaller step sizes
   ! and higher orders of extrapolation
   do i=2,NTAB
      ! try new, smaller step size
      hh=hh/CON
      coords(dir,partnum) = coordorig + hh
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighObject,ier)
      ier = kim_api_model_compute(pkim)
      if (ier.lt.KIM_STATUS_OK) then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ier)
         stop
      endif
      fp = energy
      coords(dir,partnum) = coordorig - hh
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighObject,ier)
      ier = kim_api_model_compute(pkim)
      if (ier.lt.KIM_STATUS_OK) then
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ier)
         stop
      endif
      fm = energy
      coords(dir,partnum) = coordorig
      if (doing_neighbors) &
         call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                  NBC_Method,do_update_list,coordsave,       &
                                  neighObject,ier)
      a(1,i)=(fp-fm)/(2.0_cd*hh)
      fac=CON2
      ! compute extrapolations of various orders, requiring no new function
      ! evaluations
      do j=2,i
         a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.0_cd)
         fac=CON2*fac
         ! The error strategy is to compute each new extrapolation to one order
         ! lower, both at the present step size and the previous one.
         errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
         if (errt.le.err) then ! if error is decreased, save the improved answer
            err = errt
            dfridr=a(j,i)
         endif
      enddo
      if (abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err) return ! if higher order is worse
                                                     ! by significant factor
                                                     ! `SAFE', then quit early.
   enddo
   return
   end function dfridr

end subroutine compute_numer_deriv
