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
!    Stephen M. Whalen
!


!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal (spec="SPECIES_NAME_STR") as a
!**  function of lattice spacing.
!**
!**  Works with the following NBC methods:
!**        NEIGH_RVEC_H
!**        NEIGH_RVEC_F
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!*******************************************************************************

#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__

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
  integer(c_int), parameter :: cd = c_double ! used for literal constants

!============================== VARIABLE DEFINITIONS ==========================

  ! parameters controlling behavior of test
  !
  character(len=KIM_KEY_STRING_LENGTH), parameter :: testname = "TEST_NAME_STR"
  character(len=2), parameter :: specname    = 'SPECIES_NAME_STR'
  real(c_double),   parameter :: TOL         = 1.0e-8_cd
  real(c_double),   parameter :: FCCspacing  = FCC_SPACING_STR
  real(c_double),   parameter :: MinSpacing  = 0.800_cd*FCCspacing
  real(c_double),   parameter :: MaxSpacing  = 1.200_cd*FCCspacing
  integer(c_int),   parameter :: DIM         = 3
  integer(c_int),   parameter :: SupportHalf = 1            ! True

  ! significant local variables
  !
  real(c_double) :: FinalSpacing       ! crystal lattice parameter
  real(c_double) :: FinalEnergy        ! energy per atom of crystal
                                       ! at current spacing
  integer(c_int) :: CellsPerCutoff     ! number of unit cells along
                                       ! box (of size cutoff) side

  ! neighbor list
  !
  type(neighObject_type), target :: NLRvecLocs
  integer(c_int), allocatable, target :: neighborList(:,:)
  real(c_double), allocatable, target :: RijList(:,:,:)
  integer(c_int)  :: NNeighbors  ! maximum number of neighbors for an atom

  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: modelname ! KIM-compliant model name
  type(c_ptr)             :: pkim     ! pointer to KIM API object
  integer(c_int)          :: ier      ! error flag
  integer(c_int)          :: N        ! number of atoms
  real(c_double), pointer :: cutoff;  type(c_ptr) :: pcutoff  ! cutoff radius


  ! other variables
  !
  real(c_double), external ::  get_model_cutoff_firsttime
  integer(c_int)           ::  idum

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname


  ! We'll use just one atom for this calculation!
  !
  N = 1


  ! Setup the KIM API object
  !
  call setup_KIM_API_object(pkim, testname, modelname, N, specname, SupportHalf)


  ! allocate storage for neighbor lists, compute them for the first time,
  ! and store necessary pointers in KIM API object
  !
  ! First, access the `cutoff' arguemt
  !
  pcutoff = kim_api_get_data(pkim, "cutoff", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pcutoff, cutoff)
  !
  ! Second, determine how many neighbors we will need
  !
  ! the 0.05 is a saftey factor
  CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05_cd)
  NNeighbors = 4*((2*CellsPerCutoff + 1)**3)
  !
  ! allocate memory for the neighbor list and Rij vectors
  !
  allocate(neighborList(NNeighbors+1,N))
  allocate(RijList(3,NNeighbors+1,N))
  NLRvecLocs%pneighborList = c_loc(neighborList)
  NLRvecLocs%pRijList = c_loc(RijList)
  NLRvecLocs%NNeighbors = NNeighbors
  call setup_neighborlist_Rij_KIM_access(pkim, NLRvecLocs)


  ! find equilibrium spacing by minimizing cohesive energy with respect
  ! to the periodic box size
  !
  call NEIGH_RVEC_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerCutoff,MinSpacing,MaxSpacing,  &
         TOL,N,NNeighbors,neighborlist,RijList,     &
         .false.,FinalSpacing,FinalEnergy)

  ! print results to screen
  !
  print '(80(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)
  print *
  print '("Found minimum energy configuration to within",ES25.15)', TOL
  print *
  print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', FinalEnergy, &
        FinalSpacing
  print '(80(''-''))'


  ! Don't forget to free and/or deallocate
  !
  deallocate(neighborList)
  deallocate(RijList)
  call free_KIM_API_object(pkim)

  stop

end program TEST_NAME_STR
