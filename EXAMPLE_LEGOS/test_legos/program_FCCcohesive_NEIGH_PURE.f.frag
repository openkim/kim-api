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
! Copyright (c) 2013, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
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
!**        NEIGH_PURE_H
!**        NEIGH_PURE_F
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
  use KIM_API
  implicit none

!============================== VARIABLE DEFINITIONS ==========================

  ! parameters controlling behavior of test
  !
  character(len=80), parameter :: testname    = "TEST_NAME_STR"
  character(len=2),  parameter :: specname    = 'SPECIES_NAME_STR'
  double precision,  parameter :: TOL         = 1.0d-8
  double precision,  parameter :: FCCspacing  = FCC_SPACING_STR
  double precision,  parameter :: MinSpacing  = 0.800d0*FCCspacing
  double precision,  parameter :: MaxSpacing  = 1.200d0*FCCspacing
  integer,           parameter :: DIM         = 3
  integer,           parameter :: SupportHalf = 1  ! True

  ! significant local variables
  !
  double precision     :: rcut               ! cutoff radius of the potential

  integer, allocatable :: neighborList(:,:)  ! neighbor list storage

  double precision     :: FinalSpacing       ! crystal lattice parameter

  double precision     :: FinalEnergy        ! energy per atom of crystal
                                             ! at current spacing

  integer              :: CellsPerRcut       ! number of unit cells along
                                             ! box (of size rcut) side


  ! KIM variables
  !
  character(len=80)         :: modelname     ! KIM-compliant model name

  integer(kind=kim_intptr)  :: pkim          ! pointer to KIM API object

  double precision coordum(DIM,1);   pointer(pcoor,coordum)         ! coordinate

  integer                   :: N                          ! number of atoms


  ! other variables
  !
  double precision, external  ::  get_model_cutoff_firsttime

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname


  ! Get model cutoff radius and compute number of atoms needed
  ! (We need 2*cutoff, use 2.125*cutoff for saftey)
  !
  rcut = get_model_cutoff_firsttime(testname, modelname)
  CellsPerRcut = ceiling(rcut/MinSpacing)
  N = 4*((2*CellsPerRcut)**3)


  ! Setup the KIM API object
  !
  call setup_KIM_API_object(pkim, testname, modelname, N, specname, SupportHalf)


  ! allocate storage for neighbor lists, compute them for the first time,
  ! and store necessary pointers in KIM API object
  !
  allocate(neighborList(N+1, N))
  call setup_neighborlist_no_Rij_KIM_access(pkim, N, neighborList)


  ! find equilibrium spacing by minimizing coheseive energy with respect
  ! to the periodic box size
  !
  call NEIGH_PURE_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
         TOL,N,neighborlist,.false.,             &
         FinalSpacing,FinalEnergy)

  ! print results to screen
  !
  print '(80(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname
  print *
  print '("Found minimum energy configuration to within",ES25.15)', TOL
  print *
  print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', FinalEnergy, FinalSpacing
  print '(80(''-''))'


  ! Don't forget to free and/or deallocate
  !
  deallocate(neighborList)
  call free_KIM_API_object(pkim)

  stop

end program TEST_NAME_STR

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_compute_equilibrium_spacing :
!
!    Use the Golden section search algorithm to find the equilibrium spacing by
!    minimizing the energy of the system with respect to the periodic box size.
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_compute_equilibrium_spacing(pkim, &
             DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
             TOL,N,neighborlist,verbose,             &
             RetSpacing,RetEnergy)
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)     :: pkim
  integer,                  intent(in)     :: DIM
  integer,                  intent(in)     :: CellsPerRcut
  double precision,         intent(in)     :: MinSpacing
  double precision,         intent(in)     :: MaxSpacing
  double precision,         intent(in)     :: TOL
  integer,                  intent(in)     :: N
  integer,                  intent(inout)  :: neighborList(N+1,N)
  logical,                  intent(in)     :: verbose
  double precision,         intent(out)    :: RetSpacing
  double precision,         intent(out)    :: RetEnergy

  !-- Local variables
  double precision,         parameter :: Golden      = (1.d0 + sqrt(5.d0))/2.d0
  integer ier, idum
  double precision Spacings(4)
  double precision Energies(4)
  integer MiddleAtomId
  double precision energy;         pointer(penergy,energy)
  double precision coordum(DIM,1); pointer(pcoor,coordum)
  double precision, pointer :: coords(:,:)
  double precision cutoff;         pointer(pcutoff,cutoff)
  double precision, parameter :: cutpad = CUTOFF_PADDING_STR ! cutoff radius padding
  logical :: halfflag  ! .true. = half neighbor list; .false. = full neighbor list
  character(len=64) NBC_Method;  pointer(pNBC_Method,NBC_Method)

  ! Unpack data from KIM object
  !
  call kim_api_getm_data_f(pkim, ier, &
       "energy",      penergy,  1, &
       "coordinates", pcoor,    1, &
       "cutoff",      pcutoff,  1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_getm_data_f", ier)
     stop
  endif

  call KIM_to_F90_real_array_2d(coordum, coords, DIM, N)

  ! determine which neighbor list type to use
  !
  pNBC_Method = kim_api_get_nbc_method_f(pkim, ier) ! don't forget to free
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
     halfflag = .true.
  elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
     halfflag = .false.
  else
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "Unknown NBC method", ier)
     return
  endif
  call free(pNBC_Method) ! free the memory

  ! Initialize for minimization
  !
  Spacings(1) = MinSpacing
  call create_FCC_configuration(Spacings(1), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), &
                                        MiddleAtomId, neighborList)
  ier = kim_api_model_compute_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(1) = energy
  if (verbose) &
     print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(1), Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call create_FCC_configuration(Spacings(3), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), &
                                        MiddleAtomId, neighborList)
  ! Call model compute
  ier = kim_api_model_compute_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(3) = energy
  if (verbose) &
     print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(3), Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  call create_FCC_configuration(Spacings(2), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), &
                                        MiddleAtomId, neighborList)
  ! Call model compute
  ier = kim_api_model_compute_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(2) = energy
  if (verbose) &
     print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(2), Spacings(2)


  ! iterate until convergence.
  !
  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new atom coordinates based on new spacing
     call create_FCC_configuration(Spacings(4), 2*CellsPerRcut, .true., coords, MiddleAtomId)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), &
                                           MiddleAtomId, neighborList)
     ! Call model compute
     ier = kim_api_model_compute_f(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                      "kim_api_model_compute_f", ier)
        stop
     endif
     Energies(4) = energy
     if (verbose) &
        print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(4), Spacings(4)

     ! determine the new interval
     if (Energies(4) .lt. Energies(2)) then
        ! We want the right-hand interval
        Spacings(1) = Spacings(2); Energies(1) = Energies(2)
        Spacings(2) = Spacings(4); Energies(2) = Energies(4)
     else
        ! We want the left-hand interval
        Spacings(3) = Spacings(1); Energies(3) = Energies(1)
        Spacings(1) = Spacings(4); Energies(1) = Energies(4)
     endif
  enddo

  ! pull out results and return
  !
  RetSpacing = Spacings(2)
  RetEnergy  = Energies(2)

  return

end subroutine NEIGH_PURE_compute_equilibrium_spacing
