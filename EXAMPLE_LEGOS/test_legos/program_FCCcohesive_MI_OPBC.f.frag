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
!**        MI_OPBC_H
!**        MI_OPBC_F
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
  implicit none

!============================== INTERFACE TO EXTERNAL ROUTINES ================

  interface
     subroutine setup_neighborlist_no_Rij_KIM_access(pkim, N, neighborList)
        use, intrinsic :: iso_c_binding
        type(c_ptr),            intent(in) :: pkim
        integer(c_int),         intent(in) :: N
        integer(c_int), target, intent(in) :: neighborList(N+1,N)
     end subroutine setup_neighborlist_no_Rij_KIM_access
  end interface

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
  integer(c_int),   parameter :: SupportHalf = 1 ! True

  ! significant local variables
  !
  real(c_double)                      :: rcut               ! cutoff radius of the potential
  integer(c_int), allocatable, target :: neighborList(:,:)  ! neighbor list storage
  real(c_double)                      :: FinalSpacing       ! crystal lattice parameter
  real(c_double)                      :: FinalEnergy        ! energy per atom of crystal
                                                            ! at current spacing
  integer(c_int)                      :: CellsPerSide       ! number of unit cells along
                                                            ! box side

  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: modelname  ! KIM-compliant model name
  type(c_ptr)             :: pkim          ! pointer to KIM API object
  integer(c_int)          :: N             ! number of atoms

  ! other variables
  !
  real(c_double), external  ::  get_model_cutoff_firsttime

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname


  ! Get model cutoff radius and compute number of atoms needed
  ! (We need 2*cutoff, use 2.125*cutoff for saftey)
  !
  rcut = get_model_cutoff_firsttime(testname, modelname)
  CellsPerSide = ceiling((2.125_cd*rcut)/(MinSpacing))
  N = 4*(CellsPerSide**3)


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
  call MI_OPBC_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerSide,MinSpacing,MaxSpacing, &
         TOL,N,neighborlist,.false.,             &
         FinalSpacing,FinalEnergy)

  ! print results to screen
  !
  print '(80(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)
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
! MI_OPBC_compute_equilibrium_spacing :
!
!    Use the Golden section search algorithm to find the equilibrium spacing by
!    minimizing the energy of the system with respect to the periodic box size.
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_compute_equilibrium_spacing(pkim, &
             DIM,CellsPerSide,MinSpacing,MaxSpacing, &
             TOL,N,neighborlist,verbose,             &
             RetSpacing,RetEnergy)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr),    intent(in)     :: pkim
  integer(c_int), intent(in)     :: DIM
  integer(c_int), intent(in)     :: CellsPerSide
  real(c_double), intent(in)     :: MinSpacing
  real(c_double), intent(in)     :: MaxSpacing
  real(c_double), intent(in)     :: TOL
  integer(c_int), intent(in)     :: N
  integer(c_int), intent(inout)  :: neighborList(N+1,N)
  logical,        intent(in)     :: verbose
  real(c_double), intent(out)    :: RetSpacing
  real(c_double), intent(out)    :: RetEnergy

  !-- Local variables
  real(c_double), parameter :: Golden = (1.0_cd + sqrt(5.0_cd))/2.0_cd
  integer(c_int)            :: ier, idum
  real(c_double) Spacings(4)
  real(c_double) Energies(4)
  integer(c_int) MiddleAtomId
  real(c_double), pointer :: energy;            type(c_ptr) :: penergy
  real(c_double), pointer :: coords(:,:);       type(c_ptr) :: pcoor
  real(c_double), pointer :: cutoff;            type(c_ptr) :: pcutoff
  real(c_double), pointer :: boxSideLengths(:); type(c_ptr) :: pboxSideLengths
  real(c_double), parameter :: cutpad = CUTOFF_PADDING_STR ! cutoff radius padding
  logical :: halfflag  ! .true. = half neighbor list; .false. = full neighbor list
  character(len=KIM_KEY_STRING_LENGTH), pointer :: NBC_Method; type(c_ptr) :: pNBC_Method

  ! Unpack data from KIM object
  !
  call kim_api_getm_data(pkim, ier, &
       "energy",         penergy,         1, &
       "coordinates",    pcoor,           1, &
       "cutoff",         pcutoff,         1, &
       "boxSideLengths", pboxSideLengths, 1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(penergy, energy)
  call c_f_pointer(pcoor,   coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)
  call c_f_pointer(pboxSideLengths, boxSideLengths, [DIM])

  ! determine which neighbor list type to use
  !
  pNBC_Method = kim_api_get_nbc_method(pkim, ier) ! don't forget to free
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method", ier)
     stop
  endif
  call c_f_pointer(pNBC_Method, NBC_Method)
  if (index(NBC_Method,"MI_OPBC_H").eq.1) then
     halfflag = .true.
  elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
     halfflag = .false.
  else
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", ier)
     return
  endif
  call KIM_API_c_free(pNBC_Method); NBC_Method => null()  ! free the memory

  ! Initialize for minimization
  !
  Spacings(1) = MinSpacing
  call create_FCC_configuration(Spacings(1), CellsPerSide, .true., coords, MiddleAtomId)
  boxSideLengths(:) = Spacings(1)*CellsPerSide
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call MI_OPBC_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), boxSideLengths, MiddleAtomId, neighborList)
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif
  Energies(1) = energy
  if (verbose) &
     print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(1), Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call create_FCC_configuration(Spacings(3), CellsPerSide, .true., coords, MiddleAtomId)
  boxSideLengths(:) = Spacings(3)*CellsPerSide
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call MI_OPBC_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), boxSideLengths, MiddleAtomId, neighborList)
  ! Call model compute
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif
  Energies(3) = energy
  if (verbose) &
     print '("Energy/atom = ",ES25.15,"; Spacing = ",ES25.15)', Energies(3), Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0_cd - Golden)*(MaxSpacing - MinSpacing)
  call create_FCC_configuration(Spacings(2), CellsPerSide, .true., coords, MiddleAtomId)
  boxSideLengths(:) = Spacings(2)*CellsPerSide
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call MI_OPBC_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), boxSideLengths, MiddleAtomId, neighborList)
  ! Call model compute
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
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
     call create_FCC_configuration(Spacings(4), CellsPerSide, .true., coords, MiddleAtomId)
     ! set new boxSideLengths
     boxSideLengths(:)  = Spacings(4)*CellsPerSide
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call MI_OPBC_periodic_neighborlist(halfflag, N, coords, (cutoff+cutpad), boxSideLengths, MiddleAtomId, neighborList)
     ! Call model compute
     ier = kim_api_model_compute(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_compute", ier)
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

end subroutine MI_OPBC_compute_equilibrium_spacing
