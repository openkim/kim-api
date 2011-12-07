!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to perform numerical derivative check on a model
!**
!**  Works with the following NBC methods:
!**        MI-OPBC-H
!**        MI-OPBC-F
!**        NEIGH-PURE-H
!**        NEIGH-PURE-F
!**        NEIGH-RVEC-F
!**
!**  Author: Ellad B. Tadmor, Valeriu Smirichinski, Ryan S. Elliott
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

  integer, external  :: get_neigh_no_Rij
  integer, external  :: get_neigh_Rij
  real*8,  parameter :: FCCspacing     = FCC_SPACING_STR
  integer, parameter :: nCellsPerSide  = 2
  integer, parameter :: DIM            = 3
  integer, parameter :: ATypes         = 1
  real*8,  parameter :: cutpad         = 0.75d0
  integer, parameter :: max_specs      = 10     ! most species a Model can support
  real*8,  parameter :: eps_prec       = epsilon(1.d0)
  
  integer,          parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(kind=kim_intptr), parameter :: SizeOne = 1
  real*8, allocatable :: forces_num(:,:)
  real*8, allocatable :: forces_num_err(:,:)
  character(len=3)    :: model_specs(max_specs)
  integer             :: num_specs
  character(len=4)    :: passfail
  real*8              :: forcediff
  real*8              :: forcediff_sumsq
  real*8              :: weight
  real*8              :: weight_sum
  real*8              :: alpha
  real*8              :: term
  real*8              :: term_max
  integer I,J,Imax,Jmax

  ! neighbor list
  integer,                  allocatable :: neighborList(:,:)
  integer(kind=kim_intptr), allocatable :: NLRvecLocs(:)
  double precision,         allocatable :: RijList(:,:,:)
  double precision,         allocatable :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character*80              :: testname     = "TEST_NAME_STR"
  character*80              :: modelname
  character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer nbc  ! 0- MI-OPBC-H, 1- MI-OPBC-F, 2- NEIGH-PURE-H, 3- NEIGH-PURE-F, 4- NEIGH-RVCE-F
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier, idum
  integer numberOfAtoms;   pointer(pnAtoms,numberOfAtoms)
  integer numContrib;      pointer(pnumContrib,numContrib)
  integer numberAtomTypes; pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1); pointer(patomTypesdum,atomTypesdum)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1); pointer(pforces,forcesdum)
  real*8 boxlength(DIM);   pointer(pboxlength,boxlength)
  real*8, pointer  :: coords(:,:), forces(:,:)
  integer, pointer :: atomTypes(:)
  integer middleDum
  character(len=10000) :: test_descriptor_string
  real*8 rnd, deriv, deriv_err

  ! Initialize error flag
  ier = KIM_STATUS_OK
  
  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Write out kim descriptor string for Test
  ! (Currently gets species types supported by Model from modelname. This
  ! should be replaced with a call to a KIM Service Routine when available.)
  call Write_KIM_descriptor(modelname, test_descriptor_string, &
                            max_specs, model_specs, num_specs, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "Write_KIM_descriptor", ier)
     stop
  endif

  ! Create empty KIM object conforming to fields in the KIM descriptor files
  ! of the Test and Model
  !
  ier = kim_api_init_str_testname_f(pkim,trim(test_descriptor_string)//char(0),modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_init_str_testname_f", ier)
     stop
  endif

  ! determine which NBC scenerio to use
  pNBC_Method = kim_api_get_nbc_method_f(pkim, ier) ! don't forget to free
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"MI-OPBC-H").eq.1) then
     nbc = 0
  elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
     nbc = 1
  elseif (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
     nbc = 2
  elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
     nbc = 3
  elseif (index(NBC_Method,"NEIGH-RVEC-F").eq.1) then
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

  ! Allocate storage for neighbor lists and 
  ! store pointers to neighbor list object and access function
  allocate(neighborList(N+1,N))
  if (nbc.le.3) then
     ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(neighborList))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  else
     allocate(RijList(DIM,N+1,N), NLRvecLocs(3))
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
     ier = kim_api_set_data_f(pkim, "get_half_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.1) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.2) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.3) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_neigh_no_Rij))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.4) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_neigh_Rij))
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
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier);
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  pnumContrib = kim_api_get_data_f(pkim, "numberContributingAtoms", ier);
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  pnAtomTypes = kim_api_get_data_f(pkim, "numberAtomTypes", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  patomTypesdum = kim_api_get_data_f(pkim, "atomTypes", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N)

  pcoor = kim_api_get_data_f(pkim, "coordinates", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  if (nbc.le.1) then
     pboxlength = kim_api_get_data_f(pkim, "boxlength", ier)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
        stop
     endif
  endif

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif

  pforces = kim_api_get_data_f(pkim, "forces", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(forcesdum, forces, DIM, N)

  ! Set values
  !
  ! NOTE: The numerical derivative test is currently hard-wired to work with
  !       a single species. This needs to be rewritten to more comprehensivley
  !       test Models that support multiple species.
  !
  numberOfAtoms   = N
  numContrib      = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, trim(model_specs(1)), ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, middleDum)
  if (nbc.le.1) boxlength(:)  = 600.d0 ! large enough to make the cluster isolated

  ! randomly perturb all atoms
  do I=1,N
     do J=1,DIM
        call random_number(rnd)  ! return random number between 0 and 1
        coords(J,I) = coords(J,I) + 0.1d0*(rnd-0.5d0)
     enddo
  enddo

  ! Compute neighbor lists
  do_update_list = .true.
  allocate(coordsave(DIM,N))
  call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxlength,NBC_Method,  &
                           do_update_list,coordsave,neighborList,RijList,ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "update_neighborlist", ier)
     stop
  endif

  ! Call model compute to get forces (gradient)
  call kim_api_model_compute_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_compute", ier)
     stop
  endif

  ! Turn off force computation
  call kim_api_set2_donotcompute_f(pkim, "forces", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__,"kim_api_set2_donotcompute_f", ier)
     stop
  endif

  ! Compute gradient using numerical differentiation
  allocate(forces_num(DIM,N),forces_num_err(DIM,N))
  do I=1,N
     do J=1,DIM
        call compute_numer_deriv(I,J,pkim,DIM,N,coords,cutoff,cutpad,   &
                                 boxlength,NBC_Method,do_update_list,coordsave, &
                                 neighborList,RijList,deriv,deriv_err,ier)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error_f(__LINE__, __FILE__,"compute_numer_deriv", ier)
           stop
        endif
        forces_num(J,I) = -deriv
        forces_num_err(J,I) = deriv_err
     enddo
  enddo

  ! print results to screen
  print '(120(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname
  print '("Using NBC: ",A)', NBC_Method(1:(index(NBC_Method,char(0))-1))
  print *
  print '(A6,2X,A3,2X,2A25,3A15,2X,A4)',"Atom","Dir", "Force_model",   &
        "Force_numer",  "Force diff", "pred error", "weight",          &
        "stat"
  forcediff_sumsq = 0.d0
  weight_sum = 0.d0
  do I=1,N
     do J=1,DIM
        forcediff = abs(forces(J,I)-forces_num(J,I))
        if (forcediff<forces_num_err(J,I)) then
           passfail = "    "
        else
           passfail = "FAIL"
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
        print '(I6,2X,I3,2X,2ES25.15,3ES15.5,2X,A4)', &
               I,J,forces(J,I),forces_num(J,I), &
               forcediff,forces_num_err(J,I),weight,passfail
     enddo
  enddo
  alpha = sqrt(forcediff_sumsq/weight_sum)/dble(DIM*N)
  print *
  print '("alpha = |Force_model - Force_numer|_w/(DIM*N) = ",ES15.5," (units of force)")', &
        alpha
  print *
  print '(''Maximum term obtained for Atom = '',I6,'', Dir = '',I1,&
     '', forcediff = '',ES15.5, '', forcediff/force_model = '',ES15.5)', &
     Imax,Jmax,abs(forces(Jmax,Imax)-forces_num(Jmax,Imax)),           &
     abs(forces(Jmax,Imax)-forces_num(Jmax,Imax))/abs(forces(Jmax,Imax))
  print *
  print '(120(''-''))'

  ! Don't forget to free and/or deallocate
  deallocate(forces_num)
  deallocate(forces_num_err)
  call free(pNBC_Method) 
  deallocate(coordsave)
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
