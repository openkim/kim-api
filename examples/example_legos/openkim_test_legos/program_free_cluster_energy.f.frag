!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of an
!**  isolated cluster of SPECIES_NAME_STR particles
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
program TEST_NAME_STR
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  real(c_double), parameter :: FCCspacing     = FCC_SPACING_STR
  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  integer(c_int), parameter :: ASpecies         = 1
  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter :: SizeOne        = 1

  !
  ! neighbor list
  !
  type(neighObject_type), target :: neighObject
  real(c_double), parameter      :: cutpad = CUTOFF_PADDING_STR

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname = "TEST_NAME_STR"
  character(len=KIM_KEY_STRING_LENGTH) :: testkimfile = "descriptor.kim"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
  ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  integer(c_int) nbc

  type(c_ptr)    :: pkim
  integer(c_int) :: ier, idum
  integer(c_int) :: middleDum
  integer(c_int) :: I
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  integer(c_int), pointer :: numContrib;          type(c_ptr) :: pnumContrib
  integer(c_int), pointer :: numberOfSpecies;     type(c_ptr) :: pnOfSpecies
  integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) :: pparticleSpecies
  real(c_double), pointer :: cutoff;              type(c_ptr) :: pcutoff
  real(c_double), pointer :: energy;              type(c_ptr) :: penergy
  real(c_double), pointer :: coords(:,:);         type(c_ptr) :: pcoor
  real(c_double), pointer :: boxSideLengths(:);   type(c_ptr) :: pboxSideLengths


  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_file_init(pkim, testkimfile, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_file_init", ier)
     stop
  endif

  ! determine which NBC scenerio to use
  ier = kim_api_get_nbc_method(pkim, NBC_Method)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method", ier)
     stop
  endif
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
  call kim_api_allocate(pkim, N, ASpecies, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_allocate", ier)
     stop
  endif

  ! Allocate and store pointers to neighbor list object and access function
  if (nbc.lt.6) allocate(neighObject%neighborList(N+1, N))
  if (nbc.eq.0 .or. nbc.eq.2) then
     allocate(neighObject%RijList(DIM,N+1, N))
  endif
  !
  if (nbc.ne.6) then
     ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(neighObject))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_set_data", ier)
        stop
     endif
  endif

  if (nbc.ne.6) then
     ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh))
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_set_method", ier)
        stop
     endif
  endif

  ! call model's init routine
  ier = kim_api_model_init(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_init", ier)
     stop
  endif


  ! Unpack data from KIM object
  !
  call kim_api_getm_data(pkim, ier, &
       "numberOfParticles",           pnParts,          1,                                   &
       "numberContributingParticles", pnumContrib,      TRUEFALSE((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)), &
       "numberOfSpecies",             pnOfSpecies,      1,                                   &
       "particleSpecies",             pparticleSpecies, 1,                                   &
       "coordinates",                 pcoor,            1,                                   &
       "cutoff",                      pcutoff,          1,                                   &
       "boxSideLengths",              pboxSideLengths,  TRUEFALSE((nbc.eq.4).or.(nbc.eq.5)), &
       "energy",                      penergy,          1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnParts, numberOfParticles)
  if ((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)) call c_f_pointer(pnumContrib, &
                                                               numContrib)
  call c_f_pointer(pnOfSpecies, numberOfSpecies)
  call c_f_pointer(pparticleSpecies, particleSpecies, [N])
  call c_f_pointer(pcoor, coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)
  if ((nbc.eq.4).or.(nbc.eq.5)) call c_f_pointer(pboxSideLengths, &
                                                 boxSideLengths, [DIM])
  call c_f_pointer(penergy, energy)

  ! Set values
  numberOfParticles = N
  if ((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)) numContrib = N
  numberOfSpecies = ASpecies
  particleSpecies(:)    = kim_api_get_species_code(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_species_code", ier)
     stop
  endif

  ! set up the cluster part positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, &
                                middleDum)
 ! set boxSideLengths large enough to make the cluster isolated
  if (nbc.eq.4 .or. nbc.eq.5) boxSideLengths(:) = 600.0_cd

  ! compute neighbor lists
  if (nbc.eq.0) then
     call NEIGH_RVEC_cluster_neighborlist(.true., N, coords, (cutoff+cutpad), &
                                          neighObject)
  elseif (nbc.eq.1) then
     call NEIGH_PURE_cluster_neighborlist(.true., N, coords, (cutoff+cutpad), &
                                          neighObject)
  elseif (nbc.eq.2) then
     call NEIGH_RVEC_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), &
                                          neighObject)
  elseif (nbc.eq.3) then
     call NEIGH_PURE_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), &
                                          neighObject)
  elseif (nbc.eq.4) then
     call MI_OPBC_cluster_neighborlist(.true., N, coords, (cutoff+cutpad), &
                                       boxSideLengths, neighObject)
  elseif (nbc.eq.5) then
     call MI_OPBC_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), &
                                       boxSideLengths, neighObject)
  else
     ! nothing to do for CLUSTER
  endif

  ! Call model compute
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif

  ! print results to screen
  print '(80(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)
  print '("Using NBC: ",A)', trim(NBC_Method)
  print '("Energy        = ",ES25.15)', energy

  ! Don't forget to free and/or deallocate
  if (nbc.lt.6) deallocate(neighObject%neighborList)
  if (nbc.eq.0 .or. nbc.eq.2) then
     deallocate(neighObject%RijList)
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

  stop
end program TEST_NAME_STR
