!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an
!**  isolated cluster of SPECIES_NAME_STR particles
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
  implicit none

  real(c_double), parameter :: FCCspacing     = FCC_SPACING_STR
  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  integer(c_int), parameter :: ASpecies         = 1
  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter :: one    = 1
  integer(c_int), parameter :: NN     = N
  integer(c_int), parameter :: DIMN   = DIM*N
  integer(c_int), parameter :: six    = 6

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname     = "TEST_NAME_STR"
  character(len=KIM_KEY_STRING_LENGTH) :: testkimfile  = "descriptor.kim"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  type(c_ptr)    :: pkim
  integer(c_int) :: ier, idum
  integer(c_int) :: I
  integer(c_int) :: middleDum
  integer(c_int), target :: numberOfParticles
  integer(c_int), target :: numberOfSpecies
  integer(c_int), target :: particleSpecies(N)
  real(c_double), target :: cutoff
  real(c_double), target :: energy
  real(c_double), target :: virialglob(6)
  real(c_double), target :: coords(DIM,N)
  real(c_double), target :: forces(DIM,N)


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
  ! register memory with KIM object
  call kim_api_setm_data(pkim, ier, &
   "numberOfParticles",   DIMN, c_loc(numberOfParticles),   TRUEFALSE(.true.), &
   "numberOfSpecies",     one,  c_loc(numberOfSpecies),     TRUEFALSE(.true.), &
   "particleSpecies",     NN,   c_loc(particleSpecies),     TRUEFALSE(.true.), &
   "coordinates",         DIMN, c_loc(coords),              TRUEFALSE(.true.), &
   "cutoff",              one,  c_loc(cutoff),              TRUEFALSE(.true.), &
   "energy",              one,  c_loc(energy),              TRUEFALSE(.true.), &
   "forces",              DIMN, c_loc(forces),              TRUEFALSE(.true.), &
   "virial",              six,  c_loc(virialglob),          TRUEFALSE(.true.))

  ! call model's init routine
  ier = kim_api_model_init(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_init", ier)
     stop
  endif

  ! Set values
  numberOfParticles   = N
  numberOfSpecies = ASpecies
  particleSpecies(:)    = kim_api_get_species_code(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_species_code", ier)
     stop
  endif

  ! set up the cluster particle positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, &
                                middleDum)

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
  print '("Forces:")'
  print '("Part     ' // &
  'X                        ' // &
  'Y                        ' // &
  'Z                        ")'
  print '(I2,"   ",3ES25.15)', (I,forces(:,I),I=1,N)
  print *
  print '("Energy        = ",ES25.15)', energy
  print '("Global Virial = ",3ES25.15)', (virialglob(I),I=1,3)
  print '("                ",3ES25.15)', (virialglob(I),I=4,6)

  ! don't forget to destroy and deallocate
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
