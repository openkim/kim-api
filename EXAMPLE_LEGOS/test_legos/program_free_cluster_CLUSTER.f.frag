!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an
!**  isolated cluster of SPECIES_NAME_STR atoms
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
  integer(c_int), parameter :: ATypes         = 1
  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname = "TEST_NAME_STR"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  type(c_ptr)    :: pkim
  integer(c_int) :: ier, idum
  integer(c_int) :: I
  integer(c_int) :: middleDum
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnAtoms
  integer(c_int), pointer :: numberParticleTypes; type(c_ptr) :: pnparticleTypes
  integer(c_int), pointer :: particleTypes(:);    type(c_ptr) :: pparticleTypes
  real(c_double), pointer :: cutoff;              type(c_ptr) :: pcutoff
  real(c_double), pointer :: energy;              type(c_ptr) :: penergy
  real(c_double), pointer :: virial_global(:);    type(c_ptr) :: pvirialglob
  real(c_double), pointer :: coords(:,:);         type(c_ptr) :: pcoor
  real(c_double), pointer :: forces(:,:);         type(c_ptr) :: pforces

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_init", ier)
     stop
  endif
  ! Allocate memory via the KIM system
  call kim_api_allocate(pkim, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_allocate", ier)
     stop
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
       "numberOfParticles",   pnAtoms,           1, &
       "numberParticleTypes", pnparticleTypes,   1, &
       "particleTypes",       pparticleTypes,    1, &
       "coordinates",         pcoor,             1, &
       "cutoff",              pcutoff,           1, &
       "energy",              penergy,           1, &
       "virial",              pvirialglob,       1, &
       "forces",              pforces,           1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnAtoms, numberOfParticles)
  call c_f_pointer(pnparticleTypes, numberParticleTypes)
  call c_f_pointer(pparticleTypes,  particleTypes, [N])
  call c_f_pointer(pcoor, coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)
  call c_f_pointer(penergy, energy)
  call c_f_pointer(pvirialglob, virial_global, [6])
  call c_f_pointer(pforces, forces, [DIM,N])

  ! Set values
  numberOfParticles   = N
  numberParticleTypes = ATypes
  particleTypes(:)    = kim_api_get_partcl_type_code(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_partcl_type_code", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, middleDum)

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
  print '("Atom     ' // &
  'X                        ' // &
  'Y                        ' // &
  'Z                        ")'
  print '(I2,"   ",3ES25.15)', (I,forces(:,I),I=1,N)
  print *
  print '("Energy        = ",ES25.15)', energy
  print '("Global Virial = ",3ES25.15)', (virial_global(I),I=1,3)
  print '("                ",3ES25.15)', (virial_global(I),I=4,6)

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
