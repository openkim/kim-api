!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of and forces and virial on an
!**  isolated cluster of SPECIES_NAME_STR atoms
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
  integer(c_int), parameter :: ATypes         = 1
  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter :: SizeOne        = 1

  !
  ! neighbor list
  !
  real(c_double), parameter :: cutpad = CUTOFF_PADDING_STR
  type(neighObject_type), target :: neighObject

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
  integer(c_int) :: I,J
  integer(c_int) :: middleDum
  integer(c_int), pointer :: numberOfParticles;    type(c_ptr) pnAtoms
  integer(c_int), pointer :: numContrib;           type(c_ptr) pnumContrib
  integer(c_int), pointer :: numberParticleTypes;  type(c_ptr) pnparticleTypes
  integer(c_int), pointer :: particleTypes(:);     type(c_ptr) pparticleTypes
  real(c_double), pointer :: cutoff;               type(c_ptr) pcutoff
  real(c_double), pointer :: energy;               type(c_ptr) penergy
  real(c_double), pointer :: virial_global(:);     type(c_ptr) pvirialglob
  real(c_double), pointer :: coords(:,:);          type(c_ptr) pcoor
  real(c_double), pointer :: forces(:,:);          type(c_ptr) pforces
  real(c_double), pointer :: boxSideLengths(:);    type(c_ptr) pboxSideLengths
  real(c_double), pointer :: hessian(:,:,:);       type(c_ptr) phessian

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
  call kim_api_allocate(pkim, N, ATypes, ier)
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
       "numberOfParticles",           pnAtoms,          1,                                   &
       "numberContributingParticles", pnumContrib,      TRUEFALSE((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)), &
       "numberParticleTypes",         pnparticleTypes,  1,                                   &
       "particleTypes",               pparticleTypes,   1,                                   &
       "coordinates",                 pcoor,            1,                                   &
       "cutoff",                      pcutoff,          1,                                   &
       "boxSideLengths",              pboxSideLengths,  TRUEFALSE((nbc.eq.4).or.(nbc.eq.5)), &
       "energy",                      penergy,          1,                                   &
       "virial",                      pvirialglob,      1,                                   &
       "forces",                      pforces,          1,                                   &
       "hessian",                     phessian,         1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnAtoms, numberOfParticles)
  if ((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)) call c_f_pointer(pnumContrib, &
                                                               numContrib)
  call c_f_pointer(pnparticleTypes, numberParticleTypes)
  call c_f_pointer(pparticleTypes,  particleTypes, [N])
  call c_f_pointer(pcoor, coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)
  if ((nbc.eq.4).or.(nbc.eq.5)) call c_f_pointer(pboxSideLengths, &
                                                 boxSideLengths, [DIM])
  call c_f_pointer(penergy, energy)
  call c_f_pointer(pvirialglob, virial_global, [6])
  call c_f_pointer(pforces, forces, [DIM,N])
  call c_f_pointer(phessian, hessian, [DIM,DIM,N])

  ! Set values
  numberOfParticles = N
  if ((nbc.eq.0).or.(nbc.eq.1).or.(nbc.eq.4)) numContrib = N
  numberParticleTypes = ATypes
  particleTypes(:)    = kim_api_get_partcl_type_code(pkim, "SPECIES_NAME_STR", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_partcl_type_code", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., coords, &
                                middleDum)
 ! set boxSideLengths large enough to make the cluster isolated
  if ((nbc.eq.4).or.(nbc.eq.5)) boxSideLengths(:) = 600.0_cd

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
  print '("Forces:")'
  print '("Atom     ' // &
  'X                        ' // &
  'Y                        ' // &
  'Z                        ")'
  print '(I2,"   ",3ES25.15)', (I,forces(:,I),I=1,N)
  print *
  print '("Stiffness:")'
  do I=1,N
     do J=1,N
         print '("I = ",I2,"   J = ",I2)', I, J
         print '("    ",3ES25.15)', hessian(1,:,(I-1)*N + J)
         print '("    ",3ES25.15)', hessian(2,:,(I-1)*N + J)
         print '("    ",3ES25.15)', hessian(3,:,(I-1)*N + J)
     enddo
  enddo
  print *
  print '("Energy        = ",ES25.15)', energy
  print '("Global Virial = ",3ES25.15)', (virial_global(I),I=1,3)
  print '("                ",3ES25.15)', (virial_global(I),I=4,6)

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
