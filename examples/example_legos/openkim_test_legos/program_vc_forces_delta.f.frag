!*******************************************************************************
!**
!**  PROGRAM vc_forces_delta
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
program vc_forces_delta
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  real(c_double), parameter :: cutpad         = 0.75_cd
  integer(c_int), parameter :: max_species      = 30 ! most species supported
  integer(c_int), parameter :: max_NBCs       = 20 ! maximum number of NBCs
  real(c_double), parameter :: eps_prec       = epsilon(1.0_cd)
  integer(c_int), parameter :: ndisp          = 100 ! number of displacements
                                                    ! in one test
  real(c_double)            :: FCCspacing
  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter :: SizeOne = 1

  character(len=KIM_KEY_STRING_LENGTH) :: model_species(max_species)
  character(len=KIM_KEY_STRING_LENGTH) :: model_NBCs(max_NBCs)
  character(len=KIM_KEY_STRING_LENGTH), allocatable :: cluster_species(:)
  real(c_double), allocatable   :: forces_old(:,:)
  integer(c_int)                :: num_species
  integer(c_int)                :: num_NBCs
  integer(c_int)                :: nfail
  character(len=4)              :: passfail
  real(c_double)                :: energy_old
  real(c_double)                :: abs_mean_delta
  real(c_double)                :: error
  real(c_double)                :: abs_mean_error
  real(c_double),dimension(100) :: deltas, deltas_estimated
  real(c_double), allocatable   :: cluster_coords(:,:)
  real(c_double), allocatable   :: cluster_disps(:,:)
  integer(c_int)                :: I,J,species,idisp

  !
  ! neighbor list
  !
  type(neighObject_type), target :: neighObject
  real(c_double), allocatable    :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testname     = "vc_forces_delta"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
  ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  integer(c_int) nbc
  type(c_ptr)             :: pkim
  integer(c_int)          :: ier, idum, inbc
  integer(c_int)          :: middleDum
  real(c_double)          :: rnd
  character(len=10000)    :: test_descriptor_string
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) pnParts
  integer(c_int), pointer :: numContrib;          type(c_ptr) pnumContrib
  integer(c_int), pointer :: numberOfSpecies;     type(c_ptr) pnOfSpecies
  integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) pparticleSpecies
  real(c_double), pointer :: cutoff;              type(c_ptr) pcutoff
  real(c_double), pointer :: energy;              type(c_ptr) penergy
  real(c_double), pointer :: coords(:,:);         type(c_ptr) pcoor
  real(c_double), pointer :: forces(:,:);         type(c_ptr) pforces
  real(c_double), pointer :: boxSideLengths(:);   type(c_ptr) pboxSideLengths


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
  ! Generate random displacements for all parts
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
  print *,'VERIFICATION CHECK: DELTA DERIVATIVE VERIFICATION OF FORCES'
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
     ier = kim_api_string_init(pkim,trim(test_descriptor_string), modelname)
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
           ier = kim_api_set_data(pkim, "neighObject", SizeOne, &
                                  c_loc(neighObject))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "kim_api_set_data", ier)
              stop
           endif
        else
           ier = kim_api_set_data(pkim, "neighObject", SizeOne, &
                                  c_loc(neighObject))
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "kim_api_set_data", ier)
              stop
           endif
        endif
     endif

     ! Set pointer in KIM object to neighbor list routine
     !
     if (NBC.ne.6) then
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
     call c_f_pointer(pforces, forces, [DIM,N])

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


     do_update_list = .true.
     allocate(forces_old(DIM,N))
     allocate(coordsave(DIM,N))
     do idisp=1,ndisp
        ! Compute neighbor lists
        !
        if (nbc.le.5) then
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
        ! Copy forces and energy to a place where the model cannot change them
        energy_old = energy
        forces_old(:,:) = forces

        ! Generate random displacements for all particles. The displacements
        ! are not just meant to distort the system, as above. Instead, these
        ! are actually used to test how well the change in energy deu to this
        ! displacement can be reproduced by a first-order approximation.
        !
        do I=1,N
           do J=1,DIM
              call random_number(rnd)  ! return random number between 0 and 1
              cluster_disps(J,I) = 1e-4_cd*(rnd-0.5_cd)
              coords(J,I) = coords(J,I) + cluster_disps(J,I)
           enddo
        enddo

        ! Call model compute again to get forces (gradient)
        !
        if (nbc.le.5) then
           call update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                                    NBC_Method,do_update_list,coordsave, &
                                    neighObject,ier)
           if (ier.lt.KIM_STATUS_OK) then
              idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                          "update_neighborlist", ier)
              stop
           endif
        endif
        ier = kim_api_model_compute(pkim)
        if (ier.lt.KIM_STATUS_OK) then
           idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_model_compute", ier)
           stop
        endif

        ! Store real delta and estimated delta in array for later analysis
        !
        deltas(idisp) = energy_old - energy
        deltas_estimated(idisp) = sum((forces+forces_old)*cluster_disps/2)
     enddo

     ! Print output of statistical test
     !
     print '(41(''=''))'
     print '("NBC Method = ",A28)', trim(NBC_Method)
     print '(41(''=''))'
     print *
     print '(A5,2X,A15,2X,A15,2X,A15)', "IDisp", "Delta", "Delta Est", "Error"
     abs_mean_delta = sum(abs(deltas))/ndisp
     nfail = 0
     abs_mean_error = 0.0
     do idisp=1,ndisp
        error = abs(deltas(idisp)-deltas_estimated(idisp))
        abs_mean_error = abs_mean_error + error
        if (error > abs_mean_delta) then
            passfail = "FAIL"
            nfail = nfail + 1
        else
            passfail = ""
        endif
        print '(I5,2X,ES15.7,2X,ES15.7,2X,ES15.7,2X,A4)', &
               idisp, deltas(idisp), deltas_estimated(idisp), &
               error, passfail
     enddo
     abs_mean_error = abs_mean_error/ndisp
     print *
     print '(A30,ES15.7)', "Mean absolute Error:", abs_mean_error
     print '(A30,ES15.7)', "Mean absolute Delta:", abs_mean_delta
     print '(A30,ES15.7)', "Ratio:", abs_mean_error/abs_mean_delta
     print '(A30,I15)', "Number of epic failures:", nfail
     print *

     ! Free temporary storage
     !
     deallocate(forces_old)
     deallocate(coordsave)
     if (nbc.le.5) then ! deallocate neighbor list storage
        deallocate(neighObject%neighborList)
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

end program vc_forces_delta
