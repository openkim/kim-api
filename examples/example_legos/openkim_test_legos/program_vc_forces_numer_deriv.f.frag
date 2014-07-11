!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
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
program TEST_NAME_STR
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
  character(len=4)                     :: passfail
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
  character(len=KIM_KEY_STRING_LENGTH) :: testname     = "TEST_NAME_STR"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method
  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
  ! 4- MI_OPBC_H,    5- MI_OPBC_F,    6- CLUSTER
  integer(c_int) nbc
  type(c_ptr) pkim
  integer(c_int) ier, idum, inbc
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnAtoms
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
  ! Generate random displacements for all atoms
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
          "numberOfParticles",           pnAtoms,           1,                               &
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
     call c_f_pointer(pnAtoms,          numberOfParticles)
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

     ! print results to screen
     !
     print '(41(''=''))'
     print '("NBC Method = ",A28)', trim(NBC_Method)
     print '(41(''=''))'
     print *
     print '(A6,2X,A4,2X,A3,2X,2A25,3A15,2X,A4)',"Atom","Spec","Dir", &
           "Force_model", "Force_numer", "Force diff", "pred error", "weight", &
           "stat"
     forcediff_sumsq = 0.0_cd
     weight_sum = 0.0_cd
     do I=1,N
        do J=1,DIM
           forcediff = abs(forces(J,I)-forces_num(J,I))
           if (forcediff<forces_num_err(J,I)) then
              passfail = "    "
           else
              passfail = "over"
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
              print '(I6,2X,I4,2X,I3,2X,2ES25.15,3ES15.5,2X,A4)', &
                     I,particleSpecies(I),J,forces(J,I),forces_num(J,I), &
                     forcediff,forces_num_err(J,I),weight,passfail
           else
              print '(14X,I3,2X,2ES25.15,3ES15.5,2X,A4)', &
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
     print '(''Maximum term obtained for Atom = '',I6,'', Dir = '',I1,' // &
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

end program TEST_NAME_STR
