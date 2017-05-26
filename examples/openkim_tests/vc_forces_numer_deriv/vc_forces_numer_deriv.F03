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
! Copyright (c) 2013--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
!

!
! Release: This file is part of the kim-api.git package.
!


#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

#define STRING_LENGTH 256

!-------------------------------------------------------------------------------
!
! module mod_neighborlist :
!
!    Module contains type and routines related to neighbor list calculation
!
!-------------------------------------------------------------------------------

module mod_neighborlist

  use, intrinsic :: iso_c_binding

  public get_neigh

  type neighObject_type
     integer(c_int) :: number_of_particles
     integer(c_int), pointer :: neighborList(:,:)
     real(c_double), pointer :: RijList(:,:,:)
  end type neighObject_type
contains

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
subroutine get_neigh(data_object, neighbor_list_index, request, numnei, &
  pnei1part, ierr) bind(c)
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int), parameter :: DIM = 3
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject

  if (neighbor_list_index /= 1) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, "wrong list index", 1)
  endif

  call c_f_pointer(data_object, neighObject)

  numberOfParticles = neighObject%number_of_particles

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, &
      "Invalid part ID in get_neigh", 1)
    ierr = 1
    return
  endif

  ! set the returned number of neighbors for the returned part
  numnei = neighObject%neighborList(1,request)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighObject%neighborList(2,request))

  ierr = 0
  return
end subroutine get_neigh

end module mod_neighborlist

!*******************************************************************************
!**
!**  PROGRAM vc_forces_numer_deriv
!**
!**  KIM compliant program to perform numerical derivative check on a model
!**
!*******************************************************************************

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program vc_forces_numer_deriv
  use, intrinsic :: iso_c_binding
  use kim_species_name_module
  use kim_model_module
  use kim_compute_argument_name_module
  use kim_compute_argument_attribute_module
  use kim_compute_model_compute_arguments_module
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  real(c_double), parameter :: cutpad         = 0.75_cd
  integer(c_int), parameter :: max_species      = 200 ! most species supported
  real(c_double), parameter :: eps_prec       = epsilon(1.0_cd)
  real(c_double)  FCCspacing

  integer(c_int), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  real(c_double), allocatable          :: forces_num(:,:)
  real(c_double), allocatable          :: forces_num_err(:,:)
  type(kim_species_name_type) :: model_species(max_species)
  integer(c_int), target               :: num_species
  character(len=5)                     :: passfail
  real(c_double)                       :: forcediff
  real(c_double)                       :: forcediff_sumsq
  real(c_double)                       :: weight
  real(c_double)                       :: weight_sum
  real(c_double)                       :: alpha
  real(c_double)                       :: term
  real(c_double)                       :: term_max
  real(c_double), allocatable          :: cluster_coords(:,:)
  real(c_double), allocatable          :: cluster_disps(:,:)
  type(kim_species_name_type), allocatable :: cluster_species(:)
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
  character(len=STRING_LENGTH) :: testname     = "vc_forces_numer_deriv"
  character(len=STRING_LENGTH) :: modelname

  type(kim_model_type), pointer :: model
  type(kim_compute_model_compute_arguments_type), pointer :: arguments
  integer(c_int) ierr, ierr2
  integer(c_int), target :: numberOfParticles
  integer(c_int), target :: particleSpecies(N)
  integer(c_int), target :: particleContributing(N)
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_cutoffs
  real(c_double), pointer :: cutoffs(:)
  real(c_double) :: cutoff
  real(c_double), target :: energy
  real(c_double), target :: coords(3,N)
  real(c_double), target :: forces(3,N)
  integer(c_int) middleDum
  type(kim_compute_argument_attribute_type) argument_attribute
  logical forces_optional
  character(len=10000) :: test_descriptor_string
  real(c_double) rnd, deriv, deriv_err

  numberOfParticles = N

  term_max = 0.0_cd ! initialize

  ! Initialize error flag
  ierr = 0

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Get list of particle species supported by the model
  !
  call Get_Model_Supported_Species(trim(modelname), max_species, model_species, num_species, &
                                 ierr)
  if (ierr /= 0) then
     call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Get_Model_Supported_Species", ierr)
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
  ! Generate random displacements for all particles
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

  ! Write out KIM descriptor string for Test for current NBC
  call Write_KIM_descriptor(max_species, model_species, &
                            num_species, test_descriptor_string, ierr)
  if (ierr /= 0) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Write_KIM_descriptor", ierr)
     stop
  endif

  ! Create empty KIM object conforming to fields in the KIM descriptor files
  ! of the Test and Model
  !
  call kim_model_create(trim(test_descriptor_string), trim(modelname), model, ierr)
  if (ierr /= 0) then
     call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_string_init", ierr)
     stop
  endif

  call kim_model_create_compute_arguments(model, arguments, ierr)
  if (ierr /= 0) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, &
      "create_compute_arguments", ierr)
    stop
  endif

  ! determine in forces is optional for the model
  call kim_compute_model_compute_arguments_get_argument_attribute( &
    arguments, kim_compute_argument_name_forces, argument_attribute)
  if (argument_attribute%argument_attribute_id &
    == kim_compute_argument_attribute_optional%argument_attribute_id) then
    forces_optional = .true.
  else
    forces_optional = .false.
  endif

  ! register memory with the KIM system
  ierr = 0
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_number_of_particles, numberOfParticles, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_number_of_species, num_species, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_particle_species, N, particleSpecies, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_particle_contributing, N, particleContributing, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_coordinates, 3, N, coords, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_energy, energy, ierr2)
  ierr = ierr + ierr2
  call kim_compute_model_compute_arguments_set_data(arguments, &
    kim_compute_argument_name_forces, 3, N, forces, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
     call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "set_data", ierr)
     stop
  endif

  ! Allocate storage for neighbor lists and
  ! store pointers to neighbor list object and access function
  !
  allocate(neighObject%neighborList(N+1,N))
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_compute_model_compute_arguments_set_neigh(arguments, &
    kim_language_name_fortran, c_funloc(get_neigh), c_loc(neighobject))

  call kim_model_get_influence_distance(model, influence_distance)
  call kim_model_get_cutoffs(model, number_of_cutoffs, cutoffs)
  if (number_of_cutoffs /= 1) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, &
      "too many cutoffs", 1)
  endif
  cutoff = cutoffs(1)

  ! Scale reference FCC configuration based on cutoff radius.
  FCCspacing = 0.75_cd*cutoff ! set the FCC spacing to a fraction
                              ! of the cutoff radius
  do i=1,N
    cluster_coords(:,i) = FCCspacing*cluster_coords(:,i)
  enddo
  print '("Using FCC lattice parameter: ",f12.5)', FCCspacing

  do i=1,N
    call kim_model_get_species_code(model, cluster_species(i), &
      particleSpecies(i), ierr)
  enddo
  if (ierr /= 0) then
     call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_species_code", ierr)
     stop
  endif
  do i=1,N
    particleContributing(i) = 1  ! every particle contributes
  enddo
  do i=1,N
     coords(:,i) = cluster_coords(:,i) + cluster_disps(:,i)
  enddo

  ! Compute neighbor lists
  !
  do_update_list = .true.
  allocate(coordsave(DIM,N))
  call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                           do_update_list,coordsave, &
                           neighObject,ierr)
  if (ierr /= 0) then
    call kim_report_error(__LINE__, THIS_FILE_NAME, &
      "update_neighborlist", ierr)
    stop
  endif

  ! Call model compute to get forces (gradient)
  !
  call kim_model_compute(model, arguments, ierr)
  if (ierr /= 0) then
     call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ierr)
     stop
  endif

  ! Print results to screen
  !
  print '(41(''=''))'
  print '("Energy = ",ES25.15)', energy
  print '(41(''=''))'
  print *

  ! Turn off force computation, if possible
  !
  if (forces_optional) then
    call kim_compute_model_compute_arguments_set_compute(arguments, &
      kim_compute_argument_name_forces, 0, ierr)
    if (ierr /= 0) then
      call kim_report_error(__LINE__, THIS_FILE_NAME, &
        "set_compute", ierr)
      stop
    endif
  endif

  ! Compute gradient using numerical differentiation
  !
  allocate(forces_num(DIM,N),forces_num_err(DIM,N))
  do I=1,N
     do J=1,DIM
        call compute_numer_deriv(I,J,model,arguments,DIM,N,coords,cutoff, &
                                 cutpad, energy, do_update_list, &
                                 coordsave,neighObject,deriv,deriv_err,ierr)
        if (ierr /= 0) then
           call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                       "compute_numer_deriv", ierr)
           stop
        endif
        forces_num(J,I) = -deriv
        forces_num_err(J,I) = deriv_err
     enddo
  enddo

  ! Continue printing results to screen
  !
  print '(A6,2X,A4,2X,A3,2X,2A25,3A15,2X,A4)',"Part","Spec","Dir", &
        "Force_model", "Force_numer", "Force diff", "pred error", "weight", &
        "stat"
  forcediff_sumsq = 0.0_cd
  weight_sum = 0.0_cd
  do I=1,N
     do J=1,DIM
        forcediff = abs(forces(J,I)-forces_num(J,I))
        if (forcediff<forces_num_err(J,I)) then
           passfail = "ideal"
        else
           passfail = "     "
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
           print '(I6,2X,I4,2X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
                  I,particleSpecies(I),J,forces(J,I),forces_num(J,I), &
                  forcediff,forces_num_err(J,I),weight,passfail
        else
           print '(14X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
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
  print '(''Maximum term obtained for Part = '',I6,'', Dir = '',I1,' // &
     ''', forcediff = '',ES15.5, '', forcediff/force_model = '',ES15.5)', &
     Imax,Jmax,abs(forces(Jmax,Imax)-forces_num(Jmax,Imax)),           &
     abs(forces(Jmax,Imax)-forces_num(Jmax,Imax))/abs(forces(Jmax,Imax))

  ! Free temporary storage
  !
  deallocate(forces_num)
  deallocate(forces_num_err)
  deallocate(neighObject%neighborList)
  deallocate(coordsave)

  call kim_model_destroy_compute_arguments(model, arguments)
  call kim_model_destroy(model)

  ! Print output footer
  !
  print *
  print '(120(''-''))'

  ! Free cluster storage
  !
  deallocate(cluster_coords,cluster_disps,cluster_species)

  stop

end program vc_forces_numer_deriv

!--------------
type(kim_species_name_type) function kim_species_name(species_name_string)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module
  implicit none
  character(len=*), intent(in) :: species_name_string

  integer(c_int) number_of_species
  type(kim_species_name_type) species_name
  character(len=100) name_string
  integer(c_int) i
  integer(c_int) ierr

  call kim_species_name_get_number_of_species(number_of_species)

  do i=1,number_of_species
    call kim_species_name_get_species(i, species_name, ierr)
    call kim_species_name_string(species_name, name_string)

    if (trim(name_string) == trim(species_name_string)) then
      kim_species_name = species_name
      exit
    end if
  end do
  return
end function kim_species_name
!--------------

!-------------------------------------------------------------------------------
!
!  Get number and identities of particle species supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Species(modelname, max_species, model_species, &
                                       num_species, ier)
use, intrinsic :: iso_c_binding

use kim_model_module
use kim_species_name_module
implicit none
interface
  type(kim_species_name_type) function kim_species_name(species_name_string)
    use kim_species_name_module
    implicit none
    character(len=*), intent(in) :: species_name_string
  end function kim_species_name
end interface

!-- Transferred variables
character(len=*),                     intent(in)   :: modelname
integer(c_int),                       intent(in)   :: max_species
type(kim_species_name_type), intent(out) :: model_species(max_species)
integer(c_int),                       intent(out)  :: num_species
integer(c_int),                       intent(out)  :: ier

!-- Local variables
 integer(c_int) :: maxStringLength
 character(len=:), allocatable :: kim_file
 character(len=1024) :: line_string
 integer(c_int) :: newline_index
 integer(c_int) :: spec_index
 integer(c_int) :: name_index

! Initialize error flag
ier = 1

call kim_model_get_model_kim_string_length(trim(modelname), maxStringLength, ier)
allocate(character(len=(maxStringLength+20)) :: kim_file)
call kim_model_get_model_kim_string(trim(modelname), kim_file, ier)

num_species = 0
newline_index = index(kim_file, new_line('A'))
do while (newline_index /= 0)
  line_string = adjustl(trim(kim_file(1:newline_index)))
  kim_file=kim_file(newline_index+1:)
  spec_index = index(line_string, " spec ")
  if (spec_index /= 0) then
    num_species = num_species+1
    name_index = index(line_string, " ")
    model_species(num_species) = kim_species_name(line_string(1:name_index))
  end if
  newline_index = index(kim_file, new_line('A'))
end do
deallocate(kim_file)

ier = 0
return

end subroutine Get_Model_Supported_Species

!-------------------------------------------------------------------------------
!
!  Write KIM descriptor file for MiniMol for given set of
!  supported species
!
!-------------------------------------------------------------------------------
subroutine Write_KIM_descriptor(max_species, model_species, num_species, &
                                kim_descriptor, ier)
use, intrinsic :: iso_c_binding
use kim_species_name_module
implicit none
interface
  subroutine species_string(species_name, species_name_string)
    use kim_species_name_module
    implicit none
    type(kim_species_name_type), intent(in), value :: species_name
    character(len=*), intent(out) :: species_name_string
  end subroutine species_string
end interface
!-- Transferred variables
integer(c_int),                       intent(in)   :: max_species
type(kim_species_name_type), intent(in) :: model_species(max_species)
integer(c_int),                       intent(in)   :: num_species
character(len=10000),                 intent(out)  :: kim_descriptor
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) :: i
character(len=103)  :: divider
character(len=1)    :: cr
character(len=1024) :: species_line
character(len=20)   :: species_name

! Initialize error flag
ier = 0

! Define frequently used variables
!
cr = char(10)
divider = '#######################################################################################################'

! Write Minimol descriptor file into string kim_descriptor
!
kim_descriptor = &
   divider                                                                      // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER START'                                                        // cr // &
   '#'                                                                          // cr // &
   '# The contents of this file are subject to the terms of the Common Development' // cr // &
   '# and Distribution License Version 1.0 (the "License").'                    // cr // &
   '#'                                                                          // cr // &
   '# You can obtain a copy of the license at'                                  // cr // &
   '# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the'    // cr // &
   '# specific language governing permissions and limitations under the License.' // cr // &
   '#'                                                                          // cr // &
   '# When distributing Covered Code, include this CDDL HEADER in each file and'// cr // &
   '# include the License file in a prominent location with the name LICENSE.CDDL.' // cr // &
   '# If applicable, add the following below this CDDL HEADER, with the fields' // cr // &
   '# enclosed by brackets "[]" replaced with your own identifying information:'// cr // &
   '#'                                                                          // cr // &
   '# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.' // cr // &
   '#'                                                                          // cr // &
   '# CDDL HEADER END'                                                          // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# Copyright (c) 2013--2017, Regents of the University of Minnesota.'        // cr // &
   '# All rights reserved.'                                                     // cr // &
   '#'                                                                          // cr // &
   '# Contributors:'                                                            // cr // &
   '#    Automatically generated by calling Test'                               // cr // &
   '#'                                                                          // cr // &
                                                                                   cr // &
   '#'                                                                          // cr // &
   '# See src/standard.kim for documentation about this file'                   // cr // &
   '#'                                                                          // cr // &
   divider                                                                      // cr // &
                                                                                   cr // &
   'KIM_API_Version := 2.0.0'                                                   // cr // &
                                                                                   cr // &
   'Unit_length      := A'                                                      // cr // &
   'Unit_energy      := eV'                                                     // cr // &
   'Unit_charge      := e'                                                      // cr // &
   'Unit_temperature := K'                                                      // cr // &
   'Unit_time        := ps'                                                     // cr // &
                                                                                   cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'PARTICLE_SPECIES:'                                                          // cr // &
   '# Symbol/name               Type                    code'                   // cr

 do i = 1,num_species
   call kim_species_name_string(model_species(i), species_name)
   write(species_line,'(a,'' '',''spec'',20x,i4)') trim(species_name),0
      kim_descriptor = trim(kim_descriptor) // trim(species_line) // cr
   enddo

   kim_descriptor = trim(kim_descriptor) // &
   divider                                                                      // cr // &
   'CONVENTIONS:'                                                               // cr // &
   '# Name                      Type'                                           // cr // &
                                                                                   cr // &
   'OneBasedLists               flag'                                           // cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_INPUT:'                                                               // cr // &
   '# Name                      Type         Unit       requirements'           // cr // &
   'numberOfParticles           integer      none'                              // cr // &
                                                                                   cr // &
   'numberOfSpecies             integer      none'                              // cr // &
                                                                                   cr // &
   'particleSpecies             integer      none'                              // cr // &
                                                                                   cr // &
   'particleContributing        integer      none'                              // cr // &
                                                                                   cr // &
   'coordinates                 double       length'                            // cr // &
                                                                                   cr // &
   'get_neigh                   method       none'                              // cr // &
                                                                                   cr // &
   'neighObject                 pointer      none'                              // cr // &
                                                                                   cr // &
   divider                                                                      // cr // &
   'MODEL_OUTPUT:'                                                              // cr // &
   '# Name                      Type         Unit       requirements'           // cr // &
                                                                                   cr // &
   'destroy                     method       none'                              // cr // &
                                                                                   cr // &
   'reinit                      method       none'                              // cr // &
                                                                                   cr // &
   'compute                     method       none'                              // cr // &
                                                                                   cr // &
   'energy                      double       energy'                            // cr // &
                                                                                   cr // &
   'forces                      double       force'                             // cr // &
                                                                                   cr // &
   divider                                                                            // cr

return

end subroutine Write_KIM_descriptor

subroutine update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                               do_update_list,coordsave, &
                               neighObject,ierr)
use, intrinsic :: iso_c_binding
use mod_neighborlist
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!-- Transferred variables
integer(c_int),         intent(in)    :: DIM
integer(c_int),         intent(in)    :: N
real(c_double),         intent(in)    :: coords(DIM,N)
real(c_double),         intent(in)    :: cutoff
real(c_double),         intent(in)    :: cutpad
logical,                intent(inout) :: do_update_list
real(c_double),         intent(inout) :: coordsave(DIM,N)
type(neighObject_type), intent(inout) :: neighObject
integer(c_int),         intent(out)   :: ierr

!-- Local variables
! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
! 4- MI_OPBC_H, 5- MI_OPBC_F
real(c_double) disp, disp1, disp2, cutrange, dispvec(DIM)
integer(c_int) i

! Initialize error code
!
ierr = 0

! Update neighbor lists if necessary
!
if (.not.do_update_list) then   ! if update not requested

   ! check whether a neighbor list update is necessary even if it hasn't been
   ! requested using the "two max sum" criterion
   disp1 = 0.0_cd
   disp2 = 0.0_cd
   do i=1,N
      dispvec(1:DIM) = coords(1:DIM,i) - coordsave(1:DIM,i)
      disp = sqrt( dot_product(dispvec,dispvec) )
      if (disp >= disp1) then        !  1st position taken
         disp2 = disp1               !  push current 1st into 2nd place
         disp1 = disp                !  and put this one into current 1st
      else if (disp >= disp2) then   !  2nd position taken
         disp2 = disp
      endif
   enddo
   do_update_list = ( disp1 + disp2 > cutpad )

endif

if (do_update_list) then

   ! save current coordinates
   coordsave(1:DIM,1:N) = coords(1:DIM,1:N)

   ! compute neighbor lists
   cutrange = cutoff + cutpad
   call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutrange, &
                                        neighObject)

   ! neighbor list uptodate, no need to compute again for now
   do_update_list = .false.
endif

return

end subroutine update_neighborlist



!-------------------------------------------------------------------------------
!
! NEIGH_PURE_cluster_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_cluster_neighborlist(half, numberOfParticles, coords, &
                                           cutoff, neighObject)
  use, intrinsic :: iso_c_binding
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,        intent(in)            :: half
  integer(c_int), intent(in)            :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)            :: coords
  real(c_double), intent(in)            :: cutoff
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! part j is a neighbor of part i
           if ( (j .gt. i) .OR. ((.not. half) .AND. (i.ne.j)) ) then
               a = a+1
               neighObject%neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_cluster_neighborlist

!-------------------------------------------------------------------------------
!
! create_FCC_configuration subroutine
!
!  creates a cubic configuration of FCC particles with lattice spacing
!  `FCCspacing' and `nCellsPerSide' cells along each direction.
!
!  With periodic==.true. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
!
!  With periodic==.false. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3
!
!  Returns the Id of the particle situated in the middle of the configuration
!  (this particle will have the most neighbors.)
!
!-------------------------------------------------------------------------------
subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, periodic, &
                                    coords, MiddlePartId)
  use, intrinsic :: iso_c_binding
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  real(c_double), intent(in)  :: FCCspacing
  integer(c_int), intent(in)  :: nCellsPerSide
  logical,        intent(in)  :: periodic
  real(c_double), intent(out) :: coords(3,*)
  integer(c_int), intent(out) :: MiddlePartId
  !
  ! cluster setup variables
  !
  real(c_double) FCCshifts(3,4)
  real(c_double) latVec(3)
  integer(c_int) a, i, j, k, m

  ! Create a cubic FCC cluster
  !
  FCCshifts(1,1) = 0.0_cd
  FCCshifts(2,1) = 0.0_cd
  FCCshifts(3,1) = 0.0_cd
  FCCshifts(1,2) = 0.5_cd*FCCspacing
  FCCshifts(2,2) = 0.5_cd*FCCspacing
  FCCshifts(3,2) = 0.0_cd
  FCCshifts(1,3) = 0.5_cd*FCCspacing
  FCCshifts(2,3) = 0.0_cd
  FCCshifts(3,3) = 0.5_cd*FCCspacing
  FCCshifts(1,4) = 0.0_cd
  FCCshifts(2,4) = 0.5_cd*FCCspacing
 FCCshifts(3,4) = 0.5_cd*FCCspacing

  MiddlePartID = 1 ! Always put middle particle as #1
  a = 1            ! leave space for middle particle as particle #1
  do i=1,nCellsPerSide
     latVec(1) = (i-1)*FCCspacing
     do j=1,nCellsPerSide
        latVec(2) = (j-1)*FCCspacing
        do k=1,nCellsPerSide
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
              if ((i.eq.nCellsPerside/2+1).and.(j.eq.nCellsPerSide/2+1) .and. &
                   (k.eq.nCellsPerSide/2+1) .and. (m.eq.1)) then
                 coords(:,1) = latVec + FCCshifts(:,m) ! put middle particle as #1
                 a = a - 1
              endif
           enddo
        enddo
        if (.not. periodic) then
            ! Add in the remaining three faces
            ! pos-x face
            latVec(1) = nCellsPerSide*FCCspacing
            latVec(2) = (i-1)*FCCspacing
            latVec(3) = (j-1)*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,4)
            ! pos-y face
            latVec(1) = (i-1)*FCCspacing
            latVec(2) = nCellsPerSide*FCCspacing
            latVec(3) = (j-1)*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,3)
            ! pos-z face
            latVec(1) = (i-1)*FCCspacing
            latVec(2) = (j-1)*FCCspacing
            latVec(3) = nCellsPerSide*FCCspacing
            a = a+1; coords(:,a) = latVec
            a = a+1; coords(:,a) = latVec + FCCshifts(:,2)
         endif
     enddo
     if (.not. periodic) then
         ! Add in the remaining three edges
         latVec(1) = (i-1)*FCCspacing
         latVec(2) = nCellsPerSide*FCCspacing
         latVec(3) = nCellsPerSide*FCCspacing
         a = a+1; coords(:,a) = latVec
         latVec(1) = nCellsPerSide*FCCspacing
         latVec(2) = (i-1)*FCCspacing
         latVec(3) = nCellsPerSide*FCCspacing
         a = a+1; coords(:,a) = latVec
         latVec(1) = nCellsPerSide*FCCspacing
         latVec(2) = nCellsPerSide*FCCspacing
         latVec(3) = (i-1)*FCCspacing
         a = a+1; coords(:,a) = latVec
      endif
  enddo
  if (.not. periodic) then
      ! Add in the remaining corner
      a = a+1; coords(:,a) = nCellsPerSide*FCCspacing
  endif

  return

end subroutine create_FCC_configuration

subroutine compute_numer_deriv(partnum,dir,model,arguments,DIM,N,coords,&
                               cutoff,cutpad,energy, do_update_list,    &
                               coordsave,neighObject,deriv,deriv_err,ierr)
use, intrinsic :: iso_c_binding
use kim_model_module
use kim_compute_model_compute_arguments_module
use mod_neighborlist
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!--Transferred variables
integer(c_int),         intent(in)    :: partnum
integer(c_int),         intent(in)    :: dir
type(kim_model_type),   intent(in)    :: model
type(kim_compute_model_compute_arguments_type), intent(in) :: arguments
integer(c_int),         intent(in)    :: DIM
integer(c_int),         intent(in)    :: N
real(c_double),         intent(inout) :: coords(DIM,N)
real(c_double),         intent(in)    :: cutoff
real(c_double),         intent(in)    :: cutpad
real(c_double),         intent(inout) :: energy
logical,                intent(inout) :: do_update_list
real(c_double),         intent(inout) :: coordsave(DIM,N)
type(neighObject_type), intent(inout) :: neighObject
real(c_double),         intent(out)   :: deriv
real(c_double),         intent(out)   :: deriv_err
integer(c_int),         intent(out)   :: ierr

!-- Local variables
real(c_double), parameter :: eps_init = 1.e-6_cd
integer(c_int), parameter :: number_eps_levels = 15
real(c_double)  eps, deriv_last, deriv_err_last
integer(c_int)  i

! Initialize error flag
ierr = 0

deriv_last = 0.0_cd ! initialize

! Outer loop of Ridders' method for computing numerical derivative
!
eps = eps_init
deriv_err_last = huge(1.0_cd)
do i=1,number_eps_levels
   deriv = dfridr(eps,deriv_err)
   if (ierr /= 0) then
      call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                  "compute_numer_deriv",ierr)
      stop
   endif
   if (deriv_err>deriv_err_last) then
      deriv  = deriv_last
      deriv_err = deriv_err_last
      exit
   endif
   eps = eps*10.0_cd
   deriv_last  = deriv
   deriv_err_last = deriv_err
enddo

return

contains

   !----------------------------------------------------------------------------
   !
   ! Compute numerical derivative using Ridders' method
   !
   ! Based on code from Numerical Recipes, Press et al., Second Ed., Cambridge,
   ! 1992
   !
   ! Ref: Ridders, C. J. F., "Two algorithms for the calculation of F'(x)=D",
   !      Advances in Engineering Software, Vol. 4, no. 2, pp. 75-76, 1982.
   !
   !
   ! Returns the gradient grad() of a KIM-compliant interatomic model at the
   ! current configuration by Ridders' method of polynomial extrapolation.
   ! An estimate for the error in each component of the gradient is returned in
   ! grad_err().
   !
   !----------------------------------------------------------------------------
   real(c_double) function dfridr(h,err)
   implicit none

   !-- Transferred variables
   real(c_double), intent(inout) :: h
   real(c_double), intent(out)   :: err

   !-- Local variables
   integer(c_int), parameter :: NTAB=10     ! Maximum size of tableau
   real(c_double), parameter :: CON=1.4_cd  ! Stepsize incr. by CON at each iter
   real(c_double), parameter :: CON2=CON*CON
   real(c_double), parameter :: BIG=huge(1.0_cd)
   real(c_double), parameter :: SAFE=2.0_cd ! Returns when error is SAFE worse
                                            ! than the best so far
   integer(c_int) i,j
   real(c_double) errt,fac,hh,a(NTAB,NTAB),fp,fm,coordorig

   dfridr = 0.0_cd ! initialize

   if (h.eq.0.0_cd) then
      ierr = 1
      return
   endif

   hh = h
   coordorig = coords(dir,partnum)
   coords(dir,partnum) = coordorig + hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                            do_update_list,coordsave,       &
                            neighObject,ierr)
   call kim_model_compute(model, arguments, ierr)
   if (ierr /= 0) then
      call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ierr)
      stop
   endif
   fp = energy
   coords(dir,partnum) = coordorig - hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,&
                            do_update_list,coordsave,       &
                            neighObject,ierr)
   call kim_model_compute(model, arguments, ierr)
   if (ierr /= 0) then
      call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_model_compute",ierr)
      stop
   endif
   fm = energy
   coords(dir,partnum) = coordorig
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,&
                            do_update_list,coordsave,       &
                            neighObject,ierr)
   a(1,1)=(fp-fm)/(2.0_cd*hh)
   err=BIG
   ! successive columns in the Neville tableau will go to smaller step sizes
   ! and higher orders of extrapolation
   do i=2,NTAB
      ! try new, smaller step size
      hh=hh/CON
      coords(dir,partnum) = coordorig + hh
      call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                               do_update_list,coordsave,       &
                               neighObject,ierr)
      call kim_model_compute(model, arguments, ierr)
      if (ierr /= 0) then
         call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ierr)
         stop
      endif
      fp = energy
      coords(dir,partnum) = coordorig - hh
      call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                                  do_update_list,coordsave,       &
                                  neighObject,ierr)
      call kim_model_compute(model, arguments, ierr)
      if (ierr /= 0) then
         call kim_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_model_compute",ierr)
         stop
      endif
      fm = energy
      coords(dir,partnum) = coordorig
      call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                               do_update_list,coordsave,       &
                               neighObject,ierr)
      a(1,i)=(fp-fm)/(2.0_cd*hh)
      fac=CON2
      ! compute extrapolations of various orders, requiring no new function
      ! evaluations
      do j=2,i
         a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-1.0_cd)
         fac=CON2*fac
         ! The error strategy is to compute each new extrapolation to one order
         ! lower, both at the present step size and the previous one.
         errt = max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
         if (errt.le.err) then ! if error is decreased, save the improved answer
            err = errt
            dfridr=a(j,i)
         endif
      enddo
      if (abs(a(i,i)-a(i-1,i-1)).ge.SAFE*err) return ! if higher order is worse
                                                     ! by significant factor
                                                     ! `SAFE', then quit early.
   enddo
   return
   end function dfridr

end subroutine compute_numer_deriv
