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
! Copyright (c) 2013--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
!


module error
  use, intrinsic :: iso_c_binding
  implicit none
  public

contains
  subroutine my_error(message, line, file)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message
    integer, intent(in) :: line
    character(len=*, kind=c_char), intent(in) :: file

    print *,"* Error : '", trim(message), "' ", line, ":", &
      trim(file)
    stop
  end subroutine my_error

  subroutine my_warning(message, line, file)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message
    integer, intent(in) :: line
    character(len=*, kind=c_char), intent(in) :: file

    print *,"* Warning : '", trim(message), "' ", line, ":", &
      trim(file)
  end subroutine my_warning
end module error


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

  type, bind(c) :: neighObject_type
     real(c_double) :: cutoff
     integer(c_int) :: number_of_particles
     type(c_ptr) :: neighbor_list_pointer
  end type neighObject_type
contains

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
subroutine get_neigh(data_object, number_of_neighbor_lists, cutoffs, &
  neighbor_list_index, request, numnei, pnei1part, ierr) bind(c)
  use error
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: number_of_neighbor_lists
  real(c_double),        intent(in) :: cutoffs(number_of_neighbor_lists)
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject
  integer(c_int), pointer :: neighborList(:,:)

  call c_f_pointer(data_object, neighObject)
  numberOfParticles = neighObject%number_of_particles
  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

  if (cutoffs(neighbor_list_index) > neighObject%cutoff) then
    call my_warning("neighbor list cutoff too small for model cutoff", &
      __LINE__, __FILE__)
    ierr = 1
    return
  endif

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
    print *, request
    call my_warning("Invalid part ID in get_neigh", &
      __LINE__, __FILE__)
    ierr = 1
    return
  endif

  ! set the returned number of neighbors for the returned part
  numnei = neighborList(1,request)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighborList(2,request))

  ierr = 0
  return
end subroutine get_neigh

end module mod_neighborlist

module mod_utilities
  use kim_simulator_headers_module
  implicit none
  public

contains

!-------------------------------------------------------------------------------
!
!  Check if we are compatible with the model
!
!-------------------------------------------------------------------------------
subroutine check_model_compatibility(compute_arguments_handle, &
  forces_optional, model_is_compatible, ierr)
  use, intrinsic :: iso_c_binding
  use error
  implicit none

  !-- Transferred variables
  type(kim_compute_arguments_handle_type), intent(in) :: compute_arguments_handle
  logical, intent(out) :: forces_optional
  logical, intent(out) :: model_is_compatible
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int) i
  integer(c_int) number_of_argument_names
  integer(c_int) number_of_callback_names
  type(kim_compute_argument_name_type) argument_name
  type(kim_support_status_type) support_status
  type(kim_compute_callback_name_type) callback_name


  ! assume fail
  model_is_compatible = .false.
  ierr = 0

  ! check arguments
  call kim_compute_argument_name_get_number_of_compute_argument_names(&
    number_of_argument_names)
  do i=1,number_of_argument_names
    call kim_compute_argument_name_get_compute_argument_name(i, argument_name, &
      ierr)
    if (ierr /= 0) then
      call my_warning("can't get argument name", &
        __LINE__, __FILE__)
      return
    end if
    call kim_compute_arguments_get_argument_support_status( &
      compute_arguments_handle, argument_name, support_status, ierr)
    if (ierr /= 0) then
      call my_warning("can't get argument support_status", &
        __LINE__, __FILE__)
      return
    end if

    ! can only handle energy and forces as required args
    if (support_status == kim_support_status_required) then
      if (.not. ( &
        (argument_name == kim_compute_argument_name_partial_energy) .or. &
        (argument_name == kim_compute_argument_name_partial_forces))) then
        call my_warning("unsupported required argument", &
          __LINE__, __FILE__)
        ierr = 0
        return
      end if
    end if

    ! need both energy and forces not "notSupported"
    if ((argument_name == kim_compute_argument_name_partial_energy) .and. &
      (support_status == kim_support_status_not_supported)) then
      call my_warning("model does not support energy", &
        __LINE__, __FILE__)
      ierr = 0
      return
    end if
    if (argument_name == kim_compute_argument_name_partial_forces) then
      if (support_status == kim_support_status_not_supported) then
        call my_warning("model does not support forces", &
          __LINE__, __FILE__)
        ierr = 0
        return
      else if (support_status == kim_support_status_required) then
        forces_optional = .false.
      else if (support_status == kim_support_status_optional) then
        forces_optional = .true.
      else
        call my_warning("unknown support_status for forces", &
          __LINE__, __FILE__)
        ierr = 0
        return
      end if
    end if
  end do

  ! check call backs
  call kim_compute_callback_name_get_number_of_compute_callback_names( &
    number_of_callback_names)
  do i=1,number_of_callback_names
    call kim_compute_callback_name_get_compute_callback_name(i, callback_name, &
      ierr)
    if (ierr /= 0) then
      call my_warning("can't get call back name", &
        __LINE__, __FILE__)
      return
    end if
    call kim_compute_arguments_get_callback_support_status( &
      compute_arguments_handle, callback_name, support_status, ierr)
    if (ierr /= 0) then
      call my_warning("can't get call back support_status", &
        __LINE__, __FILE__)
      return
    end if

    ! cannot handle any "required" call backs
    if (support_status == kim_support_status_required) then
      call my_warning("unsupported required call back", &
        __LINE__, __FILE__)
      ierr = 0
      return
    end if
  end do

  ! got to here, then everything must be OK
  model_is_compatible = .true.
  ierr = 0
  return
end subroutine Check_Model_Compatibility

!-------------------------------------------------------------------------------
!
!  Get number and identities of particle species supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Species(model_handle, max_species, &
  model_species, num_species, ier)
use, intrinsic :: iso_c_binding
implicit none

!-- Transferred variables
type(kim_model_handle_type),          intent(in)   :: model_handle
integer(c_int),                       intent(in)   :: max_species
type(kim_species_name_type), intent(out) :: model_species(max_species)
integer(c_int),                       intent(out)  :: num_species
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) i
integer(c_int) total_num_species
type(kim_species_name_type) :: species_name
integer(c_int) species_is_supported
integer(c_int) code

! Initialize error flag
ier = 1

call kim_species_name_get_number_of_species_names(total_num_species)

if (total_num_species .gt. max_species) return

num_species = 0
do i=1,total_num_species
  call kim_species_name_get_species_name(i, species_name, ier)
  call kim_model_get_species_support_and_code(model_handle, species_name, &
    species_is_supported, code, ier)
  if ((ier == 0) .and. (species_is_supported .ne. 0)) then
    num_species = num_species + 1
    model_species(num_species) = species_name
  end if
end do

ier = 0
return

end subroutine Get_Model_Supported_Species

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
  integer(c_int), pointer :: neighborList(:,:)
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

  neighObject%cutoff = cutoff

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
               neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighborList(1,i) = a-1
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

subroutine compute_numer_deriv(partnum,dir,model_handle, &
                               compute_arguments_handle,DIM,N,coords,&
                               cutoff,cutpad,energy, do_update_list,    &
                               coordsave,neighObject,deriv,deriv_err,ierr)
use, intrinsic :: iso_c_binding
use error
use mod_neighborlist
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!--Transferred variables
integer(c_int),         intent(in)    :: partnum
integer(c_int),         intent(in)    :: dir
type(kim_model_handle_type), intent(in) :: model_handle
type(kim_compute_arguments_handle_type), intent(in) :: compute_arguments_handle
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
     call my_error("compute_numer_deriv", &
       __LINE__, __FILE__)
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

   if (abs(h).le.tiny(0.0_cd)) then  ! avoid division by zero
      ierr = 1
      return
   endif

   hh = h
   coordorig = coords(dir,partnum)
   coords(dir,partnum) = coordorig + hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                            do_update_list,coordsave,       &
                            neighObject,ierr)
   call kim_model_compute(model_handle, compute_arguments_handle, ierr)
   if (ierr /= 0) then
     call my_error("kim_api_model_compute", &
       __LINE__, __FILE__)
   endif
   fp = energy
   coords(dir,partnum) = coordorig - hh
   call update_neighborlist(DIM,N,coords,cutoff,cutpad,&
                            do_update_list,coordsave,       &
                            neighObject,ierr)
   call kim_model_compute(model_handle, compute_arguments_handle, ierr)
   if (ierr /= 0) then
     call my_error("kim_api_model_compute", &
       __LINE__, __FILE__)
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
      call kim_model_compute(model_handle, compute_arguments_handle, ierr)
      if (ierr /= 0) then
        call my_error("kim_api_model_compute", &
          __LINE__, __FILE__)
      endif
      fp = energy
      coords(dir,partnum) = coordorig - hh
      call update_neighborlist(DIM,N,coords,cutoff,cutpad, &
                                  do_update_list,coordsave,       &
                                  neighObject,ierr)
      call kim_model_compute(model_handle, compute_arguments_handle, ierr)
      if (ierr /= 0) then
        call my_error("kim_api_model_compute", &
          __LINE__, __FILE__)
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

end module mod_utilities

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
  use error
  use mod_neighborlist
  use mod_utilities
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
  character(len=5, kind=c_char)        :: passfail
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
  integer(c_int) I,J,Imax,Jmax,species_code
  integer(c_int) seed_size
  integer(c_int), allocatable :: seed(:)

  !
  ! neighbor list
  !
  type(neighObject_type), target :: neighObject
  integer(c_int), allocatable, target :: neighborList(:,:)
  real(c_double), allocatable :: coordsave(:,:)
  logical do_update_list

  !
  ! KIM variables
  !
  character(len=256, kind=c_char) :: testname = "vc_forces_numer_deriv"
  character(len=256, kind=c_char) :: modelname

  type(kim_model_handle_type) :: model_handle
  type(kim_compute_arguments_handle_type) :: compute_arguments_handle
  integer(c_int) ierr, ierr2
  integer(c_int) species_is_supported
  integer(c_int), target :: numberOfParticles
  integer(c_int), target :: particleSpeciesCodes(N)
  integer(c_int), target :: particleContributing(N)
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_neighbor_lists
  real(c_double), allocatable :: cutoffs(:)
  integer(c_int), allocatable :: &
    model_will_not_request_neighbors_of_noncontributing_particles(:)
  real(c_double) :: cutoff
  real(c_double), target :: energy
  real(c_double), target :: coords(3,N)
  real(c_double), target :: forces_kim(3,N)
  real(c_double) :: forces(3,N)
  integer(c_int) middleDum
  logical forces_optional
  logical model_is_compatible
  integer(c_int) requested_units_accepted
  real(c_double) rnd, deriv, deriv_err
  real(c_double), pointer :: null_pointer

  nullify(null_pointer)

  numberOfParticles = N

  term_max = 0.0_cd ! initialize

  ! Initialize error flag
  ierr = 0

  ! Initialize seed for random number generator
  !
  ! NOTE: Here, we set the seed to a fixed value for reproducibility
  call random_seed(size=seed_size) ! Get seed size for this CPU
  allocate(seed(seed_size))
  seed(:) = 13
  call random_seed(put=seed)
  deallocate(seed)

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Print output header
  !
  print *
  print *,'VERIFICATION CHECK: NUMERICAL DERIVATIVE VERIFICATION OF FORCES'
  print *
  print '(120(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! Create empty KIM object
  !
  call kim_model_create(kim_numbering_one_based, &
    kim_length_unit_a, &
    kim_energy_unit_ev, &
    kim_charge_unit_e, &
    kim_temperature_unit_k, &
    kim_time_unit_ps, &
    trim(modelname), &
    requested_units_accepted, &
    model_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create", __LINE__, __FILE__)
  endif

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units", __LINE__, __FILE__)
  end if

  ! create compute_arguments object
  call kim_model_compute_arguments_create(model_handle, &
    compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_create", __LINE__, __FILE__)
  endif

  call check_model_compatibility(compute_arguments_handle, forces_optional, &
    model_is_compatible, ierr)
  if (ierr /= 0) then
    call my_error("error checking compatibility", __LINE__, __FILE__)
  end if
  if (.not. model_is_compatible) then
    call my_error("incompatibility reported by check_model_compatibility", &
      __LINE__, __FILE__)
  end if

  ! Get list of particle species supported by the model
  !
  call Get_Model_Supported_Species(model_handle, max_species, model_species, &
    num_species, ierr)
  if (ierr /= 0) then
     call my_error("Get_Model_Supported_Species", __LINE__, __FILE__)
  endif
  ! Setup random cluster
  !
  allocate(cluster_coords(3,N),cluster_disps(3,N),cluster_species(N))
  do i=1,N
     call random_number(rnd)  ! return random number between 0 and 1
     species_code = 1 + int(rnd*num_species)
     cluster_species(i) = model_species(species_code)
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

  ! register memory with the KIM system
  ierr = 0
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_number_of_particles, numberOfParticles, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_species_codes, particleSpeciesCodes, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_contributing, particleContributing, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_coordinates, coords, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_partial_energy, energy, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_partial_forces, forces_kim, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
     call my_error("set_argument_pointer", __LINE__, __FILE__)
  endif

  ! Allocate storage for neighbor lists and
  ! store pointers to neighbor list object and access function
  !
  allocate(neighborList(N+1,N))
  neighObject%neighbor_list_pointer = c_loc(neighborList)
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_compute_arguments_set_callback_pointer(compute_arguments_handle, &
    kim_compute_callback_name_get_neighbor_list, kim_language_name_fortran, &
    c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer", __LINE__, __FILE__)
  end if

  call kim_model_get_influence_distance(model_handle, influence_distance)
  call kim_model_get_number_of_neighbor_lists(model_handle, &
    number_of_neighbor_lists)
  allocate(cutoffs(number_of_neighbor_lists), &
           model_will_not_request_neighbors_of_noncontributing_particles( &
                                                     number_of_neighbor_lists))
  call kim_model_get_neighbor_list_values(model_handle, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  if (ierr /= 0) then
    call my_error("get_neighbor_list_values", __LINE__, __FILE__)
  end if
  cutoff = maxval(cutoffs)

  ! Scale reference FCC configuration based on cutoff radius.
  FCCspacing = 0.75_cd*cutoff ! set the FCC spacing to a fraction
                              ! of the cutoff radius
  do i=1,N
    cluster_coords(:,i) = FCCspacing*cluster_coords(:,i)
  enddo
  print '("Using FCC lattice parameter: ",f12.5)', FCCspacing

  do i=1,N
    call kim_model_get_species_support_and_code(model_handle, &
      cluster_species(i), species_is_supported, particleSpeciesCodes(i), ierr)
  enddo
  if (ierr /= 0) then
    call my_error("kim_api_get_species_code", __LINE__, __FILE__)
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
    call my_error("update_neighborlist", __LINE__, __FILE__)
  endif

  ! Call model compute to get forces (gradient)
  !
  call kim_model_compute(model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
     call my_error("kim_api_model_compute", __LINE__, __FILE__)
  endif

  ! Copy forces in case model will overwrite forces_kim below
  !
  forces = forces_kim

  ! Print results to screen
  !
  print '(41(''=''))'
  print '("Energy = ",ES25.15)', energy
  print '(41(''=''))'
  print *

  ! Turn off force computation, if possible
  !
  if (forces_optional) then
    call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
      kim_compute_argument_name_partial_forces, null_pointer, ierr)
    if (ierr /= 0) then
      call my_error("set_argument_pointer", __LINE__, __FILE__)
    endif
  endif

  ! Compute gradient using numerical differentiation
  !
  allocate(forces_num(DIM,N),forces_num_err(DIM,N))
  do I=1,N
     do J=1,DIM
        call compute_numer_deriv(I,J,model_handle,compute_arguments_handle,&
                                 DIM,N,coords,cutoff, &
                                 cutpad, energy, do_update_list, &
                                 coordsave,neighObject,deriv,deriv_err,ierr)
        if (ierr /= 0) then
          call my_error("compute_numer_deriv", __LINE__, __FILE__)
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
                  I,particleSpeciesCodes(I),J,forces(J,I),forces_num(J,I), &
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
  deallocate(neighborList)
  deallocate(coordsave)
  deallocate(cutoffs, model_will_not_request_neighbors_of_noncontributing_particles)

  call kim_model_compute_arguments_destroy(model_handle, &
    compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_destroy", __LINE__, __FILE__)
  endif
  call kim_model_destroy(model_handle)

  ! Print output footer
  !
  print *
  print '(120(''-''))'

  ! Free cluster storage
  !
  deallocate(cluster_coords,cluster_disps,cluster_species)

  stop

end program vc_forces_numer_deriv
