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
! Copyright (c) 2013--2015, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!    Stephen M. Whalen
!

!****************************************************************************
!**
!**  MODULE ex_model_driver_P_LJ
!**
!**  Lennard-Jones pair potential KIM Model Driver
!**  shifted to have zero energy at the cutoff radius
!**
!**  Language: Fortran 2003
!**
!**  Release: This file is part of the kim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_driver_p_lj

use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

save
private
public BUFFER_TYPE,           &
       Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi,              &
       calc_phi_dphi,         &
       calc_phi_dphi_d2phi

! Below are the definitions and values of all Model parameters
integer(c_int), parameter          :: cd = c_double  ! for literal constants
integer(c_int), parameter          :: DIM=3          ! dimensionality of space
integer(c_int), parameter          :: speccode = 1   ! internal species code

!-------------------------------------------------------------------------------
!
!  Definition of Buffer type
!
!-------------------------------------------------------------------------------
type BUFFER_TYPE
  integer(c_int) :: NBC
  integer(c_int) :: HalfOrFull
  integer(c_int) :: IterOrLoca
  integer(c_int) :: energy_ind
  integer(c_int) :: forces_ind
  integer(c_int) :: particleEnergy_ind
  integer(c_int) :: process_dEdr_ind
  integer(c_int) :: process_d2Edr2_ind
  integer(c_int) :: model_index_shift
  integer(c_int) :: numberOfParticles_ind
  integer(c_int) :: particleSpecies_ind
  integer(c_int) :: coordinates_ind
  integer(c_int) :: numberContributingParticles_ind
  integer(c_int) :: boxSideLengths_ind
  integer(c_int) :: get_neigh_ind
  integer(c_int) :: cutoff_ind
  real(c_double) :: Pcutoff
  real(c_double) :: cutsq
  real(c_double) :: epsilon
  real(c_double) :: sigma
  real(c_double) :: shift
endtype BUFFER_TYPE


contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(model_epsilon,  &
                    model_sigma,    &
                    model_shift,    &
                    model_cutoff,r,phi)
implicit none

!-- Transferred variables
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.0_cd
else
   phi = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi(model_epsilon,  &
                         model_sigma,    &
                         model_shift,    &
                         model_cutoff,r,phi,dphi)
implicit none

!-- Transferred variables
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi,dphi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
else
   phi  = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
   dphi = 24.0_cd*model_epsilon*(-2.0_cd*sor12+sor6)/r
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivatives dphi(r) and d2phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi_d2phi(model_epsilon,  &
                               model_sigma,    &
                               model_shift,    &
                               model_cutoff,r,phi,dphi,d2phi)
implicit none

!-- Transferred variables
real(c_double), intent(in)  :: model_epsilon
real(c_double), intent(in)  :: model_sigma
real(c_double), intent(in)  :: model_shift
real(c_double), intent(in)  :: model_cutoff
real(c_double), intent(in)  :: r
real(c_double), intent(out) :: phi,dphi,d2phi

!-- Local variables
real(c_double) rsq,sor,sor6,sor12

rsq  = r*r             !  r^2
sor  = model_sigma/r   !  (sig/r)
sor6 = sor*sor*sor     !
sor6 = sor6*sor6       !  (sig/r)^6
sor12= sor6*sor6       !  (sig/r)^12
if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.0_cd
   dphi   = 0.0_cd
   d2phi  = 0.0_cd
else
   phi   = 4.0_cd*model_epsilon*(sor12-sor6) + model_shift
   dphi  = 24.0_cd*model_epsilon*(-2.0_cd*sor12+sor6)/r
   d2phi = 24.0_cd*model_epsilon*(26.0_cd*sor12-7.0_cd*sor6)/rsq
endif

end subroutine calc_phi_dphi_d2phi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on particles from the positions.
!
!-------------------------------------------------------------------------------
integer(c_int) function Compute_Energy_Forces(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in)  :: pkim

!-- Local variables
real(c_double) :: r,Rsqij,phi,dphi,d2phi,dEidr,d2Eidr
integer(c_int) :: numberContrib
integer(c_int) :: i,j,jj,numnei,part_ret
integer(c_int) :: comp_force,comp_energy,comp_enepot,comp_process_dEdr, &
                  comp_process_d2Edr2
integer(c_int) :: idum
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

real(c_double), pointer :: Rij(:)
real(c_double), pointer :: Rij_pairs(:,:)
real(c_double), pointer :: r_pairs(:)
integer(c_int), pointer :: i_pairs(:), j_pairs(:)

!-- KIM variables
real(c_double), pointer :: model_cutoff;      type(c_ptr) :: pmodel_cutoff
integer(c_int), pointer :: N;                 type(c_ptr) :: pN
real(c_double), pointer :: energy;            type(c_ptr) :: penergy
real(c_double), pointer :: coor(:,:);         type(c_ptr) :: pcoor
real(c_double), pointer :: force(:,:);        type(c_ptr) :: pforce
real(c_double), pointer :: enepot(:);         type(c_ptr) :: penepot
real(c_double), pointer :: boxSideLengths(:); type(c_ptr) :: pboxSideLengths
real(c_double), pointer :: Rij_list(:,:);     type(c_ptr) :: pRij_list
integer(c_int), pointer :: numContrib;        type(c_ptr) :: pnumContrib
integer(c_int), pointer :: nei1part(:);       type(c_ptr) :: pnei1part
integer(c_int), pointer :: particleSpecies(:);type(c_ptr) :: pparticleSpecies


numberContrib = 0 ! initialize

! get model buffer from KIM object
pbuf = kim_api_get_model_buffer(pkim, Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,   &
                               "kim_api_get_model_buffer", &
                               Compute_Energy_Forces)
   return
endif
call c_f_pointer(pbuf, buf)

! Unpack the Model's cutoff stored in the KIM API object
!
pmodel_cutoff = kim_api_get_data_by_index(pkim, buf%cutoff_ind, &
                                          Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,    &
                               "kim_api_get_data_by_index", &
                               Compute_Energy_Forces)
   return
endif
call c_f_pointer(pmodel_cutoff, model_cutoff)

! Check to see if we have been asked to compute the forces, energyperpart,
! energy and d1Edr
!
call kim_api_getm_compute_by_index(pkim, Compute_Energy_Forces, &
     buf%energy_ind,         comp_energy,         1, &
     buf%forces_ind,         comp_force,          1, &
     buf%particleEnergy_ind, comp_enepot,         1, &
     buf%process_dEdr_ind,   comp_process_dEdr,   1, &
     buf%process_d2Edr2_ind, comp_process_d2Edr2, 1)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,        &
                               "kim_api_getm_compute_by_index", &
                               Compute_Energy_Forces)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data_by_index(pkim, Compute_Energy_Forces, &
     buf%numberOfParticles_ind,           pN,              1,                               &
     buf%particleSpecies_ind,             pparticleSpecies,1,                               &
     buf%coordinates_ind,                 pcoor,           1,                               &
     buf%numberContributingParticles_ind, pnumContrib,     TRUEFALSE(buf%HalfOrFull.eq.1),  &
     buf%boxSideLengths_ind,              pboxSideLengths, TRUEFALSE(buf%NBC.eq.2),         &
     buf%energy_ind,                      penergy,         TRUEFALSE(comp_energy.eq.1),     &
     buf%forces_ind,                      pforce,          TRUEFALSE(comp_force.eq.1),      &
     buf%particleEnergy_ind,              penepot,         TRUEFALSE(comp_enepot.eq.1))
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,     &
                               "kim_api_getm_data_by_index", &
                               Compute_Energy_Forces)
   return
endif

call c_f_pointer(pN,             N)
call c_f_pointer(pparticleSpecies, particleSpecies, [N])
call c_f_pointer(pcoor,          coor,          [DIM,N])
if (buf%HalfOrFull.eq.1) then
   if (buf%NBC.ne.3) then ! non-CLUSTER cases
     call c_f_pointer(pnumContrib, numContrib)
     numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif
if (buf%NBC.eq.2)     call c_f_pointeR(pboxSideLengths, boxSideLengths, [DIM])
if (comp_energy.eq.1) call c_f_pointer(penergy,         energy)
if (comp_force.eq.1)  call c_f_pointer(pforce,          force,          [DIM,N])
if (comp_enepot.eq.1) call c_f_pointer(penepot,         enepot,         [N])


allocate( Rij(DIM) )
if (comp_process_d2Edr2.eq.1) then
  allocate( r_pairs(2)       )
  allocate( Rij_pairs(DIM,2) )
  allocate( i_pairs(2)       )
  allocate( j_pairs(2)       )
endif

! Check to be sure that the species are correct
!
Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (particleSpecies(i).ne.speccode) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME,      &
                                  "Unexpected species detected", &
                                  Compute_Energy_Forces)
      return
   endif
enddo
Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces
!
if (comp_enepot.eq.1) enepot = 0.0_cd
if (comp_energy.eq.1) energy = 0.0_cd
if (comp_force.eq.1)  force  = 0.0_cd

! Initialize neighbor handling for CLUSTER NBC
!
if (buf%NBC.eq.3) then
   allocate( nei1part(N) )
endif

! Initialize neighbor handling for Iterator mode
!
if (buf%IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh(pkim,0,0,part_ret,numnei, &
                                             pnei1part,pRij_list)
   ! check for successful initialization
   if (Compute_Energy_Forces.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh", Compute_Energy_Forces)
      Compute_Energy_Forces = KIM_STATUS_FAIL
      return
   endif
endif

!
!  Compute energy and forces
!

!  Loop over particles and compute energy and forces
!
i = 0
do

   ! Set up neighbor list for next particle for all NBC methods
   !
   if (buf%IterOrLoca.eq.1) then    ! ITERATOR mode
      Compute_Energy_Forces = kim_api_get_neigh(pkim,0,1,part_ret,numnei, &
                                                pnei1part,pRij_list)
      if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                                ! incremented past the end of the list,
                                ! terminate loop
      if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
         ! some sort of problem, exit
         idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                     "kim_api_get_neigh", Compute_Energy_Forces)
         return
      endif

      i = part_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (buf%NBC.eq.3) then    ! CLUSTER NBC method
         numnei = N - i         ! number of neighbors in list i+1, ..., N
         nei1part(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
         Compute_Energy_Forces = KIM_STATUS_OK
      else                      ! All other NBCs
         Compute_Energy_Forces = kim_api_get_neigh(pkim,1,i,part_ret,numnei, &
                                                   pnei1part,pRij_list)
         if (Compute_Energy_Forces.ne.KIM_STATUS_OK) then
            ! some sort of problem, exit
            idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                        "kim_api_get_neigh",      &
                                        Compute_Energy_Forces)
            Compute_Energy_Forces = KIM_STATUS_FAIL
            return
         endif
      endif
   endif

   if (buf%NBC.ne.3) call c_f_pointer(pnei1part, nei1part, [numnei])
   if (buf%NBC.eq.0) call c_f_pointer(pRij_list, Rij_list, [DIM,numnei])

   ! Loop over the neighbors of particle i
   !
   do jj = 1, numnei

      j = nei1part(jj)                        ! get neighbor ID

      ! compute relative position vector
      !
      if (buf%NBC.ne.0) then                  ! all methods except NEIGH_RVEC
         Rij(:) = coor(:,j) - coor(:,i)       ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (buf%NBC.eq.2) then
        ! periodic boundary conditions applied where needed.
         where ( abs(Rij) .gt. 0.5_cd*boxSideLengths )
            Rij = Rij - sign(boxSideLengths,Rij)
         end where
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. buf%cutsq ) then            ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_process_d2Edr2.eq.1) then
            call calc_phi_dphi_d2phi(buf%epsilon, &
                                     buf%sigma,   &
                                     buf%shift,   &
                                     buf%Pcutoff,  &
                                     r,phi,dphi,d2phi) ! compute pair potential
                                                       !   and it derivatives
            if ((buf%HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr  = dphi                      !      double contribution
               d2Eidr = d2phi
            else                                  ! FULL mode
               dEidr  = 0.5_cd*dphi               !      regular contribution
               d2Eidr = 0.5_cd*d2phi
            endif
         elseif (comp_force.eq.1.or.comp_process_dEdr.eq.1) then
            call calc_phi_dphi(buf%epsilon, &
                               buf%sigma,   &
                               buf%shift,   &
                               buf%Pcutoff,  &
                               r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative

            if ((buf%HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5_cd*dphi                !      regular contribution
            endif
         else
            call calc_phi(buf%epsilon, &
                          buf%sigma,   &
                          buf%shift,   &
                          buf%Pcutoff,  &
                          r,phi)                  ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            enepot(i) = enepot(i) + 0.5_cd*phi    ! accumulate energy
            if ((buf%HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) &         ! HALF mode
               enepot(j) = enepot(j) + 0.5_cd*phi ! (i and j share it)
         endif
         if (comp_energy.eq.1) then
            if ((buf%HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5_cd*phi       !      add half v to total energy
            endif
         endif

         ! contribution to process_dEdr
         !
         if (comp_process_dEdr.eq.1) then
            Compute_Energy_Forces = kim_api_process_dEdr(pkim, dEidr, r, &
                                                         c_loc(Rij(1)), i, j)
         endif

         ! contribution to process_d2Edr2
         if (comp_process_d2Edr2.eq.1) then
            r_pairs(1) = r
            r_pairs(2) = r
            Rij_pairs(:,1) = Rij
            Rij_pairs(:,2) = Rij
            i_pairs(1) = i
            i_pairs(2) = i
            j_pairs(1) = j
            j_pairs(2) = j

            Compute_Energy_Forces = kim_api_process_d2Edr2(pkim, d2Eidr,       &
                                                        c_loc(r_pairs(1)),     &
                                                        c_loc(Rij_pairs(1,1)), &
                                                        c_loc(i_pairs(1)),     &
                                                        c_loc(j_pairs(1)))
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then
            force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on particle i
            force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on particle j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
deallocate( Rij )
if (comp_process_d2Edr2.eq.1) then
  deallocate( r_pairs   )
  deallocate( Rij_pairs )
  deallocate( i_pairs   )
  deallocate( j_pairs   )
endif
if (buf%NBC.eq.3) deallocate( nei1part )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model driver reinitialization routine
!
!-------------------------------------------------------------------------------
integer(c_int) function reinit(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

!-- Local variables
real(c_double) energy_at_cutoff
integer(c_int) idum
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

!-- KIM variables
real(c_double), pointer :: cutoff; type(c_ptr) :: pcutoff

! get model buffer from KIM object
pbuf = kim_api_get_model_buffer(pkim, reinit)
if (reinit.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_model_buffer", reinit)
   return
endif
call c_f_pointer(pbuf, buf)

pcutoff = kim_api_get_data_by_index(pkim, buf%cutoff_ind, reinit)
if (reinit.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_data_by_index", reinit)
   return
endif
call c_f_pointer(pcutoff, cutoff)

!
! Set new values in KIM object and buffer
!
cutoff    = buf%Pcutoff
buf%cutsq = cutoff**2
! calculate pair potential at r=cutoff with shift=0.0
call calc_phi(buf%epsilon, &
              buf%sigma,   &
              0.0_cd,      &
              buf%Pcutoff, &
              buf%Pcutoff,energy_at_cutoff)
buf%shift = -energy_at_cutoff

reinit = KIM_STATUS_OK
return

end function reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
integer(c_int) function destroy(pkim) bind(c)
implicit none

!-- Transferred variables
type(c_ptr), intent(in) :: pkim

!-- Local variables
integer(c_int) idum
type(BUFFER_TYPE), pointer :: buf; type(c_ptr) :: pbuf

! get model buffer from KIM object
pbuf = kim_api_get_model_buffer(pkim, destroy)
if (destroy.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_model_buffer", destroy)
   return
endif
call c_f_pointer(pbuf, buf)

deallocate( buf )

destroy = KIM_STATUS_OK
return

end function destroy

end module ex_model_driver_p_lj

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer(c_int) function model_driver_init(pkim, pparamfile, nmstrlen, &
                                          numparamfiles) bind(c)
use, intrinsic :: iso_c_binding
use ex_model_driver_p_lj
use KIM_API_F03
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!-- Transferred variables
type(c_ptr),        intent(in) :: pkim
type(c_ptr), value, intent(in) :: pparamfile
integer(c_int),     intent(in) :: nmstrlen
integer(c_int),     intent(in) :: numparamfiles
character(len=nmstrlen), pointer :: paramfile(:)

!-- Local variables
integer(c_int), parameter :: one=1
integer(c_int) i,j,ier, idum
type(BUFFER_TYPE), pointer :: buf;
character (len=80) :: error_message
! define variables for all model parameters to be read in
real(c_double) in_cutoff
real(c_double) in_epsilon
real(c_double) in_sigma
real(c_double) energy_at_cutoff

!-- KIM variables
real(c_double), pointer :: cutoff; type(c_ptr) :: pcutoff
character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method


call c_f_pointer(pparamfile, paramfile, [numparamfiles])

! find first null character and write spaces everywhere afterward
do i = 1, numparamfiles
  j = index(paramfile(1),char(0))
  paramfile(i)(j:)=" "
end do

! store function pointers in KIM object
call kim_api_setm_method(pkim, ier, &
     "compute", one, c_funloc(Compute_Energy_Forces), 1, &
     "reinit",  one, c_funloc(reinit),                1, &
     "destroy", one, c_funloc(destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_setm_method", ier)
   goto 42
endif

! Read in model parameters from parameter file
!
open(10,file=paramfile(1),status="old")
read(10,*,iostat=ier,err=100) in_cutoff
read(10,*,iostat=ier,err=100) in_epsilon
read(10,*,iostat=ier,err=100) in_sigma
close(10)

goto 200
100 continue
! reading parameters failed
ier = KIM_STATUS_FAIL
idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                            "Unable to read LJ parameters, kimerror = ",ier)
goto 42

200 continue

! convert to appropriate units
in_cutoff = in_cutoff * kim_api_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps", &
                                                    1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                             "kim_api_convert_to_act_unit", ier)
   goto 42
endif

in_epsilon = in_epsilon * kim_api_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps", &
                                                      0.0_cd, 1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                             "kim_api_convert_to_act_unit", ier)
   goto 42
endif

in_sigma = in_sigma * kim_api_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps", &
                                                  1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                             "kim_api_convert_to_act_unit", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_data", ier)
   goto 42
endif
call c_f_pointer(pcutoff, cutoff)
cutoff = in_cutoff

allocate( buf )

! setup buffer
! set value of parameters
buf%Pcutoff = in_cutoff
buf%cutsq   = in_cutoff**2
buf%epsilon = in_epsilon
buf%sigma   = in_sigma
call calc_phi(in_epsilon, &
              in_sigma,   &
              0.0_cd,     &
              in_cutoff,  &
              in_cutoff, energy_at_cutoff)
buf%shift   = -energy_at_cutoff

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
ier = kim_api_get_nbc_method(pkim, NBC_Method)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_get_nbc_method", ier)
   goto 42
endif
if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then
   buf%NBC        = 0
   buf%HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
   buf%NBC        = 1
   buf%HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
   buf%NBC        = 0
   buf%HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
   buf%NBC        = 1
   buf%HalfOrFull = 2
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
   buf%NBC        = 2
   buf%HalfOrFull = 1
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
   buf%NBC        = 2
   buf%HalfOrFull = 2
elseif (index(NBC_Method,"CLUSTER").eq.1) then
   buf%NBC        = 3
   buf%HalfOrFull = 1
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "Unknown NBC method", ier)
   goto 42
endif

! Determine neighbor list handling mode
!
if (buf%NBC.ne.3) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   buf%IterOrLoca = kim_api_get_neigh_mode(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  "kim_api_get_neigh_mode", ier)
      goto 42
   endif
   if (buf%IterOrLoca.ne.1 .and. buf%IterOrLoca.ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',buf%IterOrLoca
      idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                  error_message, ier)
      goto 42
   endif
else
   buf%IterOrLoca = 2   ! for CLUSTER NBC
endif

buf%model_index_shift = kim_api_get_model_index_shift(pkim)

call kim_api_getm_index(pkim, ier,                                           &
     "cutoff",                      buf%cutoff_ind,                       1, &
     "numberOfParticles",           buf%numberOfParticles_ind,            1, &
     "particleSpecies",             buf%particleSpecies_ind,              1, &
     "numberContributingParticles", buf%numberContributingParticles_ind,  1, &
     "coordinates",                 buf%coordinates_ind,                  1, &
     "get_neigh",                   buf%get_neigh_ind,                    1, &
     "boxSideLengths",              buf%boxSideLengths_ind,               1, &
     "energy",                      buf%energy_ind,                       1, &
     "forces",                      buf%forces_ind,                       1, &
     "particleEnergy",              buf%particleEnergy_ind,               1, &
     "process_dEdr",                buf%process_dEdr_ind,                 1, &
     "process_d2Edr2",              buf%process_d2Edr2_ind,               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_getm_index", ier)
   goto 42
endif
! end setup buffer

! store in model buffer
call kim_api_set_model_buffer(pkim, c_loc(buf), ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_set_model_buffer", ier)
   goto 42
endif

! set pointers to parameters in KIM object
call kim_api_setm_data(pkim, ier, &
     "PARAM_FREE_cutoff",  one, c_loc(buf%Pcutoff), 1, &
     "PARAM_FIXED_cutsq",  one, c_loc(buf%cutsq),   1, &
     "PARAM_FREE_epsilon", one, c_loc(buf%epsilon), 1, &
     "PARAM_FREE_sigma",   one, c_loc(buf%sigma),   1, &
     "PARAM_FIXED_shift",  one, c_loc(buf%shift),   1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "kim_api_setm_data", ier);
   goto 42
endif

ier = KIM_STATUS_OK
42 continue
model_driver_init = ier
return

end function model_driver_init
