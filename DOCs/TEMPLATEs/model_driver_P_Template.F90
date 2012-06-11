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
! Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!    Ellad B. Tadmor
!    Valeriu Smirichinski
!

!****************************************************************************
!**
!**  MODULE model_driver_P_<FILL model driver name>
!**
!**  <FILL model driver name> pair potential KIM Model Driver
!**
!**  Reference: <FILL>
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module model_driver_P_<FILL model driver name>

use KIM_API
implicit none

save
private
public Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi,              &
       calc_phi_dphi,         &
       calc_phi_dphi_d2phi

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(model_<FILL parameter 1>,  &
                    model_<FILL parameter 2>,  &
                    ! FILL as many parameters as needed
                    model_cutoff,r,phi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_<FILL parameter 1>
double precision, intent(in)  :: model_<FILL parameter 2>
! FILL as many parameter declarations as necessary
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi

!-- Local variables
! FILL: place any local variable definitions here

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else
   phi = !<FILL functional form of phi(r)>
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi(model_<FILL parameter 1>,  &
                         model_<FILL parameter 2>,  &
                         ! FILL as many parameters as needed
                         model_cutoff,r,phi,dphi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_<FILL parameter 1>
double precision, intent(in)  :: model_<FILL parameter 2>
! FILL as many parameter declarations as necessary
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
! FILL: place any local variable definitions here

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else
   phi  = !<FILL functional form of phi(r)>
   dphi = !<FILL functional form of dphi(r)>
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivatives dphi(r) and d2phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi_d2phi(model_<FILL parameter 1>,  &
                               model_<FILL parameter 2>,  &
                               ! FILL as many parameters as needed
                               model_cutoff,r,phi,dphi,d2phi)
implicit none

!-- Transferred variables
double precision, intent(in)  :: model_<FILL parameter 1>
double precision, intent(in)  :: model_<FILL parameter 2>
! FILL as many parameter declarations as necessary
double precision, intent(in)  :: model_cutoff
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi,d2phi

!-- Local variables
! FILL: place any local variable definitions here

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
   d2phi  = 0.d0
else
   phi   = !<FILL functional form of phi(r)>
   dphi  = !<FILL functional form of dphi(r)>
   d2phi = !<FILL functional form of d2phi(r)>
endif

end subroutine calc_phi_dphi_d2phi

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
subroutine Compute_Energy_Forces(pkim,ier)
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in)  :: pkim
integer,                  intent(out) :: ier

!-- Local variables
double precision :: Rij(DIM),Rij_pairs(DIM,2)
double precision :: r,Rsqij,phi,dphi,d2phi,dEidr,d2Eidr
double precision :: r_pairs(2)
integer :: i,j,jj,numnei,atom_ret
integer :: i_pairs(2), j_pairs(2)
integer :: comp_force,comp_energy,comp_enepot,comp_process_dEdr,comp_process_d2Edr2
integer, allocatable, target :: nei1atom_substitute(:)
integer :: idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
integer(kind=kim_intptr) bufparam(1); pointer(pbufparam, bufparam)

!-- KIM variables
real*8  model_cutoff;         pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;          pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary
integer N;                    pointer(pN,N)
real*8  energy;               pointer(penergy,energy)
real*8  coordum(DIM,1);       pointer(pcoor,coordum)
real*8  forcedum(DIM,1);      pointer(pforce,forcedum)
real*8  enepotdum(1);         pointer(penepot,enepotdum)
real*8  boxSideLengths(DIM);  pointer(pboxSideLengths,boxSideLengths)
real*8  Rij_list(DIM,1);      pointer(pRij_list,Rij_list)
integer numContrib;           pointer(pnumContrib,numContrib)
integer nei1atom(1);          pointer(pnei1atom,nei1atom)
integer particleTypes(1);     pointer(pparticleTypes,particleTypes)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer energy_ind
integer forces_ind
integer particleEnergy_ind
integer process_dEdr_ind
integer process_d2Edr2_ind
integer model_index_shift
integer numberOfParticles_ind
integer particleTypes_ind
integer coordinates_ind
integer numberContributingParticles_ind
integer boxSideLengths_ind
integer get_neigh_ind
integer cutoff_ind


! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_model_buffer_f", ier)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

! Unpack indices from buffer
!
NBC                             = bufind(1)
HalfOrFull                      = bufind(2)
IterOrLoca                      = bufind(3)
energy_ind                      = bufind(4)
forces_ind                      = bufind(5)
particleEnergy_ind              = bufind(6)
process_dEdr_ind                = bufind(7)
process_d2Edr2_ind              = bufind(8)
model_index_shift               = bufind(9)
numberOfParticles_ind           = bufind(10)
particleTypes_ind               = bufind(11)
coordinates_ind                 = bufind(12)
numberContributingParticles_ind = bufind(13)
boxSideLengths_ind              = bufind(14)
get_neigh_ind                   = bufind(15)
cutoff_ind                      = bufind(16)

! Unpack Model's parameters from buffer
!
pmodel_cutsq   = bufparam(2)
pmodel_<FILL parameter 1> = bufparam(3)
pmodel_<FILL parameter 2> = bufparam(4)
! FILL as many parameters as necessary

! Unpack the Model's cutoff stored in the KIM API object
!
pmodel_cutoff = kim_api_get_data_by_index_f(pkim, cutoff_ind, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_by_index_f", ier)
   return
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and d1Edr
!
call kim_api_getm_compute_by_index_f(pkim, ier, &
     energy_ind,         comp_energy,         1, &
     forces_ind,         comp_force,          1, &
     particleEnergy_ind, comp_enepot,         1, &
     process_dEdr_ind,   comp_process_dEdr,   1, &
     process_d2Edr2_ind, comp_process_d2Edr2, 1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_compute_by_index_f", ier)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data_by_index_f(pkim, ier, &
     numberOfParticles_ind,           pN,              1,                           &
     particleTypes_ind,               pparticleTypes,  1,                           &
     coordinates_ind,                 pcoor,           1,                           &
     numberContributingParticles_ind, pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
     boxSideLengths_ind,              pboxSideLengths, TRUEFALSE(NBC.eq.1),         &
     energy_ind,                      penergy,         TRUEFALSE(comp_energy.eq.1), &
     forces_ind,                      pforce,          TRUEFALSE(comp_force.eq.1),  &
     particleEnergy_ind,              penepot,         TRUEFALSE(comp_enepot.eq.1))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_by_index_f", ier)
   return
endif

call KIM_to_F90_real_array_2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call KIM_to_F90_real_array_2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call KIM_to_F90_real_array_1d(enepotdum,ene_pot,N)
if (HalfOrFull.eq.1) then
   if (NBC.ne.0) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif

! Check to be sure that the atom types are correct
!
ier = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (particleTypes(i).ne.speccode) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "Unexpected species type detected", ier)
      return
   endif
enddo
ier = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces
!
if (comp_enepot.eq.1) ene_pot(1:N) = 0.d0
if (comp_energy.eq.1) energy = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.0) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

! Initialize neighbor handling for Iterator mode
!
if (IterOrLoca.eq.1) then
   ier = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   ! check for successful initialization
   if (ier.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_f", ier)
      ier = KIM_STATUS_FAIL
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

   ! Set up neighbor list for next atom for all NBC methods
   !
   if (IterOrLoca.eq.1) then    ! ITERATOR mode
      ier = kim_api_get_neigh_f(pkim,0,1,atom_ret,numnei,pnei1atom,pRij_list)
      if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                                ! incremented past the end of the list,
                                ! terminate loop
      if (ier.lt.KIM_STATUS_OK) then ! some sort of problem, exit
         idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_f", ier)
         return
      endif

      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (NBC.eq.0) then        ! CLUSTER NBC method
         numnei = N - i         ! number of neighbors in list i+1, ..., N
         nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
         ier = KIM_STATUS_OK
      else                      ! All other NBCs
         ier = kim_api_get_neigh_f(pkim,1,i,atom_ret,numnei,pnei1atom,pRij_list)
         if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_f", ier)
            ier = KIM_STATUS_FAIL
            return
         endif
      endif
   endif

   ! Loop over the neighbors of atom i
   !
   do jj = 1, numnei

      j = nei1atom(jj)                            ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.3) then                          ! all methods except NEIGH_RVEC_F
         Rij(:) = coor(:,j) - coor(:,i)           ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.1) then
         where ( abs(Rij) .gt. 0.5d0*boxSideLengths )  ! periodic boundary conditions
            Rij = Rij - sign(boxSideLengths,Rij)       ! applied where needed.
         end where                                !
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_process_d2Edr2.eq.1) then
            call calc_phi_dphi_d2phi(model_<FILL parameter 1>, &
                                     model_<FILL parameter 2>, &
                                     ! FILL as many parameters as needed
                                     r,phi,dphi,d2phi) ! compute pair potential
                                                       !   and it derivatives
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr  = dphi                      !      double contribution
               d2Eidr = d2phi
            else                                  ! FULL mode
               dEidr  = 0.5d0*dphi                !      regular contribution
               d2Eidr = 0.5d0*d2phi
            endif
         elseif (comp_force.eq.1.or.comp_process_dEdr.eq.1) then
            call calc_phi_dphi(model_<FILL parameter 1>, &
                               model_<FILL parameter 2>, &
                               ! FILL as many parameters as needed
                               model_cutoff,             &
                               r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative

            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dEidr = dphi                       !      double contribution
            else                                  ! FULL mode
               dEidr = 0.5d0*dphi                 !      regular contribution
            endif
         else
            call calc_phi(model_<FILL parameter 1>, &
                          model_<FILL parameter 2>, &
                          ! FILL as many parameters as needed
                          model_cutoff,             &
                          r,phi)                  ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + 0.5d0*phi   ! accumulate energy
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) &         ! HALF mode
               ene_pot(j) = ene_pot(j) + 0.5d0*phi! (i and j share it)
         endif
         if (comp_energy.eq.1) then
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5d0*phi        !      add half v to total energy
            endif
         endif

         ! contribution to process_dEdr
         !
         if (comp_process_dEdr.eq.1) then
            call kim_api_process_dEdr_f(pkim, dEidr, r, loc(Rij), i, j, ier)
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

            call kim_api_process_d2Edr2_f(pkim, d2Eidr, loc(r_pairs), loc(Rij_pairs), &
                                         loc(i_pairs), loc(j_pairs), ier)
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then
            force(:,i) = force(:,i) + dEidr*Rij/r ! accumulate force on atom i
            force(:,j) = force(:,j) - dEidr*Rij/r ! accumulate force on atom j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
if (NBC.eq.0) deallocate( nei1atom_substitute )

! Everything is great
!
ier = KIM_STATUS_OK
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Model driver reinitialization routine
!
!-------------------------------------------------------------------------------
subroutine reinit(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
double precision energy_at_cutoff
integer ier, idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
integer(kind=kim_intptr) bufparam(1); pointer(pbufparam, bufparam)

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_model_buffer_f", ier)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

pmodel_cutoff  = bufparam(1)
pmodel_cutsq   = bufparam(2)
pmodel_<FILL parameter 1> = bufparam(3)
pmodel_<FILL parameter 2> = bufparam(4)
! FILL as many parameters as necessary

pcutoff = kim_api_get_data_by_index_f(pkim, bufind(16), ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_by_index_f", ier)
   return
endif

!
! Set new values in KIM object
!
cutoff      = model_cutoff
model_cutsq = model_cutoff**2
! FILL: store any other FIXED parameters whose values depend on FREE parameters

end subroutine reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
subroutine destroy(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer ier, idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
integer(kind=kim_intptr) bufparam(1); pointer(pbufparam, bufparam)

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

! get model buffer from KIM object
pbuffer = kim_api_get_model_buffer_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_model_buffer_f", ier)
   return
endif
pbufind   = buffer(1)
pbufparam = buffer(2)

call free(bufparam(1))
call free(bufparam(2))
! FILL free as many parameters as necessary

call free(pbufparam)
call free(pbufind)

call free(pbuffer)

end subroutine destroy

end module model_driver_P_<FILL model driver name>

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_driver_P_<FILL model driver name>_init(pkim, byte_paramfile, nmstrlen, numparamfiles)
use model_driver_P_<FILL model driver name>
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim
integer,                  intent(in) :: nmstrlen
integer,                  intent(in) :: numparamfiles
byte,                     intent(in) :: byte_paramfile(nmstrlen*numparamfiles)

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
character(len=nmstrlen) paramfile_names(numparamfiles)
integer i,j,ier, idum
integer(kind=kim_intptr):: buffer(1); pointer(pbuffer, buffer)
integer bufind(1);                    pointer(pbufind, bufind)
integer(kind=kim_intptr) bufparam(1); pointer(pbufparam, bufparam)
character*80 :: error_message
! define variables for all model parameters to be read in
double precision in_cutoff
double precision in_<FILL parameter 1>
double precision in_<FILL parameter 2>
! ...
double precision in_<FILL last parameter>

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
character*64 NBC_Method; pointer(pNBC_Method,NBC_Method)
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

!
! generic code to process model parameter file names from byte string
!
do i=1,numparamfiles
   paramfile_names(i) = "" ! initialize name to empty string
   do j=1,nmstrlen
      ! add characters to file name until a NULL is encountered
      if (char(byte_paramfile((i-1)*nmstrlen+j)) .eq. char(0)) exit
      paramfile_names(i)(j:j) = char(byte_paramfile((i-1)*nmstrlen+j))
   enddo
enddo
!
! end generic code to process model parameter file names
!

! store function pointers in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "compute", one, loc(Compute_Energy_Forces), 1, &
     "reinit",  one, loc(reinit),                1, &
     "destroy", one, loc(destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_setm_data_f", ier)
   stop
endif

! Read in model parameters from parameter file
!
open(10,file=paramfile_names(1),status="old")
read(10,*,iostat=ier,err=100) in_cutoff
read(10,*,iostat=ier,err=100) in_<FILL parameter 1>
read(10,*,iostat=ier,err=100) in_<FILL parameter 2>
! ...
read(10,*,iostat=ier,err=100) in_<FILL last parameter>
close(10)

goto 200
100 continue
! reading parameters failed
idum = kim_api_report_error_f(__LINE__, __FILE__, "Unable to read <FILL model driver name> parameters, kimerror = ", KIM_STATUS_FAIL)
stop

200 continue

! convert to appropriate units
in_cutoff = in_cutoff * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "fs", &
                                                    1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_to_act_unit_f", ier)
   stop
endif
<FILL parameter 1> = <FILL parameter 1> * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "fs", &
                                                                      <FILL exponents (5) for parameter 1>)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_to_act_unit_f", ier)
   stop
endif
<FILL parameter 2> = <FILL parameter 2> * kim_api_convert_to_act_unit_f(pkim, "A", "eV", "e", "K", "fs", &
                                                                      <FILL exponents (5) for parameter 2>)
if (ier.lt.KIM_STATUS_OK) then
   idum=kim_api_report_error_f(__LINE__, __FILE__, "kim_api_convert_to_act_unit_f", ier)
   stop
endif

! FILL as many parameter unit conversions as needed

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = in_cutoff

! allocate memory for parameters
pmodel_cutoff = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_cutoff.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_cutsq = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_cutsq.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_<FILL parameter 1> = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_<FILL parameter 1>.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
pmodel_<FILL parameter 2> = malloc(one*8) ! 8 is the size of double precision number
if (pmodel_<FILL parameter 2>.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
! FILL: repeat above statements as many times as necessary for all parameters.
! Use "FREE" and "FIXED" as appropriate. (Recall FREE parameters can be modified by
! the calling routine. FIXED parameters depend on the FREE parameters and must be
! appropriately adjusted in the reinit() routine.)

! store values in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "PARAM_FREE_cutoff",  one, pmodel_cutoff,             1, &
     "PARAM_FIXED_cutsq",  one, pmodel_cutsq,              1, &
     "<FILL parameter 1>", one, pmodel_<FILL parameter 1>, 1, &
     "<FILL parameter 2>", one, pmodel_<FILL parameter 2>, 1, &
     ! FILL as many parameters as necessary
     )
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_setm_data_f", ier);
   stop
endif

! set value of parameters
model_cutoff  = in_cutoff
model_cutsq   = in_cutoff**2
model_<FILL parameter 1> = in_<FILL parameter 1>
model_<FILL parameter 2> = in_<FILL parameter 2>
! FILL as many parameters as necessary

! allocate buffer
pbuffer   = malloc(2*kim_intptr)
!
! bufind values
!     1 - NBC
!     2 - HalfOrFull
!     3 - IterOrLoca
!     4 - energy_ind
!     5 - forces_ind
!     6 - particleEnergy_ind
!     7 - process_dEdr_ind
!     8 - process_d2Edr2_ind
!     9 - model_index_shift
!    10 - numberOfParticles_ind
!    11 - particleTypes_ind
!    12 - coordinates_ind
!    13 - numberContributingParticles_ind
!    14 - boxSideLengths_ind
!    15 - get_neigh_ind
!    16 - cutoff_ind
pbufind   = malloc(16*4)
!
! bufparam values
!     1 - Pcutoff
!     2 - cutsq
!     3 - <FILL parameter 1>
!     4 - <FILL parameter 2>
!     <FILL as many parameters as necessary>
pbufparam = malloc(<FILL number of parameters>*kim_intptr)
if (pbuffer.eq.0 .or. pbufind .eq. 0 .or. pbufparam.eq.0) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
   stop
endif
! set pointers
buffer(1) = pbufind
buffer(2) = pbufparam

! setup buffer
!
! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_nbc_method_f", ier)
   return
endif
if (index(NBC_Method,"CLUSTER").eq.1) then
   bufind(1) = 0
   bufind(2) = 1
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
   bufind(1) = 1
   bufind(2) = 1
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
   bufind(1) = 1
   bufind(2) = 2
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
   bufind(1) = 2
   bufind(2) = 1
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
   bufind(1) = 2
   bufind(2) = 2
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
   bufind(1) = 3
   bufind(2) = 2
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, __FILE__, "Unknown NBC method", ier)
   return
endif
call free(pNBC_Method) ! don't forget to release the memory...

! Determine neighbor list handling mode
!
if (bufind(1).ne.0) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   bufind(3) = kim_api_get_neigh_mode_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_mode_f", ier)
      return
   endif
   if (bufind(3).ne.1 .and. bufind(3).ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',bufind(3)
      idum = kim_api_report_error_f(__LINE__, __FILE__, error_message, ier)
      stop
   endif
else
   bufind(3) = 2   ! for CLUSTER NBC
endif

bufind(9) = kim_api_get_model_index_shift_f(pkim)

call kim_api_getm_index_f(pkim, ier, &
     "cutoff",                      bufind(16),  1, &
     "numberOfParticles",           bufind(10),  1, &
     "particleTypes",               bufind(11),  1, &
     "numberContributingParticles", bufind(13),  1, &
     "coordinates",                 bufind(12),  1, &
     "get_neigh",                   bufind(15),  1, &
     "boxSideLengths",              bufind(14),  1, &
     "energy",                      bufind(4),   1, &
     "forces",                      bufind(5),   1, &
     "particleEnergy",              bufind(6),   1, &
     "process_dEdr",                bufind(7),   1, &
     "process_d2Edr2",              bufind(8),   1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_index_f", ier)
   return
endif

! store in model buffer
call kim_api_set_model_buffer_f(pkim, pbuffer, ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_model_buffer_f", ier)
   return
endif

! store parameters in buffer
bufparam(1) = pmodel_cutoff
bufparam(2) = pmodel_cutsq
bufparam(3) = pmodel_<FILL parameter 1>
bufparam(4) = pmodel_<FILL parameter 2>
! FILL as many parameters as necessary

end subroutine model_driver_P_<FILL model driver name>_init
