!****************************************************************************
!**
!**  MODULE model_driver_P_<FILL model driver name>
!**
!**  <FILL model driver name> pair potential KIM Model Driver
!**
!**  Reference: <FILL>
!**
!**  Author: <FILL>
!**  Date  : <FILL>
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!****************************************************************************

#include "KIMstatus.h"

module model_driver_P_<FILL model driver name>

use KIMservice
implicit none

save
private
public Compute_Energy_Forces, &
       reinit,                &
       destroy,               &
       calc_phi

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
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
subroutine Compute_Energy_Forces(pkim,ier)
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in)  :: pkim
integer,                  intent(out) :: ier

!-- Local variables
double precision :: Rij(DIM)
double precision :: r,Rsqij,phi,dphi,dEidr
integer :: i,j,jj,numnei,atom_ret,comp_force,comp_enepot,comp_virial,comp_energy
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message
integer :: idum

!-- KIM variables
real*8 model_cutoff;     pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8 model_cutsq;      pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8 model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8 model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary
integer N;               pointer(pN,N)
real*8  energy;          pointer(penergy,energy)
real*8  coordum(DIM,1);  pointer(pcoor,coordum)
real*8  forcedum(DIM,1); pointer(pforce,forcedum)
real*8  enepotdum(1);    pointer(penepot,enepotdum)
real*8  boxlength(DIM);  pointer(pboxlength,boxlength)
real*8  Rij_list(DIM,1); pointer(pRij_list,Rij_list)
integer numContrib;      pointer(pnumContrib,numContrib)
integer nei1atom(1);     pointer(pnei1atom,nei1atom)
integer atomTypes(1);    pointer(patomTypes,atomTypes)
real*8  virial;          pointer(pvirial,virial)
character*64 NBC_Method; pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib

! Unpack the Model's parameters stored in the KIM API object
!
pmodel_cutoff = kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pmodel_cutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

! <Fill below as many parameters as needed> 

pmodel_<FILL parameter 1> = kim_api_get_data_f(pkim,"PARAM_FREE_<FILL parameter 1>",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pmodel_<FILL parameter 2> = kim_api_get_data_f(pkim,"PARAM_FREE_<FILL parameter 2>",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

! FILL: add PARAM_FIXED_* parameters here if there are any ...

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
   NBC = 0
   HalfOrFull = 1
elseif (index(NBC_Method,"MI-OPBC-H").eq.1) then
   NBC = 1
   HalfOrFull = 1
elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
   NBC = 1
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
   NBC = 2
   HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
   NBC = 2
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH-RVEC-F").eq.1) then
   NBC = 3
   HalfOrFull = 2
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, __FILE__, "Unknown NBC method", ier)
   return
endif
call free(pNBC_Method) ! don't forget to release the memory...

! Determine neighbor list handling mode
!
if (NBC.ne.0) then
   !*****************************
   !* IterOrLoca = 1 -- Iterator
   !*            = 2 -- Locator
   !*****************************
   IterOrLoca = kim_api_get_neigh_mode_f(pkim, ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_neigh_mode_f", ier)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      ier = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      idum = kim_api_report_error_f(__LINE__, __FILE__, error_message, ier)
      stop
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and virial
!
comp_energy = kim_api_isit_compute_f(pkim,"energy",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_force = kim_api_isit_compute_f(pkim,"forces",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_virial = kim_api_isit_compute_f(pkim,"virial",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_isit_compute_f", ier)
   return
endif

! Unpack data from KIM object
!
pN = kim_api_get_data_f(pkim,"numberOfAtoms",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

patomTypes = kim_api_get_data_f(pkim,"atomTypes",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pcoor = kim_api_get_data_f(pkim,"coordinates",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

if (HalfOrFull.eq.1) then
   pnumContrib = kim_api_get_data_f(pkim,"numberContributingAtoms",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   if (NBC.ne.0) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER case
      numberContrib = N
   endif
endif

if (NBC.eq.1) then
   pboxlength = kim_api_get_data_f(pkim,"boxlength",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
endif

if (comp_energy.eq.1) then
   penergy = kim_api_get_data_f(pkim,"energy",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
endif

if (comp_force.eq.1) then
   pforce  = kim_api_get_data_f(pkim,"forces",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor2d(forcedum,force,DIM,N)
endif

if (comp_enepot.eq.1) then
   penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N)
endif

if (comp_virial.eq.1) then
   pvirial = kim_api_get_data_f(pkim,"virial",ier)
   if (ier.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
      return
   endif
endif

call toRealArrayWithDescriptor2d(coordum,coor,DIM,N)

! Check to be sure that the atom types are correct
!
ier = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (atomTypes(i).ne.speccode) then
      idum = kim_api_report_error_f(__LINE__, __FILE__, "Unexpected species type detected", ier)
      return
   endif
enddo
ier = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) ene_pot(1:N) = 0.d0
if (comp_energy.eq.1) energy = 0.d0
if (comp_force.eq.1)  force(1:3,1:N) = 0.d0
if (comp_virial.eq.1) virial = 0.d0

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.0) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

! Initialize neighbor handling for Iterator mode
!
if (IterOrLoca.eq.1) then
   if (HalfOrFull.eq.1) then  ! HALF list
      ier = kim_api_get_half_neigh_f(pkim,0,0,atom_ret,numnei, &
                                     pnei1atom,pRij_list)
   else                       ! FULL list
      ier = kim_api_get_full_neigh_f(pkim,0,0,atom_ret,numnei, &
                                     pnei1atom,pRij_list)
   endif
   ! check for successful initialization
   if (ier.ne.KIM_STATUS_NEIGH_ITER_INIT_OK) then
      if (HalfOrFull.eq.1) then
         idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
      else
         idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
      endif
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
      if (HalfOrFull.eq.1) then ! HALF list
         ier = kim_api_get_half_neigh_f(pkim,0,1,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      else                      ! FULL list
         ier = kim_api_get_full_neigh_f(pkim,0,1,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
      if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit
                                ! incremented past the end of the list,
                                ! terminate loop
      if (ier.lt.KIM_STATUS_OK) then ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
         else
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
         endif
         return
      endif

      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (HalfOrFull.eq.1) then ! HALF list
         if (NBC.eq.0) then     ! CLUSTER NBC method
            numnei = N - i      ! number of neighbors in list i+1, ..., N
            nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
            ier = KIM_STATUS_OK
         else
            ier = kim_api_get_half_neigh_f(pkim,1,i,atom_ret,numnei, &
                                           pnei1atom,pRij_list)
         endif
      else                      ! FULL list
         ier = kim_api_get_full_neigh_f(pkim,1,i,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
      if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_half_neigh_f", ier)
         else
            idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_full_neigh_f", ier)
         endif
         ier = KIM_STATUS_FAIL
         return
      endif

   endif
      
   ! Loop over the neighbors of atom i
   !
   do jj = 1, numnei

      j = nei1atom(jj)                            ! get neighbor ID

      ! compute relative position vector
      !
      if (NBC.ne.3) then                          ! all methods except NEIGH-RVEC-F
         Rij(:) = coor(:,j) - coor(:,i)           ! distance vector between i j
      else
         Rij(:) = Rij_list(:,jj)
      endif

      ! apply periodic boundary conditions if required
      !
      if (NBC.eq.1) then
         where ( abs(Rij) .gt. 0.5d0*boxlength )  ! periodic boundary conditions
            Rij = Rij - sign(boxlength,Rij)       ! applied where needed.
         end where                                ! 
      endif

      ! compute energy and forces
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?

         r = sqrt(Rsqij)                          ! compute distance
         if (comp_force.eq.1.or.comp_virial.eq.1) then
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
         elseif (comp_energy.eq.1) then
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5d0*phi        !      add half v to total energy
            endif
         endif

         ! contribution to virial pressure
         !
         if (comp_virial.eq.1) then
            virial = virial + r*dEidr             !      virial=sum r*(dV/dr)
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

! Perform final tasks
!
if (comp_virial.eq.1) virial = - virial/DIM         ! definition of virial term
if (comp_enepot.eq.1 .and. comp_energy.eq.1) &
   energy = sum(ene_pot(1:N))                       ! compute total energy

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
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
double precision energy_at_cutoff
integer ier, idum

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

! Get FREE parameters from KIM object
!
pmodel_cutoff = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pmodel_<FILL parameter 1> = kim_api_get_data_f(pkim,"PARAM_FREE_<FILL parameter 1>",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

pmodel_<FILL parameter 2> = kim_api_get_data_f(pkim,"PARAM_FREE_<FILL parameter 2>",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif

! FILL as many parameters as needed

!
! Set new values in KIM object
!

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = model_cutoff

! store squared cutoff radius in KIM object
pmodel_cutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif
model_cutsq = model_cutoff**2

! FILL: store any other FIXED parameters whose values depend on FREE parameters

end subroutine reinit

!-------------------------------------------------------------------------------
!
! Model driver destroy routine
!
!-------------------------------------------------------------------------------
subroutine destroy(pkim)
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer ier, idum

!-- KIM variables
real*8  cutoff;          pointer(pcutoff,cutoff)
real*8  model_cutoff;    pointer(pmodel_cutoff, model_cutoff)  ! cutoff radius
real*8  model_cutsq;     pointer(pmodel_cutsq,  model_cutsq)   ! cutoff radius squared
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

! Get all parameters added to KIM object and free memory 
!
pmodel_cutoff = kim_api_get_data_f(pkim,"PARAM_FREE_cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif
call free(pmodel_cutoff)

pmodel_cutsq = kim_api_get_data_f(pkim,"PARAM_FIXED_cutsq",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif
call free(pmodel_cutsq)

pmodel_<FILL parameter 1> = kim_api_get_data_f(pkim,"PARAM_FREE_<FILL parameter 1>",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
   return
endif
call free(pmodel_<FILL parameter 1>)

! FILL: repeat above statements as many times as necessary for all FREE and
! FIXED parameters.

end subroutine destroy

end module model_driver_P_<FILL model driver name>

!-------------------------------------------------------------------------------
!
! Model driver initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_driver_P_<FILL model driver name>_init(pkim, byte_paramfile, len_paramfile)
use model_driver_P_<FILL model driver name>
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim
byte,                     intent(in) :: byte_paramfile(len_paramfile+1)
integer,                  intent(in) :: len_paramfile

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
character(len=len_paramfile) paramfile
integer i,ier, idum
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
real*8  model_<FILL parameter 1>; pointer(pmodel_<FILL parameter 1>,model_<FILL parameter 1>)
real*8  model_<FILL parameter 2>; pointer(pmodel_<FILL parameter 2>,model_<FILL parameter 2>)
! FILL as many parameter declarations as necessary

! store pointer to compute function in KIM object
ier = kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
   stop
endif

! store pointer to reinit function in KIM object
ier = kim_api_set_data_f(pkim,"reinit",one,loc(reinit))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
   stop
endif

ier = kim_api_set_data_f(pkim,"destroy",one,loc(destroy))
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier)
   stop
endif

! Read in model parameters from parameter file
!
do i=1,len_paramfile
   paramfile(i:i) = char(byte_paramfile(i))
enddo
read(paramfile,'<FILL appropriate format>',iostat=ier,err=100) in_cutoff,       &
                                                               in_<FILL parameter 1>, &
                                                               in_<FILL parameter 2>, &
                                                               ! ...
                                                               in_<FILL last parameter>
goto 200
100 continue
! reading parameters failed
idum = kim_api_report_error_f(__LINE__, __FILE__, "Unable to read <FILL model driver name> parameters, ier = ", KIM_STATUS_FAIL)
stop

200 continue

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data", ier)
   stop
endif
cutoff = in_cutoff

! allocate memory for parameter cutoff and store value
pmodel_cutoff = malloc(one*8) ! 8 is the size of double precision number
ier = kim_api_set_data_f(pkim,"PARAM_FREE_cutoff",one,pmodel_cutoff)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier);
   stop
endif
model_cutoff = in_cutoff ! Initialize

! allocate memory for parameter cutsq and store value
pmodel_cutsq = malloc(one*8) ! 8 is the size of double precision number
ier = kim_api_set_data_f(pkim,"PARAM_FIXED_cutsq",one,pmodel_cutsq)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier);
   stop
endif
model_cutsq = in_cutoff**2 ! Initialize

! allocate memory for parameter <FILL parameter 1> and store value
pmodel_<FILL parameter 1> = malloc(one*8) ! 8 is the size of double precision number
ier = kim_api_set_data_f(pkim,"PARAM_FREE_<FILL parameter 1>",one,pmodel_<FILL parameter 1>)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data", ier);
   stop
endif
model_<FILL parameter 1> = in_<FILL parameter 1>

! FILL: repeat above statements as many times as necessary for all parameters. 
! Use "FREE" and "FIXED" as appropriate. (Recall FREE parameters can be modified by
! the calling routine. FIXED parameters depend on the FREE parameters and must be
! appropriately adjusted in the reinit() routine.)

end subroutine model_driver_P_<FILL model driver name>_init


