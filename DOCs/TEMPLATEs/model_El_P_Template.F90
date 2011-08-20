!****************************************************************************
!**
!**  MODULE model_<FILL element name>_P_<FILL model name>
!**
!**  <FILL model name> pair potential model for <FILL element name>
!**
!**  Reference: <FILL>
!**
!**  Author: <FILL>
!**  Date  : <FILL>
!**
!**  Language: Fortran 90
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!****************************************************************************

module model_<FILL element name>_P_<FILL model name>

use KIMservice
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff,          &
       report_error

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code
double precision, parameter :: model_cutoff  = <FILL cutoff radius> ! cutoff radius
                                                                    ! in angstroms
double precision, parameter :: model_cutsq   = model_cutoff**2

!-------------------------------------------------------------------------------
! Below are the definitions and values of all additional model parameters
!
! Recall that the Fortran 90 format for declaring parameters is as follows:
!
! integer, parameter :: parname = value          ! This defines an integer
!                                                ! parameter called `parname' with a
!                                                ! value equal to `value' (a number)
!
! double precision, parameter :: parname = value ! This defines a real double precision
!                                                ! parameter called `parname' with a
!                                                ! value equal to `value' (a number)
!-------------------------------------------------------------------------------
double precision, parameter :: <FILL parameter name> = <FILL parameter value>

contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi)
implicit none
   
!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: phi

!-- Local variables
! <FILL place any local variable definitions here>

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
subroutine calc_phi_dphi(r,phi,dphi)
implicit none
   
!-- Transferred variables
double precision, intent(in)  :: r
double precision, intent(out) :: phi,dphi

!-- Local variables
! <FILL place any local variable definitions here>

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
double precision :: r,Rsqij,phi,dphi
integer :: i,j,jj,numnei,atom_ret,comp_force,comp_enepot,comp_virial
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer(kind=8) N;       pointer(pN,N)
real*8  energy;          pointer(penergy,energy)
real*8  coordum(DIM,1);  pointer(pcoor,coordum)
real*8  forcedum(DIM,1); pointer(pforce,forcedum)
real*8  enepotdum(1);    pointer(penepot,enepotdum)
real*8  boxlength(DIM);  pointer(pboxlength,boxlength)
real*8  Rij_list(DIM,1); pointer(pRij_list,Rij_list)
integer nei1atom(1);     pointer(pnei1atom,nei1atom)
integer atomTypes(1);    pointer(patomTypes,atomTypes)
real*8  virial;          pointer(pvirial,virial)
character*64 NBC_Method; pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer N4

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_nbc_method_f", ier)
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
   ier = 0
   call report_error(__LINE__, "Unknown NBC method", ier)
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
   if (ier.le.0) then
      call report_error(__LINE__, "kim_api_get_neigh_mode_f", ier)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      ier = 0
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      call report_error(__LINE__, error_message, ier)
      stop
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! and virial
!
comp_force = kim_api_isit_compute_f(pkim,"forces",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_enepot = kim_api_isit_compute_f(pkim,"energyPerAtom",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_isit_compute_f", ier)
   return
endif

comp_virial = kim_api_isit_compute_f(pkim,"virial",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_isit_compute_f", ier)
   return
endif

! Unpack data from KIM object
!
pN = kim_api_get_data_f(pkim,"numberOfAtoms",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_data_f", ier)
   return
endif

N4=N  ! place N in integer*4 variable which is the expected
      ! type for some methods

patomTypes = kim_api_get_data_f(pkim,"atomTypes",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_data_f", ier)
   return
endif

penergy = kim_api_get_data_f(pkim,"energy",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_data_f", ier)
   return
endif

pcoor = kim_api_get_data_f(pkim,"coordinates",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_data_f", ier)
   return
endif

if (NBC.eq.1) then
   pboxlength = kim_api_get_data_f(pkim,"boxlength",ier)
   if (ier.le.0) then
      call report_error(__LINE__, "kim_api_get_data_f", ier)
      return
   endif
endif

if (comp_force.eq.1) then
   pforce  = kim_api_get_data_f(pkim,"forces",ier)
   if (ier.le.0) then
      call report_error(__LINE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor2d(forcedum,force,DIM,N4)
endif

if (comp_enepot.eq.1) then
   penepot = kim_api_get_data_f(pkim,"energyPerAtom",ier)
   if (ier.le.0) then
      call report_error(__LINE__, "kim_api_get_data_f", ier)
      return
   endif
   call toRealArrayWithDescriptor1d(enepotdum,ene_pot,N4)
endif

if (comp_virial.eq.1) then
   pvirial = kim_api_get_data_f(pkim,"virial",ier)
   if (ier.le.0) then
      call report_error(__LINE__, "kim_api_get_data_f", ier)
      return
   endif
endif
call toRealArrayWithDescriptor2d(coordum,coor,DIM,N4)

! Check to be sure that the atom types are correct
!
ier = 0 ! assume an error
do i = 1,N
   if (atomTypes(i).ne.speccode) then
      call report_error(__LINE__, "Unexpected species type detected", i)
      return
   endif
enddo
ier = 1 ! everything is ok

! Initialize potential energies, forces, virial term
!
if (comp_enepot.eq.1) then
   ene_pot(1:N) = 0.d0
else
   energy = 0.d0
endif
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
   if (ier.ne.2) then   ! ier=2 upon successful initialization
      if (HalfOrFull.eq.1) then
         call report_error(__LINE__, "kim_api_get_half_neigh_f", ier)
      else
         call report_error(__LINE__, "kim_api_get_full_neigh_f", ier)
      endif
      ier = 0
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
      if (ier.lt.0) then     ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            call report_error(__LINE__, "kim_api_get_half_neigh_f", ier)
         else
            call report_error(__LINE__, "kim_api_get_full_neigh_f", ier)
         endif
         return
      endif
      if (ier.eq.0) exit     ! ier=0 means that the iterator has been
                             ! incremented past the end of the list,
                             ! terminate loop
      i = atom_ret

   else                         ! LOCATOR mode
      i = i + 1
      if (i.gt.N) exit          ! incremented past end of list,
                                ! terminate loop
      if (HalfOrFull.eq.1) then ! HALF list
         if (NBC.eq.0) then     ! CLUSTER NBC method
            numnei = N - i      ! number of neighbors in list i+1, ..., N
            nei1atom(1:numnei) = (/ (i+jj, jj = 1,numnei) /)
            ier = 1
         else
            ier = kim_api_get_half_neigh_f(pkim,1,i,atom_ret,numnei, &
                                           pnei1atom,pRij_list)
         endif
      else                      ! FULL list
         ier = kim_api_get_full_neigh_f(pkim,1,i,atom_ret,numnei, &
                                        pnei1atom,pRij_list)
      endif
      if (ier.ne.1) then ! some sort of problem, exit
         if (HalfOrFull.eq.1) then
            call report_error(__LINE__, "kim_api_get_half_neigh_f", ier)
         else
            call report_error(__LINE__, "kim_api_get_full_neigh_f", ier)
         endif
         ier = 0
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
            call calc_phi_dphi(r,phi,dphi)        ! compute pair potential
                                                  !   and it derivative
         else
            call calc_phi(r,phi)                  ! compute just pair potential
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + 0.5d0*phi   ! accumulate energy
            if (HalfOrFull.eq.1) &                ! HALF mode
               ene_pot(j) = ene_pot(j) + 0.5d0*phi! (i and j share it)
         else
            if (HalfOrFull.eq.1) then             ! HALF mode
               energy = energy + phi              !      add v to total energy
            else                                  ! FULL mode
               energy = energy + 0.5d0*phi        !      add half v to total energy
            endif
         endif

         ! contribution to virial pressure
         !
         if (comp_virial.eq.1) then
            if (HalfOrFull.eq.1) then             ! HALF mode
               virial = virial + r*dphi           !      virial=sum r*(dphi/dr)
            else                                  ! FULL mode
               virial = virial + 0.5d0*r*dphi     !      virial=sum 0.5*r*(dphi/dr)
            endif
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then
            force(:,i) = force(:,i) + dphi*Rij/r    ! accumulate force on atom i
            if (HalfOrFull.eq.1) &                  ! HALF mode
               force(:,j) = force(:,j) - dphi*Rij/r !    (Fji = -Fij)
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Perform final tasks
!
if (comp_virial.eq.1) virial = - virial/DIM         ! definition of virial term
if (comp_enepot.eq.1) energy = sum(ene_pot(1:N))    ! compute total energy

! Free temporary storage
!
if (NBC.eq.0) deallocate( nei1atom_substitute )

! Everything is great
!
ier = 1
return

end subroutine Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! error reporting routine
!
!-------------------------------------------------------------------------------
subroutine report_error(line, str, status)
implicit none

!-- Transferred variables
integer,          intent(in) :: line
character(len=*), intent(in) :: str
integer,          intent(in) :: status

!-- Local variables
character(len=10000), parameter :: file = __FILE__

!-- print the error message
print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status

end subroutine report_error

end module model_<FILL element name>_P_<FILL model name>

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
subroutine model_<FILL element name>_P_<FILL model name>_init(pkim)
use model_<FILL element name>_P_<FILL model name>
use KIMservice
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier

!-- KIM variables
real*8 cutoff; pointer(pcutoff,cutoff)

! store pointer to compute function in KIM object
if (kim_api_set_data_f(pkim,"compute",one,loc(Compute_Energy_Forces)).ne.1) then
   call report_error(__LINE__, "kim_api_set_data", ier)
   stop
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.le.0) then
   call report_error(__LINE__, "kim_api_get_data", ier)
   stop
endif
cutoff = model_cutoff

end subroutine model_<FILL element name>_P_<FILL model name>_init

