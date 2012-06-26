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
!**  MODULE ex_model_Al_PF_ErcolessiAdams
!**
!**  Ercolessi-Adams pair functional model for Al
!**
!**  Reference: F. Ercolessi and J. B. Adams, Europhys. Lett. 26, 583 (1994).
!**             http://www.sissa.it/furio/potentials/Al/
!**
!**  Language: Fortran 90
!**
!**  Release: This file is part of the openkim-api.git repository.
!**
!****************************************************************************


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

module ex_model_Al_PF_ErcolessiAdams

use KIM_API
implicit none

save
private
public Compute_Energy_Forces, &
       model_cutoff,          &
       Destroy

! Below are the definitions and values of all Model parameters
integer, parameter          :: DIM=3          ! dimensionality of space
integer, parameter          :: speccode = 1   ! internal species code
double precision, parameter :: model_cutoff  = 5.55805441821810d0 ! cutoff radius
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
integer, parameter :: npt= 17
integer, parameter :: nuu= 13
double precision, parameter :: rhomin = .000000000000000D+00

! pair potential data
!
double precision, parameter :: x(npt) =   (/  .202111069753385D+01, &
                                              .227374953472558D+01, &
                                              .252638837191732D+01, &
                                              .277902720910905D+01, &
                                              .303166604630078D+01, &
                                              .328430488349251D+01, &
                                              .353694372068424D+01, &
                                              .378958255787597D+01, &
                                              .404222139506771D+01, &
                                              .429486023225944D+01, &
                                              .454749906945117D+01, &
                                              .480013790664290D+01, &
                                              .505277674383463D+01, &
                                              .530541558102636D+01, &
                                              .555805441821810D+01, &
                                              .555807968210182D+01, &
                                              .555810494598553D+01 /)

double precision, parameter :: yv2(npt) = (/  .196016472197158D+01, &
                                              .682724240745344D+00, &
                                              .147370824539188D+00, &
                                             -.188188235860390D-01, &
                                             -.576011902692490D-01, &
                                             -.519846499644276D-01, &
                                             -.376352484845919D-01, &
                                             -.373737879689433D-01, &
                                             -.531351030124350D-01, &
                                             -.632864983555742D-01, &
                                             -.548103623840369D-01, &
                                             -.372889232343935D-01, &
                                             -.188876517630154D-01, &
                                             -.585239362533525D-02, &
                                              .000000000000000D+00, &
                                              .000000000000000D+00, &
                                              .000000000000000D+00 /)

double precision, parameter :: Bv2(npt) = (/ -.702739315585347D+01, &
                                             -.333140549270729D+01, &
                                             -.117329394261502D+01, &
                                             -.306003283486901D+00, &
                                             -.366656699104026D-01, &
                                              .588330899204400D-01, &
                                              .384220572312032D-01, &
                                             -.390223173707191D-01, &
                                             -.663882722510521D-01, &
                                             -.312918894386669D-02, &
                                              .590118945294245D-01, &
                                              .757939459148246D-01, &
                                              .643822548468606D-01, &
                                              .399750987463792D-01, &
                                              .177103852679117D-05, &
                                             -.590423369301474D-06, &
                                              .590654950414731D-06 /)

double precision, parameter :: Cv2(npt) = (/  .877545959718548D+01, &
                                              .585407125495837D+01, &
                                              .268820820643116D+01, &
                                              .744718689404422D+00, &
                                              .321378734769888D+00, &
                                              .566263292669091D-01, &
                                             -.137417679148505D+00, &
                                             -.169124163201523D+00, &
                                              .608037039066423D-01, &
                                              .189589640245655D+00, &
                                              .563784150384640D-01, &
                                              .100486298765028D-01, &
                                             -.552186092621482D-01, &
                                             -.413902746758285D-01, &
                                             -.116832934994489D+00, &
                                              .233610871054729D-01, &
                                              .233885865725971D-01 /)

double precision, parameter :: Dv2(npt) = (/ -.385449887634130D+01, &
                                             -.417706040200591D+01, &
                                             -.256425277368288D+01, &
                                             -.558557503589276D+00, &
                                             -.349316054551627D+00, &
                                             -.256022933201611D+00, &
                                             -.418337423301704D-01, &
                                              .303368330939646D+00, &
                                              .169921006301015D+00, &
                                             -.175759761362548D+00, &
                                             -.611278214082881D-01, &
                                             -.861140219824535D-01, &
                                              .182451950513387D-01, &
                                             -.995395392057973D-01, &
                                              .184972909229936D+04, &
                                              .362829766922787D+00, &
                                              .362829766922787D+00 /)

! electron density  data
!
double precision, parameter :: yrh(npt) = (/  .865674623712589D-01, &
                                              .925214702944478D-01, &
                                              .862003123832002D-01, &
                                              .762736292751052D-01, &
                                              .606481841271735D-01, &
                                              .466030959588197D-01, &
                                              .338740138848363D-01, &
                                              .232572661705343D-01, &
                                              .109046405489829D-01, &
                                              .524910605677597D-02, &
                                              .391702419142291D-02, &
                                              .308277776293383D-02, &
                                              .250214745349505D-02, &
                                              .147220513798186D-02, &
                                              .000000000000000D+00, &
                                              .000000000000000D+00, &
                                              .000000000000000D+00 /)

double precision, parameter :: Brh(npt) = (/  .608555214104682D-01, &
                                             -.800158928716306D-02, &
                                             -.332089451111092D-01, &
                                             -.521001991705069D-01, &
                                             -.618130637429111D-01, &
                                             -.529750064268036D-01, &
                                             -.442210477548108D-01, &
                                             -.473645664984640D-01, &
                                             -.390741582571631D-01, &
                                             -.101795580610560D-01, &
                                             -.318316981110289D-02, &
                                             -.281217210746153D-02, &
                                             -.236932031483360D-02, &
                                             -.683554708271547D-02, &
                                             -.638718204858808D-06, &
                                              .212925486831149D-06, &
                                             -.212983742465787D-06 /)

double precision, parameter :: Crh(npt) = (/ -.170233687052940D+00, &
                                             -.102317878901959D+00, &
                                              .254162872544396D-02, &
                                             -.773173610292656D-01, &
                                              .388717099948882D-01, &
                                             -.388873819867093D-02, &
                                              .385388290924526D-01, &
                                             -.509815666327127D-01, &
                                              .837968231208082D-01, &
                                              .305743500420042D-01, &
                                             -.288110886134041D-02, &
                                              .434959924771674D-02, &
                                             -.259669459714693D-02, &
                                             -.150816117849093D-01, &
                                              .421356801161513D-01, &
                                             -.842575249165724D-02, &
                                             -.843267014952237D-02 /)

double precision, parameter :: Drh(npt) = (/  .896085612514625D-01, &
                                              .138352319847830D+00, &
                                             -.105366473134009D+00, &
                                              .153300619856764D+00, &
                                             -.564184148788224D-01, &
                                              .559792096400504D-01, &
                                             -.118113795329664D+00, &
                                              .177827488509794D+00, &
                                             -.702220789044304D-01, &
                                             -.441413511810337D-01, &
                                              .954024354744484D-02, &
                                             -.916498550800407D-02, &
                                             -.164726813535368D-01, &
                                              .754928689733184D-01, &
                                             -.667110847110954D+03, &
                                             -.912720300911022D-01, &
                                             -.912720300911022D-01 /)

! Embedding function data
!
double precision, parameter :: xuu(nuu) = (/  .000000000000000D+00, &
                                              .100000000000000D+00, &
                                              .200000000000000D+00, &
                                              .300000000000000D+00, &
                                              .400000000000000D+00, &
                                              .500000000000000D+00, &
                                              .600000000000000D+00, &
                                              .700000000000000D+00, &
                                              .800000000000000D+00, &
                                              .900000000000000D+00, &
                                              .100000000000000D+01, &
                                              .110000000000000D+01, &
                                              .120000000000000D+01 /)

double precision, parameter :: yuu(nuu) = (/  .000000000000000D+00, &
                                             -.113953324143752D+01, &
                                             -.145709859805864D+01, &
                                             -.174913308002738D+01, &
                                             -.202960322136630D+01, &
                                             -.225202324967546D+01, &
                                             -.242723053979436D+01, &
                                             -.255171976467357D+01, &
                                             -.260521638832322D+01, &
                                             -.264397894381693D+01, &
                                             -.265707884842034D+01, &
                                             -.264564149400021D+01, &
                                             -.260870604452106D+01 /)

double precision, parameter :: Buu(nuu) = (/ -.183757286015853D+02, &
                                             -.574233124410516D+01, &
                                             -.236790436375322D+01, &
                                             -.307404645857774D+01, &
                                             -.251104850116555D+01, &
                                             -.196846462620234D+01, &
                                             -.154391254686695D+01, &
                                             -.846780636273251D+00, &
                                             -.408540363905760D+00, &
                                             -.286833282404628D+00, &
                                             -.309389414590161D-06, &
                                              .236958014464143D+00, &
                                              .503352368511243D+00 /)

double precision, parameter :: Cuu(nuu) = (/  .830779120415016D+02, &
                                              .432560615333001D+02, &
                                             -.951179272978074D+01, &
                                              .245037178153561D+01, &
                                              .317960779258630D+01, &
                                              .224623095704576D+01, &
                                              .199928983630817D+01, &
                                              .497202926962879D+01, &
                                             -.589626545953876D+00, &
                                              .180669736096520D+01, &
                                              .106163236918694D+01, &
                                              .130795086934864D+01, &
                                              .135599267112235D+01 /)

double precision, parameter :: Duu(nuu) = (/ -.132739501694005D+03, &
                                             -.175892847543603D+03, &
                                              .398738817043878D+02, &
                                              .243078670350231D+01, &
                                             -.311125611846847D+01, &
                                             -.823137069125319D+00, &
                                              .990913144440207D+01, &
                                             -.185388527186089D+02, &
                                              .798774635639692D+01, &
                                             -.248354997259420D+01, &
                                              .821061667205675D+00, &
                                              .160139339245701D+00, &
                                              .160139339245701D+00 /)
contains

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi(r,phi,irlast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: r
double precision, intent(out)   :: phi
integer,          intent(inout) :: irlast

!-- Local variables
integer i
double precision dx

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi = 0.d0
else
   i = seval_i(npt,r,x,irlast)
   dx = r - x(i)
   phi = yv2(i) + dx*(Bv2(i) + dx*(Cv2(i) + dx*Dv2(i)))
endif

end subroutine calc_phi

!-------------------------------------------------------------------------------
!
!  Calculate pair potential phi(r) and its derivative dphi(r)
!
!-------------------------------------------------------------------------------
subroutine calc_phi_dphi(r,phi,dphi,irlast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: r
double precision, intent(out)   :: phi,dphi
integer,          intent(inout) :: irlast

!-- Local variables
integer i
double precision dx

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   phi    = 0.d0
   dphi   = 0.d0
else
   i = seval_i(npt,r,x,irlast)
   dx = r - x(i)
   phi  = yv2(i) + dx*(Bv2(i) + dx*(Cv2(i) + dx*Dv2(i)))
   dphi = Bv2(i) + dx*(2.d0*Cv2(i) + 3.d0*dx*Dv2(i))
endif

end subroutine calc_phi_dphi

!-------------------------------------------------------------------------------
!
!  Calculate electron density g(r)
!
!-------------------------------------------------------------------------------
subroutine calc_g(r,g,irlast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: r
double precision, intent(out)   :: g
integer,          intent(inout) :: irlast

!-- Local variables
integer i
double precision dx

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   g = 0.d0
else
   i = seval_i(npt,r,x,irlast)
   dx = r - x(i)
   g = yrh(i) + dx*(Brh(i) + dx*(Crh(i) + dx*Drh(i)))
endif

end subroutine calc_g

!-------------------------------------------------------------------------------
!
!  Calculate electron density derivative dg(r)
!
!-------------------------------------------------------------------------------
subroutine calc_dg(r,dg,irlast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: r
double precision, intent(out)   :: dg
integer,          intent(inout) :: irlast

!-- Local variables
integer i
double precision dx

if (r .gt. model_cutoff) then
   ! Argument exceeds cutoff radius
   dg = 0.d0
else
   i = seval_i(npt,r,x,irlast)
   dx = r - x(i)
   dg = Brh(i) + dx*(2.d0*Crh(i) + 3.d0*dx*Drh(i))
endif

end subroutine calc_dg

!-------------------------------------------------------------------------------
!
!  Calculate embedding function U(rho)
!
!-------------------------------------------------------------------------------
subroutine calc_U(rho,U,ielast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: rho
double precision, intent(out)   :: U
integer,          intent(inout) :: ielast

!-- Local variables
integer i
double precision dx

if (rho .le. rhomin) then
   ! Argument less than the minimum stored value
   U = 0.d0
else
   i = seval_i(nuu,rho,xuu,ielast)
   dx = rho - xuu(i)
   U = yuu(i) + dx*(Buu(i) + dx*(Cuu(i) + dx*Duu(i)))
endif

end subroutine calc_U

!-------------------------------------------------------------------------------
!
!  Calculate embedding function U(rho) and first derivative dU(rho)
!
!-------------------------------------------------------------------------------
subroutine calc_U_dU(rho,U,dU,ielast)
implicit none

!-- Transferred variables
double precision, intent(in)    :: rho
double precision, intent(out)   :: U,dU
integer,          intent(inout) :: ielast

!-- Local variables
integer i
double precision dx

if (rho .le. rhomin) then
   ! Argument less than the minimum stored value
   U  = 0.d0
   dU = 0.d0
else
   i = seval_i(nuu,rho,xuu,ielast)
   dx = rho - xuu(i)
   U  = yuu(i) + dx*(Buu(i) + dx*(Cuu(i) + dx*Duu(i)))
   dU = Buu(i) + dx*(2.d0*Cuu(i) + 3.d0*dx*Duu(i))
endif

end subroutine calc_U_dU

!-------------------------------------------------------------------------------
!
!  This function performs a binary search to find the index i
!  for evaluating the cubic spline function
!
!    seval = y(i) + B(i)*(u-x(i)) + C(i)*(u-x(i))**2 + D(i)*(u-x(i))**3
!
!    where  x(i) .lt. u .lt. x(i+1), using horner's rule
!
!  if  u .lt. x(1) then  i = 1  is used.
!  if  u .ge. x(n) then  i = n  is used.
!
!  input..
!
!    n = the number of data points
!    u = the abscissa at which the spline is to be evaluated
!    x = the array of data abscissas
!    i = current value of i
!
!  if  u  is not in the same interval as the previous call, then a
!  binary search is performed to determine the proper interval.
!
!-------------------------------------------------------------------------------
integer function seval_i(n, u, x, i)
implicit none

!--Transferred variables
integer,          intent(in)     ::  n
double precision, intent(in)     ::  u, x(n)
integer,          intent(inout)  ::  i

!--Local variables
integer j, k

if ( i .ge. n ) i = 1
if ( u .lt. x(i) ) go to 10
if ( u .le. x(i+1) ) go to 30

!  binary search
!
10 i = 1
   j = n+1
20 k = (i+j)/2
   if ( u .lt. x(k) ) j = k
   if ( u .ge. x(k) ) i = k
   if ( j .gt. i+1 ) go to 20

!  got i, return
!
30 seval_i = i
   return

end function seval_i

!-------------------------------------------------------------------------------
!
! Compute energy and forces on atoms from the positions.
!
!-------------------------------------------------------------------------------
integer function Compute_Energy_Forces(pkim)
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in)  :: pkim

!-- Local variables
double precision :: Rij(DIM)
double precision :: r,Rsqij,phi,dphi,g,dg,dU,U,dphieff
double precision :: dphii,dUi,Ei,dphij,dUj,Ej
integer :: i,j,jj,numnei,comp_force,comp_enepot,comp_virial,comp_energy
double precision, allocatable :: rho(:),derU(:)
integer, allocatable, target :: nei1atom_substitute(:)
character*80 :: error_message

!-- KIM variables
integer N;                  pointer(pN,N)
real*8 energy;              pointer(penergy,energy)
real*8 coordum(DIM,1);      pointer(pcoor,coordum)
real*8 forcedum(DIM,1);     pointer(pforce,forcedum)
real*8 enepotdum(1);        pointer(penepot,enepotdum)
real*8 boxSideLengths(DIM); pointer(pboxSideLengths,boxSideLengths)
real*8 Rij_list(DIM,1);     pointer(pRij_list,Rij_list)
integer numContrib;         pointer(pnumContrib,numContrib)
integer nei1atom(1);        pointer(pnei1atom,nei1atom)
integer particleTypes(1);   pointer(pparticleTypes,particleTypes)
real*8 virialdum(1);        pointer(pvirial,virialdum)
integer irlast;             pointer(pirlast,irlast)
integer ielast;             pointer(pielast,ielast)
character*64 NBC_Method;    pointer(pNBC_Method,NBC_Method)
real*8, pointer :: coor(:,:),force(:,:),ene_pot(:),virial_global(:)
integer IterOrLoca
integer HalfOrFull
integer NBC
integer numberContrib
integer idum
integer atom_ret

! Determine neighbor list boundary condition (NBC)
! and half versus full mode:
! *****************************
! * HalfOrFull = 1 -- Half
! *            = 2 -- Full
! *****************************
!
!
pNBC_Method = kim_api_get_nbc_method_f(pkim, Compute_Energy_Forces)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method_f", Compute_Energy_Forces)
   return
endif
if (index(NBC_Method,"CLUSTER").eq.1) then
   NBC = 0
   HalfOrFull = 1
elseif (index(NBC_Method,"MI_OPBC_H").eq.1) then
   NBC = 1
   HalfOrFull = 1
elseif (index(NBC_Method,"MI_OPBC_F").eq.1) then
   NBC = 1
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH_PURE_H").eq.1) then
   NBC = 2
   HalfOrFull = 1
elseif (index(NBC_Method,"NEIGH_PURE_F").eq.1) then
   NBC = 2
   HalfOrFull = 2
elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
   NBC = 3
   HalfOrFull = 2
else
   Compute_Energy_Forces = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", Compute_Energy_Forces)
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
   IterOrLoca = kim_api_get_neigh_mode_f(pkim, Compute_Energy_Forces)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_mode_f", Compute_Energy_Forces)
      return
   endif
   if (IterOrLoca.ne.1 .and. IterOrLoca.ne.2) then
      Compute_Energy_Forces = KIM_STATUS_FAIL
      write(error_message,'(a,i1)') &
         'Unsupported IterOrLoca mode = ',IterOrLoca
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    error_message, Compute_Energy_Forces)
      return
   endif
else
   IterOrLoca = 2   ! for CLUSTER NBC
endif

! Check to see if we have been asked to compute the forces, energyperatom,
! energy and virial
!
call kim_api_getm_compute_f(pkim, Compute_Energy_Forces, &
     "energy",         comp_energy, 1, &
     "forces",         comp_force,  1, &
     "particleEnergy", comp_enepot, 1, &
     "virial",         comp_virial, 1)
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_compute_f", Compute_Energy_Forces)
   return
endif

! Unpack data from KIM object
!
call kim_api_getm_data_f(pkim, Compute_Energy_Forces, &
     "numberOfParticles",           pN,              1,                           &
     "particleTypes",               pparticleTypes,  1,                           &
     "coordinates",                 pcoor,           1,                           &
     "numberContributingParticles", pnumContrib,     TRUEFALSE(HalfOrFull.eq.1),  &
     "boxSideLengths",              pboxSideLengths, TRUEFALSE(NBC.eq.1),         &
     "energy",                      penergy,         TRUEFALSE(comp_energy.eq.1), &
     "forces",                      pforce,          TRUEFALSE(comp_force.eq.1),  &
     "particleEnergy",              penepot,         TRUEFALSE(comp_enepot.eq.1), &
     "virial",                      pvirial,         TRUEFALSE(comp_virial.eq.1), &
     "PARAM_FIXED_irlast",          pirlast,         1,                           &
     "PARAM_FIXED_ielast",          pielast,         1                            &
     )
if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data_f", Compute_Energy_Forces)
   return
endif

call KIM_to_F90_real_array_2d(coordum,coor,DIM,N)
if (comp_force.eq.1)  call KIM_to_F90_real_array_2d(forcedum,force,DIM,N)
if (comp_enepot.eq.1) call KIM_to_F90_real_array_1d(enepotdum,ene_pot,N)
if (comp_virial.eq.1) call KIM_to_F90_real_array_1d(virialdum,virial_global,6)
if (HalfOrFull.eq.1) then
   if (NBC.ne.0) then ! non-CLUSTER cases
      numberContrib = numContrib
   else               ! CLUSTER cases
      numberContrib = N
   endif
endif

! Check to be sure that the atom types are correct
!
Compute_Energy_Forces = KIM_STATUS_FAIL ! assume an error
do i = 1,N
   if (particleTypes(i).ne.speccode) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "Unexpected species type detected", Compute_Energy_Forces)
      return
   endif
enddo
Compute_Energy_Forces = KIM_STATUS_OK ! everything is ok

! Initialize potential energies, forces, virial term, electron density
!
! Note: that the variable `ene_pot' does not need to be initialized
!       because it's initial value is set during the embedding energy
!       calculation.
!
if (comp_energy.eq.1) energy         = 0.d0
if (comp_force.eq.1)  force(1:DIM,1:N) = 0.d0
if (comp_virial.eq.1) virial_global  = 0.d0
allocate( rho(N) )  ! pair functional electron density
rho(1:N) = 0.d0
if (comp_force.eq.1.or.comp_virial.eq.1) allocate( derU(N) )  ! EAM embedded energy deriv

! Initialize neighbor handling for CLUSTER NBC
!
if (NBC.eq.0) then
   allocate( nei1atom_substitute(N) )
   pnei1atom = loc(nei1atom_substitute)
endif

!
!  Compute energy and forces
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", Compute_Energy_Forces)
      return
   endif
endif

!  Loop over particles in the neighbor list a first time,
!  to compute electron density (=coordination)
!
i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,Compute_Energy_Forces)
   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "get_current_atom_neighbors", Compute_Energy_Forces)
      return
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

      ! compute contribution to electron density
      !
      Rsqij = dot_product(Rij,Rij)                ! compute square distance
      if ( Rsqij .lt. model_cutsq ) then          ! particles are interacting?
         r = sqrt(Rsqij)                          ! compute distance
         call calc_g(r,g,irlast)                  ! compute electron density
         rho(i) = rho(i) + g                      ! accumulate electron density
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) &            ! HALF mode
            rho(j) = rho(j) + g                   !      (add contrib to j)
      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

!  Now that we know the electron densities, calculate embedding part of energy
!  U and its derivative U' (derU)
!
do i = 1,N
   if (comp_force.eq.1.or.comp_virial.eq.1) then
      call calc_U_dU(rho(i),U,dU,ielast)       ! compute embedding energy
                                                  !   and its derivative
      derU(i) = dU                                ! store dU for later use
   else
      call calc_U(rho(i),U,ielast)             ! compute just embedding energy
   endif

   ! accumulate the embedding energy contribution
   !
   ! Assuming U(rho=0) = 0.0d0
   !
   if (comp_enepot.eq.1) then                     ! accumulate embedding energy contribution
      ene_pot(i) = U
   endif
   if (comp_energy.eq.1) then
      energy = energy + U
   endif

   if ((HalfOrFull.eq.1) .and. (i .gt. numberContrib)) exit
enddo

!  Loop over particles in the neighbor list a second time, to compute
!  the forces and complete energy calculation
!

! Reset iterator if one is being used
!
if (IterOrLoca.eq.1) then
   Compute_Energy_Forces = kim_api_get_neigh_f(pkim,0,0,atom_ret,numnei,pnei1atom,pRij_list)
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", Compute_Energy_Forces)
      return
   endif
endif

i = 0
do

   ! Set up neighbor list for next atom for all NBC methods
   !
   call get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                   i,numnei,pnei1atom,pRij_list,Compute_Energy_Forces)

   if (Compute_Energy_Forces.eq.KIM_STATUS_NEIGH_ITER_PAST_END) exit  ! atom counter incremented past end of list
   if (Compute_Energy_Forces.lt.KIM_STATUS_OK) then
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "get_current_atom_neighbors", Compute_Energy_Forces)
      return
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
         if (comp_force.eq.1.or.comp_virial.eq.1) then
            call calc_phi_dphi(r,phi,dphi,irlast) ! compute pair potential
                                                  !   and it derivative
            call calc_dg(r,dg,irlast)             ! compute elect dens first deriv
            if ((HalfOrFull.eq.1) .and. &
                (j .le. numberContrib)) then      ! HALF mode
               dphii  = 0.5d0*dphi
               dphij  = 0.5d0*dphi
               dUi    = derU(i)*dg
               dUj    = derU(j)*dg
            else                                  ! FULL mode
               dphii  = 0.5d0*dphi
               dphij  = 0.0d0
               dUi    = derU(i)*dg
               dUj    = 0.0d0
            endif
            dphieff = dphii + dphij + dUi + dUj
         else
            call calc_phi(r,phi,irlast)           ! compute just pair potential
         endif
         if ((HalfOrFull.eq.1) .and. &
             (j .le. numberContrib)) then         ! HALF mode
            Ei     = 0.5d0*phi
            Ej     = 0.5d0*phi
         else                                  ! FULL mode
            Ei     = 0.5d0*phi
            Ej     = 0.0d0
         endif

         ! contribution to energy
         !
         if (comp_enepot.eq.1) then
            ene_pot(i) = ene_pot(i) + Ei          ! accumulate energy Ei
            ene_pot(j) = ene_pot(j) + Ej          ! accumulate energy Ej
         endif
         if (comp_energy.eq.1) then
            energy = energy + Ei                  ! accumulate energy
            energy = energy + Ej                  ! accumulate energy
         endif

         ! contribution to virial tensor
         !
         if (comp_virial.eq.1) then
            virial_global(1) = virial_global(1) + Rij(1)*Rij(1)*dphieff/r
            virial_global(2) = virial_global(2) + Rij(2)*Rij(2)*dphieff/r
            virial_global(3) = virial_global(3) + Rij(3)*Rij(3)*dphieff/r
            virial_global(4) = virial_global(4) + Rij(2)*Rij(3)*dphieff/r
            virial_global(5) = virial_global(5) + Rij(1)*Rij(3)*dphieff/r
            virial_global(6) = virial_global(6) + Rij(1)*Rij(2)*dphieff/r
         endif

         ! contribution to forces
         !
         if (comp_force.eq.1) then                        ! Ei contribution
            force(:,i) = force(:,i) + dphieff*Rij/r ! accumulate force on atom i
            force(:,j) = force(:,j) - dphieff*Rij/r ! accumulate force on atom j
         endif

      endif

   enddo  ! loop on jj

enddo  ! infinite do loop (terminated by exit statements above)

! Free temporary storage
!
if (NBC.eq.0) deallocate( nei1atom_substitute )
deallocate( rho )
if (comp_force.eq.1.or.comp_virial.eq.1) deallocate( derU )

! Everything is great
!
Compute_Energy_Forces = KIM_STATUS_OK
return

end function Compute_Energy_Forces

!-------------------------------------------------------------------------------
!
! Get list of neighbors for current atom using all NBC methods
!
!-------------------------------------------------------------------------------
subroutine get_current_atom_neighbors(IterOrLoca,HalfOrFull,NBC,N,pkim,      &
                                      atom,numnei,pnei1atom,pRij_list,ier)
implicit none

!-- Transferred variables
integer,                  intent(in)    :: IterOrLoca
integer,                  intent(in)    :: HalfOrFull
integer,                  intent(in)    :: NBC
integer,                  intent(in)    :: N
integer(kind=kim_intptr), intent(in)    :: pkim
integer,                  intent(inout) :: atom
integer,                  intent(out)   :: numnei
integer,                  intent(out)   :: ier
integer nei1atom(1);    pointer(pnei1atom,nei1atom)
real*8 Rij_list(DIM,1); pointer(pRij_list,Rij_list)

!-- Local variables
integer atom_ret, jj
integer idum

! Set up neighbor list for next atom for all NBC methods
!
if (IterOrLoca.eq.1) then    ! ITERATOR mode
   ier = kim_api_get_neigh_f(pkim,0,1,atom_ret,numnei, &
                             pnei1atom,pRij_list)
   if (ier.eq.KIM_STATUS_NEIGH_ITER_PAST_END) then
                          ! past end of the list, terminate loop in
      return              ! calling routine
   endif
   if (ier.lt.KIM_STATUS_OK) then     ! some sort of problem, exit
      idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_get_neigh_f", ier)
      return
   endif
   atom = atom_ret

else                         ! LOCATOR mode

   atom = atom + 1
   if (atom.gt.N) then                     ! incremented past end of list,
      ier = KIM_STATUS_NEIGH_ITER_PAST_END ! terminate loop in calling routine
      return
   endif

   if (NBC.eq.0) then ! CLUSTER NBC method
      numnei = N - atom   ! number of neighbors in list atom+1, ..., N
      nei1atom(1:numnei) = (/ (atom+jj, jj = 1,numnei) /)
      ier = KIM_STATUS_OK
   else
      ier = kim_api_get_neigh_f(pkim,1,atom,atom_ret,numnei,pnei1atom,pRij_list)
      if (ier.ne.KIM_STATUS_OK) then ! some sort of problem, exit
         idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                       "kim_api_get_neigh_f", ier)
         ier = KIM_STATUS_FAIL
         return
      endif
   endif
endif

return
end subroutine get_current_atom_neighbors

!-------------------------------------------------------------------------------
!
! Model destroy routine
!
!-------------------------------------------------------------------------------
integer function Destroy(pkim)
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer ier, idum

!-- KIM variables
integer irlast; pointer(pirlast,irlast)
integer ielast; pointer(pielast,ielast)

! get irlast and ielast from KIM object and free memory
call kim_api_getm_data_f(pkim, Destroy, &
     "PARAM_FIXED_irlast", pirlast, 1, &
     "PARAM_FIXED_ielast", pielast, 1  &
     )
if (Destroy.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data_f", Destroy);
   return
endif

call free(pirlast)
call free(pielast)

Destroy = KIM_STATUS_OK
return

end function Destroy

end module ex_model_Al_PF_ErcolessiAdams

!-------------------------------------------------------------------------------
!
! Model initialization routine (REQUIRED)
!
!-------------------------------------------------------------------------------
integer function ex_model_Al_PF_ErcolessiAdams_init(pkim)
use ex_model_Al_PF_ErcolessiAdams
use KIM_API
implicit none

!-- Transferred variables
integer(kind=kim_intptr), intent(in) :: pkim

!-- Local variables
integer(kind=kim_intptr), parameter :: one=1
integer ier, idum

!-- KIM variables
real*8 cutoff;  pointer(pcutoff,cutoff)
integer irlast; pointer(pirlast,irlast)
integer ielast; pointer(pielast,ielast)

! store function pointers in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "compute", one, loc(Compute_Energy_Forces), 1, &
     "destroy", one, loc(Destroy),               1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data_f", ier)
   goto 42
endif

! store model cutoff in KIM object
pcutoff =  kim_api_get_data_f(pkim,"cutoff",ier)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data_f", ier)
   goto 42
endif
cutoff = model_cutoff

! Allocate memory for irlast and store value
! (irlast is the last entry into the pair potential and electron density
! spline tables. It is stored to speed calculations since the next value
! of r is likely close to the last one.)
pirlast = malloc(one*4) ! 4 is the size of an integer
if (pirlast.eq.0) then
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "malloc", ier);
   goto 42
endif
! Allocate memory for ielast and store value
! (ielast is the last entry into the embedding function spline table.
! It is stored to speed calculations since the next value of rho is
! likely close to the last one.)
pielast = malloc(one*4) ! 4 is the size of an integer
if (pielast.eq.0) then
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "malloc", ier);
   goto 42
endif

! store irlast and ielast in KIM object
call kim_api_setm_data_f(pkim, ier, &
     "PARAM_FIXED_irlast", one, pirlast, 1, &
     "PARAM_FIXED_ielast", one, pielast, 1)
if (ier.lt.KIM_STATUS_OK) then
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_setm_data_f", ier);
   goto 42
endif

! Initialize
irlast = 1
ielast = 1

ier = KIM_STATUS_OK
42 continue
ex_model_Al_PF_ErcolessiAdams_init = ier
return

end function ex_model_Al_PF_ErcolessiAdams_init

