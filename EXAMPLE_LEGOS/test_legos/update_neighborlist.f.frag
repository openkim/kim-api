subroutine update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths,NBC_Method,  &
                               do_update_list,coordsave,neighborList,RijList,ier)
use KIM_API
implicit none

!-- Transferred variables
integer,           intent(in)    :: DIM
integer,           intent(in)    :: N
double precision,  intent(in)    :: coords(DIM,N)
double precision,  intent(in)    :: cutoff
double precision,  intent(in)    :: cutpad
double precision,  intent(in)    :: boxSideLengths(DIM)
character(len=64), intent(in)    :: NBC_Method
logical,           intent(inout) :: do_update_list
double precision,  intent(inout) :: coordsave(DIM,N)
integer,           intent(inout) :: neighborList(N+1,N)
double precision,  intent(inout) :: RijList(DIM,N+1,N)
integer,           intent(out)   :: ier

!-- Local variables
integer nbc  ! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F, 
             ! 4- MI_OPBC_H, 5- MI_OPBC_F 
double precision disp, disp1, disp2, cutrange, dispvec(DIM)
integer i, idum

! Initialize error code
!
ier = KIM_STATUS_OK

! Determine which NBC scenario to use
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
else
   ier = KIM_STATUS_FAIL
   idum = kim_api_report_error_f(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", ier)
   stop
endif

! Update neighbor lists if necessary, if not just update Rij vectors if these
! are used
!
if (.not.do_update_list) then   ! if update not requested

   ! check whether a neighbor list update is necessary even if it hasn't been
   ! requested using the "two max sum" criterion
   disp1 = 0.d0
   disp2 = 0.d0
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
   if (nbc.eq.0) then
      call NEIGH_RVEC_cluster_neighborlist(.true., N, coords, cutrange, N, neighborList, RijList)
   elseif (nbc.eq.1) then
      call NEIGH_PURE_cluster_neighborlist(.true., N, coords, cutrange, neighborList)
   elseif (nbc.eq.2) then
      call NEIGH_RVEC_cluster_neighborlist(.false., N, coords, cutrange, N, neighborList, RijList)
   elseif (nbc.eq.3) then
      call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutrange, neighborList)
   elseif (nbc.eq.4) then
      call MI_OPBC_cluster_neighborlist(.true., N, coords, cutrange, boxSideLengths, neighborList)
   elseif (nbc.eq.5) then
      call MI_OPBC_cluster_neighborlist(.false., N, coords, cutrange, boxSideLengths, neighborList)
   endif

   ! neighbor list uptodate, no need to compute again for now
   do_update_list = .false.

else

   ! Even though neighbor lists are uptodate Rij vectors still need to be
   ! computed
   if (nbc.eq.0.or.nbc.eq.2) &
      call NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, N, neighborList, RijList)

endif

return

end subroutine update_neighborlist


