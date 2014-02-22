subroutine update_neighborlist(DIM,N,coords,cutoff,cutpad,boxSideLengths, &
                               NBC_Method,do_update_list,coordsave, &
                               neighborList,RijList,ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none
integer(c_int), parameter :: cd = c_double ! used for literal constants

!-- Transferred variables
integer(c_int),    intent(in)    :: DIM
integer(c_int),    intent(in)    :: N
real(c_double),    intent(in)    :: coords(DIM,N)
real(c_double),    intent(in)    :: cutoff
real(c_double),    intent(in)    :: cutpad
real(c_double),    intent(in)    :: boxSideLengths(DIM)
character(len=64), intent(in)    :: NBC_Method
logical,           intent(inout) :: do_update_list
real(c_double),    intent(inout) :: coordsave(DIM,N)
integer(c_int),    intent(inout) :: neighborList(N+1,N)
real(c_double),    intent(inout) :: RijList(DIM,N+1,N)
integer(c_int),    intent(out)   :: ier

!-- Local variables
! 0- NEIGH_RVEC_H, 1- NEIGH_PURE_H, 2- NEIGH_RVEC_F, 3- NEIGH_PURE_F,
! 4- MI_OPBC_H, 5- MI_OPBC_F 
integer(c_int) nbc
real(c_double) disp, disp1, disp2, cutrange, dispvec(DIM)
integer(c_int) i, idum

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
   idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                               "Unknown NBC method", ier)
   stop
endif

! Update neighbor lists if necessary, if not just update Rij vectors if these
! are used
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
   if (nbc.eq.0) then
      call NEIGH_RVEC_cluster_neighborlist(.true., N, coords, cutrange, N, &
                                           neighborList, RijList)
   elseif (nbc.eq.1) then
      call NEIGH_PURE_cluster_neighborlist(.true., N, coords, cutrange, &
                                           neighborList)
   elseif (nbc.eq.2) then
      call NEIGH_RVEC_cluster_neighborlist(.false., N, coords, cutrange, &
                                           N, neighborList, RijList)
   elseif (nbc.eq.3) then
      call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutrange, &
                                           neighborList)
   elseif (nbc.eq.4) then
      call MI_OPBC_cluster_neighborlist(.true., N, coords, cutrange, &
                                        boxSideLengths, neighborList)
   elseif (nbc.eq.5) then
      call MI_OPBC_cluster_neighborlist(.false., N, coords, cutrange, &
                                        boxSideLengths, neighborList)
   endif

   ! neighbor list uptodate, no need to compute again for now
   do_update_list = .false.

else

   ! Even though neighbor lists are uptodate Rij vectors still need to be
   ! computed
   if (nbc.eq.0.or.nbc.eq.2) &
      call NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, N, neighborList, &
                                         RijList)

endif

return

end subroutine update_neighborlist


