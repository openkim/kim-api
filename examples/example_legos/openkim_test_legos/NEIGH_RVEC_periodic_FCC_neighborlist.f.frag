!-------------------------------------------------------------------------------
!
! Neigh_RVEC_periodic_FCC_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_periodic_FCC_neighborlist(half,CellsPerHalfSide,cutoff, &
                                                FCCspacing,N,neighObject, &
                                                coords)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  logical,                             intent(in)    :: half
  integer(c_int),                      intent(in)    :: CellsPerHalfSide
  real(c_double),                      intent(in)    :: cutoff
  real(c_double),                      intent(in)    :: FCCspacing
  integer(c_int),                      intent(in)    :: N
  real(c_double), dimension(3,N),      intent(out)   :: coords
  type(neighObject_type),              intent(inout) :: neighObject
  !-- Local variables
  real(c_double) dx(3)
  real(c_double) cutoff2
  real(c_double) FCCshifts(3,4)
  real(c_double) latVec(3)
  integer(c_int) part, parti, partj, a(4), i, j, k, m

  if (N.ne.1) then ! check assumption that N==1
     print *,"* ERROR: NEIGH_RVEC_periodic_FCC_neighborlist called with N.ne.1"
     stop
  endif

  ! set coords
  coords(:,1) = 0.0_cd

  cutoff2 = cutoff**2

  ! Cubic FCC cell positions ---------------------------------------------------
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

  if (half) then
     ! Each particle gets half of its own neighbor-self images
     do part = 1, N
        a(part) = 1
        do i = 0, CellsPerHalfSide
           latVec(1) = i*FCCspacing
           do j = -CellsPerHalfSide, CellsPerHalfSide
              latVec(2) = j*FCCspacing
              do k = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(3) = k*FCCspacing
                 if (i.gt.0 .or. (i.eq.0 .and. (j.gt.0 .or. (j.eq.0 .and. &
                                                             k.gt.0)))) then
                    dx = latVec
                    if (dot_product(dx,dx).lt.cutoff2) then
                       ! we have a neighbor
                       a(part) = a(part) + 1
                       neighObject%neighborList(a(part),part) = part
                       neighObject%RijList(:,a(part)-1,part)  = dx
                    endif
                 endif
              enddo
           enddo
        enddo
     enddo

     ! particle i gets half of the other image particles
     do parti = 1, N
        do partj = parti+1, 4
           do i = -CellsPerHalfSide, CellsPerHalfSide
              latVec(1) = i*FCCspacing
              do j = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(2) = j*FCCspacing
                 do k = -CellsPerHalfSide, CellsPerHalfSide
                    latVec(3) = k*FCCspacing
                    if ((i.ge.0 .and. partj.lt.4).or.(partj.eq.4 .and. i.gt.0) &
                         .or.( partj.eq.4 .and. (i.eq.0 .and. (j.ge.0))) )  then
                       dx = (latVec + FCCshifts(:,partj)) - FCCshifts(:,parti)
                       if (dot_product(dx,dx).lt.cutoff2) then
                          ! we have a neighbor
                          a(parti) = a(parti) + 1
                          neighObject%neighborList(a(parti),parti) = 1
                          neighObject%RijList(:,a(parti)-1,parti)  = dx
                       endif
                    endif
                 enddo
              enddo
           enddo
        enddo
        ! parti has a(parti)-1 neighbors
        neighObject%neighborList(1,parti) = a(parti) - 1
     enddo
  else
     part = 1
     a(part) = 1
     do i=-CellsPerHalfSide,CellsPerHalfSide
        latVec(1) = i*FCCspacing
        do j=-CellsPerHalfSide,CellsPerHalfSide
           latVec(2) = j*FCCspacing
           do k=-CellsPerHalfSide,CellsPerHalfSide
              latVec(3) = k*FCCspacing
              do m=1,4
                 dx = latVec + FCCshifts(:,m)
                 if (dot_product(dx,dx).lt.cutoff2) then
                    if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. &
                               (m.eq.1) )) then
                       ! we have a neighbor
                       a(part) = a(part)+1
                       neighObject%neighborList(a(part),1) = 1
                       neighObject%RijList(:,a(part)-1,1) = dx
                    endif
                 endif
              enddo
           enddo
        enddo
     enddo
     ! part has a(part)-1 neighbors
     neighObject%neighborList(1,1) = a(part)-1
  endif

  return

end subroutine NEIGH_RVEC_periodic_FCC_neighborlist
