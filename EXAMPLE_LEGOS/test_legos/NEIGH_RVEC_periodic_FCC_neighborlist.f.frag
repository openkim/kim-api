!-------------------------------------------------------------------------------
!
! Neigh_RVEC_periodic_FCC_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_periodic_FCC_neighborlist(half, CellsPerHalfSide, cutoff,  &
                                                FCCspacing, N, NN, neighborList, &
                                                coords, RijList)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  logical,                             intent(in)  :: half
  integer(c_int),                      intent(in)  :: CellsPerHalfSide
  real(c_double),                      intent(in)  :: cutoff
  real(c_double),                      intent(in)  :: FCCspacing
  integer(c_int),                      intent(in)  :: N
  integer(c_int),                      intent(in)  :: NN
  integer(c_int), dimension(NN+1,N),   intent(out) :: neighborList
  real(c_double), dimension(3,N),      intent(out) :: coords
  real(c_double), dimension(3,NN+1,N), intent(out) :: RijList

  !-- Local variables
  real(c_double) dx(3)
  real(c_double) cutoff2
  real(c_double) FCCshifts(3,4)
  real(c_double) latVec(3)
  integer(c_int) atom, atomi, atomj, a(4), i, j, k, m

  if (N.ne.1) then ! check assumption that N==1
     print *,"* ERROR: NEIGH_RVEC_periodic_FCC_neighborlist called with N.ne.1"
     stop
  endif

  ! set coords
  coords(:,1) = 0.0d0

  cutoff2 = cutoff**2

  ! Cubic FCC cell positions ----------------------------------------------------------------------
  FCCshifts(1,1) = 0.d0;           FCCshifts(2,1) = 0.d0;           FCCshifts(3,1) = 0.d0
  FCCshifts(1,2) = 0.5*FCCspacing; FCCshifts(2,2) = 0.5*FCCspacing; FCCshifts(3,2) = 0.d0
  FCCshifts(1,3) = 0.5*FCCspacing; FCCshifts(2,3) = 0.d0;           FCCshifts(3,3) = 0.5*FCCspacing
  FCCshifts(1,4) = 0.d0;           FCCshifts(2,4) = 0.5*FCCspacing; FCCshifts(3,4) = 0.5*FCCspacing

  if (half) then
     ! Each atom gets half of its own neighbor-self images
     do atom = 1, N
        a(atom) = 1
        do i = 0, CellsPerHalfSide
           latVec(1) = i*FCCspacing
           do j = -CellsPerHalfSide, CellsPerHalfSide
              latVec(2) = j*FCCspacing
              do k = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(3) = k*FCCspacing
                 if (i.gt.0 .or. (i.eq.0 .and. (j.gt.0 .or. (j.eq.0 .and. k.gt.0)))) then
                    dx = latVec
                    if (dot_product(dx,dx).lt.cutoff2) then
                       ! we have a neighbor
                       a(atom) = a(atom) + 1
                       neighborList(a(atom),atom) = atom
                       RijList(:,a(atom)-1,atom)  = dx
                    endif
                 endif
              enddo
           enddo
        enddo
     enddo

     ! atom i gets half of the other image atoms
     do atomi = 1, N
        do atomj = atomi+1, 4
           do i = -CellsPerHalfSide, CellsPerHalfSide
              latVec(1) = i*FCCspacing
              do j = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(2) = j*FCCspacing
                 do k = -CellsPerHalfSide, CellsPerHalfSide
                    latVec(3) = k*FCCspacing
                    if ((i.ge.0 .and. atomj.lt.4) .or. (atomj.eq.4 .and. i.gt.0) &
                         .or. ( atomj.eq.4 .and. (i.eq.0 .and. (j.ge.0))) )  then
                       dx = (latVec + FCCshifts(:,atomj)) - FCCshifts(:,atomi)
                       if (dot_product(dx,dx).lt.cutoff2) then
                          ! we have a neighbor
                          a(atomi) = a(atomi) + 1
                          neighborList(a(atomi),atomi) = 1
                          RijList(:,a(atomi)-1,atomi)  = dx
                       endif
                    endif
                 enddo
              enddo
           enddo
        enddo
        ! atomi has a(atomi)-1 neighbors
        neighborList(1,atomi) = a(atomi) - 1
     enddo
  else
     atom = 1
     a(atom) = 1
     do i=-CellsPerHalfSide,CellsPerHalfSide
        latVec(1) = i*FCCspacing
        do j=-CellsPerHalfSide,CellsPerHalfSide
           latVec(2) = j*FCCspacing
           do k=-CellsPerHalfSide,CellsPerHalfSide
              latVec(3) = k*FCCspacing
              do m=1,4
                 dx = latVec + FCCshifts(:,m)
                 if (dot_product(dx,dx).lt.cutoff2) then
                    if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. (m.eq.1) )) then
                       ! we have a neighbor
                       a(atom) = a(atom)+1
                       neighborList(a(atom),1) = 1
                       RijList(:,a(atom)-1,1) = dx
                    endif
                 endif
              enddo
           enddo
        enddo
     enddo
     ! atom has a(atom)-1 neighbors
     neighborList(1,1) = a(atom)-1
  endif

  return

end subroutine NEIGH_RVEC_periodic_FCC_neighborlist
