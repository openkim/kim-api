!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_periodic_B2_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_periodic_B2_neighborlist(half,CellsPerHalfSide, cutoff,  &
                                      B2spacing, NN, neighborList, RijList)
  use KIM_API
  implicit none

  !-- Transferred variables
  logical,                               intent(in)  :: half
  integer,                               intent(in)  :: CellsPerHalfSide
  double precision,                      intent(in)  :: cutoff
  double precision,                      intent(in)  :: B2spacing
  integer,                               intent(in)  :: NN
  integer, dimension(NN+1,1),            intent(out) :: neighborList
  double precision, dimension(3,NN+1,1), intent(out) :: RijList

  !-- Local variables
  double precision dx(3)
  double precision cutoff2
  double precision B2shifts(3,2)
  double precision latVec(3)
  integer atom, a, i, j, k, m

  cutoff2 = cutoff**2

  ! Cubic B2 cell positions ----------------------------------------------------------------------
  B2shifts(1,1) = 0.d0;           B2shifts(2,1) = 0.d0;           B2shifts(3,1) = 0.d0
  B2shifts(1,2) = 0.5*B2spacing;  B2shifts(2,2) = 0.5*B2spacing;  B2shifts(3,2) = 0.5*B2spacing

  if (half) then
     ! Each atom gets half of its own neighbor-self images
     do atom = 2,1,-1  ! have atom 1 be last in the loop
        a = 1
        do i = 0, CellsPerHalfSide
           latVec(1) = i*B2spacing
           do j = -CellsPerHalfSide, CellsPerHalfSide
              latVec(2) = j*B2spacing
              do k = -CellsPerHalfSide, CellsPerHalfSide
                 latVec(3) = k*B2spacing
                 if (i.gt.0 .or. (i.eq.0 .and. (j.gt.0 .or. (j.eq.0 .and. k.gt.0)))) then
                    dx = latVec
                    if (dot_product(dx,dx).lt.cutoff2) then
                       ! we have a neighbor
                       a = a + 1
                       neighborList(a,atom) = atom
                       RijList(:,a-1,atom)  = dx
                    endif
                 endif
              enddo
           enddo
        enddo
        ! this atom has a-1 neighbors (so far)
        neighborList(1,atom) = a - 1
     enddo
     
     ! Atom 1 gets all images of atom 2; atom 2 gets no atom 1 images
     do i = -CellsPerHalfSide, CellsPerHalfSide
        latVec(1) = i*B2spacing
        do j = -CellsPerHalfSide, CellsPerHalfSide
           latVec(2) = j*B2spacing
           do k = -CellsPerHalfSide, CellsPerHalfSide
              latVec(3) = k*B2spacing
              dx = (latVec + B2shifts(:,2)) - B2shifts(:,1)
              if (dot_product(dx,dx).lt.cutoff2) then
                 ! we have a neighbor
                 a = a + 1
                 neighborList(a,atom) = 2
                 RijList(:,a-1,atom)  = dx
              endif
           enddo
        enddo
     enddo

     ! atom 1 has a-1 neighbors
     neighborList(1,1) = a - 1
  else
     do atom = 1,2
        a = 1
        do i=-CellsPerHalfSide,CellsPerHalfSide
           latVec(1) = i*B2spacing
           do j=-CellsPerHalfSide,CellsPerHalfSide
              latVec(2) = j*B2spacing
              do k=-CellsPerHalfSide,CellsPerHalfSide
                 latVec(3) = k*B2spacing
                 do m=1,2
                    dx = (latVec + B2shifts(:,m)) - B2shifts(:,atom)
                    if (dot_product(dx,dx).lt.cutoff2) then
                       if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. (m.eq.atom) )) then
                          ! we have a neighbor
                          a = a+1
                          neighborList(a,atom) = m
                          RijList(:,a-1,atom)  = dx
                       endif
                    endif
                 enddo
              enddo
           enddo
        enddo
        ! atom 1 has a-1 neighbors
        neighborList(1,atom) = a-1
     enddo
  endif

  return

end subroutine NEIGH_RVEC_periodic_B2_neighborlist
