!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_F_periodic_B2_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_F_periodic_B2_neighborlist(CellsPerHalfSide, cutoff, B2spacing, NN, neighborList, RijList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: CellsPerHalfSide
  double precision,                                    intent(in)  :: cutoff
  double precision,                                    intent(in)  :: B2spacing
  integer,                                             intent(in)  :: NN
  integer, dimension(NN,1),                            intent(out) :: neighborList
  double precision, dimension(3,NN,1),                 intent(out) :: RijList
  
  !-- Local variables
  double precision dx(3)
  double precision r2
  double precision cutoff2
  double precision :: B2shifts(3,2)
  double precision :: latVec(3)
  integer          :: atom, a, i, j, k, m

  cutoff2 = cutoff**2

  ! Cubic B2 cell positions ----------------------------------------------------------------------
  B2shifts(1,1) = 0.d0;           B2shifts(2,1) = 0.d0;           B2shifts(3,1) = 0.d0
  B2shifts(1,2) = 0.5*B2spacing;  B2shifts(2,2) = 0.5*B2spacing;  B2shifts(3,2) = 0.5*B2spacing

  do atom = 1,2
     a = 1
     do i=-CellsPerHalfSide,CellsPerHalfSide
        latVec(1) = i*B2spacing
        do j=-CellsPerHalfSide,CellsPerHalfSide
           latVec(2) = j*B2spacing
           do k=-CellsPerHalfSide,CellsPerHalfSide
              latVec(3) = k*B2spacing
              do m=1,2
                 dx = B2shifts(:,atom) - (latVec + B2shifts(:,m))
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

end subroutine NEIGH_RVEC_F_periodic_B2_neighborlist
