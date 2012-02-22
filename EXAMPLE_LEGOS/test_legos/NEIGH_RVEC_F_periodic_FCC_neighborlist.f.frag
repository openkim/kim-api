!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_F_periodic_FCC_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerHalfSide, cutoff, FCCspacing, N, NN, neighborList, RijList)
  use KIM_API
  implicit none

  !-- Transferred variables
  integer,                               intent(in)  :: CellsPerHalfSide
  double precision,                      intent(in)  :: cutoff
  double precision,                      intent(in)  :: FCCspacing
  integer,                               intent(in)  :: N
  integer,                               intent(in)  :: NN
  integer, dimension(NN+1,N),            intent(out) :: neighborList
  double precision, dimension(3,NN+1,N), intent(out) :: RijList

  !-- Local variables
  double precision dx(3)
  double precision cutoff2
  double precision FCCshifts(3,4)
  double precision latVec(3)
  integer a, i, j, k, m

  if (N.ne.1) then ! check assumption that N==1
     print *,"* ERROR: NEIGH_RVEC_F_periodic_FCC_neighborlist called with N.ne.1"
     stop
  endif

  cutoff2 = cutoff**2

  ! Cubic FCC cell positions ----------------------------------------------------------------------
  FCCshifts(1,1) = 0.d0;           FCCshifts(2,1) = 0.d0;           FCCshifts(3,1) = 0.d0
  FCCshifts(1,2) = 0.5*FCCspacing; FCCshifts(2,2) = 0.5*FCCspacing; FCCshifts(3,2) = 0.d0
  FCCshifts(1,3) = 0.5*FCCspacing; FCCshifts(2,3) = 0.d0;           FCCshifts(3,3) = 0.5*FCCspacing
  FCCshifts(1,4) = 0.d0;           FCCshifts(2,4) = 0.5*FCCspacing; FCCshifts(3,4) = 0.5*FCCspacing

  a = 1
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
                    a = a+1
                    neighborList(a,1) = 1
                    RijList(:,a-1,1) = dx
                 endif
              endif
           enddo
        enddo
     enddo
  enddo
  ! atom 1 has a-1 neighbors
  neighborList(1,1) = a-1

  return

end subroutine NEIGH_RVEC_F_periodic_FCC_neighborlist
