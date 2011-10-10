!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_F_update_Rij_vectors
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_F_update_Rij_vectors(DIM, N, coords, NNMAX, &
                                           neighborList, RijList)
  implicit none

  !-- Transferred variables
  integer,  intent(in)  :: DIM
  integer,  intent(in)  :: N
  real*8,   intent(in)  :: coords(DIM,N)
  integer,  intent(in)  :: NNMAX
  integer,  intent(in)  :: neighborList(NNMAX+1,N)
  real*8,   intent(out) :: RijList(DIM,NNMAX+1,N)

  !-- Local variables
  integer i, j, jj, NN
  real*8 dx(DIM)

  do i=1,N
     NN = neighborList(1,i)
     do jj=1,NN
        j = neighborList(jj+1,i)
        dx(:) = coords(:, j) - coords(:, i)
        RijList(:,jj,i) = dx(:)
     enddo
  enddo
        
  return

end subroutine NEIGH_RVEC_F_update_Rij_vectors
