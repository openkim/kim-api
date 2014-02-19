!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_update_Rij_vectors
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, NNMAX, &
                                         neighborList, RijList)
  use, intrinsic :: iso_c_binding
  implicit none

  !-- Transferred variables
  integer(c_int), intent(in)  :: DIM
  integer(c_int), intent(in)  :: N
  real(c_double), intent(in)  :: coords(DIM,N)
  integer(c_int), intent(in)  :: NNMAX
  integer(c_int), intent(in)  :: neighborList(NNMAX+1,N)
  real(c_double), intent(out) :: RijList(DIM,NNMAX+1,N)

  !-- Local variables
  integer(c_int) i, j, jj, NN
  real(c_double) dx(DIM)

  do i=1,N
     NN = neighborList(1,i)
     do jj=1,NN
        j = neighborList(jj+1,i)
        dx(:) = coords(:, j) - coords(:, i)
        RijList(:,jj,i) = dx(:)
     enddo
  enddo

  return

end subroutine NEIGH_RVEC_update_Rij_vectors
