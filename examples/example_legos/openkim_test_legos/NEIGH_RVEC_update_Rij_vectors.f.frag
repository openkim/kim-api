!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_update_Rij_vectors
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_update_Rij_vectors(DIM, N, coords, neighObject)
  use, intrinsic :: iso_c_binding
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  integer(c_int), intent(in)  :: DIM
  integer(c_int), intent(in)  :: N
  real(c_double), intent(in)  :: coords(DIM,N)
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, jj, NN
  real(c_double) dx(DIM)

  do i=1,N
     NN = neighObject%neighborList(1,i)
     do jj=1,NN
        j = neighObject%neighborList(jj+1,i)
        dx(:) = coords(:, j) - coords(:, i)
        neighObject%RijList(:,jj,i) = dx(:)
     enddo
  enddo

  return

end subroutine NEIGH_RVEC_update_Rij_vectors
