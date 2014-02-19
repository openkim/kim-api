!-------------------------------------------------------------------------------
!
! setup_neighborlist_Rij_KIM_access :
!
!    Store necessary pointers in KIM API object to access the neighbor list
!    data and methods.
!
!-------------------------------------------------------------------------------
subroutine setup_neighborlist_Rij_KIM_access(pkim, NLRvecLocs)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr), intent(in)    :: pkim
  type neighObject_type
     type(c_ptr)    :: pneighborList
     type(c_ptr)    :: pRijList
     integer(c_int) :: NNeighbors
  end type neighObject_type
  type(neighObject_type), target, intent(in) :: NLRvecLocs

  !-- Local variables
  integer(c_int), parameter :: SizeOne = 1
  integer(c_int), external  :: get_neigh_Rij
  integer(c_int)            :: ier, idum

  ! store pointers to neighbor list object and access function
  !
  ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(NLRvecLocs))
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_data", ier)
     stop
  endif
  ier = kim_api_set_method(pkim, "get_neigh", SizeOne,&
       c_funloc(get_neigh_Rij))
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_set_method", ier)
     stop
  endif

  return

end subroutine setup_neighborlist_Rij_KIM_access
