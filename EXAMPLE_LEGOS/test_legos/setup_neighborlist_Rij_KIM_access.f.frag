!-------------------------------------------------------------------------------
!
! setup_neighborlist_Rij_KIM_access :
!
!    Store necessary pointers in KIM API object to access the neighbor list
!    data and methods.
!
!-------------------------------------------------------------------------------
subroutine setup_neighborlist_Rij_KIM_access(pkim, NLRvecLocs)
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)    :: pkim
  integer(kind=kim_intptr), intent(inout) :: NLRvecLocs(3)

  !-- Local variables
  integer(kind=kim_intptr), parameter :: SizeOne = 1
  integer,                  external  :: get_neigh_Rij
  integer ier, idum

  ! store pointers to neighbor list object and access function
  !
  call kim_api_set_data_multiple_f(pkim, ier, &
       "neighObject",    SizeOne, loc(NLRvecLocs),    1, &
       "get_full_neigh", SizeOne, loc(get_neigh_Rij), 1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_set_data_multiple_f", ier)
     stop
  endif

  ! Call reinit to ensure that the model fully registers the new pointer values
  !
  ier = kim_api_model_reinit_f(pkim);

  return

end subroutine setup_neighborlist_Rij_KIM_access
