!-------------------------------------------------------------------------------
!
! get_neigh_Rij neighbor list access function 
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
integer function get_neigh_Rij(pkim,mode,request,atom,numnei,pnei1atom,pRij)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  integer,                  intent(in)  :: mode
  integer,                  intent(in)  :: request
  integer,                  intent(out) :: atom
  integer,                  intent(out) :: numnei
  integer nei1atom(1); pointer(pnei1atom, nei1atom) ! actual cray pointer associated with nei1atom
  real*8  Rij(3,1);    pointer(pRij, Rij)
  
  !-- Local variables
  integer, save :: iterVal = 0
  integer atomToReturn
  integer(kind=kim_intptr) NLRvecLocs(1);      pointer(pNLRvecLocs,NLRvecLocs)
  integer                  neighborListdum(1); pointer(pneighborListdum, neighborListdum)
  integer, pointer ::      neighborList(:,:)
  double precision RijList(1); pointer(pRijList,RijList)
  integer ier
  integer numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer N
  integer NNeighbors

  ! unpack neighbor list object
  pNLRVecLocs = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  pneighborListdum = NLRvecLocs(1)
  pRijList         = NLRvecLocs(2)
  NNeighbors       = NLRvecLocs(3)
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, NNeighbors+1, N)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_neigh_Rij = KIM_STATUS_NEIGH_ITER_INIT_OK
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_neigh_Rij = KIM_STATUS_NEIGH_ITER_PAST_END
           return
        else
           atomToReturn = iterVal
        endif
     else
        call report_error(__LINE__, "Invalid request in get_neigh_Rij", -6)
        get_neigh_Rij = KIM_STATUS_ATOM_INVALID_ID ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        call report_error(__LINE__, "Invalid request in get_neigh_Rij", -1)
        get_neigh_Rij = KIM_STATUS_ATOM_INVALID_ID
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     call report_error(__LINE__, "Invalid mode in get_neigh_Rij", -2)
     get_neigh_Rij = KIM_STATUS_NEIGH_INVALID_MODE
     return
  endif

  ! set the returned atom
  atom = atomToReturn
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1,atom)
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList(2,atom))
  
  ! set pointer to Rij to appropriate value
  pRij = loc(RijList(3*(NNeighbors+1)*(atom-1) + 1))
  
  get_neigh_Rij = KIM_STATUS_OK
  return
end function get_neigh_Rij
