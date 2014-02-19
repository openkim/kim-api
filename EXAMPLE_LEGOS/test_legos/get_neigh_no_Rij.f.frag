!-------------------------------------------------------------------------------
!
! get_neigh_no_Rij neighbor list access function
!
! This function implements Locator and Iterator modes
!
!-------------------------------------------------------------------------------
integer(c_int) function get_neigh_no_Rij(pkim,mode,request,atom,numnei,pnei1atom,pRij) bind(c)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr),    intent(in)  :: pkim
  integer(c_int), intent(in)  :: mode
  integer(c_int), intent(in)  :: request
  integer(c_int), intent(out) :: atom
  integer(c_int), intent(out) :: numnei
  type(c_ptr),    intent(out) :: pnei1atom
  type(c_ptr),    intent(out) :: pRij

  !-- Local variables
  integer(c_int), save :: iterVal = 0
  integer(c_int)  atomToReturn
  integer(c_int), pointer :: neighborList(:,:); type(c_ptr) :: pneighborList
  integer(c_int), pointer :: numberOfParticles; type(c_ptr) :: pnAtoms
  integer(c_int)  N
  integer(c_int)  ier, idum

  ! unpack number of particles
  pnAtoms = kim_api_get_data(pkim, "numberOfParticles", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pnAtoms, numberOfParticles)
  N = numberOfParticles

  ! unpack neighbor list object
  pneighborList = kim_api_get_data(pkim, "neighObject", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pneighborList, neighborList, [N+1,N])

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_neigh_no_Rij = KIM_STATUS_NEIGH_ITER_INIT_OK
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_neigh_no_Rij = KIM_STATUS_NEIGH_ITER_PAST_END
           return
        else
           atomToReturn = iterVal
        endif
     else
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Invalid request in get_neigh_no_Rij", KIM_STATUS_NEIGH_INVALID_REQUEST)
        get_neigh_no_Rij = KIM_STATUS_NEIGH_INVALID_REQUEST
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Invalid atom ID in get_neigh_no_Rij", KIM_STATUS_PARTICLE_INVALID_ID)
        get_neigh_no_Rij = KIM_STATUS_PARTICLE_INVALID_ID
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Invalid mode in get_neigh_no_Rij", KIM_STATUS_NEIGH_INVALID_MODE)
     get_neigh_no_Rij = KIM_STATUS_NEIGH_INVALID_MODE
     return
  endif

  ! set the returned atom
  atom = atomToReturn

  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1,atom)

  ! set the location for the returned neighbor list
  pnei1atom = c_loc(neighborList(2,atom))

  ! set pointer to Rij to NULL
  pRij = c_null_ptr

  get_neigh_no_Rij = KIM_STATUS_OK
  return
end function get_neigh_no_Rij
