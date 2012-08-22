!-------------------------------------------------------------------------------
!
!  Get number and identities of particle types (species) supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Types(modelname, max_types, model_types, &
                                     num_types, ier)
use KIM_API
implicit none

!-- Transferred variables
character*80,                         intent(in)   :: modelname
integer,                              intent(in)   :: max_types
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_types(max_types)
integer,                              intent(out)  :: num_types
integer,                              intent(out)  :: ier

!-- Local variables
integer :: i

!-- KIM variables
character(len=KIM_KEY_STRING_LENGTH) types(1); pointer(ptypes,types)
integer(kind=kim_intptr) pkim

! Initialize error flag
ier = KIM_STATUS_OK

! Generate empty KIM object containing generic model info
ier = kim_api_model_info_f(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Get species supported by the model
ptypes = kim_api_get_model_partcl_typs_f(pkim,num_types,ier)
if (ier.lt.KIM_STATUS_OK) return
if (num_types.gt.max_types) then
   ier = KIM_STATUS_FAIL
   return
endif
do i=1,num_types
   model_types(i) = types(i)(1:scan(types(i),char(0))-1)
enddo

! free temporary storage
call free(ptypes)
call kim_api_free_f(pkim,ier)

return

end subroutine Get_Model_Supported_Types
