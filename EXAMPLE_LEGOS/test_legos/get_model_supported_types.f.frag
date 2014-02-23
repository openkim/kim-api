!-------------------------------------------------------------------------------
!
!  Get number and identities of particle types (species) supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Types(modelname, max_types, model_types, &
                                     num_types, ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!-- Transferred variables
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: modelname
integer(c_int),                       intent(in)   :: max_types
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_types(max_types)
integer(c_int),                       intent(out)  :: num_types
integer(c_int),                       intent(out)  :: ier

!-- Local variables
integer(c_int) :: i

!-- KIM variables
character(len=KIM_KEY_STRING_LENGTH), pointer :: types(:); type(c_ptr) :: ptypes
type(c_ptr) pkim

! Initialize error flag
ier = KIM_STATUS_OK

! Generate empty KIM object containing generic model info
ier = kim_api_model_info(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Get species supported by the model
ptypes = kim_api_get_model_partcl_typs(pkim,num_types,ier)
if (ier.lt.KIM_STATUS_OK) return
if (num_types.gt.max_types) then
   ier = KIM_STATUS_FAIL
   return
endif
call c_f_pointer(ptypes, types, [num_types])
do i=1,num_types
   model_types(i) = types(i)(1:scan(types(i),char(0))-1)
enddo

! free temporary storage
call KIM_API_c_free(ptypes); types => null()
call kim_api_free(pkim,ier)

return

end subroutine Get_Model_Supported_Types
