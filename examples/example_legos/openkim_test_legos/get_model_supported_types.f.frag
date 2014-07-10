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
integer(c_int) :: maxStringLength

!-- KIM variables
character(len=KIM_KEY_STRING_LENGTH) :: species
type(c_ptr) pkim

! Initialize error flag
ier = KIM_STATUS_FAIL

! Generate empty KIM object containing generic model info
ier = kim_api_model_info(pkim, modelname)
if (ier.lt.KIM_STATUS_OK) return

! Get species supported by the model
ier = kim_api_get_num_model_species(pkim, num_types, maxStringLength)
if (ier.lt.KIM_STATUS_OK) return
if (num_types.gt.max_types) return
do i=1,num_types
   ier = kim_api_get_model_species(pkim, i, species)
   if (ier.lt.KIM_STATUS_OK) return
   model_types(i) = trim(species)
enddo

ier = KIM_STATUS_OK
return

end subroutine Get_Model_Supported_Types
