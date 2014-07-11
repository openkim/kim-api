!-------------------------------------------------------------------------------
!
!  Get number and identities of particle species supported by
!  KIM Model `modelname'
!
!-------------------------------------------------------------------------------
subroutine Get_Model_Supported_Species(modelname, max_species, model_species, &
                                       num_species, ier)
use, intrinsic :: iso_c_binding
use KIM_API_F03
implicit none

!-- Transferred variables
character(len=KIM_KEY_STRING_LENGTH), intent(in)   :: modelname
integer(c_int),                       intent(in)   :: max_species
character(len=KIM_KEY_STRING_LENGTH), intent(out)  :: model_species(max_species)
integer(c_int),                       intent(out)  :: num_species
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
ier = kim_api_get_num_model_species(pkim, num_species, maxStringLength)
if (ier.lt.KIM_STATUS_OK) return
if (num_species.gt.max_species) return
do i=1,num_species
   ier = kim_api_get_model_species(pkim, i, species)
   if (ier.lt.KIM_STATUS_OK) return
   model_species(i) = trim(species)
enddo

ier = KIM_STATUS_OK
return

end subroutine Get_Model_Supported_Species
