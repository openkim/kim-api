!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2014, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Andreas Klemenz
!    Lars Pastewka
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


#include "KIM_API_status.h"

module kim_api_f03
  use, intrinsic :: iso_c_binding
  implicit none
  integer, parameter :: KIM_KEY_STRING_LENGTH = 128
  interface
    subroutine kim_api_c_free(cp) bind(c, name="free")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), value :: cp
    end subroutine kim_api_c_free

    subroutine kim_api_allocate(kimmdl, nparts, nspecies, error) &
      bind(c, name="KIM_API_allocate")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: nparts
      integer(c_int), value :: nspecies
      integer(c_int)        :: error
    end subroutine kim_api_allocate

    subroutine kim_api_free(kimmdl, error) bind(c, name="KIM_API_free")
      use, intrinsic :: iso_c_binding
      type(c_ptr)    :: kimmdl
      integer(c_int) :: error
    end subroutine kim_api_free

    subroutine kim_api_print(kimmdl, error) bind(c, name="KIM_API_print")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end subroutine kim_api_print

    integer(c_int) function kim_api_model_compute(kimmdl) &
      bind(c, name="KIM_API_model_compute")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_compute

    integer(c_int) function kim_api_model_destroy(kimmdl) &
      bind(c, name="KIM_API_model_destroy")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_destroy

    integer(c_int) function kim_api_get_model_index_shift(kimmdl) &
      bind(c, name="KIM_API_get_model_index_shift")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_get_model_index_shift

    subroutine kim_api_set_model_buffer(kimmdl, ob, ier) &
      bind(c, name="KIM_API_set_model_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_model_buffer

    subroutine kim_api_set_sim_buffer(kimmdl, ob, ier) &
      bind(c, name="KIM_API_set_sim_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      type(c_ptr),   value :: ob
      integer(c_int)       :: ier
    end subroutine kim_api_set_sim_buffer

    type(c_ptr) function kim_api_get_model_buffer(kimmdl, ier) &
      bind(c, name="KIM_API_get_model_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_model_buffer

    type(c_ptr) function kim_api_get_sim_buffer(kimmdl, ier) &
      bind(c, name="KIM_API_get_sim_buffer")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: ier
    end function kim_api_get_sim_buffer

    integer(c_int) function kim_api_is_half_neighbors(kimmdl, error) &
      bind(c, name="KIM_API_is_half_neighbors")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_is_half_neighbors

    integer(c_int) function kim_api_model_init(kimmdl) &
      bind(c, name="KIM_API_model_init")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_init

    integer(c_int) function kim_api_model_reinit(kimmdl) &
      bind(c, name="KIM_API_model_reinit")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: kimmdl
    end function kim_api_model_reinit

    integer(c_int) function kim_api_get_num_model_species(kimmdl, &
      numberSpecies, maxStringLength) &
      bind(c, name="KIM_API_get_num_model_species")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: numberSpecies
      integer(c_int)       :: maxStringLength
    end function kim_api_get_num_model_species

    integer(c_int) function kim_api_get_num_sim_species(kimmdl, &
      numberSpecies, maxStringLength) &
      bind(c, name="KIM_API_get_num_sim_species")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: numberSpecies
      integer(c_int)       :: maxStringLength
    end function kim_api_get_num_sim_species

    integer(c_int) function kim_api_get_num_params(kimmdl, &
      numberParameters, maxStringLength) &
      bind(c, name="KIM_API_get_num_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: numberParameters
      integer(c_int)       :: maxStringLength
    end function kim_api_get_num_params

    integer(c_int) function kim_api_get_num_free_params(kimmdl, &
      numberFreeParameters, maxStringLength) &
      bind(c, name="KIM_API_get_num_free_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: numberFreeParameters
      integer(c_int)       :: maxStringLength
    end function kim_api_get_num_free_params

    integer(c_int) function kim_api_get_num_fixed_params(kimmdl, &
      numberFixedParameters, maxStringLength) &
      bind(c, name="KIM_API_get_num_fixed_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: numberFixedParameters
      integer(c_int)       :: maxStringLength
    end function kim_api_get_num_fixed_params

    type(c_ptr) function kim_api_get_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_params

    type(c_ptr) function kim_api_get_free_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_free_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_free_params

    type(c_ptr) function kim_api_get_fixed_params(kimmdl, nvpar, error) &
      bind(c, name="KIM_API_get_fixed_params")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: nvpar
      integer(c_int)       :: error
    end function kim_api_get_fixed_params

    integer(c_int) function kim_api_get_neigh_mode(kimmdl, error) &
      bind(c, name="KIM_API_get_neigh_mode")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_neigh_mode

    integer(c_int) function kim_api_get_neigh(kimmdl, mode, request, &
      part, numnei, nei1part, rij) &
      bind(c, name="KIM_API_get_neigh")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: mode
      integer(c_int), value :: request
      integer(c_int)        :: part
      integer(c_int)        :: numnei
      type(c_ptr)           :: nei1part
      type(c_ptr)           :: rij
    end function kim_api_get_neigh

    integer(c_int) function kim_api_process_dEdr(kimmdl, dE, dr, dx, i, j) &
      bind(c, name="KIM_API_process_dEdr")
      use, intrinsic :: iso_c_binding
      type(c_ptr)           :: kimmdl
      real(c_double)        :: dE
      real(c_double)        :: dr
      type(c_ptr)           :: dx
      integer(c_int)        :: i
      integer(c_int)        :: j
    end function kim_api_process_dEdr

    integer(c_int) function kim_api_process_d2Edr2(kimmdl, dE, dr, dx, i, j) &
      bind(c, name="KIM_API_process_d2Edr2")
      use, intrinsic :: iso_c_binding
      type(c_ptr)           :: kimmdl
      real(c_double)        :: dE
      type(c_ptr)           :: dr
      type(c_ptr)           :: dx
      type(c_ptr)           :: i
      type(c_ptr)           :: j
    end function kim_api_process_d2Edr2

    integer(c_int) function kim_api_get_unit_handling(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_handling")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_handling

    type(c_ptr) function kim_api_get_unit_length(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_length")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_length

    type(c_ptr) function kim_api_get_unit_energy(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_energy")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_energy

    type(c_ptr) function kim_api_get_unit_charge(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_charge")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_charge

    type(c_ptr) function kim_api_get_unit_temperature(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_temperature")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_temperature

    type(c_ptr) function kim_api_get_unit_time(kimmdl, error) &
      bind(c, name="KIM_API_get_unit_time")
      use, intrinsic :: iso_c_binding
      type(c_ptr),   value :: kimmdl
      integer(c_int)       :: error
    end function kim_api_get_unit_time

    type(c_ptr) function kim_api_get_data_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_data_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_data_by_index

    type(c_funptr) function kim_api_get_method_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_method_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_method_by_index

    integer(c_intptr_t) function kim_api_get_size_by_index(kimmdl, I, &
      error) &
      bind(c, name="KIM_API_get_size_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_size_by_index

    integer(c_intptr_t) function kim_api_get_rank_by_index(kimmdl, I, &
      error) &
      bind(c, name="KIM_API_get_rank_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_rank_by_index

    subroutine kim_api_set_compute_by_index(kimmdl, I, flag, error) &
      bind(c, name="KIM_API_set_compute_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int), value :: flag
      integer(c_int)        :: error
    end subroutine kim_api_set_compute_by_index

    integer(c_int) function kim_api_get_compute_by_index(kimmdl, I, error) &
      bind(c, name="KIM_API_get_compute_by_index")
      use, intrinsic :: iso_c_binding
      type(c_ptr),    value :: kimmdl
      integer(c_int), value :: I
      integer(c_int)        :: error
    end function kim_api_get_compute_by_index
  end interface

contains

  integer(c_int) function kim_api_file_init(kimmdl, simkimfile, mdlname)
    use :: kim_api_f03_helper, only : c_call => kim_api_file_init
    implicit none
    type(c_ptr),      intent(out) :: kimmdl
    character(len=*), intent(in)  :: simkimfile
    character(len=*), intent(in)  :: mdlname

    kim_api_file_init = c_call(kimmdl, trim(simkimfile)//c_null_char, &
      trim(mdlname)//c_null_char)
  end function kim_api_file_init

  integer(c_int) function kim_api_string_init(kimmdl, simdescriptor, modelname)
    use :: kim_api_f03_helper, only : c_call => kim_api_string_init
    implicit none
    type(c_ptr),      intent(out) :: kimmdl
    character(len=*), intent(in)  :: simdescriptor
    character(len=*), intent(in)  :: modelname

    kim_api_string_init = c_call(kimmdl, &
      trim(simdescriptor)//c_null_char, trim(modelname)//c_null_char)
  end function kim_api_string_init

  integer(c_int) function kim_api_model_info(kimmdl, modelname)
    use :: kim_api_f03_helper, only : c_call => kim_api_model_info
    implicit none
    type(c_ptr),      intent(out) :: kimmdl
    character(len=*), intent(in)  :: modelname

    kim_api_model_info = c_call(kimmdl, &
      trim(modelname)//c_null_char)
  end function kim_api_model_info

  integer(c_int) function kim_api_get_nbc_method(kimmdl, NBC_String)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_nbc_method
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(out) :: NBC_String

    type(c_ptr) :: pNBC_String
    character(len=len(NBC_String)), pointer :: fpNBC_String
    integer(c_int) :: nullindex

    kim_api_get_nbc_method = c_call(kimmdl, pNBC_String)
    if (kim_api_get_nbc_method.lt.KIM_STATUS_OK) return
    call c_f_pointer(pNBC_String, fpNBC_String)
    nullindex = scan(fpNBC_String, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_nbc_method = KIM_STATUS_FAIL
    else
      NBC_String = fpNBC_String(1:nullindex)
    endif
    return
  end function kim_api_get_nbc_method

  integer(c_int) function kim_api_get_model_species(kimmdl, index, &
    speciesString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_model_species
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    integer(c_int),   intent(in)  :: index
    character(len=*), intent(out) :: speciesString

    type(c_ptr) :: pspeciesString
    character(len=len(speciesString)), pointer :: fpSpeciesString
    integer(c_int) :: nullindex

    kim_api_get_model_species = c_call(kimmdl, index-1, pSpeciesString)
    if (kim_api_get_model_species.lt.KIM_STATUS_OK) return
    call c_f_pointer(pSpeciesString, fpSpeciesString)
    nullindex = scan(fpSpeciesString, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_model_species = KIM_STATUS_FAIL
    else
      speciesString = fpSpeciesString(1:nullindex)
    endif
    return
  end function kim_api_get_model_species

  integer(c_int) function kim_api_get_sim_species(kimmdl, index, &
    speciesString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_sim_species
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    integer(c_int),   intent(in)  :: index
    character(len=*), intent(out) :: speciesString

    type(c_ptr) :: pspeciesString
    character(len=len(speciesString)), pointer :: fpSpeciesString
    integer(c_int) :: nullindex

    kim_api_get_sim_species = c_call(kimmdl, index-1, pSpeciesString)
    if (kim_api_get_sim_species.lt.KIM_STATUS_OK) return
    call c_f_pointer(pSpeciesString, fpSpeciesString)
    nullindex = scan(fpSpeciesString, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_sim_species = KIM_STATUS_FAIL
    else
      speciesString = fpSpeciesString(1:nullindex)
    endif
    return
  end function kim_api_get_sim_species

  integer(c_int) function kim_api_get_parameter(kimmdl, index, parameterString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_parameter
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    integer(c_int),   intent(in)  :: index
    character(len=*), intent(out) :: parameterString

    type(c_ptr) :: pParameterString
    character(len=len(parameterString)), pointer :: fpParameterString
    integer(c_int) :: nullindex

    kim_api_get_parameter = c_call(kimmdl, index-1, pParameterString)
    if (kim_api_get_parameter.lt.KIM_STATUS_OK) return
    call c_f_pointer(pParameterString, fpParameterString)
    nullindex = scan(fpParameterString, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_parameter = KIM_STATUS_FAIL
    else
      parameterString = fpParameterString(1:nullindex)
    endif
    return
  end function kim_api_get_parameter

  integer(c_int) function kim_api_get_free_parameter(kimmdl, index, &
    freeParameterString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_free_parameter
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    integer(c_int),   intent(in)  :: index
    character(len=*), intent(out) :: freeParameterString

    type(c_ptr) :: pFreeParameterString
    character(len=len(freeParameterString)), pointer :: fpFreeParameterString
    integer(c_int) :: nullindex

    kim_api_get_free_parameter = c_call(kimmdl, index-1, pFreeParameterString)
    if (kim_api_get_free_parameter.lt.KIM_STATUS_OK) return
    call c_f_pointer(pFreeParameterString, fpFreeParameterString)
    nullindex = scan(fpFreeParameterString, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_free_parameter = KIM_STATUS_FAIL
    else
      freeParameterString = fpFreeParameterString(1:nullindex)
    endif
    return
  end function kim_api_get_free_parameter

  integer(c_int) function kim_api_get_fixed_parameter(kimmdl, index, &
    fixedParameterString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_fixed_parameter
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    integer(c_int),   intent(in)  :: index
    character(len=*), intent(out) :: fixedParameterString

    type(c_ptr) :: pFixedParameterString
    character(len=len(fixedParameterString)), pointer :: fpFixedParameterString
    integer(c_int) :: nullindex

    kim_api_get_fixed_parameter = c_call(kimmdl, index-1, pFixedParameterString)
    if (kim_api_get_fixed_parameter.lt.KIM_STATUS_OK) return
    call c_f_pointer(pFixedParameterString, fpFixedParameterString)
    nullindex = scan(fpFixedParameterString, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_fixed_parameter = KIM_STATUS_FAIL
    else
      fixedParameterString = fpFixedParameterString(1:nullindex)
    endif
    return
  end function kim_api_get_fixed_parameter

  integer(c_int) function kim_api_set_data(kimmdl, nm, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_data
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: size
    type(c_ptr),      intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_data = c_call(kimmdl, trim(nm)//c_null_char, &
      size_int, dt)
  end function kim_api_set_data

  integer(c_int) function kim_api_set_method(kimmdl, nm, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_method
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: size
    type(c_funptr),   intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_method = c_call(kimmdl, &
      trim(nm)//c_null_char, size_int, dt)
  end function kim_api_set_method

  type(c_ptr) function kim_api_get_data(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_data
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_data = c_call(kimmdl, trim(nm)//c_null_char, &
      error)
  end function kim_api_get_data

  type(c_funptr) function kim_api_get_method(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_method
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_method = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_method

  integer(c_intptr_t) function kim_api_get_size(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_size
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_size = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_size

  integer(c_intptr_t) function kim_api_get_rank(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_rank
    implicit none
    type(c_ptr),      intent(in) :: kimmdl
    character(len=*), intent(in) :: nm
    integer(c_int),   intent(in) :: error

    kim_api_get_rank = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_rank

  subroutine kim_api_get_shape(kimmdl, nm, shape, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_shape
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: shape(*)
    integer(c_int),   intent(out) :: error
    integer(c_intptr_t)           :: rank
    integer(c_int),   allocatable :: shape_tmp(:)
    integer(c_intptr_t)           :: i

    rank = c_call(kimmdl, trim(nm)//c_null_char, shape, error)

    allocate(shape_tmp(rank))
    do i=1,rank
      shape_tmp(i) = shape(rank - i + 1)  ! transpose shape
    end do
    shape(1:rank) = shape_tmp(1:rank)
    deallocate(shape_tmp)
  end subroutine kim_api_get_shape

  subroutine kim_api_set_shape(kimmdl, nm, shape, rank, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_shape
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(in)  :: shape(*)
    integer(c_int),   intent(in)  :: rank
    integer(c_int),   intent(out) :: error
    integer(c_int) :: shape_t(rank)
    integer(c_int) :: i

    if (rank .gt. 1) then
      do i=1,rank
        shape_t(i) = shape(rank - i + 1)  ! transpose shape
      end do
    end if

    call c_call(kimmdl, trim(nm)//c_null_char, shape_t, rank, error)
  end subroutine kim_api_set_shape

  subroutine kim_api_set_compute(kimmdl, nm, flag, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_compute
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(in)  :: flag
    integer(c_int),   intent(out) :: error

    call c_call(kimmdl, trim(nm)//c_null_char, flag, error)
  end subroutine kim_api_set_compute

  integer(c_int) function kim_api_get_compute(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_compute
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_compute = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_compute

  integer(c_int) function kim_api_get_index(kimmdl, nm, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_index
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: nm
    integer(c_int),   intent(out) :: error

    kim_api_get_index = c_call(kimmdl, trim(nm)//c_null_char, error)
  end function kim_api_get_index

  integer(c_int) function kim_api_get_species_code(kimmdl, particle, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_species_code
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer(c_int),   intent(out) :: error

    kim_api_get_species_code = c_call(kimmdl, trim(particle)//c_null_char, &
      error)
  end function kim_api_get_species_code

  subroutine kim_api_set_species_code(kimmdl, particle, code, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_species_code
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: particle
    integer(c_int),   intent(in)  :: code
    integer(c_int),   intent(out) :: error

    call c_call(kimmdl, trim(particle)//c_null_char, code, error)
  end subroutine kim_api_set_species_code

  integer(c_int) function kim_api_get_model_kim_str(modelname, kimString)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_model_kim_str
    implicit none
    character(len=*), intent(in)  :: modelname
    character(len=*), intent(out) :: kimString

    type(c_ptr)                            :: pkimString
    character(len=len(kimString)), pointer :: fpKIM_String
    integer(c_int) :: nullindex

    kim_api_get_model_kim_str = c_call(trim(modelname)//c_null_char, pkimString)
    if (kim_api_get_model_kim_str.lt.KIM_STATUS_OK) return
    call c_f_pointer(pkimString, fpKIM_String)
    nullindex = scan(fpKIM_String, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_model_kim_str = KIM_STATUS_FAIL
    else
      kimString = fpKIM_String(1:nullindex)
    endif
    return
  end function kim_api_get_model_kim_str

  integer(c_int) function kim_api_get_status_msg(status_code, status_msg)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_status_msg
    implicit none
    integer(c_int),   intent(in)  :: status_code
    character(len=*), intent(out) :: status_msg

    type(c_ptr) :: pstatus_msg
    character(len=len(status_msg)), pointer :: fpstatus_msg
    integer(c_int) :: nullindex

    kim_api_get_status_msg = c_call(status_code, pstatus_msg)
    if (kim_api_get_status_msg.lt.KIM_STATUS_OK) return
    call c_f_pointer(pstatus_msg, fpstatus_msg)
    nullindex = scan(fpstatus_msg, char(0))-1
    if (nullindex.eq.0) then
      kim_api_get_status_msg = KIM_STATUS_FAIL
    else
      status_msg = fpstatus_msg(1:nullindex)
    endif
    return
  end function kim_api_get_status_msg

  integer(c_int) function kim_api_report_error(ln, fl, usermsg, status_code)
    use :: kim_api_f03_helper, only : c_call => kim_api_report_error
    implicit none
    integer(c_int),   intent(in) :: ln
    character(len=*), intent(in) :: fl
    character(len=*), intent(in) :: usermsg
    integer(c_int),   intent(in) :: status_code

    kim_api_report_error = c_call(ln, trim(fl)//c_null_char, &
      trim(usermsg)//c_null_char, status_code)
  end function kim_api_report_error

  real(c_double) function kim_api_get_scale_conversion(u_from, u_to, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_scale_conversion
    implicit none
    character(len=*), intent(in)  :: u_from
    character(len=*), intent(in)  :: u_to
    integer(c_int),   intent(out) :: error

    kim_api_get_scale_conversion = &
      c_call(trim(u_from)//c_null_char, trim(u_to)//c_null_char, error)
  end function kim_api_get_scale_conversion

  real(c_double) function kim_api_convert_to_act_unit( &
    kimmdl, &
    length, &
    energy, &
    charge, &
    temperature, &
    time, &
    length_exponent, &
    energy_exponent, &
    charge_exponent, &
    temperature_exponent, &
    time_exponent, &
    error)
    use :: kim_api_f03_helper, only : c_call => kim_api_convert_to_act_unit
    implicit none
    type(c_ptr),      intent(in)  :: kimmdl
    character(len=*), intent(in)  :: length
    character(len=*), intent(in)  :: energy
    character(len=*), intent(in)  :: charge
    character(len=*), intent(in)  :: temperature
    character(len=*), intent(in)  :: time
    real(c_double),   intent(in)  :: length_exponent
    real(c_double),   intent(in)  :: energy_exponent
    real(c_double),   intent(in)  :: charge_exponent
    real(c_double),   intent(in)  :: temperature_exponent
    real(c_double),   intent(in)  :: time_exponent
    integer(c_int),   intent(out) :: error

    kim_api_convert_to_act_unit = c_call( &
      kimmdl, &
      trim(length)//c_null_char, &
      trim(energy)//c_null_char, &
      trim(charge)//c_null_char, &
      trim(temperature)//c_null_char, &
      trim(time)//c_null_char, &
      length_exponent, &
      energy_exponent, &
      charge_exponent, &
      temperature_exponent, &
      time_exponent, &
      error &
      )
  end function kim_api_convert_to_act_unit

  integer(c_int) function kim_api_set_data_by_index(kimmdl, I, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_data_by_index
    implicit none
    type(c_ptr),    intent(in) :: kimmdl
    integer(c_int), intent(in) :: I
    integer(c_int), intent(in) :: size
    type(c_ptr),    intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_data_by_index = c_call(kimmdl, I, size_int, dt)
  end function kim_api_set_data_by_index

  integer(c_int) function kim_api_set_method_by_index(kimmdl, I, size, dt)
    use :: kim_api_f03_helper, only : c_call => kim_api_set_method_by_index
    implicit none
    type(c_ptr),    intent(in) :: kimmdl
    integer(c_int), intent(in) :: I
    integer(c_int), intent(in) :: size
    type(c_funptr), intent(in) :: dt
    integer(c_intptr_t) :: size_int

    size_int = size
    kim_api_set_method_by_index = c_call(kimmdl, I, size_int, dt)
  end function kim_api_set_method_by_index


  subroutine kim_api_get_shape_by_index(kimmdl, I, shape, error)
    use :: kim_api_f03_helper, only : c_call => kim_api_get_shape_by_index
    implicit none
    type(c_ptr),        intent(in)  :: kimmdl
    integer(c_int),     intent(in)  :: I
    integer(c_int),     intent(out) :: shape(*)
    integer(c_int),     intent(out) :: error
    integer(c_intptr_t)             :: rank
    integer, allocatable            :: shape_tmp(:)
    integer(c_intptr_t)             :: j

    rank = c_call(kimmdl, I, shape, error)

    allocate(shape_tmp(rank))
    do j=1,rank
      shape_tmp(j) = shape(rank - j + 1)  ! transpose shape
    end do
    shape(1:rank) = shape_tmp(1:rank)
    deallocate(shape_tmp)
  end subroutine kim_api_get_shape_by_index

#include "KIM_API_F03_Multiples.incl"

end module kim_api_f03
