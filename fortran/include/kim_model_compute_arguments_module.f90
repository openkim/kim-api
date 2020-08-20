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
! Copyright (c) 2016--2020, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!

!> \brief \copybrief KIM::ModelComputeArguments
!!
!! \sa KIM::ModelComputeArguments, KIM_ModelComputeArguments
!!
!! \since 2.0
module kim_model_compute_arguments_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_compute_arguments_handle_type, &
    ! Constants
    KIM_MODEL_COMPUTE_ARGUMENTS_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_get_neighbor_list, &
    kim_process_dedr_term, &
    kim_process_d2edr2_term, &
    kim_get_argument_pointer, &
    kim_is_callback_present, &
    kim_set_model_buffer_pointer, &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string

  !> \brief \copybrief KIM::ModelComputeArguments
  !!
  !! \sa KIM::ModelComputeArguments, KIM_ModelComputeArguments
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_compute_arguments_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_compute_arguments_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_compute_arguments_handle_type), protected, save &
    :: KIM_MODEL_COMPUTE_ARGUMENTS_NULL_HANDLE

  !> \brief Compares kim_model_compute_arguments_handle_type's for
  !! equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_model_compute_arguments_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_model_compute_arguments_handle_type's for
  !! inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_model_compute_arguments_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ModelComputeArguments::GetNeighborList
  !!
  !! \sa KIM::ModelComputeArguments::GetNeighborList,
  !! KIM_ModelComputeArguments_GetNeighborList
  !!
  !! \since 2.0
  interface kim_get_neighbor_list
    module procedure kim_model_compute_arguments_get_neighbor_list
  end interface kim_get_neighbor_list

  !> \brief \copybrief KIM::ModelComputeArguments::ProcessDEDrTerm
  !!
  !! \sa KIM::ModelComputeArguments::ProcessDEDrTerm,
  !! KIM_ModelComputeArguments_ProcessDEDrTerm
  !!
  !! \since 2.0
  interface kim_process_dedr_term
    module procedure kim_model_compute_arguments_process_dedr_term
  end interface kim_process_dedr_term

  !> \brief \copybrief KIM::ModelComputeArguments::ProcessD2EDr2Term
  !!
  !! \sa KIM::ModelComputeArguments::ProcessD2EDr2Term,
  !! KIM_ModelComputeArguments_ProcessD2EDr2Term
  !!
  !! \since 2.0
  interface kim_process_d2edr2_term
    module procedure kim_model_compute_arguments_process_d2edr2_term
  end interface kim_process_d2edr2_term

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerInteger,
  !! KIM_ModelComputeArguments_GetArgumentPointerDouble
  !!
  !! \since 2.0
  interface kim_get_argument_pointer
    module procedure kim_model_compute_arguments_get_argument_pointer_int0
    module procedure kim_model_compute_arguments_get_argument_pointer_int1
    module procedure kim_model_compute_arguments_get_argument_pointer_int2
    module procedure kim_model_compute_arguments_get_argument_pointer_double0
    module procedure kim_model_compute_arguments_get_argument_pointer_double1
    module procedure kim_model_compute_arguments_get_argument_pointer_double2
  end interface kim_get_argument_pointer

  !> \brief \copybrief KIM::ModelComputeArguments::IsCallbackPresent
  !!
  !! \sa KIM::ModelComputeArguments::IsCallbackPresent,
  !! KIM_ModelComputeArguments_IsCallbackPresent
  !!
  !! \since 2.0
  interface kim_is_callback_present
    module procedure kim_model_compute_arguments_is_callback_present
  end interface kim_is_callback_present

  !> \brief \copybrief KIM::ModelComputeArguments::SetModelBufferPointer
  !!
  !! \sa KIM::ModelComputeArguments::SetModelBufferPointer,
  !! KIM_ModelComputeArguments_SetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_set_model_buffer_pointer
    module procedure kim_model_compute_arguments_set_model_buffer_pointer
  end interface kim_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelComputeArguments::GetModelBufferPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetModelBufferPointer,
  !! KIM_ModelComputeArguments_GetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_get_model_buffer_pointer
    module procedure kim_model_compute_arguments_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelComputeArguments::LogEntry
  !!
  !! \sa KIM::ModelComputeArguments::LogEntry,
  !! KIM_ModelComputeArguments_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_compute_arguments_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelComputeArguments::ToString
  !!
  !! \sa KIM::ModelComputeArguments::ToString,
  !! KIM_ModelComputeArguments_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_compute_arguments_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_compute_arguments_handle_type's for
  !! equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_compute_arguments_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_model_compute_arguments_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_compute_arguments_handle_equal = .true.
    else
      kim_model_compute_arguments_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_compute_arguments_handle_equal

  !> \brief Compares kim_model_compute_arguments_handle_type's for
  !! inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_compute_arguments_handle_not_equal(lhs, &
                                                                          rhs)
    implicit none
    type(kim_model_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_model_compute_arguments_handle_type), intent(in) :: rhs

    kim_model_compute_arguments_handle_not_equal = .not. (lhs == rhs)
  end function kim_model_compute_arguments_handle_not_equal

  !> \brief \copybrief KIM::ModelComputeArguments::GetNeighborList
  !!
  !! A Fortran PM must provide a KIM::MODEL_ROUTINE_NAME::GetNeighbotList
  !! routine.  The interface for this is given here (see also
  !! KIM::GetNeighborListFunction, \ref KIM_GetNeighborListFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine get_neighbor_list(data_object, &
  !!     number_of_neighbor_lists, cutoffs, neighbor_list_index, &
  !!     particle_number, number_of_neighbors, neighbors_of_particle, ierr) &
  !!     bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     implicit none
  !!     type(c_ptr), intent(in), value :: data_object
  !!     integer(c_int), intent(in), value :: number_of_neighbor_lists
  !!     real(c_double), intent(in) :: cutoffs(*)
  !!     integer(c_int), intent(in), value :: neighbor_list_index
  !!     integer(c_int), intent(in), value :: particle_number
  !!     integer(c_int), intent(out) :: number_of_neighbors
  !!     type(c_ptr), intent(out) :: neighbors_of_particle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine get_neighbor_ilst
  !! end interface
  !! \endcode
  !!
  !! \note The use of the "assumed size" type for `cutoffs` above is necessary
  !! for strict conformance to the Fortran/C interoperability standard.  The
  !! cutoffs array is expected to be of shape \c [number_of_neighbor_lists].
  !!
  !! \sa KIM::ModelComputeArguments::GetNeighborList,
  !! KIM_ModelComputeArguments_GetNeighborList
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_get_neighbor_list( &
    model_compute_arguments_handle, neighbor_list_index, particle_number, &
    number_of_neighbors, neighbors_of_particle, ierr)
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_neighbor_list( &
        model_compute_arguments, neighbor_list_index, particle_number, &
        number_of_neighbors, neighbors_of_particle) &
        bind(c, name="KIM_ModelComputeArguments_GetNeighborList")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        integer(c_int), intent(in), value :: neighbor_list_index
        integer(c_int), intent(in), value :: particle_number
        integer(c_int), intent(out) :: number_of_neighbors
        type(c_ptr), intent(out) :: neighbors_of_particle
      end function get_neighbor_list
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    integer(c_int), intent(in) :: neighbor_list_index
    integer(c_int), intent(in) :: particle_number
    integer(c_int), intent(out) :: number_of_neighbors
    integer(c_int), intent(out), pointer :: neighbors_of_particle(:)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_neighbor_list(model_compute_arguments, neighbor_list_index - 1, &
                             particle_number, number_of_neighbors, p)
    if (c_associated(p)) then
      call c_f_pointer(p, neighbors_of_particle, [number_of_neighbors])
    else
      nullify (neighbors_of_particle)
    end if
  end subroutine kim_model_compute_arguments_get_neighbor_list

  !> \brief \copybrief KIM::ModelComputeArguments::ProcessDEDrTerm
  !!
  !! A Fortran PM may provide a KIM::MODEL_ROUTINE_NAME::ProcessDEDrTerm
  !! routine.  The interface for this is given here (see also
  !! KIM::ProcessDEDrTermFunction, \ref KIM_ProcessDEDrTermFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine process_dedr_term(data_object, de, r, dx, i, j, &
  !!     ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     implicit none
  !!     type(c_ptr), intent(in), value :: data_object
  !!     real(c_double), intent(in), value :: de
  !!     real(c_double), intent(in), value :: r
  !!     real(c_double), intent(in) :: dx(3)
  !!     integer(c_int), intent(in), value :: i
  !!     integer(c_int), intent(in), value :: j
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine process_dedr_term
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::ModelComputeArguments::ProcessDEDrTerm,
  !! KIM_ModelComputeArguments_ProcessDEDrTerm
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_process_dedr_term( &
    model_compute_arguments_handle, de, r, dx, i, j, ierr)
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function process_dedr_term( &
        model_compute_arguments, de, r, dx, i, j) &
        bind(c, name="KIM_ModelComputeArguments_ProcessDEDrTerm")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        real(c_double), intent(in), value :: de
        real(c_double), intent(in), value :: r
        real(c_double), intent(in) :: dx
        integer(c_int), intent(in), value :: i
        integer(c_int), intent(in), value :: j
      end function process_dedr_term
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    real(c_double), intent(in) :: de
    real(c_double), intent(in) :: r
    real(c_double), intent(in) :: dx(:)
    integer(c_int), intent(in) :: i
    integer(c_int), intent(in) :: j
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = process_dedr_term(model_compute_arguments, de, r, dx(1), i, j)
  end subroutine kim_model_compute_arguments_process_dedr_term

  !> \brief \copybrief KIM::ModelComputeArguments::ProcessD2EDr2Term
  !!
  !! A Fortran PM may provide a KIM::MODEL_ROUTINE_NAME::ProcessD2EDr2Term
  !! routine.  The interface for this is given here (see also
  !! KIM::ProcessD2EDr2TermFunction, \ref KIM_ProcessD2EDr2TermFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine process_d2edr2_term(data_object, de, r, dx, i, j, &
  !!     ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     implicit none
  !!     type(c_ptr), intent(in), value :: data_object
  !!     real(c_double), intent(in), value :: de
  !!     real(c_double), intent(in) :: r(2)
  !!     real(c_double), intent(in) :: dx(3,2)
  !!     integer(c_int), intent(in) :: i(2)
  !!     integer(c_int), intent(in) :: j(2)
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine process_d2edr2_term
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::ModelComputeArguments::ProcessD2EDr2Term,
  !! KIM_ModelComputeArguments_ProcessD2EDr2Term
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_process_d2edr2_term( &
    model_compute_arguments_handle, de, r, dx, i, j, ierr)
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function process_d2edr2_term( &
        model_compute_arguments, de, r, dx, i, j) &
        bind(c, name="KIM_ModelComputeArguments_ProcessD2EDr2Term")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        real(c_double), intent(in), value :: de
        real(c_double), intent(in) :: r
        real(c_double), intent(in) :: dx
        integer(c_int), intent(in) :: i
        integer(c_int), intent(in) :: j
      end function process_d2edr2_term
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    real(c_double), intent(in) :: de
    real(c_double), intent(in) :: r(:)
    real(c_double), intent(in) :: dx(:, :)
    integer(c_int), intent(in) :: i(:)
    integer(c_int), intent(in) :: j(:)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = process_d2edr2_term(model_compute_arguments, &
                               de, r(1), dx(1, 1), i(1), j(1))
  end subroutine kim_model_compute_arguments_process_d2edr2_term

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_get_argument_pointer_int0( &
    model_compute_arguments_handle, compute_argument_name, int0, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_integer( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerInteger")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_integer
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(out), pointer :: int0
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_integer(model_compute_arguments, &
                                        compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, int0)
    else
      nullify (int0)
    end if
  end subroutine kim_model_compute_arguments_get_argument_pointer_int0

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_get_argument_pointer_int1( &
    model_compute_arguments_handle, compute_argument_name, extent1, int1, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_integer( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerInteger")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_integer
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(out), pointer :: int1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_integer(model_compute_arguments, &
                                        compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, int1, [extent1])
    else
      nullify (int1)
    end if

  end subroutine kim_model_compute_arguments_get_argument_pointer_int1

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_get_argument_pointer_int2( &
    model_compute_arguments_handle, compute_argument_name, extent1, extent2, &
    int2, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_integer( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerInteger")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_integer
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    integer(c_int), intent(out), pointer :: int2(:, :)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_integer(model_compute_arguments, &
                                        compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, int2, [extent1, extent2])
    else
      nullify (int2)
    end if
  end subroutine kim_model_compute_arguments_get_argument_pointer_int2

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine &
    kim_model_compute_arguments_get_argument_pointer_double0( &
    model_compute_arguments_handle, compute_argument_name, double0, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_double( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerDouble")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_double
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(out), pointer :: double0
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_double(model_compute_arguments, &
                                       compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, double0)
    else
      nullify (double0)
    end if
  end subroutine kim_model_compute_arguments_get_argument_pointer_double0

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine &
    kim_model_compute_arguments_get_argument_pointer_double1( &
    model_compute_arguments_handle, compute_argument_name, extent1, double1, &
    ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_double( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerDouble")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_double
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    real(c_double), intent(out), pointer :: double1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_double(model_compute_arguments, &
                                       compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, double1, [extent1])
    else
      nullify (double1)
    end if
  end subroutine kim_model_compute_arguments_get_argument_pointer_double1

  !> \brief \copybrief KIM::ModelComputeArguments::GetArgumentPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetArgumentPointer,
  !! KIM_ModelComputeArguments_GetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine &
    kim_model_compute_arguments_get_argument_pointer_double2( &
    model_compute_arguments_handle, compute_argument_name, extent1, extent2, &
    double2, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_pointer_double( &
        model_compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetArgumentPointerDouble")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(out) :: ptr
      end function get_argument_pointer_double
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    real(c_double), intent(out), pointer :: double2(:, :)
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = get_argument_pointer_double(model_compute_arguments, &
                                       compute_argument_name, p)
    if (c_associated(p)) then
      call c_f_pointer(p, double2, [extent1, extent2])
    else
      nullify (double2)
    end if
  end subroutine kim_model_compute_arguments_get_argument_pointer_double2

  !> \brief \copybrief KIM::ModelComputeArguments::IsCallbackPresent
  !!
  !! \sa KIM::ModelComputeArguments::IsCallbackPresent,
  !! KIM_ModelComputeArguments_IsCallbackPresent
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_is_callback_present( &
    model_compute_arguments_handle, compute_callback_name, present, ierr)
    use kim_compute_callback_name_module, only: kim_compute_callback_name_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function is_callback_present( &
        model_compute_arguments, compute_callback_name, present) &
        bind(c, name="KIM_ModelComputeArguments_IsCallbackPresent")
        use, intrinsic :: iso_c_binding
        use kim_compute_callback_name_module, only: &
          kim_compute_callback_name_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        integer(c_int), intent(out) :: present
      end function is_callback_present
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    integer(c_int), intent(out) :: present
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    ierr = is_callback_present(model_compute_arguments, compute_callback_name, &
                               present)
  end subroutine kim_model_compute_arguments_is_callback_present

  !> \brief \copybrief KIM::ModelComputeArguments::SetModelBufferPointer
  !!
  !! \sa KIM::ModelComputeArguments::SetModelBufferPointer,
  !! KIM_ModelComputeArguments_SetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_set_model_buffer_pointer( &
    model_compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      recursive subroutine set_model_buffer_pointer( &
        model_compute_arguments, ptr) &
        bind(c, name="KIM_ModelComputeArguments_SetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_model_buffer_pointer
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    call set_model_buffer_pointer(model_compute_arguments, ptr)
  end subroutine kim_model_compute_arguments_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelComputeArguments::GetModelBufferPointer
  !!
  !! \sa KIM::ModelComputeArguments::GetModelBufferPointer,
  !! KIM_ModelComputeArguments_GetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_get_model_buffer_pointer( &
    model_compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      recursive subroutine get_model_buffer_pointer( &
        model_compute_arguments, ptr) &
        bind(c, name="KIM_ModelComputeArguments_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    call get_model_buffer_pointer(model_compute_arguments, ptr)
  end subroutine kim_model_compute_arguments_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelComputeArguments::LogEntry
  !!
  !! \sa KIM::ModelComputeArguments::LogEntry,
  !! KIM_ModelComputeArguments_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_log_entry( &
    model_compute_arguments_handle, log_verbosity, message)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      recursive subroutine log_entry(model_compute_arguments, log_verbosity, &
                                     message, line_number, file_name) &
        bind(c, name="KIM_ModelComputeArguments_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    call log_entry(model_compute_arguments, log_verbosity, &
                   trim(message)//c_null_char, 0, ""//c_null_char)
  end subroutine kim_model_compute_arguments_log_entry

  !> \brief \copybrief KIM::ModelComputeArguments::ToString
  !!
  !! \sa KIM::ModelComputeArguments::ToString,
  !! KIM_ModelComputeArguments_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_to_string( &
    model_compute_arguments_handle, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_compute_arguments_type
    implicit none
    interface
      type(c_ptr) recursive function model_compute_string( &
        model_compute_arguments) &
        bind(c, name="KIM_ModelComputeArguments_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: &
          kim_model_compute_arguments_type
        implicit none
        type(kim_model_compute_arguments_type), intent(in) :: &
          model_compute_arguments
      end function model_compute_string
    end interface
    type(kim_model_compute_arguments_handle_type), intent(in) :: &
      model_compute_arguments_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_compute_arguments_type), pointer :: model_compute_arguments

    type(c_ptr) :: p

    call c_f_pointer(model_compute_arguments_handle%p, model_compute_arguments)
    p = model_compute_string(model_compute_arguments)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_compute_arguments_to_string
end module kim_model_compute_arguments_module
