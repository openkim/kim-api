#!/bin/sh
#

#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2014--2017, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

createGetM()
{
printf "subroutine kim_utility_compute_getm_%s( &\n" $subject
printf "  simulator, ierr, &\n"
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do
printf "  argument_name_%02i, ${hasLanguage:+language_name_%02i,} value_%02i, flag_%02i, &\n" $i ${hasLanguage:+$i} $i $i
done
printf "  argument_name_15, ${hasLanguage:+language_name_15,} value_15, flag_15 &\n"
printf "  )\n"
printf "  use, intrinsic :: iso_c_binding\n"
printf "  use :: kim_simulator_module\n"
printf "  use :: kim_compute_module\n"
printf "  implicit none\n"
printf "  type(kim_simulator_type), intent(in) :: simulator\n"
printf "  integer(c_int), intent(out) :: ierr\n"
printf "  %s(%s), intent(in) :: argument_name_01\n" $argumentNameType $argumentNameKind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(in), optional :: argument_name_%02i\n" $argumentNameType $argumentNameKind $i
done
if test "x$hasLanguage" != "x"; then
printf "  %s(%s), intent(in) :: language_name_01\n" $languageNameType $languageNameKind
  for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(in), optional :: language_name_%02i\n" $languageNameType $languageNameKind $i
  done
fi
printf "  %s(%s), intent(out) :: value_01\n" $valueType $valueKind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(out), optional :: value_%02i\n" $valueType $valueKind $i
done
printf "  integer(c_int), intent(in) :: flag_01\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), intent(in), optional :: flag_%02i\n" $i
done
printf "\n"
printf "  if (flag_01 == 1) then\n"
printf "    call kim_simulator_get_%s(simulator, argument_name_01, ${hasLanguage:+language_name_01,} value_01, ierr)\n" $subject
printf "    if (ierr /= 0) return\n"
printf "  end if\n"
printf "\n"
printf "  !process arguments\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  if (present(argument_name_%02i)) then\n" $i
printf "    if (flag_%02i .eq. 1) then\n" $i
printf "      call kim_simulator_get_%s(simulator, argument_name_%02i, ${hasLanguage:+language_name_%02i,} value_%02i, ierr)\n" $subject $i ${hasLanguage:+$i} $i
printf "      if (ierr /= 0) return\n"
printf "    end if\n"
printf "  end if\n"
done
printf "end subroutine kim_utility_compute_getm_%s\n" $subject
printf "\n"
}

createSetM()
{
printf "subroutine kim_utility_compute_setm_%s( &\n" $subject
printf "  model, ierr, &\n"
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do
printf "  argument_name_%02i, ${hasExtent:+extent_%02i,} ${hasLanguage:+language_name_%02i,} value_%02i, flag_%02i, &\n" $i ${hasExtent:+$i} ${hasLanguage:+$i} $i $i
done
printf "  argument_name_15, ${hasExtent:+extent_15,} ${hasLanguage:+language_name_15,} value_15, flag_15  &\n"
printf "  )\n"
printf "  use, intrinsic :: iso_c_binding\n"
printf "  use kim_model_module\n"
printf "  use kim_compute_module\n"
printf "  ${hasLanguage:+use kim_language_name_module}\n"
printf "  implicit none\n"
printf "  type(kim_model_type), intent(inout) :: model\n"
printf "  integer(c_int), intent(out) :: ierr\n"
printf "  %s(%s), intent(in) :: argument_name_01\n" $argumentNameType $argumentNameKind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(in), optional :: argument_name_%02i\n" $argumentNameType $argumentNameKind $i
done
if test "x$hasExtent" != "x"; then
printf "  integer(c_int), intent(in) :: extent_01\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), intent(in), optional :: extent_%02i\n" $i
done
fi
if test "x$hasLanguage" != "x"; then
printf "  %s(%s), intent(in) :: language_name_01\n" $languageNameType $languageNameKind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(in), optional :: language_name_%02i\n" $languageNameType $languageNameKind $i
done
fi
printf "  %s(%s), intent(in) :: value_01\n" $valueType $valueKind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), intent(in), optional :: value_%02i\n" $valueType $valueKind $i
done
printf "  integer(c_int), intent(in) :: flag_01\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), intent(in), optional :: flag_%02i\n" $i
done
printf "\n"
printf "  if (flag_01 == 1) then\n"
printf "    call kim_model_set_%s(model, argument_name_01, ${hasExtent:+extent_01,} ${hasLanguage:+language_name_01,} value_01, ierr)\n" $subject
printf "    if (ierr /= 0) return\n"
printf "  end if\n"
printf "\n"
printf "  !process arguments\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  if (present(argument_name_%02i)) then\n" $i
printf "    if (flag_%02i .eq. 1) then\n" $i
printf "      call kim_model_set_%s(model, argument_name_%02i, ${hasExtent:+extent_%02i,} ${hasLanguage:+language_name_%02i,} value_%02i, ierr)\n" $subject $i ${hasExtent:+$i} ${hasLanguage:+$i} $i
printf "      if (ierr /= 0) return\n"
printf "    end if\n"
printf "  end if\n"
done
printf "end subroutine kim_utility_compute_setm_%s\n" $subject
printf "\n"
}

printf "module kim_utility_compute_module\n"
printf "use, intrinsic :: iso_c_binding\n"
printf "implicit none\n"
printf "public\n"
printf "contains\n"

subject="data"
argumentNameType="type"
argumentNameKind="kim_compute_argument_name_type"
valueType="type"
valueKind="c_ptr"
hasExtent="yes"
createGetM
createSetM

subject="method"
argumentNameType="type"
argumentNameKind="kim_compute_argument_name_type"
valueType="type"
valueKind="c_funptr"
hasExtent="yes"
hasLanguage="yes"
languageNameType="type"
languageNameKind="kim_language_name_type"
createSetM

subject="compute"
argumentNameType="type"
argumentNameKind="kim_compute_argument_name_type"
valueType="integer"
valueKind="c_int"
unset hasExtent
unset hasLanguage
createGetM
createSetM

printf "end module kim_utility_compute_module\n"
