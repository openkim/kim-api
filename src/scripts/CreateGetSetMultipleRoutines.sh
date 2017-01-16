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
printf "subroutine kim_api_getm_%s( &\n" $subject
printf "  kimmdl, error, &\n"
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do
printf "  id$i,  dt$i,  k$i, &\n"
done
printf "  id15, dt15, k15 &\n"
printf "  )\n"
printf "  use :: kim_api_f03_helper, only : errcheck_mltpl_%s\n" $idtype
printf "  implicit none\n"
printf "  type(c_ptr) :: kimmdl\n"
printf "  integer(c_int) :: error\n"
printf "  %s(%s) :: id1\n" $idtype $idkind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), optional :: id$i\n" $idtype $idkind
done
printf "  %s(%s) :: dt1\n" $dttype $dtkind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), optional :: dt$i\n" $dttype $dtkind
done
printf "  integer(c_int) :: k1\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), optional :: k$i\n"
done
printf "  character(len=40) :: msg="'"'"kim_api_getm_%s"'"' $subject
printf "\n"
printf "  if ((k1 .ne. 0) .and. (k1 .ne. 1)) then\n"
printf "    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY\n"
printf "    if (errcheck_mltpl_%s(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1, id1) &\n" $idtype
printf "      .lt. KIM_STATUS_OK) return\n"
printf "  end if\n"
printf "  if (k1 .eq. 1) then\n"
if test "x$subroutine" = "xtrue"; then
printf "    call kim_api_get_%s(kimmdl, id1, dt1, error)\n" $subject
else
printf "    dt1 = kim_api_get_%s(kimmdl, id1, error);\n" $subject
fi
printf "    if (errcheck_mltpl_%s(error, msg, 1, id1) .lt. KIM_STATUS_OK) return\n" $idtype
printf "  end if\n"
printf "\n"
printf "  !check rest of the arguments\n"
printf "  error = KIM_STATUS_WRONG_MULTIPLE_ARGS\n"
printf "  if (present(id2) .and. (.not.present(dt2))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, 2, id2) .lt. KIM_STATUS_OK) return\n" $idtype
for i in 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  else if (present(id$i) .and. (.not.present(dt$i))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, $i, id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "  end if\n"
printf "\n"
printf "  error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY\n"
printf "  if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, 2, id2) .lt. KIM_STATUS_OK) return\n" $idtype
for i in 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  else if (present(k$i) .and. ((k$i .ne. 0) .and. (k$i .ne. 1))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, $i, id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "  end if\n"
printf "\n"
printf "  !process arguments\n"
printf "  error=KIM_STATUS_OK\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  if (present(id$i) .and. (k$i .eq. 1)) then\n"
if test "x$subroutine" = "xtrue"; then
printf "    call kim_api_get_%s(kimmdl, id$i, dt$i, error)\n" $subject
else
printf "    dt$i = kim_api_get_%s(kimmdl, id$i, error);\n" $subject
fi
printf "  end if\n"
printf "  if (errcheck_mltpl_%s(error, msg, $i, id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "end subroutine kim_api_getm_%s\n" $subject
printf "\n"
}

createSetM()
{
printf "subroutine kim_api_setm_%s( &\n" $subject
printf "  kimmdl, error, &\n"
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14; do
printf "  id$i, ${hasSize:+sz$i,} dt$i,  k$i, &\n"
done
printf "  id15, ${hasSize:+sz15,} dt15,  k15  &\n"
printf "  )\n"
printf "  use :: kim_api_f03_helper, only : errcheck_mltpl_%s\n" $idtype
printf "  implicit none\n"
printf "  type(c_ptr) :: kimmdl\n"
printf "  integer(c_int) :: error\n"
printf "  %s(%s) :: id1\n" $idtype $idkind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), optional :: id$i\n" $idtype $idkind
done
if test "x$hasSize" != "x"; then
printf "  integer(c_int) :: sz1\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), optional :: sz$i\n"
done
fi
printf "  %s(%s) :: dt1\n" $dttype $dtkind
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  %s(%s), optional :: dt$i\n" $dttype $dtkind
done
printf "  integer(c_int) :: k1\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  integer(c_int), optional :: k$i\n"
done
printf "  character(len=40) :: msg="'"'"kim_api_setm_%s"'"' $subject
printf "\n"
printf "  if ((k1 .ne. 0) .and. (k1 .ne. 1)) then\n"
printf "    error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY\n"
printf "    if (errcheck_mltpl_%s(KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY, msg, 1,id1) &\n" $idtype
printf "      .lt. KIM_STATUS_OK) return\n"
printf "  end if\n"
printf "  if (k1 .eq. 1) then\n"
if test "x$subroutine" = "xtrue"; then
printf "    call kim_api_set_%s(kimmdl, id1, dt1, error)\n" $subject
else
printf "    error = kim_api_set_%s(kimmdl, id1, sz1, dt1);\n" $subject
fi
printf "    if (errcheck_mltpl_%s(error, msg, 1, id1) .lt. KIM_STATUS_OK) return\n" $idtype
printf "  end if\n"
printf "\n"
printf "  !check rest of the arguments\n"
printf "  error = KIM_STATUS_WRONG_MULTIPLE_ARGS\n"
printf "  if (present(id2) .and. &\n"
printf "    (${hasSize:+.not.present(sz2) .or.} .not. present(dt2) .or. .not.present(k2))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, 2, id2) .lt. KIM_STATUS_OK) return\n" $idtype
for i in 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  else if (present(id$i) .and. &\n"
printf "    (${hasSize:+.not.present(sz$i) .or.} .not.present(dt$i) .or. .not.present(k$i))) then\n"
printf "    if (errcheck_mltpl_%s(error, msg, $i, id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "  end if\n"
printf "\n"
printf "  error = KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY\n"
printf "  if (present(k2) .and. ((k2 .ne. 0) .and. (k2 .ne. 1)))then\n"
printf "    if (errcheck_mltpl_%s(error, msg, 2,  id2) .lt. KIM_STATUS_OK) return\n" $idtype
for i in 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  else if(present(k$i) .and. ((k$i .ne. 0) .and. (k$i .ne. 1)))then\n"
printf "    if  (errcheck_mltpl_%s(error, msg, $i,  id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "  end if\n"
printf "\n"
printf "  !process arguments\n"
printf "  error = KIM_STATUS_OK\n"
for i in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
printf "  if (present(id$i) .and. (k$i .eq. 1)) then\n"
if test "x$subroutine" = "xtrue"; then
printf "    call kim_api_set_%s(kimmdl, id$i, dt$i, error)\n" $subject
else
printf "    error = kim_api_set_%s(kimmdl, id$i, sz$i, dt$i);\n" $subject
fi
printf "  end if\n"
printf "  if (errcheck_mltpl_%s(error, msg, $i, id$i) .lt. KIM_STATUS_OK) return\n" $idtype
done
printf "end subroutine kim_api_setm_%s\n" $subject
printf "\n"
}

subject="index" # only getm (no setm)
idtype="character"
idkind='len=*'
dttype="integer"
dtkind="c_int"
hasSize='yes'
subroutine=false
createGetM

subject="data"
idtype="character"
idkind='len=*'
dttype="type"
dtkind="c_ptr"
hasSize='yes'
subroutine=false
createGetM
createSetM

subject="data_by_index"
idtype="integer"
idkind="c_int"
dtkind="type"
dtkind="c_ptr"
hasSize='yes'
subroutine=false
createGetM
createSetM

# subroutine & no size argument in set
subject="compute"
idtype="character"
idkind='len=*'
dttype="integer"
dtkind="c_int"
hasSize='yes'
subroutine=false
createGetM
unset hasSize
subroutine=true
createSetM

#subroutine & no size argument in set
subject="compute_by_index"
idtype="integer"
idkind="c_int"
dttype="integer"
dtkine="c_int"
hasSize='yes'
subroutine=false
createGetM
unset hasSize
subroutine=true
createSetM

subject="method"
idtype="character"
idkind='len=*'
dttype="type"
dtkind="c_funptr"
hasSize='yes'
subroutine=false
createGetM
createSetM

subject="method_by_index"
idtype="integer"
idkind="c_int"
dttype="type"
dtkind="c_funptr"
hasSize='yes'
subroutine=false
createGetM
createSetM
