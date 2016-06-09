//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2014--2016, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <cstdio>
#include <cstring>

#define SUCCESS                     0
#define INVALID_NUMBER_OF_ARGUMENTS 1
#define UNKNOWN_OPTION              2

int main(int argc, char* argv[])
{
  int result = SUCCESS;
  if (argc == 2)
  {
    if (!strcmp(argv[1], "--cc"))
    {
      printf(CC_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--cxx"))
    {
      printf(CXX_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--fc"))
    {
      printf(FC_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--ld"))
    {
      printf(LD_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--includes"))
    {
      printf(INCLUDES_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--cflags"))
    {
      printf(CFLAGS_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--cxxflags"))
    {
      printf(CXXFLAGS_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--fflags"))
    {
      printf(FFLAGS_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--ldflags"))
    {
      printf(LDFLAGS_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--ldlibs"))
    {
      printf(LDLIBS_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--objonlyflag"))
    {
      printf(OBJONLYFLAG_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--outputinflag"))
    {
      printf(OUTPUTINFLAG_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--makefile-kim-config"))
    {
      printf(MAKEFILEKIMCONFIG_STRING "\n");
      printf("\n");
      printf(".PHONY: all clean\n");
      printf("\n");
      printf("ITEMS_LIST=$(shell find . -type d -depth 1 -not -name '.*')\n");
      printf("\n");
      printf("all: $(patsubst %%,%%-all,$(ITEMS_LIST))\n");
      printf("clean: $(patsubst %%,%%-clean,$(ITEMS_LIST))\n");
      printf("\n");
      printf("$(patsubst %%,%%-all,$(ITEMS_LIST)): ");
      printf("%%: $(KIM_MAKE_FILES) ...............@%%-making-echo\n");
      printf("	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %%-all,%%,$@) "
             "all\n");
      printf("$(patsubst %%,%%-clean,$(ITEMS_LIST)):\n");
      printf("	$(QUELL)$(MAKE) $(MAKE_FLAGS) -C $(patsubst %%-clean,%%,$@) "
             "clean\n");
      printf("\n\n");
      printf("########### for internal use ###########\n");
      printf("%%-making-echo:\n");
      printf("	@printf '\\n%%79s\\n' ' ' | sed -e 's/ /*/g'\n");
      printf("	@printf '%%-77s%%2s\\n' \"** Building... ");
      printf("`printf '$(patsubst %%-all,%%,$*)' | sed -e 's/@/ /g'`\" ");
      printf("'**'\n");
      printf("	@printf '%%79s\\n' ' ' | sed -e 's/ /*/g'\n");
      return result;
    }
    else if (!strcmp(argv[1], "--version"))
    {
      printf(VERSION_STRING "\n");
      return result;
    }
    else if (!strcmp(argv[1], "--help"))
    {
      // drop through with SUCCESS
    }
    else
    {
      // else drop through with UNKNOWN_OPTION
      result = UNKNOWN_OPTION;
    }
  }
  else
  {
    result = INVALID_NUMBER_OF_ARGUMENTS;
  }

  fprintf(stderr, "Usage: %s option\n", argv[0]);
  fprintf(stderr, "  Options:\n");
  fprintf(stderr, "    --cc\n");
  fprintf(stderr, "    --cxx\n");
  fprintf(stderr, "    --fc\n");
  fprintf(stderr, "    --ld\n");
  fprintf(stderr, "    --includes\n");
  fprintf(stderr, "    --cflags\n");
  fprintf(stderr, "    --cxxflags\n");
  fprintf(stderr, "    --fflags\n");
  fprintf(stderr, "    --ldflags\n");
  fprintf(stderr, "    --ldlibs\n");
  fprintf(stderr, "    --objonlyflag\n");
  fprintf(stderr, "    --outputinflag\n");
  fprintf(stderr, "    --makefile-kim-config\n");
  fprintf(stderr, "    --version\n");
  fprintf(stderr, "    --help\n");
  return result;
}
