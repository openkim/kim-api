
/*******************************************************************************
*
*  Release: This file is part of the openkim-api.git repository.
* 
*  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
*  All rights reserved.
* 
*  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
*
*******************************************************************************/

/* Note: All STATUS codes associated with an error must be less than KIM_STATUS_OK */

#ifndef _KIMSERVICESTATUS_H
#define _KIMSERVICESTATUS_H

#define KIM_STATUS_NEIGH_INVALID_REQUEST           -12
#define KIM_STATUS_NEIGH_HALF_METHOD_NOT_PROVIDED  -11
#define KIM_STATUS_NEIGH_FULL_METHOD_NOT_PROVIDED  -10
#define KIM_STATUS_NEIGH_TOO_MANY_NEIGHBORS         -9 
#define KIM_STATUS_API_OBJECT_INVALID               -8
#define KIM_STATUS_ARG_INVALID_SHAPE                -7
#define KIM_STATUS_NEIGH_INVALID_MODE               -6
#define KIM_STATUS_ATOM_TYPES_UNDEFINED             -5
#define KIM_STATUS_ARG_INVALID_RANK                 -4
#define KIM_STATUS_ATOM_INVALID_ID                  -3
#define KIM_STATUS_ATOM_INVALID_TYPE                -2
#define KIM_STATUS_ARG_UNKNOWN                      -1
#define KIM_STATUS_FAIL                              0
#define KIM_STATUS_OK                                1
#define KIM_STATUS_NEIGH_ITER_PAST_END               2
#define KIM_STATUS_NEIGH_ITER_INIT_OK                3

#endif
