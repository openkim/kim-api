
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

#ifndef _KIM_API_STATUS_H
#define _KIM_API_STATUS_H

#define KIM_STATUS_INCONSISTENT_BASE_UNIT          -23
#define KIM_STATUS_UNSUPPORTED_UNIT_TIME           -22
#define KIM_STATUS_UNSUPPORTED_UNIT_TEMPERATURE    -21
#define KIM_STATUS_UNSUPPORTED_UNIT_CHARGE         -20
#define KIM_STATUS_UNSUPPORTED_UNIT_ENERGY         -19
#define KIM_STATUS_UNSUPPORTED_UNIT_LENGTH         -18
#define KIM_STATUS_WRONG_UNIT_HANDLING             -17

#define KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY        -16
#define KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4      -15
#define KIM_STATUS_WRONG_MULTIPLE_ARGS             -14
#define KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_2      -13
#define KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3      -12
#define KIM_STATUS_NEIGH_INVALID_REQUEST           -11
#define KIM_STATUS_NEIGH_METHOD_NOT_PROVIDED       -10
#define KIM_STATUS_NEIGH_TOO_MANY_NEIGHBORS         -9 
#define KIM_STATUS_API_OBJECT_INVALID               -8
#define KIM_STATUS_ARG_INVALID_SHAPE                -7
#define KIM_STATUS_NEIGH_INVALID_MODE               -6
#define KIM_STATUS_PARTICLE_TYPES_UNDEFINED         -5
#define KIM_STATUS_ARG_INVALID_RANK                 -4
#define KIM_STATUS_PARTICLE_INVALID_ID              -3
#define KIM_STATUS_PARTICLE_INVALID_TYPE            -2
#define KIM_STATUS_ARG_UNKNOWN                      -1
#define KIM_STATUS_FAIL                              0
#define KIM_STATUS_OK                                1
#define KIM_STATUS_NEIGH_ITER_PAST_END               2
#define KIM_STATUS_NEIGH_ITER_INIT_OK                3


#define KIM_COMPUTE                    1
#define KIM_NOT_COMPUTE                0

#endif /* _KIM_API_STATUS_H */
