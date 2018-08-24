/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2018, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_DATA_TYPE_H_
#define KIM_DATA_TYPE_H_

struct KIM_DataType
{
  int dataTypeID;
};
#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
typedef struct KIM_DataType KIM_DataType;
#endif

KIM_DataType KIM_DataType_FromString(char const * const str);

int KIM_DataType_Equal(KIM_DataType const left, KIM_DataType const right);
int KIM_DataType_NotEqual(KIM_DataType const left, KIM_DataType const right);
char const * KIM_DataType_String(KIM_DataType const dataType);

extern KIM_DataType const KIM_DATA_TYPE_Integer;
extern KIM_DataType const KIM_DATA_TYPE_Double;

void KIM_DATA_TYPE_GetNumberOfDataTypes(int * const numberOfDataTypes);
int KIM_DATA_TYPE_GetDataType(int const index, KIM_DataType * const dataType);

#endif  /* KIM_DATA_TYPE_H_ */
