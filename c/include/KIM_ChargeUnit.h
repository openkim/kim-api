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
/* Copyright (c) 2016--2019, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_CHARGE_UNIT_H_
#define KIM_CHARGE_UNIT_H_

/**
 ** \brief \copybrief KIM::ChargeUnit
 **
 ** \sa KIM::ChargeUnit
 **
 ** \since 2.0
 **/
struct KIM_ChargeUnit
{
  /**
   ** \brief \copybrief KIM::ChargeUnit::chargeUnitID
   **
   ** \sa KIM::ChargeUnit::chargeUnitID
   **
   ** \since 2.0
   **/
  int chargeUnitID;
};
#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
/**
 ** \brief Convenience typedef.
 **
 ** \since 2.0
 **/
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

/**
 ** \brief \copybrief KIM::ChargeUnit::ChargeUnit(std::string const &)
 **
 ** \sa KIM::ChargeUnit::ChargeUnit(std::string const &)
 **
 ** \since 2.0
 **/
KIM_ChargeUnit KIM_ChargeUnit_FromString(char const * const str);

/**
 ** \brief \copybrief KIM::ChargeUnit::Known
 **
 ** \sa KIM::ChargeUnit::Known
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_Known(KIM_ChargeUnit const chargeUnit);

/**
 ** \brief \copybrief KIM::ChargeUnit::operator==()
 **
 ** \sa KIM::ChargeUnit::operator==()
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_Equal(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs);

/**
 ** \brief \copybrief KIM::ChargeUnit::operator!=()
 **
 ** \sa KIM::ChargeUnit::operator!=()
 **
 ** \since 2.0
 **/
int KIM_ChargeUnit_NotEqual(KIM_ChargeUnit const lhs, KIM_ChargeUnit const rhs);

/**
 ** \brief \copybrief KIM::ChargeUnit::ToString
 **
 ** \sa KIM::ChargeUnit::ToString
 **
 ** \since 2.0
 **/
char const * KIM_ChargeUnit_ToString(KIM_ChargeUnit const chargeUnit);

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::unused
 **
 ** \sa KIM::CHARGE_UNIT::unused
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_unused;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::C
 **
 ** \sa KIM::CHARGE_UNIT::C
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_C;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::e
 **
 ** \sa KIM::CHARGE_UNIT::e
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_e;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::statC
 **
 ** \sa KIM::CHARGE_UNIT::StatC
 **
 ** \since 2.0
 **/
extern KIM_ChargeUnit const KIM_CHARGE_UNIT_statC;

/**
 ** \brief \copybrief KIM::CHARGE_UNIT::GetNumberOfChargeUnits
 **
 ** \sa KIM::CHARGE_UNIT::GetNumberOfChargeUnits
 **
 ** \since 2.0
 **/
void KIM_CHARGE_UNIT_GetNumberOfChargeUnits(int * const numberOfChargeUnits);

/**
 ** \brief \brief \copybrief KIM::CHARGE_UNIT::GetChargeUnit
 **
 ** \sa KIM::CHARGE_UNIT::GetChargeUnit
 **
 ** \since 2.0
 **/
int KIM_CHARGE_UNIT_GetChargeUnit(int const index,
                                  KIM_ChargeUnit * const chargeUnit);

#endif /* KIM_CHARGE_UNIT_H_ */
