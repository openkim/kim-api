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
// Copyright (c) 2013--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.1.3 package.
//


//settings.outformat = "pdf";
size(20cm);

int maxrow = 5;
int maxcol = 10;

real circleSize = 0.075pt;
pen p = black+2;

int particleID = 0;

for (int i=0; i < maxcol; ++i)
{
  for (int j=0; j<maxrow; ++j)
  {
    filldraw(circle((i,j), circleSize), p);
    if ((i < maxcol-1) && (j < maxrow-1))
    {
      filldraw(circle((i+0.5,j+0.5), circleSize), p);
    }
  }
}

draw(brace((0,maxrow-0.75), (maxcol-1,maxrow-0.75), amplitude=0.5),
     L="$T_p$", N);
