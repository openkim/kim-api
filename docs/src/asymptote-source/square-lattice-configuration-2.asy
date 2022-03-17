//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api-2.3.0 package.
//


//settings.outformat = "pdf";
size(20cm);

int maxrow = 5;
int maxcol = 10;

real circleSize = 0.075pt;
pen p = black+2;

for (int i=0; i < maxcol; ++i)
{
  for (int j=0; j<maxrow; ++j)
  {
    if (i < maxcol/2)
    {
      filldraw(circle((i,j), circleSize), p);
      if ((i < maxcol-1) && (j < maxrow-1))
        filldraw(circle((i+0.5,j+0.5), circleSize), p);
    }
    else
    {
      draw(circle((i,j), circleSize), p);
      if ((i < maxcol-1) && (j < maxrow-1))
        draw(circle((i+0.5,j+0.5), circleSize), p);
    }
  }
}

draw(brace((0,maxrow), (maxcol-1,maxrow), amplitude=0.5),
     L="$T_p$", N);
draw(brace((0,maxrow-0.75), (maxcol/2-0.5,maxrow-0.75), amplitude=0.5),
     L="$A_p$", N);
draw(brace((maxcol/2,maxrow-0.75), (maxcol-1,maxrow-0.75), amplitude=0.5),
     L="$B_p$", N);
