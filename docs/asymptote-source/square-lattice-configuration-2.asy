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
