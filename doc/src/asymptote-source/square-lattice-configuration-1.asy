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
