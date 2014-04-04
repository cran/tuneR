/*
 * Function to generate variable shape pulses.
 * Copyright (C) 2014 Guillaume Guénard, Université de Montréal
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
# include <Rmath.h>

void pulsewav (int* n, double* width, double* interval, double* plateau, double* x, double* y) {
  unsigned int i;
  double xc, S[7];   // {b,c,d,e,f,g,h}
  // Calculating control points
  S[2] = 0.5 * *width;
  S[3] = (0.5 - S[2]) * *interval + S[2];
  S[6] = S[3] + S[2];
  S[0] = 0.5 * S[2] * (1.0 - *plateau);
  S[1] = S[2] - S[0];
  S[4] = S[3] + S[0];
  S[5] = S[6] - S[0];
  /*for(i=0;i<6;i++)
    printf("%f\t",S[i]);
  printf("\n");*/
  for(i = 0; i < *n; i++) {
    xc = x[i] - floor(x[i]);
    if(xc < S[0])
      y[i] = xc / S[0];
    else if(xc <= S[1])
      y[i] = 1.0;
    else if(xc < S[2])
      y[i] = 1.0 - (xc-S[1])/(S[2]-S[1]);
    else if(xc <= S[3])
      y[i] = 0.0;
    else if(xc < S[4])
      y[i] = -(xc-S[3])/(S[4]-S[3]);
    else if(xc <= S[5])
      y[i] = -1.0;
    else if(xc < S[6])
      y[i] = (xc-S[5])/(S[6]-S[5])-1.0;
    else
      y[i] = 0.0;
  }
  return;
}

