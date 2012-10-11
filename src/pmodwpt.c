#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <string.h>


void modwtLP(double *Vin, int *N, int *j, int *L, double *ht, double *gt, 
	   double *Wout, double *Vout){
 int k, n, t;
  for(t = 0; t < *N; t++) {
    k = t;
    Vout[t] = gt[0] * Vin[k];
    for(n = 1; n < *L; n++) {
      k -= (int) pow(2.0, (double) *j - 1.0);
      if(k < 0) k += *N;
      Vout[t] += gt[n] * Vin[k];
    }
  }
}



void modwtHP(double *Vin, int *N, int *j, int *L, double *ht, double *gt, 
	   double *Wout, double *Vout){
 int k, n, t;
  for(t = 0; t < *N; t++) {
    k = t;
    Wout[t] = ht[0] * Vin[k];
    for(n = 1; n < *L; n++) {
      k -= (int) pow(2.0, (double) *j - 1.0);
      if(k < 0) k += *N;
      Wout[t] += ht[n] * Vin[k];
    }
  }
}




void modwtBoth(double *Vin, int *N, int *j, int *L, double *ht, double *gt, 
	   double *Wout, double *Vout){
 int k, n, t;

  for(t = 0; t < *N; t++) {
    k = t;
    Wout[t] = ht[0] * Vin[k];
    Vout[t] = gt[0] * Vin[k];
    for(n = 1; n < *L; n++) {
      k -= (int) pow(2.0, (double) *j - 1.0);
      if(k < 0) k += *N;
      Wout[t] += ht[n] * Vin[k];
      Vout[t] += gt[n] * Vin[k];
    }
  }
}



void pmodwpt(double *Vin, int *N, int *j,int *code, int *L, double *ht, double *gt, 
	   double *Wout, double *Vout)
{
//Rprintf("Complete brunch...\n");
if (*code==0){//just the low pass filter
 	modwtLP(Vin,N,j,L,ht,gt,Wout,Vout);
	return;
}
 if (*code==1){//just the high pass filter
	modwtHP(Vin,N,j,L,ht,gt,Wout,Vout);
	return;
}
 if (*code==2){//both filters
	modwtBoth(Vin,N,j,L,ht,gt,Wout,Vout);
	return; 
}


}


