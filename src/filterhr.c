#include <R.h>
#include <math.h>

void filterhr(double *hr, int *n, int *lon, int *last, int *minbpm, int *maxbpm, 
              int *index) {
  int i, ulast, umean;
  double *buffer, med, desv(), tmp;
  buffer = (double *) malloc((*lon) * sizeof(double));
  ulast = *last;
  umean = 1.5 * (*last);
  med = buffer[0] = hr[0];
  index[0] = 1;
  for (i = 1; i < (*n) - 1; i++) {
    if (i < (*lon)) {
      med = (med * i + hr[i]) / (i + 1);
      buffer[i] = hr[i];
    } else {
      med = (med * (*lon) + (hr[i] - buffer[i % (*lon)])) / (*lon);
      buffer[i % (*lon)] = hr[i];
    }
    if ((100 * fabs(hr[i] - hr[i - 1]) / hr[i - 1] < ulast || 
        100 * fabs(hr[i] - hr[i + 1]) / hr[i + 1] < ulast ||
        100 * fabs(hr[i] - med) / med < umean) && hr[i] > (*minbpm) && hr[i] < (*maxbpm)) {
      index[i] = 1;
    } else
      index[i] = 0;
    if (i % (*lon) == 0 && i >= (*lon)) {
      tmp = 10 + desv(buffer, med, (*lon));
      if (tmp < 12)
        tmp = 12;
      if (tmp > 20)
        tmp = 20;
      ulast = (int) tmp;
      umean = (int) (1.5 * ulast);
    }
  }
  free(buffer);
}

double desv(double *data, double med, int n) {
  int i;
  double v;
  v = 0;
  for (i = 0; i < n; i++) {
    v += pow((data[i] - med), 2.0);
  }
  return sqrt((double) (v / (double) n));
}
