// Output generated with tools::package_native_routine_registration_skeleton(".")
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void filterhr(void *, void *, void *, void *, void *, void *, void *);
extern void pmodwpt(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"filterhr", (DL_FUNC) &filterhr, 7},
  {"pmodwpt",  (DL_FUNC) &pmodwpt,  9},
  {NULL, NULL, 0}
};

void R_init_RHRV(DllInfo *dll) {
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
