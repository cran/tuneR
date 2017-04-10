#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rinternals.h>
#include "pulse.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

extern SEXP do_read_mp3(SEXP s_blob);

static R_NativePrimitiveArgType pulsewav_t[] = {
  INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP
};

static const R_CMethodDef cMethods[] = {
  {"pulsewav", (DL_FUNC) &pulsewav, 6, pulsewav_t},
  {NULL, NULL, 0, NULL}
};

static const R_CallMethodDef R_CallDef[]  = {
  CALLDEF(do_read_mp3, 1),
  {NULL, NULL, 0}
};

void R_init_tuneR(DllInfo *dll)
{
    R_registerRoutines(dll, cMethods, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
