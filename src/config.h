#ifndef __CONFIG_H__
#define __CONFIG_H__

/* Favour accuracy over speed */
#define OPT_ACCURACY 1

/* Disable assert() and Co. */
/* #define NDEBUG */

/* Select fixed point routines based on architecture.
 * 
 * We generally favor accuracy over speed and therefore try to avoid
 * the default implementation (FPM_DEFAULT) if at all possible.
 */
#if !defined(FPM_INTEL) && !defined(FPM_64BIT) && !defined(FPM_SPARC) && !defined(FPM_DEFAULT)
#  if defined(__LONG_LONG_MAX__) && __LONG_LONG_MAX__ >= 9223372036854775807LL
#    define FPM_64BIT
#  elif defined(__amd64__) 
#     define FPM_64BIT
#  elif defined(__WIN64)
#     define FPM_64BIT
#  elif defined(__WIN32)
/* This relies on the fact that R under Windows will be built using a
 * recent GCC.
 */
#    define FPM_64BIT
#  elif defined(__i386__)
#    define FPM_INTEL
#  elif defined(__sparc__)
#    define FPM_SPARC
#  else
#    define FPM_DEFAULT
#  endif
#endif

#endif
