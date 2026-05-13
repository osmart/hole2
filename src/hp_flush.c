#include <stdio.h>
/* flush callable from Fortran - gfortran uses trailing underscore */
void flush_(void)
{
  fflush(stdout);
}
/* Also provide without underscore for compatibility */
void flush(void)
{
  fflush(stdout);
}
