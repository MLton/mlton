/* Glut-export.c */
#include <GL/gl.h>
#include <GL/glut.h>
#include "GLUT_h.h"

void callGlutDisplayFunc ()
{
  glutDisplayFunc (glutDisplayFuncArgument);
}

void callGlutInit ()
{
  int argc = 1;
  char *args[]={"Glut",""};
  glutInit ( &argc, args );
}


void *mlton_glut_stroke_roman(void)
{
  return (GLUT_STROKE_ROMAN);
}
void *mlton_glut_stroke_mono_roman(void)
{
  return (GLUT_STROKE_MONO_ROMAN);
}
	    
void *mlton_glut_bitmap_9_by_15(void)
{
  return (GLUT_BITMAP_9_BY_15);
}
void *mlton_glut_bitmap_8_by_13(void)
{
  return (GLUT_BITMAP_8_BY_13);
}
void *mlton_glut_bitmap_times_roman_10(void)
{
  return (GLUT_BITMAP_TIMES_ROMAN_10);
}
void *mlton_glut_bitmap_times_roman_24(void)
{
  return (GLUT_BITMAP_TIMES_ROMAN_24);
}
void *mlton_glut_bitmap_helvetica_10(void)
{
  return (GLUT_BITMAP_HELVETICA_10);
}
void *mlton_glut_bitmap_helvetica_12(void)
{
  return (GLUT_BITMAP_HELVETICA_12);
}
void *mlton_glut_bitmap_helvetica_18(void)
{
  return (GLUT_BITMAP_HELVETICA_18);
}

