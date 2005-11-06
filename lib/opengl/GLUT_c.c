/* Glut-export.c */
#include "platform.h"
#include "GLUT_h.h"

int callGlutCreateMenu ()
{
  return glutCreateMenu (glutCreateMenuArgument);
}

void callGlutDisplayFunc ()
{
  glutDisplayFunc (glutDisplayFuncArgument);
}

void callGlutReshapeFunc ()
{
  glutReshapeFunc (glutReshapeFuncArgument);
}

void callGlutKeyboardFunc ()
{
  glutKeyboardFunc (glutKeyboardFuncArgument);
}

void callGlutMouseFunc ()
{
  glutMouseFunc (glutMouseFuncArgument);
}


void callGlutSpecialFunc ()
{
  glutSpecialFunc (glutSpecialFuncArgument);
}

void callGlutIdleFunc ()
{
  glutIdleFunc (glutIdleFuncArgument);
}

void callGlutVisibilityFunc ()
{
  glutVisibilityFunc (glutVisibilityFuncArgument);
}

void callGlutInit ()
{
  int argc = 1;
  char *args[]={"Glut",""};
  glutInit ( &argc, args );
}


Pointer mlton_glut_stroke_roman(void)
{
  return ((Pointer) GLUT_STROKE_ROMAN);
}
Pointer mlton_glut_stroke_mono_roman(void)
{
  return ((Pointer) GLUT_STROKE_MONO_ROMAN);
}
            
Pointer mlton_glut_bitmap_9_by_15(void)
{
  return ((Pointer) GLUT_BITMAP_9_BY_15);
}
Pointer mlton_glut_bitmap_8_by_13(void)
{
  return ((Pointer) GLUT_BITMAP_8_BY_13);
}
Pointer mlton_glut_bitmap_times_roman_10(void)
{
  return ((Pointer) GLUT_BITMAP_TIMES_ROMAN_10);
}
Pointer mlton_glut_bitmap_times_roman_24(void)
{
  return ((Pointer) GLUT_BITMAP_TIMES_ROMAN_24);
}
Pointer mlton_glut_bitmap_helvetica_10(void)
{
  return ((Pointer) GLUT_BITMAP_HELVETICA_10);
}
Pointer mlton_glut_bitmap_helvetica_12(void)
{
  return ((Pointer) GLUT_BITMAP_HELVETICA_12);
}
Pointer mlton_glut_bitmap_helvetica_18(void)
{
  return ((Pointer) GLUT_BITMAP_HELVETICA_18);
}
