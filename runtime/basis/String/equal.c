#include "mlton-basis.h"
#include "my-lib.h"

Int String_equal(Pointer s1, Pointer s2) {
	if (s1 == s2) return TRUE;
	else {
		int n1, n2;

		n1 = Array_numElements(s1);
		n2 = Array_numElements(s2);
		if (n1 != n2) 
			return FALSE;
		else {
			int i;	
			for(i = 0; i < n1 ; ++i)
				if (*(s1 + i) != *(s2 + i)) 
					return FALSE;
		}
	}
	
	return TRUE;
}
