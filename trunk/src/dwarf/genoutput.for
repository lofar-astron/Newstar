C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENSW_OUTPUT
C.Keywords:	Terminal output
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	SUN
C.Comments:
C.Version:	920316 GvD - Created
C		JPH 951101 - GEN_FLUSH calls for batch applications
C		HjV 960618 - Use TFLUSH iso. GEN_FLUSH
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE GEN_OUTPUT (TEXT)
C	     ENTRY GEN_OUTPUT_NOCR (TEXT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	TEXT		! (i) text to be written
C
C.Purpose:	Force immediate write of a text to the terminal
C		GEN_OUTPUT_NOCR does the same without carriage-return.
C.Returns:	not applicable
C.Notes:
C-------------------------------------------------------------------------
C
	PRINT 1010,TEXT
	CALL TFLUSH()
1010	FORMAT (A)
	RETURN
C
	ENTRY GEN_OUTPUT_NOCR (TEXT)
	PRINT 1000,TEXT
	CALL TFLUSH()
1000	FORMAT (A,$)
	RETURN
C
	END
