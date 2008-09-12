C+ NGEM.FOR
C  JPH 931108
C
C  Revisions:
C	HjV 931214	Change test logical functions
C
	SUBROUTINE NGEN
C
C Dummy program to allow setting NGEN parameters through DWEXE /NORUN/SAVE 
C
C  Result:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDINI
	LOGICAL WNDDAB
C
C  Data declarations:
C
C-
C
C PRELIMINARIES
C
	IF (.NOT.WNDINI(PRGNAM)) CALL WNGEX	!Exit if no DWARF start
	CALL WNDLON(I)
	IF (.NOT.WNDDAB()) CALL WNGEX		!Open database
 	CALL WNGEX
	RETURN
	END
