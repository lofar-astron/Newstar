C+ WNPEXH.FOR
C  WNB 911126
C
C  Revisions:
C
	SUBROUTINE WNPEXH
C
C  Exit handler for WNP routines
C
C  Result:
C
C	CALL WNPEXH		Close and dispose all plot files
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
C
C  Data declarations:
C
C-
	CALL WQCLOS				!CLOSE ALL FILES
C
	RETURN
C
C
	END
