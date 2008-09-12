C+ WNCEXH.FOR
C  WNB 890202
C
C  Revisions:
C
	SUBROUTINE WNCEXH
C
C  Exit handler for WNC routines
C
C  Result:
C
C	CALL WNCEXH		Close and dispose all printfiles
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
C
C  Equivalences:
C
C
C  Commons:
C
C-
	CALL WNCFCL(F_ALL)			!CLOSE ALL FILES
C
	RETURN
C
C
	END

