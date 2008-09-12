C+ NCLINI.FOR
C  WNB 910809
C
C  Revisions:
C
	SUBROUTINE NCLINI
C
C  Initialize NCLEAN program
C
C  Result:
C
C	CALL NCLINI	will set header lines, logging, DWARF interface
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NCL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDINI			!INIT DWARF
	LOGICAL WNDDAB			!OPEN DATABASE
C
C  Data declarations:
C
C-
C
C SET HEADER LINES
C
	CALL WNCFHD(F_P,1,'!40C\Program to clean MAP files')
C
C START DWARF
C
	IF (.NOT.WNDINI(PRGNAM)) CALL WNGEX	!EXIT IF NO DWARF START
C
C LOGGING
C
	CALL WNDLOG(LOGCD)			!PROPER LOGGING
C
C DATABASE
C
	IF (.NOT.WNDDAB()) CALL WNGEX		!OPEN DATABASE
C
	RETURN					!READY
C
C
	END
