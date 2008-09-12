c+ NMOINI.FOR
C  WNB 900327
C
C  Revisions:
C
	SUBROUTINE NMOINI
C
C  Initialize NMODEL program
C
C  Result:
C
C	CALL NMOINI	will set header lines, logging, DWARF interface
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
	LOGICAL WNDINI			!INIT DWARF
	LOGICAL WNDDAB			!OPEN DATABASE
	LOGICAL NMOSLI			!GET SOURCE LIST
C
C  Data declarations:
C
C-
C
C SET HEADER LINES
C
	CALL WNCFHD(F_P,1,'!40C\Program to handle Model (MDL) files')
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
C SOURCE LIST
C
	IF (.NOT.NMOSLI(1024)) CALL WNGEX	!GET SOURCE LIST
C
	RETURN					!READY
C
C
	END
