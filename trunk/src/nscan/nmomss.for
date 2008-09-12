C+ NMOMSS.FOR
C  WNB 900903
C
C  Revisions:
C
	SUBROUTINE NMOMSS(SETSX)
C
C  Save model data in scan file
C
C  Result:
C
C	CALL NMOMSS( SETSX_J(0:*):I)		saves model data in scan file
C
C  PIN references
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'MDH_O_DEF'		!MODEL HEADER
	INCLUDE 'MDL_O_DEF'		!SOURCE LINE
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Entries:
C
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER SETSX(0:*)		!SETS TO DO
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C READY
C
 800	CONTINUE
	CALL WNFCL(FCAOUT)				!CLOSE FILE
C
	RETURN
C
C
	END
