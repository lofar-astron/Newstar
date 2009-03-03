C+ WNPCLR.FOR
C  WNB 911213
C
C  Revisions:
C
	SUBROUTINE WQCLR(ID)
C
C  Clear screen
C
C  Result:
C
C	CALL WQCLR ( ID_J:I)
C				Clear screen for device for device ID
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE	'WQD_O_DEF'		!DEVICE AREA
C
C  Parameters:
C
C
C  Entry points:
C
C
C  Arguments:
C
	INTEGER ID		!PLOT ID
C
C  Function references:
C
	LOGICAL WNPCID		!CHECK ID
C
C  Data declarations:
C
C-
C
C INIT
C
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!WRONG STATE
	  RETURN
	END IF
C
C CHECK ID
C
	IF (.NOT.WNPCID(ID)) THEN			!WRONG DEVICE
	  E_C=20
	  RETURN
	END IF
C
C CHECK TYPE
C
	IF (IAND(1,A_J((ID-A_OB)/LB_J+WQD_TYP_J)).EQ.0) THEN !NOT OUTPUT
	  E_C=34
	  RETURN
	END IF
C
C CLEAR SCREEN
C
	CALL WNPDEX(5,ID,0)				!WRITE MESSAGE
C
	RETURN
C
C
	END