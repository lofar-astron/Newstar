C+ WNPMSG.FOR
C  WNB 911121
C
C  Revisions:
C
	SUBROUTINE WQMSG(ID,MSG)
C
C  Show header message
C
C  Result:
C
C	CALL WQMSG ( ID_J:I, MSG_C*:I)
C				Show header message MSG for device ID
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
	CHARACTER*(*) MSG	!MESSAGE
C
C  Function references:
C
	INTEGER WNGARA		!GET ADDRESS
	LOGICAL WNPCID		!CHECK ID
C
C  Data declarations:
C
	INTEGER VP(2)		!PARAMETERS
	CHARACTER*512 LMSG	!LOCAL TEXT
	  BYTE LMSGB(512)
	  EQUIVALENCE (LMSG,LMSGB)
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
	IF (IAND(2,A_J((ID-A_OB)/LB_J+WQD_TYP_J)).NE.0) THEN !DISS
	  E_C=35
	  RETURN
	END IF
C
C WRITE MESSAGE
C
	LMSG=MSG					!COPY MESSAGE
	VP(1)=WNGARA(LMSGB)
	VP(2)=MIN(LEN(MSG),LEN(LMSG))
	CALL WNPDEX(2,ID,VP)				!WRITE MESSAGE
C
	RETURN
C
C
	END
