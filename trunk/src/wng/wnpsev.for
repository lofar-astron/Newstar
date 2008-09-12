C+ WNPSEV.FOR
C  WNB 911127
C
C  Revisions:
C
C  Set windows etc. routines
C
	LOGICAL FUNCTION WQSLNT(ID)
C
C  Result:
C
C	WQSLNT( ID_J:I)			Select norm. transform
C	WQSWIN( ID_J:I, WIN_E(4))	Set window
C	WQSVIE( ID_J:I, WIN_E(4))	Set viewport
C	WQSDVW( ID_J:I, WIN_E(4))	Set device window
C	WQSDVV( ID_J:I, WIN_E(4))	Set device viewport
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE 'WQD_O_DEF'		!DEVICE AREA
C
C  Entry points:
C
	LOGICAL WQSWIN			!SET WINDOW
	LOGICAL WQSVIE			!SET VIEWPORT
	LOGICAL WQSDVW			!SET DEVICE WINDOW
	LOGICAL WQSDVV			!SET DEVICE VIEWPORT
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER ID			!DEVICE ID
	REAL WIN(4)			!WINDOW/VIEWPORT
C
C  Function references:
C
	LOGICAL WNPCID			!CHECK DEVICE ID
	LOGICAL WQOPEN			!OPEN SYSTEM
C
C  Data declarations:
C
	REAL LNTR(0:3,0:2)		!DEVICE WINDOW/VIEW
	REAL R2,R3
C-
C
C SLNT
C
	WQSLNT=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSLNT=WQOPEN()		!OPEN SYSTEM
	IF (WQSLNT) THEN
	  IF (ID.GT.WQG__NMXTR) THEN
	    E_C=40
	    WQSLNT=.FALSE.
	  ELSE
	    WQG_CTR=MAX(0,ID)				!SET INDEX
	  END IF
	END IF
C
	RETURN
C
C SWIN
C
	ENTRY WQSWIN(ID,WIN)
C
	WQSWIN=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSWIN=WQOPEN()		!OPEN SYSTEM
	IF (WQSWIN) THEN
	  IF (ID.LE.0 .OR. ID.GT.WQG__NMXTR) THEN
	    E_C=40
	    WQSWIN=.FALSE.
	  ELSE IF (WIN(1).GE.WIN(3) .OR. WIN(2).GE.WIN(4)) THEN
	    E_C=41
	    WQSWIN=.FALSE.
	  ELSE
	    WQG_NTR(0,1,ID)=WIN(1)
	    WQG_NTR(1,1,ID)=WIN(2)
	    WQG_NTR(2,1,ID)=WIN(3)
	    WQG_NTR(3,1,ID)=WIN(4)
 100	    CONTINUE
	    R0=WQG_NTR(1,1,ID)-WQG_NTR(3,1,ID)		!W0-W1
	    R1=WQG_NTR(1,2,ID)-WQG_NTR(3,2,ID)		!V0-V1
	    WQG_NTR(2,0,ID)=R1/R0			!A
	    R2=R0*WQG_NTR(3,2,ID)			!V1*(W0-W1)
	    R3=R1*WQG_NTR(3,1,ID)			!W1*(V0-V1)
	    WQG_NTR(3,0,ID)=(R2-R3)/R0			!B
	    R0=WQG_NTR(0,1,ID)-WQG_NTR(2,1,ID)		!W0-W1
	    R1=WQG_NTR(0,2,ID)-WQG_NTR(2,2,ID)		!V0-V1
	    WQG_NTR(0,0,ID)=R1/R0			!A
	    R2=R0*WQG_NTR(2,2,ID)			!V1*(W0-W1)
	    R3=R1*WQG_NTR(2,1,ID)			!W1*(V0-V1)
	    WQG_NTR(1,0,ID)=(R2-R3)/R0			!B
	  END IF
	END IF
C
	RETURN
C
C SVIE
C
	ENTRY WQSVIE(ID,WIN)
C
	WQSVIE=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSVIE=WQOPEN()		!OPEN SYSTEM
	IF (WQSVIE) THEN
	  IF (ID.LE.0 .OR. ID.GT.WQG__NMXTR) THEN
	    E_C=40
	    WQSVIE=.FALSE.
	  ELSE IF (WIN(1).GE.WIN(3) .OR. WIN(2).GE.WIN(4)) THEN
	    E_C=41
	    WQSVIE=.FALSE.
	  ELSE IF (WIN(1).LT.0 .OR. WIN(2).LT.0 .OR. WIN(3).GT.1 .OR.
	1		WIN(4).GT.1) THEN
	    E_C=43
	    WQSVIE=.FALSE.
	  ELSE
	    WQG_NTR(0,2,ID)=WIN(1)
	    WQG_NTR(1,2,ID)=WIN(2)
	    WQG_NTR(2,2,ID)=WIN(3)
	    WQG_NTR(3,2,ID)=WIN(4)
	    GOTO 100
	  END IF
	END IF
C
	RETURN
C
C SDVW
C
	ENTRY WQSDVW(ID,WIN)
C
	WQSDVW=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!ILLEGAL STATE
	  WQSDVW=.FALSE.
	END IF
	IF (WQSDVW) THEN
	  IF (.NOT.WNPCID(ID)) THEN
	    E_C=20					!WRONG ID
	    WQSDVW=.FALSE.
	  END IF
	END IF
	J0=(ID-A_OB)/LB_J				!POINTER
	IF (WQSDVW) THEN
	  IF (IAND(4,A_J(J0+WQD_TYP_J)).NE.0) THEN	!DISS
	    E_C=35
	    WQSDVW=.FALSE.
	  END IF
	END IF
	IF (WQSDVW) THEN
	  IF (IAND(8,A_J(J0+WQD_TYP_J)).NE.0 .AND.
	1	IAND(2,A_J(J0+WQD_TYP_J)).NE.0) THEN	!META INPUT
	    E_C=32
	    WQSDVW=.FALSE.
	  END IF
	END IF
	IF (WQSDVW) THEN
	  IF (WIN(1).GE.WIN(3) .OR. WIN(2).GE.WIN(4)) THEN
	    E_C=41
	    WQSDVW=.FALSE.
	  ELSE IF (WIN(1).LT.0 .OR. WIN(2).LT.0 .OR. WIN(3).GT.1 .OR.
	1		WIN(4).GT.1) THEN
	    E_C=44
	    WQSDVW=.FALSE.
	  ELSE
	    A_E(J0+WQD_NTR_E+1*4+0)=WIN(1)		!SET WINDOW
	    A_E(J0+WQD_NTR_E+1*4+1)=WIN(2)
	    A_E(J0+WQD_NTR_E+1*4+2)=WIN(3)
	    A_E(J0+WQD_NTR_E+1*4+3)=WIN(4)
 200	    CONTINUE
	    A_E(J0+WQD_NTR_E+2+2*4)=A_E(J0+WQD_NTR_E+2+2*4)-
	1		A_E(J0+WQD_NTR_E+0+2*4) 	!SHIFT TO CORNER
	    A_E(J0+WQD_NTR_E+3+2*4)=A_E(J0+WQD_NTR_E+3+2*4)-
	1		A_E(J0+WQD_NTR_E+1+2*4)
	    A_E(J0+WQD_NTR_E+0+2*4)=0
	    A_E(J0+WQD_NTR_E+1+2*4)=0
	    DO J=0,2					!SAVE
	      DO J1=0,3
		LNTR(J1,J)=A_E(J0+WQD_NTR_E+J1+J*4)
	      END DO
	    END DO
	    R0=(LNTR(2,1)-LNTR(0,1))/LNTR(2,2)		!ASPECT
	    R1=(LNTR(3,1)-LNTR(1,1))/LNTR(3,2)
	    IF (R0.GT.R1) THEN
	      LNTR(3,2)=LNTR(3,2)*R1/R0			!ASPECT RATIO
	    ELSE IF (R0.LT.R1) THEN
	      LNTR(2,2)=LNTR(2,2)*R0/R1
	    END IF
	    R0=LNTR(1,1)-LNTR(3,1)			!W0-W1
	    R1=LNTR(1,2)-LNTR(3,2)			!V0-V1
	    A_E(J0+WQD_NTR_E+2+0*4)=R1/R0		!A
	    R2=R0*LNTR(3,2)				!V1*(W0-W1)
	    R3=R1*LNTR(3,1)				!W1*(V0-V1)
	    A_E(J0+WQD_NTR_E+3+0*4)=(R2-R3)/R0		!B
	    R0=LNTR(0,1)-LNTR(2,1)			!W0-W1
	    R1=LNTR(0,2)-LNTR(2,2)			!V0-V1
	    A_E(J0+WQD_NTR_E+0+0*4)=R1/R0		!A
	    R2=R0*LNTR(2,2)				!V1*(W0-W1)
	    R3=R1*LNTR(2,1)				!W1*(V0-V1)
	    A_E(J0+WQD_NTR_E+1+0*4)=(R2-R3)/R0		!B
	  END IF
	END IF
C
	RETURN
C
C SDVV
C
	ENTRY WQSDVV(ID,WIN)
C
	WQSDVV=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!ILLEGAL STATE
	  WQSDVV=.FALSE.
	END IF
	IF (WQSDVV) THEN
	  IF (.NOT.WNPCID(ID)) THEN
	    E_C=20					!WRONG ID
	    WQSDVV=.FALSE.
	  END IF
	END IF
	J0=(ID-A_OB)/LB_J				!POINTER
	IF (WQSDVV) THEN
	  IF (IAND(4,A_J(J0+WQD_TYP_J)).NE.0) THEN	!DISS
	    E_C=35
	    WQSDVV=.FALSE.
	  END IF
	END IF
	IF (WQSDVV) THEN
	  IF (IAND(8,A_J(J0+WQD_TYP_J)).NE.0 .AND.
	1	IAND(2,A_J(J0+WQD_TYP_J)).NE.0) THEN	!META INPUT
	    E_C=32
	    WQSDVV=.FALSE.
	  END IF
	END IF
	IF (WQSDVV) THEN
	  IF (WIN(1).GE.WIN(3) .OR. WIN(2).GE.WIN(4)) THEN
	    E_C=41
	    WQSDVV=.FALSE.
	  ELSE IF (WIN(1).LT.0 .OR. WIN(2).LT.0 .OR.
	1		WIN(3).GT.A_E(J0+WQD_XHI_E) .OR.
	1		WIN(4).GT.A_E(J0+WQD_YHI_E)) THEN
	    E_C=45
	    WQSDVV=.FALSE.
	  ELSE
	    A_E(J0+WQD_NTR_E+2*4+0)=WIN(1)
	    A_E(J0+WQD_NTR_E+2*4+1)=WIN(2)
	    A_E(J0+WQD_NTR_E+2*4+2)=WIN(3)
	    A_E(J0+WQD_NTR_E+2*4+3)=WIN(4)
	    GOTO 200
	  END IF
	END IF
C
	RETURN
C
C
	END
