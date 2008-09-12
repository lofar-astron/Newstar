C+ WNCFAD.FOR
C  WNB 880725
C
C  Revisions:
C	GvD 920506	Do right adjust backwards (went wrong on SUN)
C
	SUBROUTINE WNCFAD(FRST,WID,FRL,FRW)
C
C  Adjust string in a field
C
C  Result:
C
C	CALL WNCFAD(FRST_C*:IO,WID_J:I,FRL_J:I,FRW_J:IO)
C				Adjust the string FRST(1:FRW) in a field
C				given by width WID.
C				If WID=0: trim string to FRST(1:) and set FRW
C				If WID>0: right adjust to FRST(1:WID) set FRW
C				If WID<0: left adjust using |WID|.
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
	CHARACTER*(*) FRST			!INPUT STRING
	INTEGER WID				!ADJUSTMENT WIDTH
	INTEGER FRL				!LENGTH FRST
	INTEGER FRW				!INPUT POINTER
C
C  Function references:
C
	INTEGER WNCAL0				!GET STRING LENGTH
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
	IF (FRW.GT.0) THEN
	  DO WHILE(FRST(1:1).EQ.' ' .AND. FRST(:FRW).NE.' ') !TRIM FIRST
	    FRST(1:FRW)=FRST(2:FRW)		!TRIM
	  END DO
	  FRW=WNCAL0(FRST(1:FRW))
	ELSE
	  FRW=0
	END IF
	IF (FRW.GT.0) THEN
	  IF (WID.GT.0) THEN			!RIGHT ADJUST
	    I1=MIN(WID,FRL)			!END STRING
	    I=I1-FRW				!SPACES TO ADD
	    IF (I.GT.0) THEN
	      DO I2=I1,I+1,-1
		FRST(I2:I2)=FRST(I2-I:I2-I)
	      END DO
	      FRST(1:I)=' '
	    ELSE IF (I.LT.0) THEN		!TRUNCATE
	      FRST(1:I1)=FRST(FRW-I1+1:FRW)
	    END IF
	    FRW=I1				!NEW LENGTH
	  ELSE IF (WID.LT.0) THEN		!LEFT ADJUST
	    I1=MIN(-WID,FRL)			!END STRING
	    FRST(1:I1)=FRST(1:FRW)		!ADJUST OR TRUNCATE
	    FRW=I1				!NEW LENGTH
	  END IF
	END IF
C
	RETURN
C
C
	END
