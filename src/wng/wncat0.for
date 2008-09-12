C+ WNCAT0.FOR
C  WNB 890427
C
C  Revisions:
C	WNB 911118	DW DATA statement problems
C	WNB 911118	DW split from ATA
C	HjV 920520	HP does not allow extended source lines
C	WNB 930526	Add AFX
C
	LOGICAL FUNCTION WNCAFN(STR,PT,NAM)
C
C  Get type of character
C
C  Result:
C
C	WNCAFU_J = WNCAFU( STR_C*:I, NL_C*(*):I) Check if the name given in STR
C						can be uniquely found in the
C						list NL (minimax) (Last member
C						in list blank), and return order
C						number or zero.
C	WNCAFN_L = WNCAFN( STR_C*:I, PT_J:IO, NAM_C*:O) Get a field name in
C						NAM, starting at PT and updating
C						PT till end of field. True if
C						NAM field not empty. NAM will
C						be in upper case.
C	WNCAFF_L = WNCAFF( STR_C*:I, PT_J:IO, NAM_C*:O) Get remaining data in
C						string, excluding !comment.
C						Convert to UC unless in "".
C	WNCAFX_L = WNCAFX( STR_C*:I, PT_J:IO, NAM_C*:O) Get remaining data in
C						string, excluding !comment.
C						Convert to UC unless in "".
C						Preserve ""
C	WNCAFS_L = WNCAFS( STR_C*:I, PT_J:IO, NAM_C*:O) Get field in
C						string, excluding !comment.
C						Convert to UC unless in "".
C						Field ends at separator or blank
C	WNCAFT_L = WNCAFT( STR_C*:I, PT_J:IO, NAM_C*:O, CH_C*:I) Get field in
C						string, excluding !comment.
C						Convert to UC unless in "".
C						Field ends at a separator in
C						CH or blank
C	WNCAFP_L = WNCAFP( STR_C*:I, PT_J:IO, NAM_C*:O, CH_C*:I) Get field in
C						string, excluding !comment.
C						Convert to UC unless in "".
C						Field ends at a separator in
C						CH or blank. Preserve ".
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
	CHARACTER*(*) STR				!INPUT STRING
	INTEGER PT					!STRING POINTER
	CHARACTER *(*) CH				!CHECK CHARACTER
	CHARACTER *(*) NAM				!NAME FIELD
	CHARACTER *(*) NL(*)				!NAME LIST TO CHECK
C
C  Entry points:
C
	LOGICAL WNCAFF,WNCAFX,WNCAFS,WNCAFT,WNCAFP	!GET FIELD
	INTEGER WNCAFU					!MINIMAX TEST
C
C  Function references:
C
	CHARACTER*1 WNCAUP				!UC CONVERSION
	INTEGER WNCALN					!STRING LENGTH
C
C  Data declarations:
C
	CHARACTER*52 ALPH
	CHARACTER*10 DIG
	CHARACTER*2 XALPH
	CHARACTER*1 SEP
	  DATA ALPH(01:26)/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	  DATA ALPH(27:52)/'abcdefghijklmnopqrstuvwxyz'/
	  DATA DIG/'0123456789'/
	  DATA XALPH/'_$'/
	  DATA SEP/','/
C-
C
C WNCAFN
C
	WNCAFN=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR)) THEN			!CAN DO
	  IF (INDEX(ALPH//XALPH,STR(PT:PT)).GT.0) THEN
	    WNCAFN=.TRUE.
	    NAM=WNCAUP(STR(PT:PT))
	    I=I+1
	    PT=PT+1
	    DO WHILE (PT.LE.LEN(STR))
	      IF (INDEX(ALPH//XALPH//DIG,STR(PT:PT)).GT.0) THEN
		IF (I.LE.LEN(NAM)) THEN			!ADD TO NAME
		  NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
		  I=I+1
		END IF
		PT=PT+1
	      ELSE
		GOTO 30
	      END IF
	    END DO
 30	  CONTINUE
	  END IF
	END IF
C
	RETURN
C
C WNCAFU
C
	ENTRY WNCAFU(STR,NL)
C
	WNCAFU=0					!ASSUME ERROR
	I=MIN(WNCALN(STR),LEN(NL(1)))			!CHECK LENGTH
	I1=1						!COUNT IN NAMELIST
	L0=.TRUE.					!NO DUPLICATE
	DO WHILE (NL(I1).NE.' ')
	  IF (STR(1:I).EQ.NL(I1)(1:I)) THEN		!MATCH FOUND
	    IF (STR.EQ.NL(I1)) THEN			!FULL MATCH
	      WNCAFU=I1					!SET
	      GOTO 40					!READY
	    END IF
	    I2=I1+1
	    DO WHILE (NL(I2).NE.' ')			!CHECK DUPLICATES
	      IF (STR(1:I).EQ.NL(I2)(1:I)) THEN		!POSSIBLE DUPLICATE
		IF (STR.EQ.NL(I2)) THEN			!FULL MATCH
		  WNCAFU=I2
		  GOTO 40
		END IF
		L0=.FALSE.				!DUPLICATE SEEN
	      END IF
	      I2=I2+1
	    END DO
	    IF (L0) WNCAFU=I1				!NO DUPLICATE SEEN
	    GOTO 40					!READY
	  END IF
	  I1=I1+1					!NEXT IN NAMELIST
	END DO
 40	CONTINUE
C
	RETURN
C
C WNCAFF
C
	ENTRY WNCAFF(STR,PT,NAM)
C
	WNCAFF=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR) .AND. STR(PT:PT).NE. '!') THEN !CAN DO
	  WNCAFF=.TRUE.					!FOUND SOME
	  J=0						!NO "/' SEEN
	  DO WHILE (PT.LE.LEN(STR))
	    IF (J.EQ.0) THEN				!NORMAL
	      IF (STR(PT:PT).EQ.'!') GOTO 50		!READY
	      IF (STR(PT:PT).EQ.'"') THEN
		J=1					!SET " SEEN
	      ELSE					!SET CHAR
		IF (I.EQ.1) THEN
		  NAM=WNCAUP(STR(PT:PT))
		ELSE
		  NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    ELSE
	      IF (STR(PT:PT).EQ.'"') THEN		!"
		IF (PT.LT.LEN(STR)) THEN		!MAYBE ""
		  IF (STR(PT+1:PT+1).EQ.'"') THEN
		    PT=PT+1				!ACT AS IF NORMAL
		    GOTO 51
		  END IF
		END IF
		J=0					!RESET " SEEN
	      ELSE					!NORMAL
 51		CONTINUE
		IF (I.EQ.1) THEN
		  NAM=STR(PT:PT)
		ELSE
		  NAM=NAM(1:I-1)//STR(PT:PT)
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    END IF
	  END DO
	END IF
 50	CONTINUE
C
	RETURN
C
C WNCAFX
C
	ENTRY WNCAFX(STR,PT,NAM)
C
	WNCAFX=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR) .AND. STR(PT:PT).NE. '!') THEN !CAN DO
	  WNCAFX=.TRUE.					!FOUND SOME
	  J=0						!NO "/' SEEN
	  DO WHILE (PT.LE.LEN(STR))
	    IF (J.EQ.0) THEN				!NORMAL
	      IF (STR(PT:PT).EQ.'!') GOTO 52		!READY
	      IF (STR(PT:PT).EQ.'"') J=1		!SET " SEEN
	      IF (I.EQ.1) THEN				!SET CHAR
		NAM=WNCAUP(STR(PT:PT))
	      ELSE
		NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
	      END IF
	      I=I+1					!NAME LENGTH
	      PT=PT+1					!SKIP CHAR
	    ELSE
	      IF (STR(PT:PT).EQ.'"') THEN		!"
		NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
	        I=I+1					!NAME LENGTH
		IF (PT.LT.LEN(STR)) THEN		!MAYBE ""
		  IF (STR(PT+1:PT+1).EQ.'"') THEN
		    PT=PT+1				!ACT AS IF NORMAL
		    GOTO 53
		  END IF
		END IF
		J=0					!RESET " SEEN
	      ELSE					!NORMAL
 53		CONTINUE
		IF (I.EQ.1) THEN
		  NAM=STR(PT:PT)
		ELSE
		  NAM=NAM(1:I-1)//STR(PT:PT)
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    END IF
	  END DO
	END IF
 52	CONTINUE
C
	RETURN
C
C WNCAFS
C
	ENTRY WNCAFS(STR,PT,NAM)
C
	WNCAFS=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR) .AND. STR(PT:PT).NE.'!' .AND.
	1		ICHAR(STR(PT:PT)).GT.ICHAR(' ') .AND.
	2		INDEX(SEP,STR(PT:PT)).EQ.0) THEN !CAN DO
	  WNCAFS=.TRUE.					!FOUND SOME
	  J=0						!NO " SEEN
	  DO WHILE (PT.LE.LEN(STR))
	    IF (J.EQ.0) THEN				!NORMAL
	      IF (STR(PT:PT).EQ.'!' .OR. INDEX(SEP,STR(PT:PT)).NE.0 .OR.
	1		ICHAR(STR(PT:PT)).LE.ICHAR(' ')) GOTO 60 !READY
	      IF (STR(PT:PT).EQ.'"') THEN
		J=1					!SET " SEEN
	      ELSE					!SET CHAR
		IF (I.EQ.1) THEN
		  NAM=WNCAUP(STR(PT:PT))
		ELSE
		  NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    ELSE
	      IF (STR(PT:PT).EQ.'"') THEN		!"
		IF (PT.LT.LEN(STR)) THEN		!MAYBE ""
		  IF (STR(PT+1:PT+1).EQ.'"') THEN
		    PT=PT+1				!ACT AS IF NORMAL
		    GOTO 61
		  END IF
		END IF
		J=0					!RESET " SEEN
	      ELSE					!NORMAL
 61		CONTINUE
		IF (I.EQ.1) THEN
		  NAM=STR(PT:PT)
		ELSE
		  NAM=NAM(1:I-1)//STR(PT:PT)
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    END IF
	  END DO
	END IF
 60	CONTINUE
C
	RETURN
C
C WNCAFT
C
	ENTRY WNCAFT(STR,PT,NAM,CH)
C
	WNCAFT=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR) .AND. STR(PT:PT).NE.'!' .AND.
	1		ICHAR(STR(PT:PT)).GT.ICHAR(' ') .AND.
	2		INDEX(CH,STR(PT:PT)).EQ.0) THEN !CAN DO
	  WNCAFT=.TRUE.					!FOUND SOME
	  J=0						!NO " SEEN
	  DO WHILE (PT.LE.LEN(STR))
	    IF (J.EQ.0) THEN				!NORMAL
	      IF (STR(PT:PT).EQ.'!' .OR. INDEX(CH,STR(PT:PT)).NE.0 .OR.
	1		ICHAR(STR(PT:PT)).LE.ICHAR(' ')) GOTO 70 !READY
	      IF (STR(PT:PT).EQ.'"') THEN
		J=1					!SET " SEEN
	      ELSE					!SET CHAR
		IF (I.EQ.1) THEN
		  NAM=WNCAUP(STR(PT:PT))
		ELSE
		  NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    ELSE
	      IF (STR(PT:PT).EQ.'"') THEN		!"
		IF (PT.LT.LEN(STR)) THEN		!MAYBE ""
		  IF (STR(PT+1:PT+1).EQ.'"') THEN
		    PT=PT+1				!ACT AS IF NORMAL
		    GOTO 71
		  END IF
		END IF
		J=0					!RESET " SEEN
	      ELSE					!NORMAL
 71		CONTINUE
		IF (I.EQ.1) THEN
		  NAM=STR(PT:PT)
		ELSE
		  NAM=NAM(1:I-1)//STR(PT:PT)
		END IF
		I=I+1					!NAME LENGTH
	      END IF
	      PT=PT+1					!SKIP CHAR
	    END IF
	  END DO
	END IF
 70	CONTINUE
C
	RETURN
C
C WNCAFP
C
	ENTRY WNCAFP(STR,PT,NAM,CH)
C
	WNCAFP=.FALSE.					!ASSUME ERROR
	NAM=' '						!FIELD NAME
	I=1						!LENGTH NAME
	IF (PT.LE.LEN(STR) .AND. STR(PT:PT).NE.'!' .AND.
	1		ICHAR(STR(PT:PT)).GT.ICHAR(' ') .AND.
	2		INDEX(CH,STR(PT:PT)).EQ.0) THEN !CAN DO
	  WNCAFP=.TRUE.					!FOUND SOME
	  J=0						!NO " SEEN
	  DO WHILE (PT.LE.LEN(STR))
	    IF (J.EQ.0) THEN				!NORMAL
	      IF (STR(PT:PT).EQ.'!' .OR. INDEX(CH,STR(PT:PT)).NE.0 .OR.
	1		ICHAR(STR(PT:PT)).LE.ICHAR(' ')) GOTO 80 !READY
	      IF (STR(PT:PT).EQ.'"') J=1		!SET " SEEN
	      IF (I.EQ.1) THEN
		NAM=WNCAUP(STR(PT:PT))
	      ELSE
		NAM=NAM(1:I-1)//WNCAUP(STR(PT:PT))
	      END IF
	      I=I+1					!NAME LENGTH
	      PT=PT+1					!SKIP CHAR
	    ELSE
	      IF (STR(PT:PT).EQ.'"') J=0		!RESET " SEEN
	      IF (I.EQ.1) THEN
		NAM=STR(PT:PT)
	      ELSE
		NAM=NAM(1:I-1)//STR(PT:PT)
	      END IF
	      I=I+1					!NAME LENGTH
	      PT=PT+1					!SKIP CHAR
	    END IF
	  END DO
	END IF
 80	CONTINUE
C
	RETURN
C
C
	END
