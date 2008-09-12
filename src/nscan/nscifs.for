C+ NSCIFS.FOR
C  WNB 900417
C
C  Revisions:
C	GvD 920429	Declare WNDPAR as logical iso. integer
C	HjV 920520	HP does not allow extended source lines
C	WNB 920827	Add option 4
C	WNB 921104	Add T,U for AT
C	WNB 930824	Add IF1, make function, add 200 code
C	CMV 940822	Make -* switch off auto-correlations as well
C	JPH 940902	Format as WNCXPL does
C	JPH 940909	Call WNDPOHC
C	JPH 960307	Comment on use of IFRS
C	HjV 970403	Logging of selected IFRs also in logfile
C
C
	LOGICAL FUNCTION NSCIFS(TYP,IFRS)
C
C  Select/de-select interferometers
C
C  Result:
C	NSCIFS_L = NSCIFS ( TYP_J:I, IFRS_B(0:*,0:*):IO)
C				Include (.true.) or exclude (.false.)
C				interferometers in IFRS. TYP can be:
C				0  use as given (show first)
C				1  pre-select all
C				2  pre-select all cross correlations
C				3  pre-select fixed-movable only
C				4  pre-select none
C				TYP can be TYP+100 to suppress asking.
C				TYP can be TYP+200 to suppress asking and 
C				initial message
C				Assume WSRT telescopes. .FALSE. if
C				input error or # given (check E_C)
C	NSCIF1_L = NSCIF1 ( TYP_J:I, IFRS_B(0:*,0:*):IO, STHJ_J(0:*):I)
C				As IFS, but check for instrument used
C
C NOTE:
C	This routine sets both the upper and lower triangle of the IFRS even
C though only one of the two would suffice. Routines using IFRS may read it
C either way. Any code outside this routine that modifies IFRS must modify both
C triangles!
C
C 
C  Pin references:
C
C	SELECT_IFRS
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Entry points:
C
	LOGICAL NSCIF1
C
C  Parameters:
C
	INTEGER MAXDEF			!MAXIMUM ENTRIES PIN ENTRY
	  PARAMETER (MAXDEF=40)
C
C  Arguments:
C
	INTEGER TYP				!SELECTION TYPE
	BYTE IFRS(0:STHTEL-1,0:STHTEL-1)	!SELECTION IFR TABLE
	INTEGER STHJ(0:*)			!SET HEADER
C
C  Function references:
C
	LOGICAL WNDPAR				!GET USER DATA
C
C  Data declarations:
C
	CHARACTER*(STHTEL+8) TEL		!TELESCOPE NAMES
	  DATA TEL/'0123456789ABCD*FMYZPTU'/
	INTEGER IS(STHTEL+8)			!START VALUES
	  DATA IS/0,1,2,3,4,5,6,7,8,9,10,11,12,13,
	1			00,0,10,10,12,1,08,0/
	INTEGER IE(STHTEL+8)			!END VALUES
	  DATA IE/0,1,2,3,4,5,6,7,8,9,10,11,12,13,
	1			13,9,13,11,13,0,13,7/
C		  0 1 2 3 4 5 6 7 8 9  A  B  C  D
C				 * F  M  Y  Z P  T U
	LOGICAL LP				!PRINT INDICATOR
	LOGICAL ADD				!INCLUDE/EXCLUDE
	INTEGER INSTR				!INSTRUMENT (0=WSRT, 1=ATCA)
	CHARACTER*4 RD(MAXDEF)			!INPUT
	CHARACTER*(STHTEL) IFTXT		!LIST
C-
C
C NSCIFS
C
	INSTR=0					!ASSUME WSRT
	GOTO 100
C
C NSCIF1
C
	ENTRY NSCIF1(TYP,IFRS,STHJ)
C
	INSTR=STHJ(STH_INST_J)			!GET INSTRUMENT
	GOTO 100
C
C INIT
C
 100	CONTINUE
	A_J(0)=1				! inhibit reset of dynamic
						!  prompts texts
	NSCIFS=.TRUE.				!ASSUME OK
	LP=.FALSE.				!ASSUME NO PRINT
	IF (MOD(TYP,100).EQ.1) THEN		!PRE-SELECT ALL
	  IF (TYP.LT.200)
	1	CALL WNCTXT(F_TP,
	1	'!4C\All auto/cross interferometers pre-selected')
	  DO I1=0,STHTEL-1
	    DO I2=0,STHTEL-1
	      IF (INSTR.EQ.1 .AND. (I1.LT.8 .OR. I2.LT.8)) THEN
	        IFRS(I1,I2)=.FALSE.
	      ELSE
	        IFRS(I1,I2)=.TRUE.
	      END IF
	    END DO
	  END DO
	ELSE IF (MOD(TYP,100).EQ.2) THEN	!ALL CROSS
	  IF (TYP.LT.200)
	1	CALL WNCTXT(F_TP,
	1	'!4C\All cross interferometers pre-selected')
	  DO I1=0,STHTEL-1
	    DO I2=0,STHTEL-1
	      IF (I1.EQ.I2 .OR.
	1		(INSTR.EQ.1 .AND. (I1.LT.8 .OR. I2.LT.8))) THEN
	        IFRS(I1,I2)=.FALSE.
	      ELSE
	        IFRS(I1,I2)=.TRUE.
	      END IF
	    END DO
	  END DO
	ELSE IF (MOD(TYP,100).EQ.3) THEN	!FIXED-MOVABLE
	  IF (INSTR.EQ.1 .AND. TYP.LT.200) THEN
	    CALL WNCTXT(F_TP,
	1	'!4C\All cross interferometers pre-selected')
	  ELSE IF (TYP.LT.200) THEN
	    CALL WNCTXT(F_TP,
	1	'!4C\All fixed/movable interferometers pre-selected')
	  END IF
	  DO I1=0,STHTEL-1
	    DO I2=0,STHTEL-1
	      IF ((I1.LE.9 .AND. I2.GE.10) .OR.
	1		(I1.GE.10 .AND. I2.LE.9) .OR.
	1		(INSTR.EQ.1 .AND. I1.GE.8 .AND. I2.GE.8)) THEN
	        IFRS(I1,I2)=.TRUE.
	      ELSE
	        IFRS(I1,I2)=.FALSE.
	      END IF
	    END DO
	  END DO
	ELSE IF (MOD(TYP,100).EQ.4) THEN	!NONE
	  IF (TYP.LT.200)
	1	CALL WNCTXT(F_TP,
	1	'!4C\No interferometers pre-selected')
	  DO I1=0,STHTEL-1
	    DO I2=0,STHTEL-1
	      IFRS(I1,I2)=.FALSE.
	    END DO
	  END DO
	ELSE					!START WITH GIVEN
	  LP=.TRUE.				!PRINT FIRST
	END IF
C
C GET USER DATA
C
 10	CONTINUE
	IF (LP) THEN				!PRINT IFRS
	  CALL WNCTXT(F_TP,'!/!4C  !AS',TEL(1:STHTEL)) !HEADING
	  DO I1=1,STHTEL			!ALL LINES
	    IFTXT=' '
	    DO I2=I1,STHTEL
	      IF (IFRS(I1-1,I2-1)) THEN		!SELECT
		IFTXT(I2:I2)='+'
	      ELSE				!DESELECT
		IFTXT(I2:I2)='.'
	      END IF
	    END DO
	    CALL WNCTXT(F_TP,'!4C!AS !#$AS',TEL(I1:I1),STHTEL,IFTXT)
	  END DO
	END IF
C
 11	CONTINUE
	IF (TYP.GE.100) GOTO 20			!READY
	IF (.NOT.WNDPAR('SELECT_IFRS',RD,MAXDEF*4,J0,'""')) THEN !GET INFO
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN
	    NSCIFS=.FALSE.			!SHOW END
	    GOTO 20				!READY
	  END IF
	  GOTO 11				!REPEAT
	ELSE IF (J0.EQ.0) THEN
	  GOTO 20				!READY
	ELSE IF (J0.LT.0) THEN			!ASSUME +*
	  IF (INSTR.EQ.1) THEN			!ATCA
	    RD(1)='+T'
	  ELSE
	    RD(1)='+*'
	  END IF
	  J0=1
	END IF
C
	DO I=1,J0
	  ADD=.TRUE.				!ASSUME INCLUDE
	  I1=1					!CHARACTER PTR
 31	  CONTINUE
	  IF (I1.GT.4) GOTO 30			!EMPTY
	  IF (RD(I)(I1:I1).EQ.' ') THEN
	    I1=I1+1				!SKIP SPACE
	    GOTO 31
	  ELSE IF (RD(I)(I1:I1).EQ.'+') THEN
	    I1=I1+1				!SKIP +
	    GOTO 31
	  ELSE IF (RD(I)(I1:I1).EQ.'-') THEN
	    I1=I1+1
	    ADD=.NOT.ADD			!EXCLUDE
	    GOTO 31
	  ELSE
	    I2=INDEX(TEL,RD(I)(I1:I1))		!GET TELESCOPE
	    IF (I2.EQ.0) GOTO 30		!UNKNOWN
	    I1=I1+1
	    IF (I1.GT.4) GOTO 30
	    IF (RD(I)(I1:I1).EQ.'#') THEN	!AUTO CORRELATIONS
	      DO I4=IS(I2),IE(I2)
		IFRS(I4,I4)=ADD
	      END DO
	    ELSE
	      IF (RD(I)(I1:I1).EQ.' ') THEN
	        IF (INSTR.EQ.1 .AND. ADD) THEN	!ATCA +
	          I3=STHTEL+7			!ASSUME T
		ELSE
	          I3=STHTEL+1			!ASSUME *
		END IF
	      ELSE
	        I3=INDEX(TEL,RD(I)(I1:I1))
	        IF (I3.EQ.0) GOTO 30		!UNKNOWN
	      END IF
	      DO I4=IS(I2),IE(I2)		!DO FOR SPECIFIED TEL.
	        DO I5=IS(I3),IE(I3)
		  IF (I4.NE.I5) THEN		!ONLY CROSS
	  	    IFRS(I4,I5)=ADD		!ALL TELESCOPES
		    IFRS(I5,I4)=ADD
	 	  END IF
	        END DO
	      END DO
C
	      IF (.NOT.ADD.AND.
	1	  I2.EQ.STHTEL+1.AND.I3.EQ.STHTEL+1) THEN  ! -* also AutoCor.
	        DO I4=IS(I2),IE(I2)
		  IFRS(I4,I4)=ADD
	        END DO
	      END IF
C
	    END IF
	  END IF
 30	  CONTINUE
	END DO
	LP=.TRUE.
	GOTO 10					!MORE
C
 20	CONTINUE
	CALL WNDPOHC
C
	RETURN
C
C
	END
