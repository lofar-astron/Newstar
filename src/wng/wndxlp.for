C+ WNDXLP.FOR
C  WNB 910913
C
C  Revisions:
C	WNB 911003	Correct multiple loops
C	WNB 920303	SUN problems ()
C	WNB 920403	Correct n+1 problem multiple loops
C	WNB 921203	Add WNDXL1
C	CMV 931210	Keyword passed as an argument
C	CMV 931220	FCA of input file passed as an argument
C	JPH 940831	Comments
C
	LOGICAL FUNCTION WNDXLP(KW,FCAIN)
C
C  General set loop control
C
C  Result:
C
C	WNDXLP_L = WNDXLP(KW_C*(*):I, FCAIN_J:I)
C				Obtain the loop control parameters, and
C				initialise loop. User is prompted with 
C				keyword KW, FCAIN is passed to WNDSTA
C				and should hold the FCA of the input
C				file or be <= 0
C	WNDXL1_L = WNDXL1()	Set parameters for one loop; initialise
C	WNDXLI_L = WNDXLI( LPOFF_J(0:7):O)
C				Initialise the loop offset LPOFF
C	WNDXLN_L = WNDXLN( LPOFF_J(0:7):O)
C				Get next loop offset or .FALSE.
C
C	This module controls a single nest of loops, whose parameters are
C prompted for by WNDXLP and stored in local data structures defined by WND_DEF.
C	Loop definition is independent of both the associated SETS specification
C (which defines the starting point of the loops) and the file in which the 
C sets are to be read/written. It is the caller's responsibility to implement
C the proper combination of SETS, LOOPS and files. 
C	It is possible to loop over input and output sets simultaneously. 
C (Example: NCAPOC)
C	The only function of the FCAIN parameter is to enable WNDSTA to show the
C input file's layout.
C 
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WND_DEF'
C
C  PIN references:
C
C	LOOPS
C
C  Entry points:
C
	LOGICAL WNDXL1		!SET 1 LOOP
	LOGICAL WNDXLI		!INIT. LOOPS
	LOGICAL WNDXLN		!NEXT LOOP OFFSET
C
C  Parameters:
C
C
C  Arguments:
C
	CHARACTER*(*) KW	!KEYWORD TO PROMPT FOR
	INTEGER FCAIN		!FCA of input file
	INTEGER LPOFF(0:7)	!PROGRAM LOOP OFFSETS
C
C  Function references:
C
	LOGICAL WNDSTA		!GET SETS
C
C  Data declarations:
C
C-
C
C INIT
C
	WNDXLP=.TRUE.					!ASSUME OK
C
C GET LOOP LIST
C
 10	CONTINUE
	IF (.NOT.WNDSTA(KW,2*MXNLOP,XPOFF(0,1,0),FCAIN)) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN		!LEAVE WITH ERROR
	    WNDXLP=.FALSE.
	    XPOFF(0,1,0)=0				!ASSUME 1 LOOP
	  ELSE
	    GOTO 10					!RETRY
	  END IF
	END IF
	GOTO 11
C
C WNDXL1
C
	ENTRY WNDXL1()
C
	WNDXL1=.TRUE.					!ASSUME OK
	XPOFF(0,1,0)=0					!ASSUME 1 LOOP
	GOTO 11
C
C INTERPRET LIST
C
 11	CONTINUE
	XPOFF(0,1,0)=XPOFF(0,1,0)/2			!PAIRS SPECIFIED
	IF (XPOFF(0,1,0).LE.0) THEN			!ASSUME 1
	  XPOFF(0,1,0)=1				!ASSUME 1 LOOP
	  XPOFF(0,0,1)=1				!WITH COUNT 1
	END IF
	DO I=1,XPOFF(0,1,0)				!ALL ENTRIES
	  IF (IAND(XPOFF(0,0,I),'ffff0000'X).NE.0)
	1		XPOFF(0,0,I)=1			!ASSUME 1 COUNT
	  XPOFF(0,0,I)=MAX(1,XPOFF(0,0,I))		!MIN. 1 COUNT
	  DO I1=0,7
	    IF (IAND(XPOFF(I1,1,I),'ffff0000'X).NE.0)	!LOOP OR *
	1		XPOFF(I1,1,I)=0			!NO INCREMENT
	  END DO
	END DO
 40	CONTINUE
	XPOFF(0,0,0)=0					!MAJOR LOOP COUNT
	XLPTR=0						!CURRENT INDEX
	XLCNT(0)=0					!COUNT INDEX 0
C
	GOTO 900					!READY
C
C WNDXLI
C
	ENTRY WNDXLI(LPOFF)				!SET INITIAL OFFSET
C
	WNDXLI=.TRUE.					!ASSUME OK
C
 20	CONTINUE
	DO I=0,7					!SET LPOFF START
	  LPOFF(I)=0
	END DO
	GOTO 40						!ASSURE CORRECT START
C
	GOTO 900
C
C WNDXLN
C
	ENTRY WNDXLN(LPOFF)				!GET NEXT OFFSET
C
	WNDXLN=.TRUE.					!ASSUME OK
C
C NEXT INDEX
C
 30	CONTINUE
	DO WHILE (XLPTR.LT.XPOFF(0,1,0))		!MORE INDICES
	  DO I=0,7					!SAVE OFFSET
	    XLSAV(I,XLPTR)=LPOFF(I)
	  END DO
	  XLPTR=XLPTR+1					!NEXT INDEX
	  XLCNT(XLPTR)=0				!START COUNT INDEX
	END DO
C
C UPDATE INDEX
C
	DO WHILE (XLPTR.GT.0)				!MORE
	  IF (XLCNT(XLPTR).LT.XPOFF(0,0,XLPTR)) THEN	!CAN DO MORE
	    IF (XLCNT(XLPTR).GT.0) THEN			!UPDATE
	      DO I=0,7
	        LPOFF(I)=LPOFF(I)+XPOFF(I,1,XLPTR)
	      END DO
	    END IF
	    IF (XLPTR.GE.XPOFF(0,1,0)) THEN		!LAST INDEX
	      XLCNT(XLPTR)=XLCNT(XLPTR)+1		!UPDATE INDEX
	    ELSE
	      GOTO 30					!GET ALL INDICES
	    END IF
	    GOTO 900					!RETURN UPDATED INDEX
	  END IF
	  XLPTR=XLPTR-1					!TRY PREVIOUS INDEX
	  DO I=0,7					!RESTORE PREVIOUS
	    LPOFF(I)=XLSAV(I,XLPTR)
	  END DO
	  XLCNT(XLPTR)=XLCNT(XLPTR)+1			!UPDATE INDEX
	END DO
	WNDXLN=.FALSE.					!READY
	GOTO 20						!RESET TO START
C
C READY
C
 900	CONTINUE
	RETURN
C
C
	END
