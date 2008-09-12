C+ NPLOT.FOR
C  WNB 910617
C
C  Revisions:
C	WNB 910828	Add RUN
C	WNB 910913	Change loops
C	WNB 911220	Loop maps
C       HjV 940112      Reorganize NPLTEL and NPLRES
C	CMV 940822	Option to abort loops
C	CMV 960122	Warning if /NORUN ignored
C	JPH 960622	Call WNGSCC
C	JPH 960805	ST_INIT=ST_MODE
C	JPH 960814	Close FCAIN after map NO_MORE
C	JPH 961120	Clear control-C count before exit
C	JPH 970129	Minor change to message text
C	HjV 970723	Remove control-C stuff (commented out with CCC)
C
C
	SUBROUTINE NPLOT
C
C  Main routine to handle data/map plotting
C
C  Result:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDRUN			!TEST RUN
	LOGICAL WNDXLN			!NEXT LOOP
C
C  Data declarations:
C
	LOGICAL ACT			!FIRST ACTION ASK PLOTTER
	  DATA ACT/.TRUE./
C-
C
C PRELIMINARIES
C
 	CALL NPLINI				!INIT PROGRAM
	IF (.NOT.WNDRUN()) 
	1	CALL WNCTXT(F_TP,'Ignored option /NORUN')
C
C DISTRIBUTE
C
 10	CONTINUE
	CALL NPLDAT(ACT)			!GET USER DATA
	IF (ACT) RETURN				!NOTHING DEFINED
	IF (OPT.EQ.'QUI') THEN			!READY
CCC	  CALL WNGCCC				! clear control-C count
	  CALL WNGEX
CCC	ELSE
CCC	  CALL WNCTXT(F_T,
CCC	1 '!/!4C\'//
CCC	1 'Use control-C to interrupt a plot and start the next one'//
CCC	1 '!/!4C\'//
CCC	1 'Use two control-C''s to abort the program!/' )
	ENDIF
 	IF (OPT.EQ.'DAT' .OR. 
	1      OPT.EQ.'MOD' .OR.
	2      OPT.EQ.'RES' .OR.
	3      OPT.EQ.'INT' .OR.
	4      OPT.EQ.'TEL') THEN		!PLOT DATA/RESID. OR TEL. ERRORS
	  CALL WNDXLI(LPOFF)			!CLEAR OFFSETS
	  DO WHILE (WNDXLN(LPOFF))		!MORE
 	    CALL NPLLOD				!DO PLOTS
 	    IF (NO_MORE) GOTO 10
	  END DO
	  CALL WNFCL(FCAIN)			!CLOSE SCAN FILE
	  GOTO 10				!RETRY
	ELSE IF (OPT.EQ.'MAP') THEN		!PLOT MAP
	  CALL WNDXLI(LPOFF)			!CLEAR OFFSETS
	  DO WHILE (WNDXLN(LPOFF))		!MORE
 	    CALL NPLMAP				!DO PLOTS
 	    IF (NO_MORE) GOTO 11
	  END DO
 11	  CONTINUE
	  CALL WNFCL(FCAIN)			!CLOSE MAP FILE
	  GOTO 10
	END IF
C
	RETURN					!READY
C
C
	END





