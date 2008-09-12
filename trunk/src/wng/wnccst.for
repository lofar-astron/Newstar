C+ WNCCST.FOR
C  WNB 910320
C
C  Revisions:
C
	SUBROUTINE WNCCST(TP)
C
C  Show computer statistics
C
C  Result:
C
C	CALL WNCCST( TP_J:I)
C				Show computing statistics on TP.
C	CALL WNCCSX( TP_J:I, TXT_C*:I)
C				Show statistics with text.
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
	INTEGER TP			!OUTPUT TYPE
	CHARACTER*(*) TXT		!OUTPUT TEXT
C
C  Function references:
C
C
C  Data declarations:
C
	REAL RAR(0:3)			!TIMES
	INTEGER IAR(0:3)		!COUNTS
C-
	CALL WNGCST(RAR,IAR)			!GET COMPUTING STATISTICS
	CALL WNCTXT(TP,'At !%T Elapsed: !EHD8  CPU: !EHD8  '//
	1		'I/O: !UJ  Faults: !UJ',
	1		RAR(0)/240.,RAR(1)/240.,IAR(0),IAR(1))
C
	RETURN
C
C WNCCSX
C
	ENTRY WNCCSX(TP,TXT)
C
	CALL WNGCST(RAR,IAR)			!GET COMPUTING STATISTICS
	IF (TXT.EQ.' ') THEN
	  CALL WNCTXT(TP,'At !%T Elapsed: !EHD8  CPU: !EHD8  '//
	1		'I/O: !UJ  Faults: !UJ',
	1		RAR(0)/240.,RAR(1)/240.,IAR(0),IAR(1))
	ELSE
	  CALL WNCTXT(TP,'!AS at !%T (Wall: !EHD8  CPU: !EHD8  '//
	1		'I/O: !UJ  P/F: !UJ)',
	1		TXT,RAR(0)/240.,RAR(1)/240.,IAR(0),IAR(1))
	END IF
C
	RETURN
C
C
	END
