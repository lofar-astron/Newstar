C+ WNGSRT.FUN
C  WNB 900327
C
C  Revisions:
C	WNB 921216	Make FUN
C
	LOGICAL FUNCTION WNGSRT(AREA,NREC,RECL,ROUT)
C
C  Sort a buffer in memory
C
C  Result:
C
C	WNGSRT_L = WNGSRT ( AREA_B(*):IO, NREC_J:I, RECL_J:I, ROUT_EXT:I)
C				Sort the AREA with NREC records of length
C				RECL using the routine ROUT. ROUT is
C				a function with at least 2 arguments,
C				the records to be compared. It returns
C				a J value:	 0: equal value
C						-1: 1st before 2nd
C						+1: 2nd before first
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
	BYTE AREA(*)				!AREA TO SORT
	INTEGER NREC				!# OF RECORDS TO SORT
	INTEGER RECL				!LENGTH ONE RECORD
	EXTERNAL ROUT				!COMPARISON ROUTINE
C
C  Function references:
C
C
C  Data declarations:
C
C-
	WNGSRT=.TRUE.					!ASSUME OK
	CALL QSORT(AREA,NREC,RECL,ROUT)			!DO SORT
C
	RETURN
C
C
	END
