C+NPLCLO.FOR
C CMV 940822
C
C       Close plot, check close status
C
C  Revisions:
C	CMV 940822	Created
C
C
	SUBROUTINE NPLCLO(DQID_I,NHV_I)
C
C  Result:
C
C	CALL NPLCLO(DQID_J(*):I,NHV_J(*):I)	Close plot, check output status
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER DQID_I(*)		!DEVICE ID
	INTEGER NHV_I(*)		!PAGE LAYOUT
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C CLOSE PLOT
C
	CALL WQ_MCLOSE (DQID_I,NHV_I)               !CLOSE DEVICE
C
C CHECK CLOSE STATUS
C
	IF (WQG_XSTAT(1:2).EQ.'>X') THEN	!EXIT ALL
	   NO_MORE=.TRUE.
	ELSE
	   IF (WQG_XSTAT(1:2).EQ.'>D') THEN	!ASKED HARDCOPY
						!NOT YET IMPLEMENTED
	   END IF
	END IF
C
C CLOSE WQ SYSTEM
C
	CALL WQCLOS	                       !CLOSE WQ SYSTEM
C
	RETURN
	END
