C+ WNFINI.FOR
C  WNB 890724
C
C  Revisions:
C
	SUBROUTINE WNFINI
C
C  Initialise File system
C
C  Result:
C
C	CALL WNFINI		Make sure FCA queue and exit handler set
C
C  PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'FCQ_DEF'			!FCA QUEUE
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	EXTERNAL WNFEXH				!EXIT HANDLER ROUTINE
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
	IF (FCAEXH(1).EQ.0) CALL WNGSXH(FCAEXH(1),WNFEXH) !INIT EXIT HANDLER
C
	RETURN
C
C
	END
