C+ WNFEXH.FOR
C  WNB 890724
C
C  Revisions:
C
	SUBROUTINE WNFEXH
C
C  Do exit handler for file system
C
C  Result:
C
C	CALL WNFEXH		Close/dismount all files
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
	DO WHILE (FCAQUE.NE.0)				!SCAN LIST
	  CALL WNFDMO(FCAQUE)				!CLOSE AND DISMOUNT
	END DO
C
	RETURN
C
C
	END
