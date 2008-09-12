C+ WNDDAP.FOR
C  WNB 900913
C
C  Revisions:
C	WNB 930602	Add IREF, CLK
C	WNB 930607	Add WNDDUF
C	JPH 930615	Remove comments on flag bits
C	JEN 931209	Add entry-points WNDDUF_SET and WNDDAP_SET
C	WNB 940215	Add WNDDAM
C
	SUBROUTINE WNDDAP(CAP,CDAP)
C
C  Get/set current apply/de-apply corrections or UFLAG bits
C
C  Result:
C
C	CALL WNDDAP( CAP_J:O, CDAP_J:O)
C			Return the current apply (CAP) and de-apply (CDAP)
C			corrections.
C	CALL WNDDUF( CAP_J:O)
C			Return the current unflag (CAP) flags
C	CALL WNDDAM( CAP_J:O)
C			Return the current memory chunk size
C	CALL WNDDAP_SET( CAP_J:I, CDAP_J:I)
C			Override the current apply (CAP) and de-apply (CDAP)
C			corrections.
C	CALL WNDDUF_SET( CAP_J:I)
C			Override the current unflag (CAP) flags
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WND_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER CAP		!CORRECTIONS TO APPLY
	INTEGER CDAP		!CORRECTYIONS TO DE-APPLY
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C WNDDAP
C
	CAP=XCAP				!GET
	CDAP=XCDAP
C
	RETURN					!READY
C
C WNDDUF
C
	ENTRY WNDDUF(CAP)
C
	CAP=XUFLAG				!GET
C
	RETURN
C
C WNDDAM
C
	ENTRY WNDDAM(CAP)
C
	CAP=XMEM				!GET
C
	RETURN
C
C WNDDAP_SET
C
	ENTRY WNDDAP_SET(CAP,CDAP)
C
	XCAP=CAP				!SET
	XCDAP=CDAP
C
	RETURN					!READY
C
C WNDDUF_SET
C
	ENTRY WNDDUF_SET(CAP)
C
	XUFLAG=CAP				!SET
C
	RETURN
C
	END
