C+ WNGEX.FOR
C  WNB 890308
C
C  Revisions:
C	WNB 910828	Add WNGEX0 for ^C handler
C	JPH 960622	Count control-C interrupts; re-establish handler
C
	SUBROUTINE WNGEX
C
C  Finish off everything
C
C  Result:
C
C	CALL WNGEX			Finish and close everything
C	CALL WNGEX0( A_J:I, B_J:I)	Finish off after ^C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WXH_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER A,B			!DUMMY FOR WNGEX0
C
C  Function references:
C
C
C  Data declarations:
C
C-
	GOTO 10
C
C WNGEX0
C
	ENTRY WNGEX0(A,B)
C
cc	print*,'X0',xhcc
	IF (XHCC(0).NE.0) THEN			! control-C inhibited
	  XHCC(1)=XHCC(1)+1			! SET 'SEEN'
	  CALL WNGSC0				! re-establish control-C handler
 	  RETURN				! return from interrupt
	END IF
	GOTO 10
C
 10	CONTINUE
cc	print*,'exit'
	CALL WNGSXX				!DO EXIT HANDLERS
	IF (IAND(1,E_C).EQ.1) E_C=1
	CALL EXIT(E_C)				!RETURN WITH ERROR CODE
C
	RETURN
C
C
	END
