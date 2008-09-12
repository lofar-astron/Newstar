C+ WNPCID.FOR
C  WNB 910624
C
C  Revisions:
C
	LOGICAL FUNCTION WNPCID(ID)
C
C  Check if device present
C
C  Result:
C
C	WNPCID_L= WNPCID( ID_J:I)
C				Check if device pointed to by ID is present.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE	'WQD_O_DEF'		!DEVICE AREA
C
C  Parameters:
C
C
C  Entry points:
C
C
C  Arguments:
C
	INTEGER ID		!DEVICE ID
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C CHECK ID
C
	WNPCID=.TRUE.					!ASSUME PRESENT
	IF (ID.NE.0) THEN				!MAYBE OPEN
	  J=WQG_QOP					!START DEVICE QUEUE
	  DO WHILE (J.NE.0)				!CHECK QUEUE
	    IF (ID.EQ.J) RETURN				!PRESENT
	    J=A_J((J-A_OB)/LB_J)			!NEXT IN LIST
	  END DO
	END IF
	WNPCID=.FALSE.					!NOT FOUND
C
	RETURN
C
C
	END
