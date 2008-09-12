C+ WNPDEX.FOR
C  WNB 910624
C
C  Revisions:
C
	SUBROUTINE WNPDEX(TYP,ID,VP)
C
C  Execute plot device driver
C
C  Result:
C
C	CALL WNPDEX( TYP_J:I, ID_J:I, VP_J(*):I)
C				Execute the device routine for device ID
C				for type of action TYP with device parameters
C				in VP
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQD_O_DEF'	!DEVICE AREA
C
C  Parameters:
C
C
C  Entry points:
C
C
C  Arguments:
C
	INTEGER TYP		!EXECUTION TYPE
	INTEGER ID		!DEVICE ID
	INTEGER VP(*)		!ROUTINE PARAMETERS
C
C  Function references:
C
C
C  Data declarations:
C
C-
	CALL WNPDXR(A_B(A_J((ID-A_OB)/LB_J+WQD_DVRT_J)-A_OB),TYP,ID,VP) !DO
C
	RETURN
C
C
	END
