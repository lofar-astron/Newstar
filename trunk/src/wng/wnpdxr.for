C+ WNPDXR.FOR
C  WNB 911121
C
C  Revisions:
C
	SUBROUTINE WNPDXR(ROUT,TYP,ID,VP)
C
C  Execute plot device driver
C
C  Result:
C
C	CALL WNPDXR( ROUT:I, TYP_J:I, ID_J:I, VP_J(*):I)
C				Execute the device routine ROUT for device ID
C				for type of action TYP with device parameters
C				in VP
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Entry points:
C
C
C  Arguments:
C
	EXTERNAL ROUT		!ROUTINE
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
	CALL ROUT(TYP,ID,VP)				!DO ROUTINE
C
	RETURN
C
C
	END
