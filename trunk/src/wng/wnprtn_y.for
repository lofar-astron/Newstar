C+ WNPRTN_Y.FOR
C  WNB 911121
C
C  Revisions:
C
C  General routines WNP package
C
	LOGICAL FUNCTION WNP_ALLOC(NIN)
C
C  Result:
C
C	WNP_ALLOC_J = WNP_ALLOC( NIN_J:I)
C				Allocate OUT* areas if necessary
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER NIN			!NUMBER OF WORDS
C
C  Function references:
C
	LOGICAL WNGGVM			!GET MEMORY
C
C  Data declarations:
C
C-
	WNP_ALLOC=.TRUE.				!ASSUME OK
	IF (NIN.LE.WQG_LOUT) RETURN			!ENOUGH AVAILABLE
	IF (WQG_LOUT.GT.0) THEN				!DEALLOCATE
	  CALL WNGFVM(WQG_LOUT*LB_J,WQG_OUT1)
	  CALL WNGFVM(WQG_LOUT*LB_J,WQG_OUT2)
	  CALL WNGFVM(WQG_LOUT*LB_J,WQG_OUT3)
	END IF
	WNP_ALLOC=.FALSE.				!ASSUME ERROR
	WQG_LOUT=0
	IF (WNGGVM(NIN*LB_J,WQG_OUT1)) THEN		!ALLOCATE
	  IF (WNGGVM(NIN*LB_J,WQG_OUT2)) THEN
	    IF (WNGGVM(NIN*LB_J,WQG_OUT3)) THEN
	      WNP_ALLOC=.TRUE.				!OK
	      WQG_LOUT=NIN
	    END IF
	  END IF
	END IF
C
	RETURN
C
C
	END