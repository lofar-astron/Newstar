C+ NFLDAT.FOR
C  WNB 930618
C
C  Revisions:
C
	SUBROUTINE NFLDAT
C
C  Get NFLAG program parameters
C
C  Result:
C
C	CALL NFLDAT	will ask and set all program parameters
C
C PIN references:
C
C	OPTION
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
C
C  Data declarations:
C
C-
C
C SET DEFAULTS
C
C
C GET OPTION
C
 100	CONTINUE
	IF (.NOT.WNDPAR('OPTION',OPTION,LEN(OPTION),J0,'QUIT')) THEN
	  OPTION='QUIT'				!ASSUME END
	ELSE IF (J0.LE.0) THEN
	  OPTION='QUIT'				!ASSUME END
	END IF
C
	RETURN					!READY
C
C
	END
