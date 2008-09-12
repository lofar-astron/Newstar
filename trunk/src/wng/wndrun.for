c+ WNDRUN.FOR
C  WNB 910828
C
C  Revisions:
C	WNB 920303	SUN problems ()
C
	LOGICAL FUNCTION WNDRUN()
C
C  Test RUN keyword
C
C  Result:
C
C	WNDRUN_L = WNDRUN
C			Test if the program should be run or not.
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
C
C  Function references:
C
C
C  Data declarations:
C
C-
	WNDRUN=RUNCD				!SET RUN CODE
C
	RETURN					!READY
C
C
	END
