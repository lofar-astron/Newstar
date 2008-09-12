c+ WNDINI.FOR
C  WNB 900130
C
C  Revisions:
C	HjV 930205	Prog_start needs two arguments
C	JPH 940912	WNDPOH
C
C
	LOGICAL FUNCTION WNDINI(PNM)
C
C  Initialise DWARF
C
C  Result:
C
C	WNDINI_J = WNDINI( PNM_C*:I)
C			Initialise DWARF for program PNM
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	CHARACTER*(*) PNM			!PROGRAM NAME
C
C  Function references:
C
	INTEGER PROG_START			!START DWARF
C
C  Data declarations:
C
C-
	E_C=PROG_START(PNM,0)			!INIT. DWARF
	CALL WNDPOHC				! init. dynamic defaults
	IF (IAND(E_C,1).EQ.1) THEN		!CHECK ERROR
	  WNDINI=.TRUE.
	ELSE
	  WNDINI=.FALSE.
	END IF
C
	RETURN					!READY
C
C
	END
