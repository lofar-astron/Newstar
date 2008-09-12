C+ WNDPOH.FOR
C  JPH 940907
C
C  Revisions:
C
C
	SUBROUTINE WNDPOH (PROMPT,OPTIONS,HELP)
C(
C		Set local prompt, options and help texts 
C
C  Result:
C
C	CALL WNDPOH(PROMPT_C:I, OPTIONS_C:I, HELP_C:I)
C		Sets local prompt, options and help texts in the DWARF parameter
C		interface. The former two override the .PPD-file strings, the 
C		latter is prefixed. Blank arguments are skipped, i.e. the
C		existing local string is left untouched. The settings remain 
C		until cleared (WNDPOHC) or altered.
C	CALL WNDPOHC
C		Clear the local values. This routine is intended for use by
C		WNDPAR and by those routines that call WNDPAR but for which the
C		calling programs calls WNDPOH (e.g. WNDSTA, WNDNOD, NSCPLS, ...)
C
C  The operating context of this module is described in a Newstar document 
C  wndpoh.txt
C
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
	CHARACTER*(*)	PROMPT
	CHARACTER*(*)	OPTIONS
	CHARACTER*(*)	HELP
C
C  Entry points:
C
C
C  Function references:
C
	INTEGER PPD_PRSTR_LSET, PPD_OPSTR_LSET, PPD_HSTR_LSET
C
C  Data declarations:
C
C
C-
C
	IF (PROMPT.NE.' ') I=PPD_PRSTR_LSET(PROMPT)
	IF (OPTIONS.NE.' ') I=PPD_OPSTR_LSET(OPTIONS)
	IF (HELP.NE.' ') I=PPD_HSTR_LSET(HELP)
C
	RETURN
C
C
	ENTRY WNDPOHC

	I=PPD_PRSTR_LSET(' ')
	I=PPD_OPSTR_LSET(' ')
	I=PPD_HSTR_LSET(' ')
	A_J(0)=0			! clear 'hold prompt etc.'
C
	RETURN
C
	END
