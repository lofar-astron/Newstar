C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_UPCASE
C.Keywords:	String, Convert to upper case
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.File:		[.SRC.GEN]STRUPCASE.FOR
C.Comments:
C.Version:	880309 FMO - creation
C-------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_UPCASE (STRING)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) string to be modified
C
C.Purpose:	Convert a character string to uppercase
C.Returns:	1
C.Notes:
C-------------------------------------------------------------------------
C
	CHARACTER*26	LOW, UP
		PARAMETER (LOW = 'abcdefghijklmnopqrstuvwxyz')
		DATA       UP  / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
	INTEGER*4	PTR, INX
C
C
C				Uppercase conversion
C
	DO PTR = 1,LEN(STRING)
		INX = INDEX(LOW,STRING(PTR:PTR))
		IF (INX.GT.0) THEN
			STRING(PTR:PTR) = UP(INX:INX)
		ENDIF
	ENDDO
C
C				Set function value
C
	STR_UPCASE = 1
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_UPCCPY (STRING,STROUT,LENOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) source string
	CHARACTER*(*)	STROUT		! (m) destination string
	INTEGER*4	LENOUT		! (m) current used length of STROUT
C
C.Purpose:	Make uppercase copy of a character string
C.Returns:	Nr of characters copied or -nr of characters truncated
C.Notes:
C	First, LENOUT is forced to a correct value: >= 0 and <= LEN(STROUT).
C	Then, the source string is converted to upper case and appended
C	to STROUT(:LENOUT) as far as possible. LENOUT will be updated.
C-------------------------------------------------------------------------
C
	CHARACTER*26	LOW, UP
		PARAMETER (LOW = 'abcdefghijklmnopqrstuvwxyz')
		DATA       UP  / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
	INTEGER*4	PTR, INX
	INTEGER*4	LENSTR, SAVLEN, NCOPY
C
C
C
C				Determine lengths
C
	LENSTR = LEN(STRING)
	SAVLEN = MAX(0,MIN(LENOUT,LEN(STROUT)))	! current length of dest-string
	LENOUT = MIN(LEN(STROUT),SAVLEN+LENSTR)	! new length of dest-string
	NCOPY = LENOUT-SAVLEN			! actual length of copy-string
C
C				Uppercase/copy as much as possible
C
	DO PTR = 1,NCOPY
		INX = INDEX(LOW,STRING(PTR:PTR))
		IF (INX.GT.0) THEN
			STROUT(SAVLEN+PTR:SAVLEN+PTR) = UP(INX:INX)
		ELSE
			STROUT(SAVLEN+PTR:SAVLEN+PTR) = STRING(PTR:PTR)
		ENDIF
	ENDDO
C
C				Set function value
C
	IF (LENSTR.EQ.NCOPY) THEN		! complete copy
		STR_UPCCPY = NCOPY		! nr of characters copied
	ELSE					! incomplete copy
		STR_UPCCPY = NCOPY-LENSTR	! negated nr of lost characters
	ENDIF
C
	RETURN
	END
