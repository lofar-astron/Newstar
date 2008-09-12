C+++
C.Ident:	STR_COPY
C.Keywords:	String, Copy
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.File:		[.SRC.GEN]STRCOPY.FOR
C.Comments:
C.Version:	880516 FMO - creation
C-------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_COPY_R (STRIN,STROUT,LOUT,WIDTH)
C	          ENTRY    STR_COPY   (STRIN,STROUT,LOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	STR_COPY	! entry point
C
	CHARACTER*(*)	STRIN		! (i) source string
	CHARACTER*(*)	STROUT		! (m) destination string
	INTEGER*4	LOUT		! (m) its current length
	INTEGER*4	WIDTH		! (i) width of destination field
C
C.Purpose:	Copy string with or without right-adjustment
C.Returns:	Nr of characters copied or -nr of characters truncated
C.Notes:
C	First, LOUT is forced to a correct value: >= 0 and <= LEN(STROUT).
C	Then, if WIDTH is given and positive, the source string is written
C	right-adjusted into a field of that width (padded with blanks or
C	truncated on the left), and the field is appended to the destination
C	string. If necessary, the field is truncated on the left.
C	If WIDTH is not given or not positive, the source string is appended
C	to the destination string (truncated on the right if necessary).
C-------------------------------------------------------------------------
C
	INTEGER*4	SAVL, NDIF
C
C
	IF (WIDTH.LE.0) GOTO 100			! no right adjust
C
	SAVL = MAX(0,MIN(LOUT,LEN(STROUT)))		! destination length
	LOUT = MIN(LEN(STROUT),SAVL+WIDTH)		! new dest length
	NDIF = LOUT-SAVL-LEN(STRIN)			! nr chars to pad/trunc
C
	IF (LEN(STRIN).EQ.0) THEN			! append blanks
		IF (NDIF.GT.0) STROUT(SAVL+1:LOUT) = ' '
		STR_COPY_R = 0
	ELSE IF (NDIF.GT.0) THEN			! padded copy
		STROUT(SAVL+1:SAVL+NDIF) = ' '
		STROUT(SAVL+NDIF+1:LOUT) = STRIN
		STR_COPY_R = LEN(STRIN)
	ELSE IF (NDIF.EQ.0) THEN			! exact copy
		STROUT(SAVL+1:LOUT) = STRIN
		STR_COPY_R = LEN(STRIN)
	ELSE						! truncated copy
		STROUT(SAVL+1:LOUT) = STRIN(1-NDIF:)
		STR_COPY_R = NDIF
	ENDIF
C
	RETURN
C
C
	ENTRY STR_COPY (STRIN,STROUT,LOUT)
C
 100	SAVL = MAX(0,MIN(LOUT,LEN(STROUT)))		! destination length
	LOUT = MIN(LEN(STROUT),SAVL+LEN(STRIN))		! new dest length
	NDIF = LOUT-SAVL-LEN(STRIN)			! nr chars to truncate
C
	IF (LEN(STRIN).EQ.0) THEN			! empty copy
		STR_COPY = 0
	ELSE IF (NDIF.EQ.0) THEN			! exact copy
		STROUT(SAVL+1:LOUT) = STRIN
		STR_COPY = LEN(STRIN)
	ELSE						! truncated copy
		STROUT(SAVL+1:LOUT) = STRIN
		STR_COPY = NDIF
	ENDIF
C
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_COPY_U (MATCH,STRIN,PTR,STROUT,LOUT)
C	INTEGER*4 ENTRY    STR_COPY_W (MATCH,STRIN,PTR,STROUT,LOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	STR_COPY_W
C
	CHARACTER*(*)	MATCH		! (i) match characters
	CHARACTER*(*)	STRIN		! (i) source string
	INTEGER*4	PTR		! (m) current position in source string
	CHARACTER*(*)	STROUT		! (m) destination string
	INTEGER*4	LOUT		! (m) its current length
C
C.Purpose:	Copy characters until/while the character matches
C.Returns:	Nr of characters copied or -nr of characters truncated
C.Notes:
C	First a skip_until or skip_while operation is performed on the
C	source string to find the substring to be copied.
C	Then the substring is appended to the destination string via STR_COPY.
C
C	Conceptually, the source string is preceded and followed by an
C	infinite number of non-skip characters:
C	The end of the string corresponds to PTR=LEN(STRIN)+1). If at the
C	start PTR < 1 or PTR > LEN(STRIN), the function just forces LOUT
C	to be correct and returns a function value 0, leaving PTR unchanged.
C-------------------------------------------------------------------------
C
	INTEGER*4	STR_SKIP_U, STR_SKIP_W, STR_COPY
C
	INTEGER*4	SAVP, NSKIP
C
C
C					Find substring to be copied
C
	SAVP = PTR
	NSKIP = STR_SKIP_U (MATCH,STRIN,PTR)
	GOTO 100
C
	ENTRY STR_COPY_W (MATCH,STRIN,PTR,STROUT,LOUT)
C
	SAVP = PTR
	NSKIP = STR_SKIP_W (MATCH,STRIN,PTR)
	GOTO 100
C
C					Do the copy
C
 100	STR_COPY_U = STR_COPY (STRIN(SAVP:PTR-1),STROUT,LOUT)
C
	RETURN
	END
