C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_LENGTH
C.Keywords:	String, Significant length
C.Author:	Johan Hamaker (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.File:		[.SRC.GEN]STRLENGTH.FOR
C.Comments:
C.Version:	840220 JPH - creation VAX-macro version (GENLENCH)
C.Version:	880309 FMO - Fortran version; no minimum length of 1
C.Version:	920221 GvD - set correct file name in header
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_SIGLEN (STRING)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) character string
C
C.Purpose:	Get significant length of a character string
C.Returns:	Significant length
C.Notes:
C	The significant length of a string is the position of the last
C	non-blank/tab character (0 if there is none).
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, TAB, WHITE
		PARAMETER (BLANK=' ')
		PARAMETER (TAB='	')
		PARAMETER (WHITE=BLANK//TAB)
C
C
	I = LEN(STRING)
	DO WHILE (I.GT.0 .AND. INDEX(WHITE,STRING(I:I)).NE.0)
		I = I-1
	ENDDO
	STR_SIGLEN = I
	RETURN
C
C
	END
