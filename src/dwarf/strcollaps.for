C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_COLLAPS
C.Keywords:	String, Collapse
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	821115 GVD - creation DWC_REMBLK
C.Version:	881018 FMO - complete revision
C.Version:	920221 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_COLLAPS (STRING)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) character string
C
C.Purpose:	Remove blanks and tabs that are not part of a quoted substring
C.Returns:	Significant length of collapsed string
C.Notes:
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	QUOTE, BLANK, TAB, WHITE
		PARAMETER (QUOTE = '"')
		PARAMETER (BLANK = ' ')
		PARAMETER (TAB   = '	')
		PARAMETER (WHITE = BLANK//TAB)
C
	INTEGER*4	STR_SIGLEN, STR_COPY, STR_COPY_U, STR_SKIP_W
C
	CHARACTER*255	WORK
	INTEGER*4	IS, PTR, LS, LW
C
C
	LS = STR_SIGLEN (STRING)
	PTR = 1
	LW = 0
C
	IS = STR_COPY_U (WHITE//QUOTE,STRING(:LS),PTR,WORK,LW)
							if (is.lt.0) goto 999
	DO WHILE (PTR.LE.LS)
		IF (STRING(PTR:PTR).NE.QUOTE) THEN
			IS = STR_SKIP_W (WHITE,STRING(:LS),PTR)
		ELSE
			IS = STR_COPY (QUOTE,WORK,LW)
			PTR = PTR+1
			IS = STR_COPY_U (QUOTE,STRING(:LS),PTR,WORK,LW)
			IF (PTR.LE.LS) THEN
				IS = STR_COPY (QUOTE,WORK,LW)
				PTR = PTR+1
			ENDIF
							if (is.lt.0) goto 999
		ENDIF
		IS = STR_COPY_U (WHITE//QUOTE,STRING(:LS),PTR,WORK,LW)
							if (is.lt.0) goto 999
	ENDDO
C
	STRING = WORK(:LW)
	STR_COLLAPS = LW
	RETURN
C
 999	IS = 4
	CALL WNCTXT(DWLOG,'Work string in STR_COLLAPS is too short')
	STR_COLLAPS = LS
	RETURN
C
	END
