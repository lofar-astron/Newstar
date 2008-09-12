C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_FILNAM
C.Keywords:	File Names
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	900415 FMO - creation
C.Version:	920225 GvD - no optional arguments anymore
C.Version:	920501 GvD - allow absence of colon (as in HOME[GVD.B]X.Y)
C.Version:	930923 CMV - revised for Unix
C.Version:	940315 CMV - treatment ./ and ../ as absolute paths
C-------------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION FILNAM_FULL (SPEC,XSPEC,LX,DEFSPEC)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SPEC		! (i) file specification
	CHARACTER*(*)	XSPEC		! (o) resulting spec
	INTEGER*4	LX		! (o) its length
	CHARACTER*(*)	DEFSPEC		! (i) default spec (optional)
C
C.Purpose:	Make full file specification
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C	error		2	CURD buffer overflow or invalid syntax
C	fatal	GEN_STROVFLO	XSPEC is too short
C	false status code returned by referenced routines
C.Notes:
C
C	DWARF file specifications on Unix systems are normal Unix paths
C	(absolute or relative) consisting of one or more components 
C	separated by flags. The trailing component (the filename) should 
C	have an extension following a dot. 
C
C	SPEC is always converted to lowercase, so pathspecifcations
C	that include Capitals should be supplied through DEFSPEC.
C	This is a consequence of the conventions in the CPL routines.
C
C	If SPEC is a relative path and if DEFSPEC is not empty and does
C	not start with a dot, DEFSPEC is used as a directory prefix. 
C	If DEFSPEC starts with a slash it is just prefixed, otherwise it
C	is treated as an environment variable and translated.
C
C	If the path in SPEC does not have an extension, and if DEFSPEC 
C	starts with a dot, than the extension in DEFSPEC (converted to
C	lowercase) is appended to SPEC to give the full filename.
C
C------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, SLASH, DOT, DOLLAR
		PARAMETER (BLANK = ' ')
		PARAMETER (SLASH = '/')
		PARAMETER (DOT   = '.')
		PARAMETER (DOLLAR = '$')
C
	INTEGER WNCALN
C
	CHARACTER	CURD*128, LSPEC*128
	INTEGER*4	LS, LD, IS
C
C	Clear XSPEC to start with, get length of strings
C
	XSPEC = BLANK
	LX = 0
	LS = WNCALN(SPEC)
	LD = WNCALN(DEFSPEC)
	LSPEC=SPEC(1:LS)
	CALL WNCALC(LSPEC(1:LS))
C
C	SPEC starts with slash, ./ or ../: copy absolute path
C
	IF (SPEC(1:1).EQ.SLASH      .OR.
	1   SPEC(1:2).EQ.DOT//SLASH .OR.
	1   SPEC(1:3).EQ.DOT//DOT//SLASH) THEN
	   XSPEC=LSPEC(1:LS)
C
C	SPEC is relative path, DEFSPEC is absolute
C
	ELSE IF (DEFSPEC.NE.BLANK.AND.DEFSPEC(1:1).EQ.SLASH) THEN
	   XSPEC=DEFSPEC(1:LD)//LSPEC(1:LS)
C
C	SPEC is relative path, DEFSPEC is not an extension
C
	ELSE IF (DEFSPEC.NE.BLANK.AND.DEFSPEC(1:1).NE.DOT) THEN
	   CALL WNGSEG(DEFSPEC,CURD)
	   IF (CURD.NE.' ') THEN
	      LD=WNCALN(CURD)
	      XSPEC=CURD(1:LD)//SLASH//LSPEC(1:LS)
	   ELSE
	      XSPEC=LSPEC(1:LS)
	   ENDIF
C
C	Otherwise just use the relative path
C
	ELSE
	   XSPEC=LSPEC(1:LS)
	ENDIF
C
C	Now check the extension
C
	LX=WNCALN(XSPEC)
	IF (DEFSPEC(1:1).EQ.DOT) THEN
	   I=LX
	   DO WHILE (I.GT.1.AND.XSPEC(I:I).NE.DOT
     &	                   .AND.XSPEC(I:I).NE.SLASH)
	     I=I-1
	   ENDDO
	   IF (XSPEC(I:I).NE.DOT) THEN
	      CALL WNCALC( DEFSPEC(1:LD) )
	      XSPEC(LX+1:)=DEFSPEC(1:LD)
	      LX=LX+LD
	   ENDIF
	ENDIF
C
C	That's all, success always
C
	FILNAM_FULL = GEN_SUCCESS
	RETURN
	END
