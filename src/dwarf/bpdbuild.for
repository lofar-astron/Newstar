C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_BUILD
C.Keywords:	PPD File, Build, Parameter Description
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920115 GvD - call PPD_MAX_XGET with %REF(VALUE) iso. VALUE
C.Version:	930427 HjV - Change size VALUE from 2000 to 2500
C.Version:	930613 HjV - Change size VALUE from 2500 to 5000
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940829 HjV - Change size VALUE from 5000 to 10000
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_BUILD (PROGNAM,PASSNR,DO_UPDATE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	INTEGER*4	PASSNR		! (i) compilation pass number
	LOGICAL*4	DO_UPDATE	! (i) update mode ?
C
C.Purpose:	Build the program-parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_INTREF	internal reference found in first pass
C	false status codes returned by referenced routines
C.Notes:
C	For each description field:
C	- Get the specification from the CPL work buffer (function CPL_WRK_GET).
C	- If nothing is specified and COPY is active, the template will be used
C	  and only the consistency checks will be performed.
C	- Check whether the specification is valid and consistent with the other
C	  fields, and store it in the description array PPDPD_. These actions
C	  are performed by the functions PPD_<field_name>_PUT.
C	- In case of syntax errors, the error code and the appropriate source-
C	  line nr are stored in the error buffer (CPL_ERR_PUT). The rest of the
C	  fields will still be analysed.
C	- For all other errors (like buffer overflows) the function returns
C	  immediately with an error status.
C
C	NOTE: The order in which the fields are analysed is important for the
C	consistency checking.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	CHARACTER*(*)	COLON
		PARAMETER (COLON = ':')
C
	INTEGER*4	CPL_WRK_GET,	CPL_ERR_PUT
	INTEGER*4	BPD_REF_UPDATE
	INTEGER*4	PPD_INIT,	PPD_EXIT,	PPD_READ_U
	INTEGER*4	PPD_LENG_INIT
	INTEGER*4	PPD_UNAM_PUT,	PPD_PNAM_PUT,	PPD_CMAS_PUT
	INTEGER*4	PPD_AMAS_PUT,	PPD_IOCD_PUT,	PPD_DTYPE_PUT
	INTEGER*4	PPD_PLEN_PUT,	PPD_NSETS_PUT,	PPD_NVAL_PUT
	INTEGER*4	PPD_MNVAL_PUT,	PPD_MXVAL_PUT
	INTEGER*4	PPD_MIN_XGET,	PPD_MIN_PUT
	INTEGER*4	PPD_MAX_XGET,	PPD_MAX_PUT
	INTEGER*4	PPD_USTR_XGET,	PPD_USTR_PUT
	INTEGER*4	PPD_SSTR_XGET,	PPD_SSTR_PUT
	INTEGER*4	PPD_DVSTR_XGET,	PPD_DVSTR_PUT
	INTEGER*4	PPD_OPSTR_XGET,	PPD_OPSTR_PUT
	INTEGER*4	PPD_PRSTR_XGET,	PPD_PRSTR_PUT
	INTEGER*4	PPD_HSTR_XGET,	PPD_HSTR_PUT
	INTEGER*4	STR_SIGLEN
C
	CHARACTER	VALUE*10000
	CHARACTER*16	PPDREF, UNAMREF, GROUP
	INTEGER*4	LVAL, LPREF, LUREF, LGRP
	INTEGER*4	LNR, LNRCOPY, NSEP
	INTEGER*4	IS, REFIS, TMP
	LOGICAL*4	DO_CHECK, DO_COPY, IS_EXTREF
C
C
C
C					COPY
C					====
C
	DO_COPY = .FALSE.
	IS = CPL_WRK_GET (W_COPY,LNRCOPY,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (LVAL.GT.0) THEN
		NSEP = INDEX (VALUE(:LVAL),COLON)
		IS_EXTREF = NSEP.GT.1
C
C					Internal reference
C					- if 1st pass: postpone the processing
C					  of this parameter till the 2nd pass
C					- if 2nd pass: handle as "external"
C					  reference to the 1st-pass PPD file
C
		IF (.NOT.IS_EXTREF) THEN
			IF (PASSNR.EQ.1) THEN
				BPD_BUILD = PPD_INTREF
				RETURN
			ELSE
				PPDREF = PROGNAM
				LPREF = STR_SIGLEN (PROGNAM)
				UNAMREF = VALUE(NSEP+1:LVAL)
				LUREF = STR_SIGLEN (UNAMREF)
			ENDIF
C
C					External reference:
C					- get the names of the referenced
C					  PPD file and program parameter
C
		ELSE
			PPDREF = VALUE(:NSEP-1)
			LPREF = STR_SIGLEN (PPDREF)
			IF (NSEP.LT.LVAL) THEN
				UNAMREF = VALUE(NSEP+1:LVAL)
			ELSE
				IS = CPL_WRK_GET (W_UNAM,LNR,VALUE,LVAL)
				UNAMREF = VALUE(:LVAL)
			ENDIF
			LUREF = STR_SIGLEN (UNAMREF)
		ENDIF
C
C					Get referenced parameter description
C					- open the referenced PPD file (it must
C					  exist in n_exe for an update)
C					- copy the relevant parameter
C					  description into PPDPD_
C					- set the DO_COPY flag
C					- update the list of references
C
		REFIS = PPD_INIT (PPDREF(:LPREF))
		IF (IAND(REFIS,1).NE.0) THEN
		    IS = PPD_READ_U (UNAMREF(:LUREF))
		    IF (IAND(IS,1).NE.0) THEN
			DO_COPY = .TRUE.
			IF (IS_EXTREF) THEN
			    IF (DO_UPDATE .AND. REFIS.NE.1)
	1			IS = CPL_ERR_PUT (PPD_PPDNOTFND,LNRCOPY)
			    IF (IAND(IS,1).NE.0)
	1			IS = BPD_REF_UPDATE (PPDREF(:LPREF)//COLON)
			ENDIF
		    ELSE
			TMP = PPD_EXIT ()
			IS = CPL_ERR_PUT (IS,LNRCOPY)
		    ENDIF
		ELSE
		    IS = CPL_ERR_PUT (REFIS,LNRCOPY)
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	IS = PPD_LENG_INIT ()
C
C					USER'S NAME (KEYWORD)
C					=====================
C
	IS = CPL_WRK_GET (W_UNAM,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IS = PPD_UNAM_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					PROGRAM'S NAME
C					==============
C					(after UNAM)
C
	IS = CPL_WRK_GET (W_PNAM,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IS = PPD_PNAM_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					CHECKS
C					======
C					(after COPY)
C
	IS = CPL_WRK_GET (W_CMAS,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_CMAS_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					ATTRIBUTES
C					==========
C					(after COPY and CMAS)
C
	IS = CPL_WRK_GET (W_AMAS,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_AMAS_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					INPUT/OUTPUT CODE
C					=================
C					(after COPY)
C
	IS = CPL_WRK_GET (W_IOCD,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_IOCD_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					DATA TYPE
C					=========
C					(after COPY and CMAS)
C
	IS = CPL_WRK_GET (W_DTYPE,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_DTYPE_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					PARAMETER LENGTH
C					================
C					(after COPY and DTYPE)
C
	IS = CPL_WRK_GET (W_PLEN,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_PLEN_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					MAXIMUM NR OF SETS
C					==================
C					(after COPY)
C
	IS = CPL_WRK_GET (W_NSETS,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_NSETS_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					NR OF VALUES PER SET
C					====================
C					(after COPY, AMAS and CMAS)
C
	IS = CPL_WRK_GET (W_NVAL,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_NVAL_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					MINIMUM NR OF VALUES PER SET
C					============================
C					(after COPY and NVAL)
C
	IS = CPL_WRK_GET (W_MNVAL,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_MNVAL_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					MAXIMUM NR OF VALUES PER SET
C					============================
C					(after COPY, NVAL and MNVAL)
C
	IS = CPL_WRK_GET (W_MXVAL,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_MXVAL_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					MINIMUM VALUES
C					==============
C					(after COPY, DTYPE, PLEN, NVAL, AMAS
C					 AND CMAS)
C
	IS = CPL_WRK_GET (W_MIN,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_MIN_XGET (%REF(VALUE),LEN(VALUE),LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_MIN_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					MAXIMUM VALUES
C					==============
C					(after COPY, DTYPE, PLEN, NVAL, AMAS
C					 AND CMAS)
C
	IS = CPL_WRK_GET (W_MAX,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_MAX_XGET (%REF(VALUE),LEN(VALUE),LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_MAX_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					UNITS
C					=====
C					(after COPY)
C
	IS = CPL_WRK_GET (W_USTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_USTR_XGET (VALUE,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_USTR_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					SEARCH STRATEGY
C					===============
C					(after COPY)
C
	IS = CPL_WRK_GET (W_SSTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	GROUP = ' '
	LGRP = 0
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_SSTR_XGET (VALUE,LVAL,GROUP,LGRP)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_SSTR_PUT (VALUE(:LVAL),GROUP(:LGRP),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					DEFAULT VALUES
C					==============
C					(after COPY and SSTR)
C
	IS = CPL_WRK_GET (W_DVSTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_DVSTR_XGET (VALUE,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_DVSTR_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					OPTIONS
C					=======
C					(after COPY, CMAS, DTYPE and PLEN)
C
	IS = CPL_WRK_GET (W_OPSTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_OPSTR_XGET (VALUE,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_OPSTR_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					PROMPT
C					======
C					(after COPY)
C
	IS = CPL_WRK_GET (W_PRSTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_PRSTR_XGET (VALUE,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_PRSTR_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
C					HELP
C					====
C					(after COPY)
C
	IS = CPL_WRK_GET (W_HSTR,LNR,VALUE,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DO_CHECK = .TRUE.
	IF (LVAL.EQ.0 .AND. DO_COPY) THEN
		LNR = LNRCOPY
		IS = PPD_HSTR_XGET (VALUE,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CHECK = .FALSE.
	ENDIF
C
	IS = PPD_HSTR_PUT (VALUE(:LVAL),DO_CHECK)
	IF (IAND(IS,1).EQ.0) THEN
		IS = CPL_ERR_PUT (IS,LNR)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	BPD_BUILD = PPD_SUCCESS
	IF (DO_COPY) IS = PPD_EXIT ()
	RETURN
C
 999	BPD_BUILD = IS
	IF (DO_COPY) IS = PPD_EXIT ()
	RETURN
	END
