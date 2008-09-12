C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_READ
C.Keywords:	PPD File, Read Parameter Decsription
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_READ_P (PNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PNAM		! (i) program's parameter name
C
C.Purpose:	Read the description of the specified parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no parameters at all (only for blank PNAM)
C	false status codes returned by referenced routines
C.Notes:
C	- A blank name indicates the first normal parameter in the index.
C	- The address of the description will be saved in PPS$NXTPAR.
C	- The description will be read into common array PPDPD_.
C	- The index nr of the parameter will be saved in PPS$NRINXPR.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_INDEX_GETP, PPD_PARM_GET
C
	INTEGER*4	IS, PDOFF
C
C
C					Get the offset of the parameter
C					description in the mapped PPD file
C
	IS = PPD_INDEX_GETP (PNAM,PDOFF)
	IF (IAND(IS,1).EQ.0 .OR. IS.EQ.PPD_ENDOFFILE) GOTO 999
C
C					Read the description into common
C
	IS = PPD_PARM_GET (PDOFF)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	PPD_READ_P = PPD_SUCCESS
	RETURN
C
 999	PPD_READ_P = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_READ_U (UNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	UNAM		! (m) user's parameter name
C
C.Purpose:	Read the description of the specified parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no parameters at all (only for blank UNAM)
C	false status codes returned by referenced routines
C.Notes:
C	- A blank name or a single '$' indicates the first normal or prototype
C	  parameter in the description list (PIN order).
C	- If the name indicates a prototype parameter (starts with '$'),
C	  PPS$ENTYP = 1 will be set and the address of the description will
C	  be saved in PPS$NXTPROT. Otherwise, PPS$ENTYP = 0 and the address
C	  is saved in PPS$NXTPAR.
C	- The description will be read into common array PPDPD_.
C	- The index pointer PPS$NRINXPR will be set to zero.
C	- If an abbreviated name is given, the complete user's name will be
C	  returned in UNAM.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_SETT
	INTEGER*4	PPD_INDEX_GETU, PPD_PARM_GET, PPD_PROTO_GET
	INTEGER*4	STR_SIGLEN
C
	INTEGER*4	IS, PDOFF, LNAM
	LOGICAL*4	PROTOTYPE
C
C
C					Determine the parameter type
C
	LNAM = STR_SIGLEN (UNAM)
	PROTOTYPE = LNAM.GT.0 .AND. UNAM(1:1).EQ.'$'
	IF (PROTOTYPE) LNAM = LNAM-1
C
C					Get the offset of the parameter
C					description in the mapped PPD file
C
	IF (LNAM.GT.0) THEN
		IS = PPD_INDEX_GETU (UNAM,PDOFF)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ELSE
		PDOFF = 0
	ENDIF
C
C					Read the description into common
C					and save the parameter type
C
	IF (PROTOTYPE) THEN
		IS = PPD_PROTO_GET (PDOFF)
	ELSE
		IS = PPD_PARM_GET (PDOFF)
	ENDIF
	IF (IAND(IS,1).EQ.0 .OR. IS.EQ.PPD_ENDOFFILE) GOTO 999
	IS = PPD_STAT_SETT (PROTOTYPE)
C
C
	PPD_READ_U = PPD_SUCCESS
	RETURN
C
 999	PPD_READ_U = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_READ_PNXT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Read the description of the next parameter in PNAM order
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no more parameters
C	false status codes returned by referenced routines
C.Notes:
C	- The address of the description will be saved in PPS$NXTPAR.
C	- The description will be read into common array PPDPD_.
C	- The index pointer PPS$NRINXPR will be updated.
C	- PPD_READ_PNXT can be called after a PPD_INIT (the first parameter
C	  will be selected) or after a PPD_READ_P (the parameter following
C	  the one in READ_P will be selected).
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_INDEX_GETNXT, PPD_PARM_GET
C
	INTEGER*4	IS, PDOFF
C
C
C					Get the offset of the parameter
C					description in the mapped PPD file
C
	IS = PPD_INDEX_GETNXT (PDOFF)
	IF (IAND(IS,1).EQ.0 .OR. IS.EQ.PPD_ENDOFFILE) GOTO 999
C
C					Read the description into common
C
	IS = PPD_PARM_GET (PDOFF)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	PPD_READ_PNXT = PPD_SUCCESS
	RETURN
C
 999	PPD_READ_PNXT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_READ_UNXT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Read the description of the next parameter (in PIN order)
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no more prototype/parameters
C	false status codes returned by referenced routines
C.Notes:
C	- The address of the description will be saved in PPS$NXTPAR
C	  for PPS$ENTYP = 0 or in PPS$NXTPROT if PPS$ENTYP = 1.
C	- The description will be read into common array PPDPD_
C	- PPD_READ_UNXT can be called after a PPD_INIT (the first parameter
C	  will be selected) or after a PPD_READ_U (the prototype/parameter
C	  following the one in READ_U will be selected).
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_INQT, PPD_PARM_NEXT, PPD_PROTO_NEXT
C
	INTEGER*4	IS
	LOGICAL*4	PROTOTYPE
C
C
C					Get parameter type
C
	IS = PPD_STAT_INQT (PROTOTYPE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Read the description into common
C
	IF (PROTOTYPE) THEN
		IS = PPD_PROTO_NEXT ()
	ELSE
		IS = PPD_PARM_NEXT ()
	ENDIF
	IF (IAND(IS,1).EQ.0 .OR. IS.EQ.PPD_ENDOFFILE) GOTO 999
C
	PPD_READ_UNXT = PPD_SUCCESS
	RETURN
C
 999	PPD_READ_UNXT = IS
	RETURN
	END
