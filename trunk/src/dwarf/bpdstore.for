C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_STORE
C.Keywords:	PPD File, Store Parameter Description
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	HjV 930427 - Change size STRING from 2000 to 2500
C.Version:	HjV 930613 - Change size STRING from 2500 to 5000
C.Version:	HjV 940829 - Change size STRING from 5000 to 10000
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_STORE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Add a parameter description to the dynamic buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_UNAM_GET, PPD_DVSTR_GET
	INTEGER*4	BPD_PARM_PUT, BPD_PROTO_PUT, BPD_INDEX_PUT
	INTEGER*4	BP_DEF_CHECK, CPL_ERR_PUT
C
	CHARACTER	STRING*10000, NAME*16
	INTEGER*4	IS, LN, LMIN, PDOFF
	LOGICAL*4	PROTOTYPE
	INTEGER*4	LSTR, LNR
C
C
C					Add the description and an index
C					entry to the appropriate buffers
C
	IS = PPD_UNAM_GET (NAME,LN,LMIN,PROTOTYPE)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (PROTOTYPE) THEN
		IS = BPD_PROTO_PUT (PDOFF)
	ELSE
		IS = BPD_PARM_PUT (PDOFF)
	ENDIF
	IF (IAND(IS,1).NE.0) IS = BPD_INDEX_PUT (PDOFF)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Check default value string
C
	IS = PPD_DVSTR_GET (STRING,LSTR)
	IF (LSTR.GT.0) THEN
		IS = BP_DEF_CHECK (STRING(:LSTR))
		IF (IAND(IS,1).EQ.0) THEN
			IS = CPL_ERR_PUT (PPD_DEFVALINV,LNR)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
	ENDIF
C
C
	BPD_STORE = PPD_SUCCESS
	RETURN
C
 999	BPD_STORE = IS
	RETURN
	END
