C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_HELP
C.Keywords:	PPD File, Build, Help Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	BPD$HELP(1)	! (r) extend size in bytes
C	INTEGER*4	BPD$HELP(2)	! (m) current size in bytes
C	INTEGER*4	BPD$HELP(3)	! (m) current address
C	INTEGER*4	BPD$HELP(4)	! (m) nr of bytes written
C	INTEGER*4	BPD$HELP(5)	! (m) nr of entries written
C	INTEGER*4	BPD$HELP(6)	! (m) offset of last-written entry
C
C.Version:	900415 FMO - recreation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_HELP_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the help buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	CLEAR_BLJ
C
	INTEGER*4	IS
C
C
	IS = CLEAR_BLJ (BPD$HELP(2),5)
C
	BPD_HELP_INIT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_HELP_PUT (STRING,HOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) help text
	INTEGER*4	HOFF		! (o) offset of new entry
C
C.Purpose:	Add an entry to the help buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	STR_SIGLEN, CPL_DYN_PUT
C
	INTEGER*4	IS, LENGTH
C
C
	HOFF = BPD$HELP(4)
	LENGTH = STR_SIGLEN (STRING)
	IF (LENGTH.GT.0) THEN
		IS = CPL_DYN_PUT (LENGTH,%REF(STRING),BPD$HELP)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	BPD_HELP_PUT = PPD_SUCCESS
	RETURN
C
 999	BPD_HELP_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_HELP_WRITE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Write the help buffer to the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	CPL_DYN_WRITE
C
	INTEGER*4	IS
	LOGICAL*4	DO_RELEASE
C
C
	DO_RELEASE = .TRUE.
	IS = CPL_DYN_WRITE (BPD$HELP,DO_RELEASE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_HELP_WRITE = PPD_SUCCESS
	RETURN
C
 999	BPD_HELP_WRITE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_HELP_INQ (NHELP,LHELP)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NHELP		! (o) nr of entries
	INTEGER*4	LHELP		! (o) significant length in bytes
C
C.Purpose:	Get information about the help buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	NHELP = BPD$HELP(5)
	LHELP = BPD$HELP(4)
C
	BPD_HELP_INQ = PPD_SUCCESS
	RETURN
	END
