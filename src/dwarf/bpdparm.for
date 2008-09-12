C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_PARM
C.Keywords:	PPD File, Build, Parameter-Description Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	BPD$PARM(1)	! (r) extend size in bytes
C	INTEGER*4	BPD$PARM(2)	! (m) current size in bytes
C	INTEGER*4	BPD$PARM(3)	! (m) current address
C	INTEGER*4	BPD$PARM(4)	! (m) nr of bytes written
C	INTEGER*4	BPD$PARM(5)	! (m) nr of description entries written
C	INTEGER*4	BPD$PARM(6)	! (m) offset of last-written entry
C	INTEGER*4	BPD$PROT(1)
C	INTEGER*4	BPD$PROT(2)
C	INTEGER*4	BPD$PROT(3)
C	INTEGER*4	BPD$PROT(4)
C	INTEGER*4	BPD$PROT(5)
C	INTEGER*4	BPD$PROT(6)
C
C	INTEGER*4	PPDPD$EXTEN	! (m) offset of extension area
C	INTEGER*4	PPDPD$FORW	! (m) offset of next description
C	BYTE		PPDPD_(*)	! (r) description
C
C.Version:	900415 FMO - recreation
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PARM_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the parameter description buffer
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
	IS = CLEAR_BLJ (BPD$PARM(2),5)
C
	BPD_PARM_INIT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PARM_PUT (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (o) offset of new entry
C
C.Purpose:	Add an entry to the parameter description buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status returned by referenced routines
C.Notes:
C	First the offsets of the extension area and the next entry are set.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	CPL_DYN_PUT
C
	INTEGER*4	IS, LENGTH
C
C
C					Complete the description entry
C					- extension and forward offsets
C
	PPDPD$EXTEN = UNDEF_J
	PPDPD$FORW = BPD$PARM(4)+PPDPD$LENG
C
C					Add the entry to the description buffer
C
	PDOFF = BPD$PARM(4)
	LENGTH = PPDPD$LENG
	IS = CPL_DYN_PUT (LENGTH,PPDPD_,BPD$PARM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_PARM_PUT = PPD_SUCCESS
	RETURN
C
 999	BPD_PARM_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PARM_PUTL (PDOFF,LUNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (i) offset of description entry
	INTEGER*4	LUNAM		! (i) user's name minimum-match length
C
C.Purpose:	Put the minimum-match length in the description entry
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ
C
	INTEGER*4	IS
C
C
	IS = MOVE_BLJ (LUNAM,
	1	A_B(BPD$PARM(3)+PDOFF+PPDPD_LUNAM-1-A_OB),1)
C
C
	BPD_PARM_PUTL = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PARM_WRITE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Write the parameter description buffer to the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	First, the forward pointer of the last description is set to UNDEF_J.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	MOVE_BLJ, CPL_DYN_WRITE
C
	INTEGER*4	IS
	LOGICAL*4	DO_RELEASE
C
C
	IF (BPD$PARM(2).NE.0)
	1	IS = MOVE_BLJ (UNDEF_J,
	1		A_B(BPD$PARM(3)+BPD$PARM(6)-A_OB),1)
C
	DO_RELEASE = .TRUE.
	IS = CPL_DYN_WRITE (BPD$PARM,DO_RELEASE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_PARM_WRITE = PPD_SUCCESS
	RETURN
C
 999	BPD_PARM_WRITE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PARM_INQ (NPARM,LPARM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NPARM		! (o) nr of entries
	INTEGER*4	LPARM		! (o) significant length in bytes
C
C.Purpose:	Get information about the parameter description buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	NPARM = BPD$PARM(5)
	LPARM = BPD$PARM(4)
C
	BPD_PARM_INQ = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PROTO_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the prototype description buffer
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
	IS = CLEAR_BLJ (BPD$PROT(2),5)
C
	BPD_PROTO_INIT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PROTO_PUT (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (o) offset of new entry
C
C.Purpose:	Add an entry to the prototype description buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status returned by referenced routines
C.Notes:
C	First the offsets of the extension area and the next entry are set.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	CPL_DYN_PUT
C
	INTEGER*4	IS, LENGTH
C
C
C					Complete the description entry
C					- extension and forward offsets
C
	PPDPD$EXTEN = UNDEF_J
	PPDPD$FORW = BPD$PROT(4)+PPDPD$LENG
C
C					Add the entry to the description buffer
C
	PDOFF = BPD$PROT(4)
	LENGTH = PPDPD$LENG
	IS = CPL_DYN_PUT (LENGTH,PPDPD_,BPD$PROT)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_PROTO_PUT = PPD_SUCCESS
	RETURN
C
 999	BPD_PROTO_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PROTO_PUTL (PDOFF,LUNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (i) offset of description entry
	INTEGER*4	LUNAM		! (i) user's name minimum-match length
C
C.Purpose:	Put the minimum-match length in the description entry
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ
C
	INTEGER*4	IS
C
C
	IS = MOVE_BLJ (LUNAM,
	1	A_B(BPD$PROT(3)+PDOFF+PPDPD_LUNAM-1-A_OB),1)
C
C
	BPD_PROTO_PUTL = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PROTO_WRITE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Write the prototype description buffer to the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	First, the forward pointer of the last description is set to UNDEF_J.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	MOVE_BLJ, CPL_DYN_WRITE
C
	INTEGER*4	IS
	LOGICAL*4	DO_RELEASE
C
C
	IF (BPD$PROT(2).NE.0)
	1	IS = MOVE_BLJ (UNDEF_J,
	1		A_B(BPD$PROT(3)+BPD$PROT(6)-A_OB),1)
C
	DO_RELEASE = .TRUE.
	IS = CPL_DYN_WRITE (BPD$PROT,DO_RELEASE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_PROTO_WRITE = PPD_SUCCESS
	RETURN
C
 999	BPD_PROTO_WRITE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_PROTO_INQ (NPROT,LPROT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NPROT		! (o) nr of entries
	INTEGER*4	LPROT		! (o) significant length in bytes
C
C.Purpose:	Get information about the prototype description buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	NPROT = BPD$PROT(5)
	LPROT = BPD$PROT(4)
C
	BPD_PROTO_INQ = PPD_SUCCESS
	RETURN
	END
