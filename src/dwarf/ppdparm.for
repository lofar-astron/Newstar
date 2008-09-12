C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_PARM
C.Keywords:	PPD File, Parameter Descriptions
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPS$PARB	! (r) address of parameter descr area
C	INTEGER*4	PPS$PROTB	! (r) address of prototype descr area
C	INTEGER*4	PPS$NXTPAR	! (m) address of parameter description
C	INTEGER*4	PPS$NXTPROT	! (m) address of prototype description
C	BYTE		PPDPD_(*)	! (m) prototype/parameter description
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some MOVE_BLI into MOVE_BLJ
C.Version:	940120 CMV - use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PARM_GET (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (i) offset of description
C
C.Purpose:	Read the parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no parameters at all (only for zero PDOFF)
C	error	PPD_SEQERROR	no PPD file open
C.Notes:
C	- The address of the description will be saved in PPS$NXTPAR.
C	- The description will be read into the common array PPDPD_.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, NBYTES
C
C
	IF (PPS$PARB.EQ.0) GOTO 999
	IF (PPS$PARB.EQ.UNDEF_J) GOTO 998
	PPS$NXTPAR = PPS$PARB+PDOFF
	IS = MOVE_BLJ (A_B(PPS$NXTPAR+PPDPD_LENG-A_OB),PPDPD$LENG,1)
	NBYTES = PPDPD$LENG
	IS = MOVE_BLB (A_B(PPS$NXTPAR+1-A_OB),PPDPD_,NBYTES)
C
	PPD_PARM_GET = PPD_SUCCESS
	RETURN
C
 998	PPD_PARM_GET = PPD_ENDOFFILE
	RETURN
C
 999	PPD_PARM_GET = MSG_SET (PPD_SEQERROR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PARM_NEXT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Read the first or next parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no more parameters
C	error	PPD_SEQERROR	no PPD file open
C.Notes:
C	- The offset of the next description is given in the current
C	  description (field PPDPD$FORW).
C	- The address of the description will be saved in PPS$NXTPAR.
C	- The description will be read into the common array PPDPD_.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, PDOFF, NBYTES
C
C
C					Get the offset of the first or next
C					description in the mapped PPD file
C					- make sure that a PPD file is open
C					- make sure that there are parameters
C
	IF (PPS$PARB.EQ.0) GOTO 999
	IF (PPS$PARB.EQ.UNDEF_J) GOTO 998
	IF (PPS$NXTPAR.EQ.0) THEN
		PDOFF = 0
	ELSE
		IS = MOVE_BLJ (A_B(PPS$NXTPAR+PPDPD_FORW-A_OB),PDOFF,1)
		IF (PDOFF.EQ.UNDEF_J) GOTO 998
	ENDIF
C
C					Read the description
C
	PPS$NXTPAR = PPS$PARB+PDOFF
	IS = MOVE_BLJ (A_B(PPS$NXTPAR+PPDPD_LENG-A_OB),PPDPD$LENG,1)
	NBYTES =PPDPD$LENG
	IS = MOVE_BLB (A_B(PPS$NXTPAR+1-A_OB),PPDPD_,NBYTES)
C
C
	PPD_PARM_NEXT = PPD_SUCCESS
	RETURN
C
 998	PPD_PARM_NEXT = PPD_ENDOFFILE
	RETURN
C
 999	PPD_PARM_NEXT = MSG_SET (PPD_SEQERROR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PROTO_GET (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (i) offset of description
C
C.Purpose:	Read the prototype parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no prototypes at all (only for zero PDOFF)
C	error	PPD_SEQERROR	no PPD file open
C.Notes:
C	- The address of the description will be saved in PPS$NXTPROT.
C	- The description will be read into the common array PPDPD_.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, NBYTES
C
C
	IF (PPS$PROTB.EQ.0) GOTO 999
	IF (PPS$PROTB.EQ.UNDEF_J) GOTO 998
	PPS$NXTPROT = PPS$PROTB+PDOFF
	IS = MOVE_BLJ (A_B(PPS$NXTPROT+PPDPD_LENG-A_OB),PPDPD$LENG,1)
	NBYTES = PPDPD$LENG
	IS = MOVE_BLB (A_B(PPS$NXTPROT+1-A_OB),PPDPD_,NBYTES)
C
	PPD_PROTO_GET = PPD_SUCCESS
	RETURN
C
 998	PPD_PROTO_GET = PPD_ENDOFFILE
	RETURN
C
 999	PPD_PROTO_GET = MSG_SET (PPD_SEQERROR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PROTO_NEXT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Read the first or next prototype parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no more prototype parameters
C	error	PPD_SEQERROR	no PPD file open
C.Notes:
C	- The offset of the next description is given in the current
C	  description (field PPDPD$FORW).
C	- The address of the description will be saved in PPS$NXTPROT.
C	- The description will be read into the common array PPDPD_.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLB, MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, PDOFF, NBYTES
C
C
C					Get the offset of the first or next
C					description in the mapped PPD file
C					- make sure that a PPD file is open
C					- make sure that there are prototypes
C
	IF (PPS$PROTB.EQ.0) GOTO 999
	IF (PPS$PROTB.EQ.UNDEF_J) GOTO 998
	IF (PPS$NXTPROT.EQ.0) THEN
		PDOFF = 0
	ELSE
		IS = MOVE_BLJ (A_B(PPS$NXTPROT+PPDPD_FORW-A_OB),PDOFF,1)
		IF (PDOFF.EQ.UNDEF_J) GOTO 998
	ENDIF
C
C					Read the description
C
	PPS$NXTPROT = PPS$PROTB+PDOFF
	IS = MOVE_BLJ (A_B(PPS$NXTPROT+PPDPD_LENG-A_OB),PPDPD$LENG,1)
	NBYTES = PPDPD$LENG
	IS = MOVE_BLB (A_B(PPS$NXTPROT+1-A_OB),PPDPD_,NBYTES)
C
C
	PPD_PROTO_NEXT = PPD_SUCCESS
	RETURN
C
 998	PPD_PROTO_NEXT = PPD_ENDOFFILE
	RETURN
C
 999	PPD_PROTO_NEXT = MSG_SET (PPD_SEQERROR,0)
	RETURN
	END
