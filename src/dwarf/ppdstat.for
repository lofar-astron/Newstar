C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_STAT
C.Keywords:	PPD File, Status Array
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPS_(*)		! (m) status array of mapped PPD file
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use indirect addressing
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_CLEAR ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Clear the status array of the mapped PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
C
	INTEGER*4	CLEAR_BLJ
C
	INTEGER*4	IS
C
C
	IS = CLEAR_BLJ (PPS_,PPS__LENGTH)
C
	PPD_STAT_CLEAR = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_FILL ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Fill the status array of the mapped PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C	- The fixed part of the status array is filled with information
C	  from the file-description block (first record of the PPD file).
C	- The variable part is initialized.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ
C
	INTEGER*4	IS
C
C
C					Copy the file description block
C					into the local array PPDFD_
C
	IS = MOVE_BLJ (A_B(PPS$MAPB-A_OB),PPDFD_,PPDFD__LENGTH)
C
C					Complete the fixed part of PPS_
C
	PPS$INXB = UNDEF_J
	IF (PPDFD$INDEX.NE.UNDEF_J) PPS$INXB = PPS$MAPB+PPDFD$INDEX-1
	PPS$PARB = UNDEF_J
	IF (PPDFD$PARM.NE.UNDEF_J) PPS$PARB = PPS$MAPB+PPDFD$PARM-1
	PPS$HLPB = UNDEF_J
	IF (PPDFD$HELP.NE.UNDEF_J) PPS$HLPB = PPS$MAPB+PPDFD$HELP-1
	PPS$PROTB = UNDEF_J
	IF (PPDFD$PARMPT.NE.UNDEF_J) PPS$PROTB = PPS$MAPB+PPDFD$PARMPT-1
	PPS$NRINX = PPDFD$NINDEX
C
C					Initialize the variable part of PPS_
C
	PPS$NXTPAR = 0
	PPS$NXTPROT = 0
	PPS$ENTYP = 0
	PPS$NRINXPR = 0
C
	PPD_STAT_FILL = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_INQ (MAPB,ADDR,HLPB)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	MAPB		! (o) address mapped PPD file
	INTEGER*4	ADDR		! (o) address current parm description
	INTEGER*4	HLPB		! (o) address help area
C
C.Purpose:	Get information from the status array of the mapped PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C	- MAPB = 0 if no PPD file is currently mapped
C	- ADDR = 0 if no current parameter was selected yet
C	- HLPB = UNDEF_J if the help area is empty
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
C
C
	MAPB = PPS$MAPB
	HLPB = PPS$HLPB
	ADDR = PPS$NXTPAR
	IF (PPS$ENTYP.NE.0) ADDR = PPS$NXTPROT
C
	PPD_STAT_INQ = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_INQT (PROTOTYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	PROTOTYPE	! (o) prototype parameter ?
C
C.Purpose:	Get the type of the last parameter read
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
C
C
	PROTOTYPE = PPS$ENTYP.EQ.1
C
	PPD_STAT_INQT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_SETT (PROTOTYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	PROTOTYPE	! (i) prototype parameter ?
C
C.Purpose:	Save the type of the parameter last read
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
C
C
	PPS$ENTYP = 0
	IF (PROTOTYPE) PPS$ENTYP = 1
C
	PPD_STAT_SETT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_STAT_SAVE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	PPD_STAT_RESTORE
C
C
C.Purpose:	Save/restore the status array of the mapped PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	sequence error
C.Notes:
C	- SAVE copies the current status block into the save array, and clears
C	  the status block. A PPD file must be open, and the save array must
C	  be empty.
C	- RESTORE copies the save array back into the status block, and clears
C	  the save array. No PPD file should be open, and the save array should
C	  not be empty.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
C
	INTEGER*4	MSG_SET  , MOVE_BLJ, CLEAR_BLJ
C
	INTEGER*4	IS, SAVESTAT(PPS__LENGTH)
		SAVE SAVESTAT
		DATA SAVESTAT /PPS__LENGTH*0/
C
C
	IF (PPS$MAPB.EQ.0 .OR. SAVESTAT(1).NE.0) GOTO 991
	IS = MOVE_BLJ (PPS_,SAVESTAT,PPS__LENGTH)
	IS = CLEAR_BLJ (PPS_,PPS_LENGTH)
C
	PPD_STAT_SAVE = PPD_SUCCESS
	RETURN
C
 991	PPD_STAT_SAVE = MSG_SET (PPD_SEQERROR,0)
	RETURN
C
C
C	-------------------------
	ENTRY PPD_STAT_RESTORE ()
C	-------------------------
C
	IF (PPS$MAPB.NE.0 .OR. SAVESTAT(1).EQ.0) GOTO 992
	IS = MOVE_BLJ (SAVESTAT,PPS_,PPS__LENGTH)
	IS = CLEAR_BLJ (SAVESTAT,PPS__LENGTH)
C
	PPD_STAT_RESTORE = PPD_SUCCESS
	RETURN
C
 992	PPD_STAT_RESTORE = MSG_SET (PPD_SEQERROR,0)
	RETURN
	END
