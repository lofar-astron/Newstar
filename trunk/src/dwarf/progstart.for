C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PROG_START
C.Keywords:	DWARF, Program, Start
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900407 FMO - new code
C.Version:	920206 GvD - no optional arguments anymore
C			- add argument to MSG_INIT and MSG_SWRITE
C.Version:	940117 CMV - Call WNGSXH directly for PROG_END, remove
C			version checks, flags argument
C.Version:	940218 CMV - Add version number to start message
C.Version:      940301 HjV - Write line in $n_import/newstar.use
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PROG_START (PROG,flags)
C
C	Include files
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C	Arguments
C
	CHARACTER*(*)	PROG		! (i) program name
	INTEGER		flags
C
C.Purpose:	Start a DWARF program (call interface)
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	.FALSE. status codes will not occur (the program will stop)
C.Notes:
C	- Initialize the program control parameters.
C	- Start the messenger (and write a starting message).
C	- Open the PPD file.
C	- Close the messenger if the caller himself wants to start it.
C	- Declare the exit handler.
C	- If PROG_START detects an error, it will print a message and stop
C	  the program.
C-------------------------------------------------------------------------
C
	EXTERNAL	PROG_END_EXH
C
	INTEGER*4	DWC_CTL_OPEN, DWC_CTL_UPDATE, DWC_EXH_DECLR
	INTEGER*4	DWC_PROG_GET, DWC_STREAM_GET, DWC_PRCMODE_INQ
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		WNCALN
	INTEGER*4	PPD_INIT
C
	CHARACTER*30	PROGSTRM
	INTEGER*4	IS, LP, LS
C
C	Exit handler block for WNGSXH (put this in dsc file later)
C
	INTEGER*4	DWCEXH(6)
	SAVE		DWCEXH
C
C					Initialize the program control
C
	IS = DWC_CTL_OPEN ()				! ignore false return
	IS = DWC_CTL_UPDATE (PROG)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Start the messenger
C					- if subprocess: write message
C
	IS = DWC_PROG_GET (PROGSTRM,LP)
	IF (IAND(IS,1).NE.0)
	1		IS = DWC_STREAM_GET (PROGSTRM(LP+1:),LS,.FALSE.)
	IF (IAND(IS,1).NE.0) IS = MSG_INIT (PROGSTRM(:LP+LS), F_T)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IAND(DWC_PRCMODE_INQ('SUBPROCESS'),1) .NE. 0)
	1	IS = MSG_SET (DWC_IMGSUBPRC,0)
C
C					Open the PPD file
C
	IS = PPD_INIT (PROGSTRM(:LP))
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Declare the exit handler
C					- ignore false status (for Alliant)
C
	IF (DWCEXH(1).EQ.0) CALL WNGSXH(DWCEXH,PROG_END_EXH)
C
	IS = MSG_SET(GEN_STMESSAG,-1)
	J1 = WNCALN(PRGVER)
	CALL WNCTXT(DWLOG,DWMSG,
	1	PROGSTRM(:LP+LS)//' (v'//PRGVER(:J1)//')')
	CALL PRTUSE(PROGSTRM(:LP),PRGVER(:J1))
	PROG_START = DWC_SUCCESS
	RETURN
C
 999	PROG_START = MSG_SET(DWC_PROGSTERR,0)
C
	RETURN
	END

