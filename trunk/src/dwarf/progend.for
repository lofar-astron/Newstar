C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PROG_END
C.Keywords:	DWARF Program Exit
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	910820 FMO - recreation
C.Version:	920206 GvD - no optional arguments anymore
C			added entry PROG_END_STAT instead
C.Version:	940117 CMV - just call the exit handler and exit,
C			should pass an exit status.
C.Version:	940120 CMV - Changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PROG_END (STAT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	STAT		! (i) program exit status
C
C.Purpose:	Terminate the program (call interface)
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- Execute the declared exit handlers. Among them the one declared by
C	  PROG_START, which prints all messages that are still in the message
C	  buffer and sets the exit status, defines symbols for the parameter
C	  values that the user wanted to save (indicated via the SAVELAST 
C	  option), closes the the PPD file, closes the messenger and symbol 
C	  facility.
C	- Terminate the program with the given status.
C-------------------------------------------------------------------------
C
C
C	Execute the exit handlers and terminate the program
C
	CALL WNGSXX()
	CALL EXIT(STAT)
C
	PROG_END = DWC_SUCCESS			!does not get here
	RETURN
	END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE PROG_END_EXH ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C.Purpose:	Exit handler declared by PROG_START
C.Returns:	Not applicable
C.Notes:
C	- Print all messages that are still in the message buffer.
C	- Define symbols for the parameter values that the user wanted to save
C	  (indicated via the SAVELAST option).
C	- Close the PPD file
C	- Close the messenger (write an end message).
C-------------------------------------------------------------------------
C
	INTEGER		GP_CTL_END, PPD_EXIT, SYMBOL_EXIT, MSG_SET
	INTEGER		DWC_PROG_GET, DWC_STREAM_GET
C
	INTEGER		IS, LP, LS
	CHARACTER*30	PRG
	CHARACTER*8	ERRSTR(0:1)
	DATA		ERRSTR/'Error','Success'/
C
C
	IS = GP_CTL_END ()			!define all save symbols
	IS = PPD_EXIT ()			!close the PPD file
	IS = SYMBOL_EXIT ()			!close the symbol facility
	IS = DWC_PROG_GET (PRG,LP)		!get program name
	IF (IAND(IS,1).NE.0)
	1	IS = DWC_STREAM_GET (PRG(LP+1:),LS,.FALSE.) ! and stream
	IS = MSG_SET(GEN_ENDMESSAG,-1)		!get end message
	CALL WNCTXT(DWLOG,DWMSG,PRG,
	1	ERRSTR(IAND(E_C,1)))		!print it
C
	RETURN
	END
