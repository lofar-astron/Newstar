C+MSG.FOR
C CMV 940121
C
C   Revisions:
C	CMV 940121	Created
C
C   This module is a replacement for the original DWARF Message Facility
C   designed by Henny Lem, adapted by Friso Olnon and Ger van Diepen.
C
C   The facility has been stripped down completely because hardly anything
C   of the functionality was being used in Newstar. The facility has been
C   redefined to be used together with the WNG/Newstar routines. 
C
C   The routine WNGIN has to be called before any of the messenger 
C   routines is being used. The file 'WNG_DEF' has to be included.
C   This makes two global variables available: DWLOG and DWMSG. 
C   DWLOG is the "output device" to be used in calls to WNCTXT.
C   DWMSG is used by MSG_CODE (see below) to set the message text.
C
C   The routines typically used for passing messages are:
C
C      IS = MSG_INIT(PROGRAM_C(*):I,FILES_J:I)
C	 Define the output files for the messenger, depending on the
C	 value of FLAG which should be a code that can be used in 
C	 calls to WNCTXT (typically either F_T or F_TP). Program will
C        be used to prefix messages and can be any string in fact.
C
C      IS = MSG_SET(MSGCODE_J:I,FLAG_J:I)
C        Return the MSGCODE to IS and set the message string in DWMSG
C        Typically followed by CALL WNCTXT(DWLOG,DWMSG,...)
C	 If FLAG.EQ.0, the message is written immediately, assuming 
C	 there are no additional arguments necessary; if the MSGCODE
C	 is the same as in the previous call, it is not printed.
C	 If FLAG.LT.0  the program name is not prefixed to the message
C
C	 At the moment, only error-messages are printed when FLAG.EQ.0
C	 
C
C      CALL WNCTXT(DWLOG,Format,...)
C
C   There is no need to close the messenger. If printfiles other than
C   F_T and F_P are used in MSG_INIT, they can best be given a name
C   through a call to WNCFSN
C
C-
C
	INTEGER FUNCTION MSG_INIT (PROGNAM,CODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	INTEGER		CODE		! (i) output code
	INTEGER		MESS		! (i) message number
	INTEGER		FLAG		! (i) prefix program name
C
C	Entry points
C
	INTEGER		MSG_SET
C
C	Function references
C
	INTEGER		WNCALN
C
C	Static variables
C
	INTEGER		LAST_STAT
	CHARACTER*25	PROG
	DATA		PROG/' '/,LAST_STAT/0/
	SAVE		PROG,LAST_STAT
C
	PROG=PROGNAM
	DWLOG=CODE
	IF (IAND(DWLOG,F_ALL).EQ.0) DWLOG=F_T	!No code: to terminal
	CALL WNCFOP(DWLOG,' ')			!Open files
	MSG_INIT=DWC_SUCCESS			!Success of course
	RETURN
C
	ENTRY MSG_SET(MESS,FLAG)
C
	CALL GEN_GETMSG(MESS,J,DWMSG,J1,B0)	!Get message text
	IF (FLAG.GE.0.AND.PROG.NE.' ') 
	1	DWMSG=PROG(1:WNCALN(PROG))//': '//DWMSG !Prefix
	IF (FLAG.EQ.0.AND.MESS.NE.DWC_SUCCESS.AND.
	1	IAND(MESS,1).EQ.0.AND.
	1	MESS.NE.LAST_STAT) 
	1	CALL WNCTXT(DWLOG,DWMSG)	!Output directly
	LAST_STAT=MESS				!Save code
	MSG_SET=MESS				!Return code
	RETURN
C
	END
