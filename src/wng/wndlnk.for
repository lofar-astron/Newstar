C+ WNDLNK.FOR
C  WNB 900306
C
C  Revisions:
C	JPH 920222	Detailed comments, informative variable names
C	WNB 930303	Correct PARAMETER
C	JPH 930420	Legibilisation revisited
C	JPH 920527	Detailed comments, informative variable names
C
C
	LOGICAL FUNCTION WNDLNK(LHDP,NEWP,IDOFF,FCA)
C
C  Link an entity in a file
C
C  Result:
C
C	WNDLNK_L = WNDLNK( LHDP_J:I, NEWP_J:I, IDOFF_J:I, FCA_J:I)
C			Link a new area on disk at address NEWP at the tail
C			of a linked list with listhead LHDP in file FCA. If
C			IDOFF>0 an identification is set at offset IDOFF in
C			the new area.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
	INTEGER F,B,N				!FIELDS
	PARAMETER (F=0, B=1, N=2)		!forward/backward link fields
C
C  Arguments:
C
	INTEGER LHDP				!DISK POINTER LINK HEAD
	INTEGER NEWP				!DISK POINTER AREA
	INTEGER IDOFF				!OFFSET TO AN ID
	INTEGER FCA				!FILE CONTROL AREA
C
C  Function references:
C
	LOGICAL WNFRD				!READ FILE
	LOGICAL WNFWR				!WRITE FILE
C
C  Data declarations:
	INTEGER LHD(F:N)			!copy of listhead
	INTEGER NEW(F:B)			!copy of NEWP's LINK field
	INTEGER TAIL(F:B)			!copy of LINK field
						!of last element in queue
C-
	WNDLNK=.TRUE.				!ASSUME OK
C
C Read 12 bytes at offset LHDP into LHD:
C  The parent SGH's LINKG+LINKGN fields, i.e. the listhead plus length of the 
C  queue
	IF (.NOT.WNFRD(FCA,12,LHD,LHDP))
	1	GOTO 900
C
C Read 8 bytes af offset LHD(B) (which is the back link) into TAIL. This is 
C  the LINK field of the last area in the queue
	IF (.NOT.WNFRD(FCA,8,TAIL,LHD(B)))
	1	GOTO 900			!READ OLD LINK
C
C Link NEWP into queue; NEW is the new LINK field for NEWP
C
	NEW(F)=TAIL(F)				!successor of TAIL becomes 
						!successor of NEWP
	TAIL(F)=NEWP				!NEWP becomes successor of TAIL
	NEW(B)=LHD(B)				!TAIL becomes 
						!predecessor of NEW
	LHD(B)=NEWP				!NEW becomes tail
	IF (IDOFF.GT.0) LHD(N)=LHD(N)+1		!increment parent's LINKGN
C
C Copy NEW into NEWP's LINK field
	IF (.NOT.WNFWR(FCA,8,NEW,NEWP)) GOTO 900
C
C Write NEWP into the forward LINK field of NEW's predecessor
	IF (.NOT.WNFWR(FCA,4,TAIL,NEW(B))) GOTO 900
C
C Update back link and count fields in listhead
	IF (.NOT.WNFWR(FCA,8,LHD(B),LHDP+4))
	1	GOTO 900
C
C Copy old count value into NEWP's ID field
	IF (IDOFF.GT.0) THEN			!SET ID
	  IF (.NOT.WNFWR(FCA,4,LHD(N)-1,
	1	NEWP+IDOFF)) GOTO 900
	END IF
	RETURN
C
C ERROR
C
 900	CONTINUE
	WNDLNK=.FALSE.				!ERROR
	RETURN
C
C
	END
