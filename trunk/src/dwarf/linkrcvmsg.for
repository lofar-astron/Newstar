C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_LINK_RCVMSG
C.Keywords:	Messenger, Link
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Any
C.Comments:
C.Version:	920110 GvD - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_RCV_MSG (SD,BUF,BUFC)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (i) socket descriptor for the link
	BYTE		BUF(*)		! (i) message buffer
	CHARACTER*(*)	BUFC		! (i) message buffer as a string
C
C.Purpose:	Write a message received via the link
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	set to status of message received
C.Notes:	Integers are received in network byte order
C		(ie. opposite to VAX)
C-------------------------------------------------------------------------
C
	INTEGER*4	IS ,STS, LENG
C
C					Set the status and message length
C					(convert to host byte order)
C
	CALL LINK_NTOHJ (BUF(5), STS,  1)
	CALL LINK_NTOHJ (BUF(9), LENG, 1)
C
C					Write the message (if any)
C					Return the status
C
	IF (LENG.GT.0) THEN
	    CALL WNCTXT(DWLOG,BUFC(13:12+LENG))
	ENDIF
	LINK_RCV_MSG = STS
	RETURN
	END
