C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_LINKF
C.Keywords:	Network Task-to-task Communication
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	911231 GvD - creation
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		0	no heap storage available
C			2	no event flag
C			4	no assign
C			6	get address client
C			8	listen
C			10	accept
C			12	bind
C			14	get address server
C			16	connect
C			18	close
C--------------------------------------------------------------------------
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINKF_SINIT   (SD,PORT,MAXC,SNODE)
C		     ENTRY LINKF_SACC    (SD)
C		     ENTRY LINKF_CINIT   (SD,PORT,SNODE)
C		     ENTRY LINKF_BREAD   (SD,BUF,LBUF,LOUT)
C		     ENTRY LINKF_BWRITE  (SD,BUF,LBUF,LOUT)
C		     ENTRY LINKF_SCCLOSE (SD,CHNR)
C		     ENTRY LINKF_SCEND   (SD,CHNR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4 LINKF_SACC,    LINKF_CINIT, LINKF_BREAD, LINKF_BWRITE
	INTEGER*4 LINKF_SCCLOSE, LINKF_SCEND
C
	INTEGER*4	SD		! (o,i) socket descriptor
	INTEGER*2	PORT		! (i) port where server is listening
C					      and default port for client
	INTEGER*4	MAXC		! (i) max nr of clients
	CHARACTER*(*)	SNODE		! (i) node name of server system
	BYTE		BUF(*)		! (o,i) bytes read/to be written
	INTEGER*4	LBUF		! (i) length of BUF
	INTEGER*4	LOUT		! (o) nr of bytes read / written
	INTEGER*4	CHNR		! (i) channel nr to close (1 or 2)
C					   or channels to free (1 or 2)
C
C.Purpose:	Interface between Fortran LINK routine and C LINKC routine
C		to store an error message in the message buffer.
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C--------------------------------------------------------------------------
C
	INTEGER*4	IS, IS1
	BYTE		MSG(80)
 
	INTEGER		LINKC_SINIT, LINKC_SACC,   LINKC_CINIT
	INTEGER		LINKC_BREAD, LINKC_BWRITE, LINKC_SCCLOSE, LINKC_SCEND
C
C
	IS = LINKC_SINIT    (MSG,SD,PORT,MAXC,SNODE)
	GOTO 900
C
	ENTRY LINKF_SACC    (SD)
	IS = LINKC_SACC     (MSG,SD)
	GOTO 900
C
	ENTRY LINKF_CINIT   (SD,PORT,SNODE)
	IS = LINKC_CINIT    (MSG,SD,PORT,SNODE)
	GOTO 900
C
	ENTRY LINKF_BREAD   (SD,BUF,LBUF,LOUT)
	IS = LINKC_BREAD    (MSG,SD,BUF,LBUF,LOUT)
	GOTO 900
C
	ENTRY LINKF_BWRITE  (SD,BUF,LBUF,LOUT)
	IS = LINKC_BWRITE   (MSG,SD,BUF,LBUF,LOUT)
	GOTO 900
C
	ENTRY LINKF_SCCLOSE (SD,CHNR)
	IS = LINKC_SCCLOSE  (MSG,SD,CHNR)
	GOTO 900
C
	ENTRY LINKF_SCEND   (SD,CHNR)
	IS = LINKC_SCEND    (MSG,SD,CHNR)
	GOTO 900
C
C
C				If error store message in buffer
C				(message is an ASCIZ string).
C
 900	IF (IS.NE.1) THEN
	    CALL WNCTXT(DWLOG,'!AZ',MSG)
	ENDIF
	LINKF_SINIT = IS
	RETURN
	END
