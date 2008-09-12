C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_LINK
C.Keywords:	Network Task-to-task Communication
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Any
C.Comments:
C.Pdoc:
C	Task-to-task communication is the exchange of data over a logical link
C	between two programs running on the same or different network nodes.
C
C.Pdoc:	Setup
C	The communication is opened by one of the programs (the client) when
C	it requests a logical link to the other program (the server) via a call
C	to LINK_CL_START. The server on its turn completes the link by calling
C	LINK_ACCEPT. From then on, there is no distinction between the server
C	and the client.
C	The server must have been started before the client, otherwise the
C	client cannot make the connection.
C
C.Pdoc:	Exchange data
C	Either program can send data to or receive data from the other program
C	via LINK_WRITE and LINK_READ calls. The programs must cooperate, i.e.,
C	each WRITE in one program must match a READ in the other program.
C
C.Pdoc:	Wrapup
C	At any time, the server can stop a connection via LINK_SV_CLOSE.
C	Thereafter he can use LINK_ACCEPT to accept another connection.
C	The server can entirely be stopped via LINK_SV_END.
C	The client can be stopped via LINK_CL_END.
C
C.Version:	891202 FMO - creation
C.Version:	900519 FMO - new GEN_LUN calls and documentation
C.Version:	911231 GvD - rewritten to use Internet iso. DECnet
C.Version:	921207 HjV - Some lines to long for HP
C.Version:	01087  AXC - write .read changed
C--------------------------------------------------------------------------
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_SV_START (SD,TASK,SNODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (o) socket descriptor
	CHARACTER*(*)	TASK		! (i) name of server task
	CHARACTER*(*)	SNODE		! (i) node name of server host
C						blank = use default
C
C.Purpose:	Initiate a logical link
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C	- This function is called by the server program when it wants to start.
C	- TASK is the name of the server task to be executed and will be
C	  mapped to a port number.
C--------------------------------------------------------------------------
C
	INTEGER*4	STR_SIGLEN, STR_UPCASE
	INTEGER*4	LINK_GETTASK, LINKF_SINIT, LINK_SV_END, LINK_ERROR
C
	INTEGER*4	IS, IS1, NRCL, LENG
	INTEGER*2	PORT
	CHARACTER	NODEX*32
C
C
C				Get the port number for the given task.
C				Also get max #clients and default node.
C
	SD = 0
	IS = LINK_GETTASK (TASK,PORT,NRCL,NODEX)
	IF (IS.NE.1) GOTO 990
C
C				Get length of node name and convert to upper.
C				Append a 0 to the strings for C-routines.
C				Use node from caller if given.
C				Start the server.
C				Clear everything if error.
C
	IF (SNODE.NE.' ') NODEX = SNODE
	LENG = STR_SIGLEN(NODEX)
	IF (LENG.GE.LEN(NODEX)) LENG = LEN(NODEX) - 1
	IS   = STR_UPCASE (NODEX(:LENG))
	NODEX(LENG+1:LENG+1) = CHAR(0)
	IS = LINKF_SINIT (SD, PORT, NRCL, NODEX(:LENG))
	IF (IS.GT.1) THEN
	    IS1 = LINK_SV_END (SD)
	ENDIF
	IF (IS.NE.1) THEN
	    IS = LINK_ERROR (IS,TASK,NODEX(:LENG),PORT)
	ENDIF
C
C
 990	LINK_SV_START = IS
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_SV_ACCEPT (SD,TASK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (o) socket descriptor
	CHARACTER*(*)	TASK		! (i) name of server task
C
C.Purpose:	Accept a connection
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C	- This function is called by the server program when it wants to start
C	  a connection.
C--------------------------------------------------------------------------
C
	INTEGER*4	IS
C
	INTEGER		LINKF_SACC, LINK_ERROR
C
C
C					Accept a connection.
C
	IS = LINKF_SACC (SD)
	IF (IS.NE.1) THEN
	    IS = LINK_ERROR (IS,TASK,' ',0)
	ENDIF
C
	LINK_SV_ACCEPT = IS
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_CL_START (SD,TASK,SNODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (o) socket descriptor
	CHARACTER*(*)	TASK		! (i) name of server task
	CHARACTER*(*)	SNODE		! (i) node name of server host
C						blank = use default
C
C.Purpose:	Initiate a logical link
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C	- This function is called by the client program when it wants to start.
C	- TASK is the name of the server task to be connected to and will be
C	  mapped to a port number.
C--------------------------------------------------------------------------
C
	INTEGER*4	STR_SIGLEN, STR_UPCASE
	INTEGER*4	LINK_GETTASK, LINKF_CINIT, LINK_CL_END, LINK_ERROR
C
	INTEGER*4	IS, IS1, NRCL, LENG
	INTEGER*2	PORT
	CHARACTER	NODEX*32
C
C
C				Get the port number for the given task.
C				Also get max #clients and default server node.
C
	SD = 0
	IS = LINK_GETTASK (TASK,PORT,NRCL,NODEX)
	IF (IS.NE.1) GOTO 990
C
C				Get length of node name and convert to upper.
C				Use server node from caller if given.
C				Also convert client node name to uppercase.
C				Append a 0 to the strings for C-routines.
C				Start the client.
C				Clear everything if error.
C
	IF (SNODE.NE.' ') NODEX = SNODE
	LENG  = STR_SIGLEN(NODEX)
	IF (LENG.GE.LEN(NODEX)) LENG = LEN(NODEX) - 1
	IS    = STR_UPCASE (NODEX(:LENG))
	NODEX(LENG+1:LENG+1) = CHAR(0)
	IS = LINKF_CINIT (SD, PORT, NODEX(:LENG))
	IF (IS.GT.1) THEN
	    IS1 = LINK_CL_END (SD)
	ENDIF
	IF (IS.NE.1) THEN
	    IS = LINK_ERROR (IS,TASK,NODEX(:LENG),PORT)
	ENDIF
C
C
 990	LINK_CL_START = IS
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_READ  (SD,BUF,LBUF,LOUT)
C	             ENTRY LINK_WRITE (SD,BUF,LBUF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER		LINK_WRITE
C
	INTEGER*4	SD		! (i) socket descriptor
	BYTE		BUF(0:*)	! (o,i) bytes read / to write
	INTEGER*4	LBUF		! (i) length of BUF
	INTEGER*4	LOUT		! (o) #bytes actually read
C
C.Purpose:	Read or write a block on the logical link
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C	The first part (4 bytes) is the length of the block sent.
C	Large blocks are split into fragments of 4096 bytes.
C--------------------------------------------------------------------------
C
	INTEGER    MAX__LEN
	PARAMETER (MAX__LEN = 4096)
	BYTE       TMP(0:MAX__LEN-1)
C
	INTEGER IS, IS1, LBLK, LENG, REST, LMAX
C
	INTEGER LINKF_BREAD, LINKF_BWRITE, LINK_ERROR
	INTEGER MOVE_BLB
C
C
C				Read a block.
C				At least 4 bytes are needed to get the length.
C
	LBLK = 0
100	IS   = LINKF_BREAD (SD, TMP(LBLK), MAX__LEN-LBLK, LENG)
	IF (IS.NE.1) GOTO 900
	LBLK = LBLK + LENG
	IF (LBLK.LT.4) GOTO 100
C
C				Get and test the length.
C				Move the rest to the user buffer
C				(up to LBUF bytes).
C
	CALL LINK_NTOHJ (TMP, LOUT, 1)
	LBLK = LBLK - 4					!minus length field
CCC	write (20,1000) lout,lblk,lmax !!!
CCC1000	format (' to read:',3i10) !!!
	LMAX = LBLK
	IF (LMAX.GT.LBUF) LMAX = LBUF
	IS1  = MOVE_BLB (TMP(4), BUF, LMAX)
C
C				Read the rest if necessary.
C				Read it directly into the user buffer
C				in parts of max. MAX__LEN bytes.
C				Read up to LBUF bytes.
C
	LMAX = LOUT
	IF (LMAX.GT.LBUF) LMAX = LBUF
200	REST = LMAX - LBLK
	IF (REST.GT.0) THEN				!more to read into BUF
	    IF (REST.GT.MAX__LEN) REST = MAX__LEN
	    IS   = LINKF_BREAD (SD, BUF(LBLK), REST, LENG)
	    IF (IS.NE.1) GOTO 900
	    LBLK = LBLK + LENG
CCC	write (20,1100) leng  !!!
CCC1100	format (' read:',i10)  !!!
	    GOTO 200
	ENDIF
C
C				If the user buffer is too small:
C				- read the rest
C				- generate an error message
C
	IF (LOUT.GT.LBUF) THEN
250	    IF (LOUT.GT.LBLK) THEN			!still more to read
		IS   = LINKF_BREAD (SD, TMP, MAX__LEN, LENG)
		IF (IS.NE.1) GOTO 900
		LBLK = LBLK + LENG
		GOTO 250
	    ENDIF
	    CALL WNCTXT(DWLOG,
     *		'Buffer too small; !SL given, !SL bytes needed',
     *				LBUF,LOUT)
	    LOUT = LBUF
	    GOTO 900
	ENDIF
	GOTO 990
	
C
C
	ENTRY LINK_WRITE (SD,BUF,LBUF)
C
C				Write the length of the block before the buffer.
C				Write the first part of the buffer.
C				At least 4 bytes should be written to write
C				the length.
C
	CALL LINK_HTONJ (LBUF, TMP, 1)
	REST = LBUF + 4					!part of buffer that
	IF (REST.GT.MAX__LEN) REST = MAX__LEN		!can be written
	IS1  = MOVE_BLB (BUF, TMP(4), REST-4)
	LBLK = 0					!nothing written yet
300	IS   = LINKF_BWRITE (SD, TMP(LBLK), REST-LBLK, LENG)
	IF (IS.NE.1) GOTO 900
	LBLK = LBLK + LENG
	IF (LBLK.LT.4) GOTO 300
	LBLK = LBLK - 4
CCC	write (20,1010) lbuf,leng !!!
CCC1010	format (' to write:',2i10) !!!
C
C				Now write the remaining in parts of max.
C				MAX__LEN bytes.
C				REST is max. #bytes written per time
C				LENG is actual #bytes written per time
C
400	REST = LBUF - LBLK
	IF (REST.EQ.0) GOTO 990				!everything is written
	IF (REST.GT.MAX__LEN) REST = MAX__LEN
	IS   = LINKF_BWRITE (SD, BUF(LBLK), REST, LENG)
	IF (IS.NE.1) GOTO 900
	LBLK = LBLK + LENG
CCC	write (20,1110) leng !!!
CCC1110	format (' written:',i10) !!!
	GOTO 400
C
C
900	IS = LINK_ERROR (IS,' ',' ',0)
C
990	LINK_READ = IS
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_SV_CLOSE (SD)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (m) socket descriptor
C
C.Purpose:	Close the server communication socket
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C--------------------------------------------------------------------------
C
	INTEGER IS
C
	INTEGER LINKF_SCCLOSE, LINK_ERROR
C
C				Close the communication socket.
C
	IS = LINKF_SCCLOSE (SD,2)
	IF (IS.NE.1) THEN
	    IS = LINK_ERROR (IS,' ',' ',0)
	ENDIF
C
	LINK_SV_CLOSE = IS
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_SV_END (SD)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (m) socket descriptor
C
C.Purpose:	Close and release the server socket
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C--------------------------------------------------------------------------
C
C
	INTEGER LINKF_SCCLOSE, LINKF_SCEND
C
C				Close the socket.
C				Release everything.
C
	LINK_SV_END = LINKF_SCCLOSE (SD,1)
	LINK_SV_END = LINKF_SCEND   (SD,2)
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_CL_END (SD)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SD		! (i) socket descriptor
C
C.Purpose:	Close and release the client socket
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C.Notes:
C--------------------------------------------------------------------------
C
C
	INTEGER LINKF_SCCLOSE, LINKF_SCEND
C
C				Close the socket.
C				Release everything.
C
	LINK_CL_END = LINKF_SCCLOSE (SD,1)
	LINK_CL_END = LINKF_SCEND   (SD,1)
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_GETTASK (TASK,PORT,NRCL,NODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	TASK		! (i) name of server task
	INTEGER*2	PORT		! (o) port number of server task
	INTEGER*4	NRCL		! (o) max nr of possible clients
	CHARACTER*(*)	NODE		! (o) node name of server host
C
C.Purpose:	Get the number of the port where the server is listening
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C--------------------------------------------------------------------------
C
C				Define all tasks, ports and max. nr
C				of clients.
C
	INTEGER*4	NR_TASK
	PARAMETER	(NR_TASK = 3)
C
	CHARACTER*6 NAME(NR_TASK)
	DATA NAME      /'TAPE',
     *			'ANZA',
     *			'DID'/
C
	INTEGER*4   MAXC(NR_TASK)
	DATA MAXC /10,1,1/
C
	INTEGER*2   PRTN(NR_TASK)
	DATA PRTN /1100,1101,1104/
C
	CHARACTER*6 NONM(NR_TASK)
	DATA NONM /3*'RZMVX4'/
C
C
	INTEGER*4	IS
	CHARACTER*32	TASKX
C
	INTEGER*4	STR_UPCASE
C
C
C				Convert task name to uppercase.
C				Then try to find it.
C
	TASKX = TASK
	IS = STR_UPCASE (TASKX)
	DO I = 1,NR_TASK
	    IF (TASKX .EQ. NAME(I)) THEN
		PORT = PRTN(I)
		NRCL = MAXC(I)
		NODE = NONM(I)
	WRITE(*,*) 'port:'
	READ(*,*) port
		LINK_GETTASK = 1			!Success
		RETURN
	    ENDIF
	ENDDO
C
C			Task not found.
C
	CALL WNCTXT(DWLOG,'Update LINK.FOR to add a new server task')
	CALL WNCTXT(DWLOG,'Server task !AS is unknown',TASKX)
	LINK_GETTASK = 2
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_ERROR (ERR,TASK,SNODE,PORT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER		ERR		! (i) error code from LINKF module
	CHARACTER*(*)	TASK		! (i) name of server task
	CHARACTER*(*)	SNODE		! (i) node name of server host
	INTEGER*2	PORT		! (i) port number for server task
C
C.Purpose:	Generate message for error from LINKF
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	error		2	error report left in message buffer
C--------------------------------------------------------------------------
C
	INTEGER IS
C
C
C				Generate a message for the various errors
C
	IF (ERR.EQ.0) THEN
	    CALL WNCTXT(DWLOG,'No heap storage available')
	ELSEIF (ERR.EQ.2) THEN
	    CALL WNCTXT(DWLOG,'No event flag available')
	ELSEIF (ERR.EQ.4) THEN
	    CALL WNCTXT(DWLOG,'$ASSIGN for communication channel failed')
	ELSEIF (ERR.EQ.6) THEN
	    CALL WNCTXT(DWLOG,'Internet address of client host not found')
	ELSEIF (ERR.EQ.8) THEN
	    CALL WNCTXT(DWLOG,
     *		'Listen failed for task !AS on port !SW server !AS',
     *				TASK,PORT,SNODE)
	ELSEIF (ERR.EQ.10) THEN
	    CALL WNCTXT(DWLOG,'Accept failed for server task !AS',)
	ELSEIF (ERR.EQ.12) THEN
	    CALL WNCTXT(DWLOG,'Bind failed for task !AS on port !SW',
     *				TASK,PORT)
	ELSEIF (ERR.EQ.14) THEN
	    CALL WNCTXT(DWLOG,
     *		'Internet address of server !AS could not be found',
     *				SNODE)
	ELSEIF (ERR.EQ.16) THEN
	    CALL WNCTXT(DWLOG,
     *		'Connect failed to task !AS on port !SW server !AS',
     *				TASK,PORT,SNODE)
	ELSEIF (ERR.EQ.18) THEN
	    CALL WNCTXT(DWLOG,'Close failed')
	ELSEIF (ERR.EQ.20) THEN
	    CALL WNCTXT(DWLOG,'Read failed')
	ELSEIF (ERR.EQ.22) THEN
	    CALL WNCTXT(DWLOG,'Write failed')
	ENDIF
C
	LINK_ERROR = IS
	RETURN
	END
