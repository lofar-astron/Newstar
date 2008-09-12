C+ NGIDMP.FOR
C  WNB 930514
C
C  Revisions:
C	WNB 930517	Blank data points outside range
C	WNB 930602	Correct compression range
C	CMV 931029	Changed parameter list
C	CMV 931220	Correct compression 
C	CMV 940120	Correct test on buffer size (was XSIZ*LB_E)
C	CMV 940506	Increase buffer size
C	CMV 940929	Correct rounding if odd number of pixels on axis
C
	LOGICAL FUNCTION NGIDMP(MPHJ,MID,SID)
C
C  Load a map into the GIDS-display
C
C  Result:
C
C       NGIDMP_L = NGIDMP( MPHJ_J(0:*):I, MID_C(*):I, SID_C(*):I )
C		      Load map in GIDS display GID. Using:
C				MPHJ	map header
C				MID,SID GIDS Headers
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'
	INCLUDE 'NGI_DEF'
C
C  Parameters:
C
	INTEGER BUFSIZ		!SIZE LINE BUFFER
	  PARAMETER (BUFSIZ=2*8192)
C
C  Arguments:
C
	INTEGER MPHJ(0:MPHHDL/4-1) !MAP HEADER
	CHARACTER MID*(*),SID*(*)  !GIDS Header
C
C  Function references:
C
	INTEGER N_GDI_IMWRITE 
	LOGICAL WNFRD		!READ FILE
	INTEGER	WNCALN		!LENGTH STRING
	LOGICAL NGITRA		!SHOW DATA
C
C  Data declarations:
C
	INTEGER LPTR		!DATA POINTER
	INTEGER XX,YY,DX2
	REAL RBUF(BUFSIZ),DBUF(BUFSIZ)
C
C-
	NGIDMP=.TRUE.				!ASSUME OK
C
	IPTR=0					!OFFSET IN BYTE BUFFER
C
	DX2=MPHJ(MPH_NRA_J)/2
	LPTR=MPHJ(MPH_MDP_J)+LB_E*
	1	(MPHJ(MPH_NDEC_J)*DX2+TEAR(2)*MPHJ(MPH_NRA_J)) !FIRST LINE TO READ
C
C	Some tests
C
	IF (BUFSIZ.LT.XSIZ .OR. BUFSIZ.LT.TAREA(2)) THEN
	   CALL WNCTXT(F_TP,'Internal buffer too small...')
	   CALL WNGEX()
	END IF
C
C	Fill and load the buffer
C
	DO YY=1,YSIZ
C
C	For each memory line:
C	   average the next band of map lines
C	   in boxes of COMPRESS*COMPRESS pixels
C
	   CALL WNGMVZ(LB_E*XSIZ,RBUF)        	!CLEAR COMPRESSION BUF
	   DO I2=1,COMPR                     	!DO LINES IN COMPR BAND
	      IF (.NOT.WNFRD(FCAIN,LB_E*TAREA(2),DBUF,
	1		LPTR+LB_E*(TEAR(0)+DX2))) GOTO 990
	      LPTR=LPTR+MPHJ(MPH_NRA_J)*LB_E	!NEXT LINE
	      J1=1				!POINT TO FIRST PIXEL
	      DO XX=1,XSIZ                    	!DO COMPRESSION BOXES
		DO I1=1,COMPR             	!DO PIXELS IN BOX
		  RBUF(XX)=RBUF(XX)+AVGFAC*DBUF(J1) !ADD IN VALUE
		  J1=J1+1                     	!POINT TO NEXT PIXEL
		END DO
	      END DO
	   END DO
C
C	truncate and scale the compressed data points, 
C	and pack them into the byte buffer
C
	   DO XX=1,XSIZ
	     IF (RBUF(XX).LT.RANGE(1)) THEN
		I2=MINCOL
	      ELSE IF (RBUF(XX).GT.RANGE(2)) THEN
		I2=MAXCOL
	      ELSE
		I2=NINT(RBUF(XX)*CSCALE+CZERO)
	      END IF
	      IF (I2.GT.127) I2=I2-256		!MAP 256 TO -1, ETC.
	      IF (IPTR.GT.LBUF) THEN
	         CALL WNCTXT(F_TP,'Arghh, out of buffer')
	      END IF
	      A_B(DMAP+IPTR)=I2
	      IPTR=IPTR+1
	   END DO
	END DO
C
C	Write the plane
C
	IF (.NOT.NGITRA(MID,SID)) GOTO 990
C
	GOTO 800
C
C ERRORS
C
 990	CONTINUE
	NGIDMP=.FALSE.
C
 800	CONTINUE
C
	RETURN
C
C
	END
