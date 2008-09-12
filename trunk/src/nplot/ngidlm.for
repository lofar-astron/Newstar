C+ NGIDLM.FOR
C  WNB 930514
C
C  Revisions:
C	WNB 930517	Blank data points outside range
C	WNB 930602	Correct compression range
C	CMV 940929	Correct rounding for odd axis length
C
	LOGICAL FUNCTION NGIDLM(GID,FCAIN,MPHJ,COMPR,TAREA,TEAR,
	1			RANGE,MID,SID)
C
C  Load a map into the GIDS-display
C
C  Result:
C
C       NGIDLM_L = NGIDLM( GID_J:I, FCAIN_J:I, MPHJ_J(0:*):I,
C			COMPR_J:I, TAREA_J(0:3):I, TEAR_J(0:3):I,
C			RANGE_E(2):I, MID_C*:I, SID_C*:I)
C				Load map in GIDS display GID. Using:
C				FCAIN	file
C				MPHJ	map header
C				COMPR	compression factor (>=1)
C				TAREA	area
C				TEAR	edge-type area
C				RANGE	min/max value to be coded
C				MID	map id
C				SID	map sub-id
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'
C
C  Parameters:
C
	INTEGER BUFSIZ		!SIZE LINE BUFFER
	  PARAMETER (BUFSIZ=8192)
C
C  Arguments:
C
	INTEGER GID		!GIDS DISPLAY ID
	INTEGER FCAIN		!INPUT FILE
	INTEGER MPHJ(0:MPHHDL/4-1) !MAP HEADER
	INTEGER COMPR		!COMPRESSION FACTOR (>=1)
	INTEGER TAREA(0:3)	!AREA TO LOAD
	INTEGER TEAR(0:3)	!AREA IN EDGE-FORMAT
	REAL RANGE(2)		!RANGE OF DATA VALUES TO USE
	CHARACTER*(*) MID	!MAP ID
	CHARACTER*(*) SID	!MAP SUB-ID
C
C  Function references:
C
	INTEGER N_GDI_CINFO,N_GDI_DEFIMG,N_GDI_IMWRITE 
	INTEGER N_GDI_IMMID,N_GDI_IMSID
	LOGICAL WNFRD		!READ FILE
	INTEGER	WNCALN		!LENGTH STRING
C
C  Data declarations:
C
	INTEGER LPTR		!DATA POINTER
	INTEGER MINCOL,MAXCOL,NCOL,BLANK !GIDS color-values
	INTEGER GLO(2),GHI(2)  	! GIDS image boundaries
	REAL SCLFAC             ! Color scale factor
	REAL AVGFAC             ! Compression factor
	REAL BSCALE		! Scaling factor from 
	REAL BZERO		! display data to real data
C				! REAL = BSCALE*DISPLAY+BZERO
	INTEGER LBUF,DX2
	INTEGER XX,XSIZ,YBEG,YY,YSIZ,YSIZMAX
	REAL RBUF(BUFSIZ),DBUF(BUFSIZ)
	BYTE BUF(BUFSIZ)
C
C-
	NGIDLM=.TRUE.				!ASSUME OK
C
C                               Obtain minimum and maximum color value.
C
	E_C=N_GDI_CINFO(GID,MINCOL,MAXCOL,NCOL,BLANK)
	IF (E_C.LT.0) GOTO 990			!ERROR
C
C                                       Define the image-size for GIDS.
C
	GLO(1)=TEAR(0)/COMPR
	GLO(2)=TEAR(2)/COMPR
	GHI(1)=GLO(1)+(TEAR(1)-TEAR(0)+1)/COMPR-1
	GHI(2)=GLO(2)+(TEAR(3)-TEAR(2)+1)/COMPR-1
C
C                                       Define the map size.
C
	BSCALE=(RANGE(2)-RANGE(1))/NCOL		!SCALE FACTOR
	BZERO=RANGE(1)-(BSCALE*MINCOL)
	E_C=N_GDI_DEFIMG(GID,GLO,GHI,BSCALE,BZERO)
	IF (E_C.LT.0) GOTO 990			!ERROR
C
C                                       Now load the map.
C                                       First determine sizes and factors.
C
	XSIZ=GHI(1)-GLO(1)+1			!NR MEMORY PIXELS/LINE
	YSIZMAX=BUFSIZ/XSIZ			!NR OF LINES PER BUFFER
	AVGFAC=1./COMPR**2			!AVERAGING FACTOR
	SCLFAC=NCOL/(RANGE(2)-RANGE(1))		!SCALE FACTOR
C
C                                       Fill and load the buffer
C                                       - in bunches of YSIZMAX memory lines
C
	DX2=MPHJ(MPH_NRA_J)/2
	LPTR=MPHJ(MPH_MDP_J)+LB_E*
	1	(MPHJ(MPH_NDEC_J)*DX2+TEAR(2)*MPHJ(MPH_NRA_J)) !FIRST LINE TO READ
	DO YBEG=GLO(2),GHI(2),YSIZMAX
	  LBUF=0
	  YSIZ=MIN(YSIZMAX,GHI(2)-YBEG+1)   	!NR LINES TO BE LOADED
C
C                                       For each memory line:
C                                       - average the next band of map lines
C                                         in boxes of COMPRESS*COMPRESS pixels
C
	  DO YY=1,YSIZ                        	!DO ysiz MEMORY LINES
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
C                                       - truncate and scale the compressed
C                                         data points, and pack them into
C                                         the byte buffer
C
	    DO XX=1,XSIZ
	      IF (RBUF(XX).LT.RANGE(1)) THEN
		I2=BLANK			!!MINCOL
	      ELSE IF (RBUF(XX).GT.RANGE(2)) THEN
		I2=BLANK			!!MAXCOL
	      ELSE
		I2=NINT(SCLFAC*(RBUF(XX)-RANGE(1))) + MINCOL
	      END IF
	      IF (I2.GT.127) I2=I2-256		!MAP 256 TO -1, ETC.
	      LBUF=LBUF+1                     	!TO HANDLE IT AS A BYTE
	      BUF(LBUF)=I2
	    END DO
	  END DO
C
C                                       Write the YSIZ memory lines
C
	  E_C=N_GDI_IMWRITE(GID,BUF,LBUF,0)
	  IF (E_C.LT.0) GOTO 990
	END DO
C
C                                       Write the image identification
C                                       (Max. 15 characters are possible).
C                                       Take last part of filename.
C
	I2=WNCALN(MID)
	I1=MAX(1,I2-14)
	I0=N_GDI_IMMID(GID,MID(I1:I2))
	I0=N_GDI_IMSID(GID,SID(:MIN(15,LEN(SID))))
C
	GOTO 800
C
C ERRORS
C
 990	CONTINUE
	NGIDLM=.FALSE.
C
 800	CONTINUE
C
	RETURN
C
C
	END
