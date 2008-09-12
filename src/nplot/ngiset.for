C+ NGISET.FOR
C  CMV 931029
C
C  Revisions:
C	CMV 931029	Created
C	WNB 931221	Correct use of IAND
C	CMV 940120	Removed %VAL
C	CMV 940203	Changed handling of polarisation
C	CMV 940817	Add DO_CLIP for flagging
C
	LOGICAL FUNCTION NGISET(TEAR_I)
C
C  Set size, scale and grid in GIDS display
C
C  Result:
C
C       NGISET_L = NGISET(TEAR_J(0:3):I)
C
C	  Call NGISET before a series of planes is loaded.
C	  It will define the scales and define the size of the
C	  image based on TEAR. Virtual memory will be allocated 
C	  for map and flag planes (in DMAP and DFLG).
C
C	NGICOV_L = NGICOV()
C
C	  Clears the overlay for the planes determined by ALL_CHAN 
C	  and ALL_POLS.
C
C	NGITRA_L = NGITRA(MID_C*(*):I,SID_C*(*):I)
C
C	  Write Map ID and Sub ID to GIDS header for this set
C
C
C	NGISFL_L = NGISFL(IPOL_J:I,IFR_J:I,CCHAN_J:I)
C
C	  Handle flags using regions mode, for polarisation IPOL,
C	  interferometer IFR (if CHAN mode) or channel CCHAN (if IFRS mode).
C
C	NGICLR_L = NGICLR()
C
C	  Frees DMAP and DFLG is they had been previously set.
C
C   NOTE:  The bits in array DFLG are used as follows:
C
C		bit 0:  Flags that have been read from the SCN file ('old')
C		bit 1:  Flags that have been set earlier by NGIDS   ('fresh')
C		bit 2:  Flags that are set by the regions option    ('new')
C		bit 3:  not used
C
C  PIN references
C    CLIP_LEVEL
C
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'FLF_O_DEF'	!FLAG FILE ENTRY
	INCLUDE 'STH_O_DEF'	!FOR STHTEL
	INCLUDE 'NGI_DEF'
C
C  Parameters:
C
	INTEGER MAXGRID		!MAX NUMBER OF ANNOTATED GRIDPOINTS
	  PARAMETER(MAXGRID=2048)
C
C  Arguments:
C
	INTEGER TEAR_I(0:3)	!AREA in edge-format
C
	CHARACTER*(*) MID	!MAP ID
	CHARACTER*(*) SID	!MAP SUB-ID
C
	INTEGER IPOL		!CURRENT POLARISATION
	INTEGER IFR		!CURRENT INTERFEROMETER
	INTEGER CCHAN		!CURRENT CHANNEL
C
C  Entry points:
C
	LOGICAL NGITRA			!Transfer data, write ID
	LOGICAL NGICLR			!Deallocate any memory etc
	LOGICAL NGICOV			!Clear overlay planes
	LOGICAL NGISFL			!Handle flags
C
C  Function references:
C
	INTEGER N_GDI_CINFO,N_GDI_DEFIMG,N_GDI_IMWRITE 
	INTEGER N_GDI_SETXGRID, N_GDI_SETYGRID
	INTEGER N_GDI_IMMID,N_GDI_IMSID
	INTEGER N_GDI_GRON, N_GDI_GRCOL, N_GDI_GRWRITE, N_GDI_GRREAD
	INTEGER N_GDI_GRCLEAR, N_GDI_GRREGION
	LOGICAL NFLFL1		!WRITE FLAG ENTRY
	LOGICAL WNGGVM		!GET VIRTUAL MEMORY
	LOGICAL WNCALN
	LOGICAL WNDPAR			!GET User Input
C
C  Data declarations:
C
	INTEGER GLO(2),GHI(2)	!GIDS image boundaries
	INTEGER XX,YY		!Counters in flags array
	REAL    RBUF(3)		!RGB VALUES
	REAL    RDAT		!DATA VALUE
	CHARACTER CGRID(0:MAXGRID-1)*7	!GRID DESCRIPTION
	REAL    BSCALE,BZERO	!SCALES FOR GIDS
	INTEGER NCOL		!NUMBER OF COLORS AVAILABLE
C
	LOGICAL COLSET		!HAVE COLORS BEEN SET?
	DATA    COLSET/.FALSE./	!INITIALLY NOT (OF COURSE)
	INTEGER SAVPOL		!PREVIOUS POLARISATION
	DATA	SAVPOL/-9999/	!NONE
	INTEGER SAVCHAN		!PREVIOUS CHANNEL
	DATA	SAVCHAN/-9999/	!NONE
	SAVE    COLSET,SAVPOL,SAVCHAN	!REMEMBER
C
	REAL    CLEVEL		!Clip level for flagging
	INTEGER ILEVEL		!Level after scaling
C
	REAL BASEL(0:STHTEL*STHTEL-1)	!BASELINE LNGTHS (FOR SORT)
C
	BYTE FLF(0:FLFHDL-1)		!FLAG ENTRY
	  INTEGER*2 FLFI(0:FLFHDL/LB_I-1)
	  INTEGER FLFJ(0:FLFHDL/LB_J-1)
	  REAL FLFE(0:FLFHDL/LB_E-1)
	  EQUIVALENCE(FLF,FLFI,FLFJ,FLFE)
C-
	NGISET=.TRUE.				!ASSUME OK
	SAVPOL=-9999				!NO PREVIOUS
	SAVCHAN=-9999				!NO PREVIOUS
C
C	Obtain minimum and maximum color value.
C
	E_C=N_GDI_CINFO(GID,MINCOL,MAXCOL,NCOL,BLANK)
	IF (E_C.LT.0) GOTO 990			!ERROR
C
C	Define and clear graphics planes for existing and new flags flags
C
	CALL N_GDI_GINFO(GID,I1,I2)
	IF (I2.NE.7) THEN
	   E_C=N_GDI_GRON(GID,7)		!PLANE=1:old, 2:fresh, 4:new
	   IF (E_C.LT.0) GOTO 990		!ERROR
	END IF
C
	IF (.NOT.COLSET) THEN
	  COLSET=.TRUE.
C
	  I1=1
	  RBUF(1)=0.75				!COLOR OF PLANE
	  RBUF(2)=0.0
	  RBUF(3)=0.25
	  E_C=N_GDI_GRCOL(GID,I1,RBUF(1),RBUF(2),RBUF(3))
	  IF (E_C.LT.0) GOTO 990			!ERROR
C
	  I1=2
	  RBUF(1)=0.25				!COLOR OF PLANE
	  RBUF(2)=0.0
	  RBUF(3)=0.75
	  E_C=N_GDI_GRCOL(GID,I1,RBUF(1),RBUF(2),RBUF(3))
	  IF (E_C.LT.0) GOTO 990			!ERROR
C
	  I1=3
	  RBUF(1)=0.25				!COLOR OF PLANE
	  RBUF(2)=0.75
	  RBUF(3)=0.0
	  E_C=N_GDI_GRCOL(GID,I1,RBUF(1),RBUF(2),RBUF(3))
	  IF (E_C.LT.0) GOTO 990			!ERROR
C
	END IF
C
C	Inform GIDS about map size and scale if these are new values.
C
	IF (DEFIMG.OR.
	1	(TEAR_I(1)-TEAR_I(0)+1)/COMPR.NE.XSIZ.OR.
	1	(TEAR_I(3)-TEAR_I(2)+1)/COMPR.NE.YSIZ ) THEN
C
C	Define scales based on range
C
	   RDAT=FLOAT(MAXCOL-MINCOL)
	   BSCALE=(RANGE(2)-RANGE(1))/RDAT		!SCALE FACTOR
	   BZERO=(MAXCOL*RANGE(1)-MINCOL*RANGE(2))/RDAT
	   CSCALE=1.0/BSCALE
	   CZERO=(MAXCOL*RANGE(1)-MINCOL*RANGE(2))/(RANGE(1)-RANGE(2))
C
C	Define the image-size for GIDS
C
	   GLO(1)=TEAR_I(0)/COMPR
	   GLO(2)=TEAR_I(2)/COMPR
	   GHI(1)=GLO(1)+(TEAR_I(1)-TEAR_I(0)+1)/COMPR-1
	   GHI(2)=GLO(2)+(TEAR_I(3)-TEAR_I(2)+1)/COMPR-1
C
C	Inform GIDS about those values (this will reset the zooming!)
C
	   E_C=N_GDI_DEFIMG(GID,GLO,GHI,BSCALE,BZERO)
	   IF (E_C.LT.0) GOTO 990			!ERROR
	   DEFIMG=.FALSE.				!Done
C
C	Initialise some local parameters
C
	   XSIZ=GHI(1)-GLO(1)+1			!NR MEMORY PIXELS/LINE
	   YSIZ=GHI(2)-GLO(2)+1			!NR OF LINES IN THE MAP
	   AVGFAC=1./COMPR**2			!AVERAGING FACTOR
C
C	Define the grid if IFRS or CHAN
C
	   IF (MAPTYP.EQ.'IFRS'.OR.MAPTYP.EQ.'BASE'.OR.
	1	MAPTYP.EQ.'CHAN') THEN
	      DO I1=0,MIN(XSIZ,MAXGRID)-1
	         WRITE(CGRID(I1),'(F7.2)') 
	1	       360.0*(HARAN(0)+HAINC*FLOAT(I1))
	      END DO
	      I1=MIN(XSIZ,MAXGRID)
	      E_C=N_GDI_SETXGRID(GID,I1,CGRID)
	   END IF
	   IF (MAPTYP.EQ.'IFRS'.OR.MAPTYP.EQ.'BASE') THEN	!Show ifr's
	      DO I1=0,STHTEL*STHTEL-1				!Init: none
	         IFRLUT(I1)=-1
	         BASEL(I1)=10.E10
	      END DO
	      DO I1=0,STHTEL-1
	         DO I2=0,STHTEL-1		!No line known yet
	            IDXLUT(I1,I2)=0
	         END DO
	         DO I2=I1,STHTEL-1		!Fill with normal order
	            IFRLUT(I2*STHTEL+I1)=I2*256+I1
	            BASEL( I2*STHTEL+I1)=TELPOS(I1)-TELPOS(I2)
	         END DO
	      END DO
	      IF (MAPTYP.EQ.'BASE') THEN	!Sort on baseline
	         DO I=0,STHTEL*STHTEL-2
	            DO I1=0,STHTEL*STHTEL-2-I
	               IF (ABS(BASEL(I1)-BASEL(I1+1)).GT.1.AND.
	1	               BASEL(I1).LT.BASEL(I1+1)) THEN	!Swap
	                  R0=BASEL(I1)
	                  BASEL(I1)=BASEL(I1+1)
	                  BASEL(I1+1)=R0
	                  I2=IFRLUT(I1)
	                  IFRLUT(I1)=IFRLUT(I1+1)
	                  IFRLUT(I1+1)=I2
	               END IF
	            END DO
	         END DO
	      END IF
	      DO I=0,STHTEL*STHTEL-1		!Make linenumbers and strings
	         IF (IFRLUT(I).LT.0) THEN
	            CGRID(I)=' '
	         ELSE
	           I1=MOD(IFRLUT(I),256)
	           I2=IFRLUT(I)/256
	           IDXLUT(I1,I2)=I
	           CGRID(I)='Ifr '//TELNAM(I1+1:I1+1)//
	1	                    TELNAM(I2+1:I2+1)
	         END IF
	      END DO
	      I1=STHTEL*STHTEL			!Pass to GIDS
	      E_C=N_GDI_SETYGRID(GID,I1,CGRID)
	   ELSE IF (MAPTYP.EQ.'CHAN') THEN
	      DO I1=0,YSIZ-1
	         WRITE(CGRID(I1),'(A3,I4)') 'Ch ',RCHAN(0)+I1
	      END DO
	      E_C=N_GDI_SETYGRID(GID,YSIZ,CGRID)
	   END IF
	END IF
C
C	Get memory for the map if not already there
C
	IF (DMAP.EQ.0.OR.DFLG.EQ.0.OR.XSIZ*YSIZ.GT.LBUF) THEN
	   IF (DMAP.NE.0) CALL WNGFVM(LBUF,DMAP+A_OB)	!FREE BUFFERS
	   IF (DFLG.NE.0) CALL WNGFVM(LBUF,DFLG+A_OB)
	   DMAP=0
	   DFLG=0
C
	   LBUF=XSIZ*YSIZ
	   IF (.NOT.WNGGVM(LBUF,DMAP)) THEN	!GET AREA FOR DATA
	      CALL WNCTXT(F_TP,'Cannot allocate memory buffer')
	      GOTO 990
	   END IF
	   DMAP=(DMAP-A_OB)	                 !ARRAY POINTER
	   DO I=0,LBUF-1
	      A_B(DMAP+I)=BLANK
	   END DO
C
	   IF (MAPTYP.NE.'MAP') THEN
	     IF (.NOT.WNGGVM(LBUF,DFLG)) THEN	!GET AREA FOR FLAGS
	        CALL WNCTXT(F_TP,'Cannot allocate memory buffer')
	        CALL WNGFVM(LBUF,DMAP+A_OB)	!FREE BUFFER
	        DMAP=0
	        GOTO 990
	     END IF
	     DFLG=(DFLG-A_OB)	                 !ARRAY POINTER
	     DO I=0,LBUF-1
	        A_B(DFLG+I)=0
	     END DO
           END IF
	END IF
C
	RETURN
C
C	Entry NGITRA transfers data and writes two ID's to GIDS header
C
	ENTRY NGITRA(MID,SID)
C
	NGITRA=.TRUE.
C
C	Write the map from the byte-buffer
C
	E_C=N_GDI_IMWRITE(GID,A_B(DMAP),XSIZ*YSIZ,0)
	IF (E_C.LT.0) THEN
	   CALL WNCTXT(F_TP,'Cannot write map-data')
	   GOTO 990
	END IF
C
C	Write the image identification (Max. 15 characters are possible).
C
	I2=WNCALN(MID)
	I1=MAX(1,I2-14)
	I0=N_GDI_IMMID(GID,MID(I1:I2))
	I0=N_GDI_IMSID(GID,SID( :MIN(15,LEN(SID)) ))
C
C	Write graphics plane for existing flags
C
	IF (DFLG.NE.0) THEN
	   E_C=N_GDI_GRWRITE(GID,A_B(DFLG),XSIZ*YSIZ,0)
	   IF (E_C.LT.0) THEN
	      CALL WNCTXT(F_TP,'Cannot write graphics plane')
	      GOTO 990
	   END IF
	END IF
C
	RETURN
C
C	Entry NGICLR clears virtual memory
C
	ENTRY NGICLR
C
	NGICLR=.TRUE.
	IF (DMAP.NE.0) CALL WNGFVM(LBUF,DMAP+A_OB)	!FREE BUFFERS
	IF (DFLG.NE.0) CALL WNGFVM(LBUF,DFLG+A_OB)
	DMAP=0
	DFLG=0
C
	RETURN
C
	ENTRY NGICOV(IPOL,CCHAN)
C
C	Clear map data and flags, keep "fresh" flags if ALLCH set
C
	DO I=0,LBUF-1
	   A_B(DMAP+I)=BLANK
	END DO
C
	I1=2					!Keep plane 2 and bit 4-7
	IF (.NOT.ALLCH) THEN			!Not all channels the same
	   IF (CCHAN.NE.SAVCHAN) I1=0		!Keep nothing if not same
	END IF					!  channel as previous
	SAVCHAN=CCHAN
C
	IF (.NOT.ALLPOL) THEN			!Not all pol's the same
	   IF (IPOL.NE.SAVPOL) I1=0		!Keep nothing if not same
	END IF					!  pol as previous
	SAVPOL=IPOL
C
	DO I=0,LBUF-1
	   I3=A_B(DFLG+I)
	   A_B(DFLG+I)=IAND(I3,I1)
	END DO
C
	RETURN
C
	ENTRY NGISFL(IPOL,IFR,CCHAN)
C
	NGISFL=.TRUE.				!ASSUME ALL RIGHT
C
C	Fill in the fixed part of the flag entry
C
	FLFJ(FLF_FLAG_J)=IAND(FL_ALL,UFL)	  !FILL FLAG TO USE
C
	IF (ALLPOL) THEN
	   FLFI(FLF_POL_I)=-1			  !ALL POLS
	ELSE
	   FLFI(FLF_POL_I)=IPOL		          !POL.CODE
	END IF
C
	IF (ALLCH) THEN
	   FLFJ(FLF_CHAN_J)=-1	   	          !ALL CHANNELS
	ELSE IF (MAPTYP.EQ.'IFRS'.OR.MAPTYP.EQ.'BASE') THEN
	   FLFJ(FLF_CHAN_J)=CCHAN		  !THIS CHANNEL ONLY
	END IF
C
	IF (MAPTYP.EQ.'CHAN') FLFI(FLF_IFR_I)=IFR !THIS IFR ONLY
C
C	Either use regions or cliplevel
C	
	IF (DO_CLIP) THEN
C
C	Get the cliplevel
C
  300	  CONTINUE
	  IF (.NOT.WNDPAR('CLIP_LEVEL',CLEVEL,LB_E,J0,'""')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) RETURN	!NO FLAGS SET
	    GOTO 300                            !RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE.OR.J0.EQ.0) THEN
            RETURN				!NO FLAGS SET
          ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 300                            !MUST SPECIFY
	  END IF
	  ILEVEL=NINT(CLEVEL*CSCALE+CZERO)	!SCALE AS GIDS DATA
C
C	Go through the data and the graphics plane
C
	  I1=0					!NUMBER OF FLAGS SET
	  DO XX=0,XSIZ-1
	    DO YY=0,YSIZ-1
 	      I3=A_B(DFLG+YY*XSIZ+XX)		!FLAG FOR PIXEL
	      I4=A_B(DMAP+YY*XSIZ+XX)		!VALUE OF PIXEL
	      IF (I4.LT.0) I4=I4+256		!CORRECT FOR SIGNED BYTE
	      IF (I4.NE.BLANK.AND.I4.GE.ILEVEL.AND.
	1	  IAND(I3,2).EQ.0) THEN 	!FOUND NEW FLAG
	          I1=I1+1
	          FLFE(FLF_HA_E)=XX*HAINC+HARAN(0)	   !HA of pixel
C
C	IFRS/BASE mode: get IFR and mark pixel as freshly flagged
C
	          IF (MAPTYP.EQ.'IFRS'.OR.MAPTYP.EQ.'BASE') THEN
	             FLFI(FLF_IFR_I)=IFRLUT(YY)		!IFR of pixel
	             A_B(DFLG+YY*XSIZ+XX)=IAND(I3,1)+2
C
C	CHAN mode and ALLCH: mark all channels as freshly flagged
C
	          ELSE IF (ALLCH) THEN
	             DO I2=0,YSIZ-1
	                A_B(DFLG+YY*XSIZ+XX)=IAND(I3,1)+2
	             END DO
C
C	CHAN mode and .NOT.ALLCH: get channel for this map
C
	          ELSE
	             FLFJ(FLF_CHAN_J)=YY+RCHAN(0)	 !THIS CHANNEL ONLY
	          END IF
	          IF (.NOT.NFLFL1(DFAR,FLF))
	1		CALL WNCTXT(F_TP,'Error writing FLF entry')
	      END IF
	    END DO
	  END DO
C
	ELSE
C
C	Enter regions mode
C
	  CALL WNCTXT(F_T,'Press DEFINE to start, READY when done')
	  I1=4
	  E_C=N_GDI_GRREGION(GID,I1)
	  IF (E_C.LT.0) GOTO 990			!ERROR
C
C	Read back the graphics plane
C
	  E_C=N_GDI_GRREAD(GID,A_B(DFLG),XSIZ*YSIZ,0)
	  IF (E_C.LT.0) THEN
	     CALL WNCTXT(F_TP,'Cannot read graphics plane')
	     GOTO 990
	  END IF
C
C	Scan every pixel and output the appropriate flag word
C
	  I1=0					!NUMBER OF FLAGS SET
	  DO XX=0,XSIZ-1
	    DO YY=0,YSIZ-1
 	      I3=A_B(DFLG+YY*XSIZ+XX)
	      IF (A_B(DMAP+YY*XSIZ+XX).NE.BLANK.AND.
	1	 IAND(I3,4).NE.0.AND.IAND(I3,2).EQ.0) THEN !FOUND NEW FLAG
	          I1=I1+1
	          FLFE(FLF_HA_E)=XX*HAINC+HARAN(0)	   !HA of pixel
C
C	IFRS/BASE mode: get IFR and mark pixel as freshly flagged
C
	          IF (MAPTYP.EQ.'IFRS'.OR.MAPTYP.EQ.'BASE') THEN
	             FLFI(FLF_IFR_I)=IFRLUT(YY)		!IFR of pixel
	             A_B(DFLG+YY*XSIZ+XX)=IAND(I3,1)+2
C
C	CHAN mode and ALLCH: mark all channels as freshly flagged
C
	          ELSE IF (ALLCH) THEN
	             DO I2=0,YSIZ-1
	                A_B(DFLG+YY*XSIZ+XX)=IAND(I3,1)+2
	             END DO
C
C	CHAN mode and .NOT.ALLCH: get channel for this map
C
	          ELSE
	             FLFJ(FLF_CHAN_J)=YY+RCHAN(0)	 !THIS CHANNEL ONLY
	          END IF
	          IF (.NOT.NFLFL1(DFAR,FLF))
	1		CALL WNCTXT(F_TP,'Error writing FLF entry')
	      END IF
	    END DO
	  END DO
	END IF
C
C	If Flags found, rewrite graphics plane
C
	CALL WNCTXT(F_T,'Found !UJ new flags',I1)
	IF (I1.NE.0) THEN
	   I1=2
	   E_C=N_GDI_GRWRITE(GID,A_B(DFLG),XSIZ*YSIZ,0)
	   IF (E_C.LT.0) THEN
	      CALL WNCTXT(F_TP,'Cannot rewrite graphics plane')
	      GOTO 990
	   END IF
	END IF
C
	RETURN
C
C	Errors for all entry points
C
 990	CONTINUE
	NGISET=.FALSE.
C
	RETURN

	END
