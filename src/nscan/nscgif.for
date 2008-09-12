C+ NSCGIF.FOR
C  CMV 940425
C
C  Revisions:
C	CMV 940425	Created
C	CMV 940426	Add entry NSCGF1
C	CMV 940429	GNCAL gives strategy as used by Newstar per scan
C	CMV 940429	Add entry NSCGF2
C	CMV 940513	Correct gain-method criteria
C	CMV 940628	Option to add X and Y
C	CMV 940930	Split off calculations to NSCGGN
C
C
	LOGICAL FUNCTION NSCGIF(MODE,INFCA,STHJ,HA1,HA2,DAT)
C
C  Read IF/Total Power data from a SCN file
C
C  Result:
C
C	NSCGIF_L = NSCGIF( MODE_C*(*):I, 
C			   INFCA_J:I, STHJ_J:I, HA1_E:I, HA2_E:I,
C			   DAT_E(0:STHTEL-1,0:1):O )
C
C			MODE is a character string selecting the type
C			     of data to return, at present this can be:
C				TPoff   Total power, Noise source off
C			        TPon    Total power, Noise source on
C				Gain    IF-Gain, calculated from TP's
C			                 according to GNCAL etc.
C				Tsys	System temperature, idem
C				Isys	System temperature, add X and Y
C				GNCAL   Gain correction strategy
C				TSYSI   Constant system temperatures
C				TNOISEI Constant noise source temperatures
C				RGAINI  Constant receiver gain
C
C			INFCA is the SCN file to be read
C			SCHJ  is the sector header
C			HA1,HA2 form the hour angle range to return
C				(data will be averaged over the range)
C			DAT   will return the data
C
C	NSCGF1_L = NSCGF1( INFCA_J:I, STHJ(0:*)_J:I,
C			    HAB_E:O, HAI_E:O, NTP_J:O)
C			Return begin HA, HA increment and number of TP
C			points from IFH header
C
C	NSCGF2_L = NSCGF2( INFCA_J, STHJ(0:*)_J:I, OUT )
C			Print header information formatted on OUT (F_P, F_TP)
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NSC_DEF'
	INCLUDE 'SHW_O_DEF'			!SH BLOCK
	INCLUDE 'SHW_T_DEF'
	INCLUDE 'IHW_O_DEF'			!IH BLOCK
	INCLUDE 'IHW_T_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
	INCLUDE 'IFH_O_DEF'			!IF-SET HEADER
C
C  Entry points:
C
	LOGICAL NSCGF1				!RETURN HAB,HAI,NTP
	LOGICAL NSCGF2				!Print header
C
C  Parameters:
C
C
C  Arguments:
C
	CHARACTER MODE*(*)			!Data type to return
	INTEGER INFCA				!INPUT FILE DESCRIPTOR
	INTEGER STHJ(0:*)			!SET HEADER
	REAL	HA1,HA2				!Hour angle range
	REAL	DAT(0:STHTEL-1,0:1)		!Return data
	INTEGER INFCA1				!INPUT FILE DESCRIPTOR
	INTEGER STHJ1(0:*)			!SET HEADER
	REAL    HAB,HAI				!Hour angle start/increment
	INTEGER NTP				!Number of points
	INTEGER INFCA2				!INPUT FILE DESCRIPTOR
	INTEGER STHJ2(0:*)			!SET HEADER
	INTEGER OUT				!Output text files
C
C  Function references:
C
	LOGICAL WNFRD				!READ DATA
	INTEGER WNGARA				!FIND ADDRESS
	INTEGER WNMEJC				!CEIL
	INTEGER WNMEJF				!FLOOR
C
C  Data declarations:
C
	CHARACTER*10 LMODE			!Local copy of mode
	LOGICAL DO_DATA				!Need to read data?
	INTEGER NN				!# OF INTEGRATED POINTS
	INTEGER GNC				!Actual correction method
	INTEGER*2 DBUF(2,0:1,0:STHTEL-1)	!INPUT BUFFER
	REAL GNOUT(0:STHTEL-1,0:1)		!Gain factors 
	REAL TSYS(0:STHTEL-1,0:1)		!System temperatures
	REAL GNCAL(0:STHTEL-1,0:1)		!Correction method used
C
	INTEGER IFHP				!POINTER TO PREVIOUS IFH
	  DATA IFHP/0/				!NOTHING READ YET
	  SAVE IFHP
C
	BYTE IFH(0:IFHHDL-1)			!IF-SET HEADER
	  INTEGER*2 IFHI(0:IFHHDL/2-1)
	  INTEGER   IFHJ(0:IFHHDL/4-1)
	  REAL IFHE(0:IFHHDL/4-1)
	  REAL*8 IFHD(0:IFHHDL/8-1)
	  EQUIVALENCE (IFH,IFHI,IFHJ,IFHE,IFHD)
	  SAVE IFH				!KEEP THE CURRENT HEADER
C-
C
C INIT
C
	NSCGIF=.TRUE.				!ASSUME OK
	LMODE=MODE				!LOCAL COPY OF MODE
	CALL WNCAUC(LMODE)			!MAKE UPPER CASE
C
	DO I1=0,STHTEL-1			!CLEAR DATA
	   DAT(I1,0)=0
	   DAT(I1,1)=0
	END DO
C
	IF (IFHP.NE.STHJ(STH_IFHP_J)) THEN	!NEW IF-SET
	   IFHP=STHJ(STH_IFHP_J)
	   IF (.NOT.WNFRD(INFCA,IFHHDL,IFH,IFHP)) GOTO 900	!READ IT
	END IF
C
C Check mode for the constant parameters
C
	DO_DATA=.FALSE.
	IF (LMODE(1:5).EQ.'TSYSI') THEN		!CONSTANT TSYS
	  DO I1=0,1				!X,Y
	    DO I2=0,STHTEL-1			!IF's
	      DAT(I2,I1)=IFHE(IFH_TSYSI_E+2*I2+I1)
	    END DO	   
	  END DO	   
	ELSE IF (LMODE(1:7).EQ.'TNOISEI' .OR.
	1	 LMODE(1:6).EQ.'TNOISI') THEN	!CONSTANT TNOIS
	  DO I1=0,1				!X,Y
	    DO I2=0,STHTEL-1			!IF's
	      DAT(I2,I1)=IFHE(IFH_TNOISEI_E+2*I2+I1)
	    END DO	   
	  END DO	   
	ELSE IF (LMODE(1:6).EQ.'RGAINI') THEN	!CONSTANT GAIN
	  DO I1=0,1				!X,Y
	    DO I2=0,STHTEL-1			!IF's
	      DAT(I2,I1)=1.0/IFHE(IFH_RGAINI_E+2*I2+I1)
	    END DO	   
	  END DO	   
	ELSE IF (LMODE(1:5).EQ.'TPOFF' .OR.
	1	 LMODE(1:4).EQ.'TPON'  .OR.
	1	 LMODE(1:4).EQ.'GAIN'  .OR.
	1	 LMODE(1:5).EQ.'GNCAL' .OR.
	1	 LMODE(1:4).EQ.'TSYS'  .OR.
	1	 LMODE(1:4).EQ.'ISYS' ) THEN	!NEED DATA FOR THESE
	   DO_DATA=.TRUE.
	ELSE
	   CALL WNCTXT(F_TP,'Invalid data-type !AS in NSCGIF',LMODE)
	   NSCGIF=.FALSE.
	END IF
C
C Find Hour-angle range
C
	IF (DO_DATA) THEN
	  I1=WNMEJF((HA1-IFHE(IFH_HAB_E))/IFHE(IFH_HAI_E)) !FIRST POINT TO READ
	  I2=WNMEJC((HA2-IFHE(IFH_HAB_E))/IFHE(IFH_HAI_E)) !LAST POINT TO READ
	  IF (HA2.LE.HA1) I2=I1			 !NO INVERSE RANGE
C
C Average the TP data for the hour angles in the range
C
	  I3=STHTEL*4*LB_I			!LENGTH POINT
	  NN=0					!NUMBER OF POINTS
	  DO I=I1,I2
	     IF (I.GE.0.AND.I.LT.IFHJ(IFH_NTP_J)) THEN	!IN RANGE
	        IF (.NOT.WNFRD(INFCA,I3,DBUF,
	1			IFHP+IFHHDL+I*I3)) GOTO 900	!READ POINT
	        CALL NSCGGN(GNOUT,TSYS,GNCAL,IFH,STHJ,DBUF)	!CALCULATE
		NN=NN+1					!Count point
	        DO I5=0,1				!X,Y
	          DO I4=0,STHTEL-1			!IF's
	             IF (LMODE(1:5).EQ.'TPOFF') THEN
	                DAT(I4,I5)=DAT(I4,I5)+DBUF(1,I5,I4)
	             ELSE IF (LMODE(1:4).EQ.'TPON') THEN
	                DAT(I4,I5)=DAT(I4,I5)+DBUF(2,I5,I4)
	             ELSE IF (LMODE(1:4).EQ.'GAIN') THEN
	                DAT(I4,I5)=DAT(I4,I5)+GNOUT(I4,I5)
	             ELSE IF (LMODE(1:4).EQ.'TSYS' .OR.
	1	              LMODE(1:4).EQ.'ISYS') THEN
	                DAT(I4,I5)=DAT(I4,I5)+TSYS(I4,I5)
		     ELSE IF (LMODE(1:5).EQ.'GNCAL') THEN
	                DAT(I4,I5)=MAX(DAT(I4,I5),GNCAL(I4,I5))	! WORST CASE
	             END IF
	          END DO
	        END DO
	     END IF
	  END DO
C
	  IF (NN.GT.1) THEN				!GOT SOME POINTS
	    IF (LMODE(1:5).NE.'GNCAL') THEN		!GNCAL: Show worst case
	      DO I5=0,1					!X,Y
	        DO I4=0,STHTEL-1			!IF's
	           DAT(I4,I5)=DAT(I4,I5)/NN		!Else: Average
	        END DO
	      END DO
	    END IF
	    IF (LMODE(1:4).EQ.'ISYS' ) THEN		!Add X,Y for ISYS
	       DO I2=0,STHTEL-1
	          R0=DAT(I2,0)+DAT(I2,1)
	          DAT(I2,0)=R0
	          DAT(I2,1)=R0
	       END DO
	    END IF
	  END IF
	END IF
C
	RETURN						!READY
C
C Entry NSCGF1: Just return HAB, HAE, NTP
C
	ENTRY NSCGF1(INFCA1,STHJ1,HAB,HAI,NTP)
C
	NSCGF1=.TRUE.				!ASSUME OK
C
	IF (IFHP.NE.STHJ1(STH_IFHP_J)) THEN	!NEW IF-SET
	   IFHP=STHJ1(STH_IFHP_J)
	   IF (.NOT.WNFRD(INFCA1,IFHHDL,IFH,IFHP)) GOTO 900	!READ IT
	END IF
C
	HAB=IFHE(IFH_HAB_E)
	HAI=IFHE(IFH_HAI_E)
	NTP=IFHJ(IFH_NTP_J)
C
	RETURN
C
C Entry NSCGF2: Print header
C
	ENTRY NSCGF2(INFCA2,STHJ2,OUT)
C
	NSCGF2=.TRUE.				!ASSUME OK
C
	IF (IFHP.NE.STHJ2(STH_IFHP_J)) THEN	!NEW IF-SET
	   IFHP=STHJ2(STH_IFHP_J)
	   IF (.NOT.WNFRD(INFCA2,IFHHDL,IFH,IFHP)) GOTO 900	!READ IT
	END IF
C
	CALL WNCTXT(OUT,' ')
	CALL WNCTXT(OUT,'IF-Header for Channel !UI',IFHI(IFH_CHAN_I))
	CALL WNCTXT(OUT,'Total-power integration time used '//
	1	'during observations: !UJ sec',IFHJ(IFH_TPINT_J))
	CALL WNCTXT(OUT,'Ha-range: !E10.4 to !E10.4, increment !E10.4 ',
	1	360*IFHE(IFH_HAB_E),
	1	360*(IFHE(IFH_HAB_E)+(IFHJ(IFH_NTP_J)-1)*
	1		IFHE(IFH_HAI_E)),
	1	360*IFHE(IFH_HAI_E))
	CALL WNCTXT(OUT,'Gain correction method: !UI, TP-points: !UJ',
	1	IFHI(IFH_GNCAL_I),IFHJ(IFH_NTP_J))
	CALL WNCTXT(OUT,' ')
	CALL WNCTXT(OUT,'Tel Pol  GNCAL      Tsys      Gain     Tnoise')
	DO I=0,STHTEL-1
	  CALL WNCTXT(OUT,
	1	' !1$XJ  X  !5$UI   !10$E10.3 !10$E10.3 !10$E10.3',I,
	1	IFHI(IFH_GNCAL_I+2*I),
	1	IFHE(IFH_TSYSI_E+2*I),
	1	IFHE(IFH_RGAINI_E+2*I),
	1	IFHE(IFH_TNOISEI_E+2*I))
	  CALL WNCTXT(OUT,
	1	'    Y  !5$UI   !10$E10.3 !10$E10.3 !10$E10.3',
	1	IFHI(IFH_GNCAL_I+2*I+1),
	1	IFHE(IFH_TSYSI_E+2*I+1),
	1	IFHE(IFH_RGAINI_E+2*I+1),
	1	IFHE(IFH_TNOISEI_E+2*I+1))
	END DO
	CALL WNCTXT(OUT,' ')
C
	RETURN
C
C ERROR FINISH
C
 900	CONTINUE
	CALL WNCTXT(F_TP,'Error reading IF data')
	NSCGIF=.FALSE.
C
	RETURN
C
C
	END
