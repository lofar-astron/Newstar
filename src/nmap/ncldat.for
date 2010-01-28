C+ NCLDAT.FOR
C  WNB 910809
C
C  Revisions:
C	WNB 910916	Error in error handling WNDNOD
C	HjV 920520	HP does not allow extended source lines
C	WNB 920810	Changed sign restore beam angle
C	WNB 921202	Add DATA clean
C	WNB 921216	Add grating factor
C	HjV 921228	Line to long for HP
C	HjV 930423	Change name of some keywords
C	CMV 931116	Change query for memory use with /ASK
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	JPH 940221	Comments. - DMEMORY_USE prompt with default
C       HjV 950512	Add data factor
C       WNB 950621	New LSQ routines
C       WNB 100128      Allow larger beam clean area
C 
C
	SUBROUTINE NCLDAT
C
C  Get NCLEAN program parameters
C
C  Result:
C
C	CALL NCLDAT	will ask and set all program parameters
C
C  PIN references:
C
C	OPTION
C	CMEMORY_USE
C	MAP_FACTOR
C	CLEAN_LIMIT
C	COMPON LIMIT
C	LOOP_GAIN
C	PRUSSIAN_HAT
C	DECONVOLUTION
C	CYCLE_DEPTH
C	GRATING_FACTOR
C       DATA_FACTOR
C	RESIDUAL
C	RESTORE
C	RESTORE_BEAM
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'NCL_DEF'
	INCLUDE 'MPH_O_DEF'		!MAP HEADER
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNDNOD			!GET NODE NAME
	LOGICAL WNDSTA			!GET SETS TO DO
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFRD			!READ FILE
	LOGICAL WNMLTN			!TRIANGULAR EQUATIONS
	LOGICAL NMASTG			!GET A SET
C
C  Data declarations:
C
	INTEGER FAREA(0:3)		!FULL MAP AREA
	INTEGER MXAREA(0:3)		!MAX. SHOW AREA
	INTEGER BMAR			!SOLUTION AREA
	REAL BMC(3),BMS(3),BMU,BMMU	!RESTORE BEAM SOL.
	REAL CVCON			!CONVERSION FACTOR
	INTEGER MPHP			!MAP POINTER
	BYTE MPH(0:MPHHDL-1)		!MAP HEADER
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  DOUBLE PRECISION MPHD(0:MPHHDL/8-1)
	  EQUIVALENCE (MPH,MPHJ,MPHE,MPHD)
	INTEGER APHP			!AP POINTER
	BYTE APH(0:MPHHDL-1)		!AP HEADER
	  INTEGER APHJ(0:MPHHDL/4-1)
	  DOUBLE PRECISION APHD(0:MPHHDL/8-1)
	  EQUIVALENCE (APH,APHJ,APHD)
	REAL LBUF(0:8191)		!LINE BUFFER
	BYTE LBT
C-
C
C GET OPTION
C
	CVCON=2.*3600.*360.*SQRT(LOG(2.))	!CONVERSION ARCSEC/INTERNAL
 100	CONTINUE
	IF (.NOT.WNDPAR('OPTION',OPTION,LEN(OPTION),J0,'QUIT')) THEN
	  OPTION='QUIT'				!ASSUME END
	ELSE IF (J0.LE.0) THEN
	  OPTION='QUIT'				!ASSUME END
	END IF
	IF (OPT.EQ.'QUI') RETURN		!READY
C
C GET MEMORY SIZE
C
	IF (.NOT.WNDPAR('CMEMORY_USE',MEMSIZ,LB_J,J0,'150000')) THEN
	  MEMSIZ=150000				!ASSUME VALUE
	ELSE IF (J0.LE.0) THEN
	  MEMSIZ=150000				!ASSUME VALUE
	END IF
C
C HIST
C
	IF (OPT.EQ.'HIS') THEN
 10	  CONTINUE
	  IF (.NOT.WNDNOD('INPUT_WMP_NODE',' ','WMP','R',NODMAP,FILMAP)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY OPTION
	    GOTO 10				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 10				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAMAP,FILMAP,'R')) THEN !OPEN MAP FILE
	    GOTO 10				!RETRY
	  END IF
	  IF (.NOT.WNDSTA('WMP_SETS',MXNSET,MSETS,FCAMAP)) THEN !MAPS TO DO
	    GOTO 10				!RETRY FILE
	  END IF
	  IF (.NOT.NMASTG(FCAMAP,MSETS,MPH,MPHP,MAPNAM)) GOTO 10 !NO MAP
	  CALL WNDSTR(FCAMAP,MSETS)		!RESET SEARCH STATUS
	  CALL WNDSTI(FCAMAP,MAPNAM)		!MAKE INDEX
	  DO I=0,3
	    TAREA(I,0)=0			!DEFAULT AREA
	    FAREA(I)=0				!FULL AREA
	    MXAREA(I)=0				!MAX. AREA
	  END DO
	  FAREA(2)=MPHJ(MPH_NRA_J)		!LENGTH LINE
	  FAREA(3)=MPHJ(MPH_NDEC_J)
	  DO I=2,3
	    MXAREA(I)=FAREA(I)
	    TAREA(I,0)=FAREA(I)
	  END DO
	  CALL NMADAR(MXNAR,NAREA,FAREA,0,MXAREA,TAREA(0,0),PAREA(0,1,0),
	1		TAREA(0,1),PAREA(0,1,1)) !GET AREA
	  IF (NAREA.LE.0) GOTO 10		!NO AREA SPECIFIED
C
C Clean and restore options
C
	ELSE IF (OPT.EQ.'BEA' .OR. OPT.EQ.'UVC' .OR.
	1		OPT.EQ.'URE' .OR. OPT.EQ.'COM' .OR.
	1		OPT.EQ.'DAT') THEN
 20	  CONTINUE
	  IF (.NOT.WNDNOD('INPUT_WMP_NODE',' ','WMP','R',NODMAP,FILMAP)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY OPTION
	    GOTO 20				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 20				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAMAP,FILMAP,'R')) THEN !OPEN MAP FILE
	    CALL WNCTXT(F_TP,'Cannot open map')
	    GOTO 20				!RETRY
	  END IF
 21	  CONTINUE
	  IF (.NOT.WNDSTA('WMP_SETS',MXNSET,MSETS,FCAMAP)) THEN !MAPS TO DO
	    GOTO 20				!RETRY FILE
	  END IF
	  DO I=1,MSETS(0,0)			!ALL LINES
	    MSETS(4,I)=0			!MAKE SURE MAP
	  END DO
	  IF (.NOT.NMASTG(FCAMAP,MSETS,MPH,MPHP,MAPNAM)) GOTO 27 !NO MAP
	  CALL WNDSTR(FCAMAP,MSETS)		!RESET SEARCH STATUS
	  CALL WNDSTI(FCAMAP,MAPNAM)		!MAKE INDEX
	  IF (MAPNAM(4).NE.0) THEN
 27	    CONTINUE
	    CALL WNCTXT(F_TP,'Specified map set does not exist, '//
	1		'or not of type MAP')
	    GOTO 21
	  END IF
 22	  CONTINUE
	  IF (.NOT.WNDSTA('AP_WMP_SET',MXNSET,ASETS,FCAMAP)) THEN !APS TO USE
	    IF (E_C.EQ.DWC_NULLVALUE .AND. OPT.EQ.'URE') GOTO 28
	    GOTO 20				!RETRY FILE
	  END IF
	  DO I=1,ASETS(0,0)			!ALL LINES
	    ASETS(4,I)=1			!MAKE SURE AP
	  END DO
	  IF (.NOT.NMASTG(FCAMAP,ASETS,APH,APHP,APNAM)) GOTO 28 !NO AP
	  CALL WNDSTR(FCAMAP,ASETS)		!RESET SEARCH STATUS
	  CALL WNDSTI(FCAMAP,APNAM)		!MAKE INDEX
	  IF (APNAM(4).NE.1) THEN
 28	    CONTINUE
	    IF (OPT.EQ.'URE') THEN
	      ASETS(0,0)=0			!NO AP
	    ELSE
	      CALL WNCTXT(F_TP,'Unknown map set, or not of type AP')
	      GOTO 22
	    END IF
	  END IF
	  IF (ASETS(0,0).NE.0 .AND. OPT.NE.'URE') THEN
	    IF (APHJ(MPH_NRA_J).NE.MPHJ(MPH_NRA_J) .OR.
	1		APHJ(MPH_NDEC_J).NE.MPHJ(MPH_NDEC_J)) THEN
	      CALL WNCTXT(F_TP,'Map and antenna pattern differ in size')
	      GOTO 20
	    END IF
	  END IF
	  IF (OPT.EQ.'UVC') THEN		!TEST POWER OF 2
	    IF (2**NINT(LOG(FLOAT(APHJ(MPH_NRA_J)))/LOG(2.)).NE.
	1		APHJ(MPH_NRA_J) .OR.
	1		2**NINT(LOG(FLOAT(APHJ(MPH_NDEC_J)))/LOG(2.)).NE.
	1		APHJ(MPH_NDEC_J)) THEN
	      CALL WNCTXT(F_TP,'Sorry, but I can only cater for power'//
	1		'of 2 sizes')
	      GOTO 20
	    END IF
	  END IF
C
C AREA
C
 23	  CONTINUE
	  DO I=0,3
	    TAREA(I,0)=0			!DEFAULT AREA
	    FAREA(I)=0				!FULL AREA
	    MXAREA(I)=0				!MAX. AREA
	  END DO
	  FAREA(2)=MPHJ(MPH_NRA_J)		!LENGTH LINE
	  FAREA(3)=MPHJ(MPH_NDEC_J)
	  DO I=2,3
	    MXAREA(I)=FAREA(I)
	    TAREA(I,0)=31
	    IF (OPT.EQ.'UVC' .OR. OPT.EQ.'COM' .OR.
	1		OPT.EQ.'DAT') THEN
	      TAREA(I,0)=2*(NINT(0.50*MPHJ(MPH_NRA_J+I-2))/2)+1 !MAKE ODD
	    END IF
	  END DO
	  IF (OPT.NE.'URE') THEN		!all cleaning options
	    CALL NMADAR(MXNAR,NAREA,FAREA,3,MXAREA,TAREA(0,0),
	1		PAREA(0,1,0),TAREA(0,1),PAREA(0,1,1)) !GET AREAS
	  ELSE
	    NAREA=1				!FORCE FULL MAP
	    DO I=0,3
	      TAREA(I,0)=FAREA(I)
	      PAREA(I,1,0)=FAREA(I)
	    END DO
	    DO I=0,1
	      TAREA(2*I,1)=TAREA(I,0)-TAREA(I+2,0)/2
	      TAREA(2*I+1,1)=TAREA(I,0)+TAREA(I+2,0)/2-1
	    END DO
	    DO I=0,3
	      PAREA(I,1,1)=TAREA(I,1)
	    END DO
	  END IF
	  IF (NAREA.LE.0) GOTO 10		!NO AREA SPECIFIED
	  I=TAREA(2,0)*TAREA(3,0)*LB_E*3
	  IF (OPT.EQ.'BEA' .AND. I.GT.MEMSIZ) THEN
C
C  This used to be done by asking CMEMORY_USE with /ASK in the default.
C  That did not work, at least not for UNIX (didn't test on VAX).
C  Now we ask for a different keyword which has the additional 
C  advantage that we can supply more extensive help.
C
	    CALL WNCTXT(F_TP,'Cannot do this area in memory work area')
	    CALL WNCTXT(F_TP,'You may try to increase the memory size')
	    IF (.NOT.WNDPAR('DMEMORY_USE',	!prompt with I as default value
	1	MEMSIZ,LB_J,J0,
	1	A_B(-A_OB),I/16,1)) THEN
	      MEMSIZ=I               		!ASSUME VALUE
	    ELSE IF (J0.LE.0) THEN
	      MEMSIZ=I				!ASSUME VALUE
	    END IF
	    MEMSIZ=16*MEMSIZ+16                 !MAKE LARGER
	    GOTO 23				!RETRY
	  END IF
C
C LIMITS
C
 24	  CONTINUE
	  IF (OPT.EQ.'DAT') THEN		!data
	    IF (.NOT.WNDPAR('DATA_FACTOR',DATAFAC,LB_E,J0,'1.')) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 23 !NEW AREA
	      GOTO 24				!RETRY
	    END IF
	    IF (J0.EQ.0) GOTO 23
	    IF (J0.LT.0) DATAFAC=1.
	  ELSE
	    DATAFAC=1.
	  END IF
	  IF (OPT.EQ.'URE') THEN		!restore
	    IF (.NOT.WNDPAR('MAP_FACTOR',CLFAC,LB_E,J0,'1.')) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 23	!NEW AREA
	      GOTO 24				!RETRY
	    END IF
	    IF (J0.EQ.0) GOTO 23
	    IF (J0.LT.0) CLFAC=1
	  ELSE					!all clean operations
	    IF (OPT.NE.'COM') THEN		!Hogbom, Clark
	      IF (.NOT.WNDPAR('CLEAN_LIMIT',CLLIM,LB_E,J0,'.1')) THEN
	        IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 23 !NEW AREA
	        GOTO 24				!RETRY
	      END IF
	      IF (J0.EQ.0) GOTO 23
	      IF (J0.LE.0) CLLIM=.1
	    ELSE
	      CLLIM=.1				!COMPON operation
	    END IF
	    IF (.NOT.WNDPAR('COMPON_LIMIT',SRCLIM,LB_J,J0,'100')) GOTO 24
	    IF (J0.EQ.0) GOTO 24
	    IF (J0.LE.0) SRCLIM=100
	    CMPLOG(1)=MAX(1,SRCLIM/40)
	    CMPLOG(2)=CMPLOG(1)
	    IF (.NOT.WNDPAR('COMPON_LOG',CMPLOG,2*LB_J,J0,A_B(-A_OB),
	1		CMPLOG,2)) GOTO 24
	    IF (J0.EQ.0) GOTO 24
	    IF (J0.LE.0) THEN
	      CMPLOG(1)=MAX(1,SRCLIM/40)
	      CMPLOG(2)=CMPLOG(1)
	    END IF
	    IF (.NOT.WNDPAR('LOOP_GAIN',CLFAC,LB_E,J0,'.4')) GOTO 24
	    IF (J0.EQ.0) GOTO 24
	    IF (J0.LE.0) CLFAC=.4
	    IF (OPT.NE.'DAT') THEN
	      IF (.NOT.WNDPAR('PRUSSIAN_HAT',PRHAT,LB_E,J0,'0.')) GOTO 24
	      IF (J0.EQ.0) GOTO 24
	      IF (J0.LE.0) PRHAT=0
	    END IF
	    IF (OPT.EQ.'UVC') THEN
	      IF (.NOT.WNDPAR('DECONVOLUTION',LBT,LB_B,J0,'NO')) GOTO 24
	      IF (J0.EQ.0) GOTO 24
	      IF (J0.LE.0) LBT=.FALSE.
	      APDCV=LBT
	    END IF
	    IF (OPT.EQ.'UVC' .OR. OPT.EQ.'COM' .OR.
	1		OPT.EQ.'DAT') THEN
	      IF (.NOT.WNDPAR('CYCLE_DEPTH',MPDEP,LB_E,J0,'.05')) GOTO 24
	      IF (J0.EQ.0) GOTO 24
	      IF (J0.LE.0) MPDEP=.05
	      IF (.NOT.WNDPAR('GRATING_FACTOR',GRFAC,LB_E,J0,'1.')) 
	1		GOTO 24
	      IF (J0.EQ.0) GOTO 24
	      IF (J0.LE.0) GRFAC=1.
	    END IF
	  END IF
C
C RESIDUAL/RESTORE/MODEL
C
 25	  CONTINUE
	  RONMDL=.FALSE.			!NOT RESTORE ONLY
	  RSTMDL=.FALSE.			!NOT RESTORE
	  IF (OPT.EQ.'URE') THEN
	    RESMDL=.FALSE.			!NO RESIDUAL
	    RSTMDL=.TRUE.			!WANT RESTORE
	    RONMDL=.TRUE.			!RESTORE ONLY
	  ELSE IF (OPT.NE.'COM') THEN
	    IF (OPT.NE.'DAT') THEN
	      IF (.NOT.WNDPAR('RESIDUAL',LBT,LB_B,J0,'YES')) THEN
	        IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 24 !BACK UP
	        GOTO 25
	      END IF
	      IF (J0.EQ.0) LBT=.FALSE.		!ASSUME NOT
	      IF (J0.LT.0) LBT=.TRUE.		!ASSUME YES
	      RESMDL=LBT			!SAVE RESULT
	    END IF
	    IF (OPT.EQ.'UVC' .OR. OPT.EQ.'DAT') THEN
	      IF (.NOT.WNDPAR('RESTORE',LBT,LB_B,J0,'NO')) THEN
	        IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 24 !BACK UP
	        GOTO 25
	      END IF
	      IF (J0.EQ.0) LBT=.FALSE.		!ASSUME NOT
	      IF (J0.LT.0) LBT=.TRUE.		!ASSUME YES
	      RSTMDL=LBT			!SAVE RESULT
	    END IF
	  END IF
	  IF (RESMDL .OR. RSTMDL .OR.
	1		OPT.EQ.'DAT') THEN	!MAKE WRITEABLE OUTPUT
	    CALL WNFCL(FCAMAP)
	    IF (.NOT.WNFOP(FCAMAP,FILMAP,'U')) THEN !OPEN MAP (SHOULD DO)
	 	CALL WNCTXT(F_TP,
	1		'Cannot open !AS for update',FILMAP)
		GOTO 20
	    END IF
	  END IF
 26	  CONTINUE
	  IF (OPT.EQ.'URE') THEN
	    CALL NMODAX(J)			!GET MODEL TO RESTORE
	    IF (J.LE.0) GOTO 25			!NO SOURCES PRESENT
	  ELSE
	    IF (.NOT.WNDNOD('OUTPUT_MDL_NODE',' ','MDL','W',NODAP,FILAP))
	1			GOTO 25		!ASK FOR AUTO MODEL
	    CALL WNFCL(FCAAP)			!SURE NO
	    IF (E_C.NE.DWC_NULLVALUE .AND. E_C.NE.DWC_WILDCARD) THEN !SPECIFIED
	      IF (.NOT.WNFOP(FCAAP,FILAP,'U')) THEN
	        CALL WNCTXT(F_TP,'Cannot open model output file')
	        GOTO 26
	      END IF
	    END IF
	  END IF
C
C GET BEAM VALUES
C
 30	  CONTINUE
	  IF (RSTMDL) THEN			!RESTORE ASKED
	    IF (ASETS(0,0).EQ.0) THEN		!no antenna pattern,
	      BMS(1)=12.*1400./MPHD(MPH_FRQ_D)	! so GUESS BEAM
	      BMS(2)=BMS(1)/ABS(SIN(MPHD(MPH_DEC_D)*PI2))
	      BMS(3)=0
	    ELSE				!fit beam parameters to
						! antenna pattern
	      CALL WNMLGA(BMAR,LSQ_T_REAL,3)	!ZERO SOLUTION
	      J=APHJ(MPH_NRA_J)			!GET BEAM POINTER
	      J=APHJ(MPH_MDP_J)+LB_E*(J*APHJ(MPH_NDEC_J)/2)
	1					!BEAM LINE ZERO
	      J2=LB_E*APHJ(MPH_NRA_J)		!LENGTH LINE
	      DO I=0,APHJ(MPH_NDEC_J)/2-1	!DO ALL LINES
		IF (.NOT.WNFRD(FCAMAP,J2,LBUF,J)) THEN !READ A LINE
		  CALL WNCTXT(F_TP,'Read error beam')
		  CALL WNGEX			!STOP
		END IF
		J=J+J2				!NEXT PTR
		BMC(2)=-(FLOAT(I)**2)		!EQUATION
		DO I1=APHJ(MPH_NRA_J)/2,
	1			APHJ(MPH_NRA_J)-1 !ALL POS. POINTS
		  IF (LBUF(I1).LT.0.25) GOTO 40	!READY
		  BMC(1)=-(FLOAT(I1-APHJ(MPH_NRA_J)/2)**2)
		  BMC(3)=-FLOAT(I1-APHJ(MPH_NRA_J)/2)*FLOAT(I)
		  CALL WNMLMN(BMAR,LSQ_C_REAL,
	1	       BMC,1E0,LOG(LBUF(I1))) 	!MAKE EQUATION
		END DO
 40		CONTINUE
		IF (I.NE.0) THEN		!OTHER HALF
		  DO I1=APHJ(MPH_NRA_J)/2-1,0,-1 !ALL NEG. POINTS
		    IF (LBUF(I1).LT.0.25) GOTO 41 !READY
		    BMC(1)=-(FLOAT(I1-APHJ(MPH_NRA_J)/2)**2)
		    BMC(3)=-FLOAT(I1-APHJ(MPH_NRA_J)/2)*FLOAT(I)
		    CALL WNMLMN(BMAR,LSQ_C_REAL,
	1		 BMC,1E0,LOG(LBUF(I1)))	!MAKE EQUATION
		  END DO
		END IF
 41		CONTINUE
		IF (LBUF(APHJ(MPH_NRA_J)/2).LT.0.25) GOTO 42 !READY
	      END DO
 42	      CONTINUE
	      IF (.NOT.WNMLTN(BMAR)) THEN	!INVERT
		BMS(1)=1			!ASSUME 1 GRID POINT
		BMS(2)=1
		BMS(3)=0
	      ELSE
		CALL WNMLSN(BMAR,BMS,BMMU,BMU)	!SOLVE
		IF (BMS(1).LE.0 .OR. BMS(2).LE.0) THEN
		  BMS(1)=1			!FORCE
		  BMS(2)=1
		  BMS(3)=0
		ELSE IF (ABS(BMS(3)).LT.1E-6) THEN
		  BMS(3)=0			!CANNOT SOLVE ANGLE
		ELSE
		  R0=ATAN2(BMS(3),BMS(1)-BMS(2)) !2*ANGLE
		  BMS(3)=BMS(3)/SIN(R0) 	!L**2-M**2
		  BMS(1)=(BMS(1)+BMS(2)+BMS(3))/2. !L**2
		  BMS(2)=BMS(1)-BMS(3)		!M**2
		  BMS(3)=R0/2.			!ANGLE
		  IF (BMS(1).LE.0) BMS(1)=1
		  IF (BMS(2).LE.0) BMS(2)=1
		END IF
	      END IF
	      BMS(1)=CVCON*APHD(MPH_SRA_D)*SQRT(1./BMS(1)) !MAKE ARCSEC FWHP
	      BMS(2)=CVCON*APHD(MPH_SDEC_D)*SQRT(1./BMS(2))
	      BMS(3)=180.*BMS(3)/PI		!DEGREES
	      CALL WNMLFA(BMAR)			!FREE SOLUTION AREA
	    END IF
	    IF (.NOT.WNDPAR('RESTORE_BEAM',BMS,3*LB_E,J0,
	1		A_B(-A_OB),BMS,3)) THEN	!ASK BEAM
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 26	!BACKUP
	      GOTO 30				!RETRY
	    END IF
	    IF (J0.LE.0) GOTO 26
	    IF (ASETS(0,0).EQ.0) THEN		!DEFAULT BEAM
	      RESDL=PI*BMS(1)/CVCON/MPHD(MPH_SRA_D)/
	1		MPHJ(MPH_NRA_J)		!MAKE UNITS
	      RESDM=PI*BMS(2)/CVCON/MPHD(MPH_SDEC_D)/
	1		MPHJ(MPH_NDEC_J)
	      RESDAN=+BMS(3)*PI/180.		!RADIANS
	    ELSE
	      RESDL=PI*BMS(1)/CVCON/APHD(MPH_SRA_D)/
	1		APHJ(MPH_NRA_J)		!MAKE UNITS
	      RESDM=PI*BMS(2)/CVCON/APHD(MPH_SDEC_D)/
	1		APHJ(MPH_NDEC_J)
	      RESDAN=+BMS(3)*PI/180.		!RADIANS
	    END IF
	  END IF! RSTMDL
C
	END IF! clean and restore options
C
C GET MAP MAKE DATA
C
	IF (OPT.EQ.'DAT') THEN
	  CALL NMADAC(FCAMAP,MPHP)
	END IF
C
 900	CONTINUE
C
	RETURN					!READY
C
C
	END
