C+ NMOADD.FOR
C  WNB 900827
C
C  Revisions:
C	WNB 910909	Add NMOADC
C	WNB 911230	Add NMOANC
C	WNB 920107	Add NMOAM2
C	WNB 920109	Change NMOANC definition
C	WNB 920626	Add NMOACD
C	WNB 920818	Add NMOAAD
C	WNB 931005	Change L_, text
C	CMV 941118	Correct bug in NMOADC if SOURCE_LIST given
C	JEN 960403      Add NMOAMR: merge sources within radius
C	JEN 960404	remove bug: merge did not affect Q,U,V
C
	SUBROUTINE NMOADD
C
C  Add a source to model
C
C  Result:
C
C	CALL NMOADD		will add a source to the general list
C	CALL NMOAED		will edit a source
C	CALL NMOADL		will delete sources
C	CALL NMOANC		will delete low-level non-clean components
C	CALL NMOACD		will delete low-level clean components
C	CALL NMOAAD		will delete sources in area
C	CALL NMOAMG		will merge sources
C	CALL NMOAMR		will merge sources within given area
C	CALL NMOAM1( IDX_J:I)	will merge sources in header # IDX
C	CALL NMOAM2( IDX_J:I, NST_J:I, NND_:I)
C				will merge sources in header # IDX between
C				NST and NND
C	CALL NMOAFB( OFF_J:I)	will edit B field at offset OFF
C	CALL NMOAFJ( OFF_J:I)	will edit J field at offset OFF
C	CALL NMOAFE( OFF_J:I)	will edit E field at offset OFF
C	CALL NMOADC		wIll calibrate sources
C
C  PIN references
C
C	SOURCE
C	SOURCE_NUMBER
C	SOURCE_LIST
C	SOURCE_RANGE
C	SOURCE_FACTORS
C	DELETE_LEVEL
C	MERGE_RADIUS
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'MDH_O_DEF'			!MODEL HEADER
	INCLUDE 'MDL_O_DEF'			!MODEL LINE
C
C  Entries:
C
C
C  Parameters:
C
	INTEGER MXSRCL				!LENGTH SOURCE LIST
	  PARAMETER (MXSRCL=128)
C
C  Arguments:
C
	INTEGER IDX				!HEADER INDEX
	INTEGER OFF				!MODEL LIST OFFSET
	INTEGER NST,NND				!START/END SOURCES
C
C  Function references:
C
	LOGICAL WNDPAR				!GET USER DATA
	LOGICAL NMOSLI				!GET GENERAL SOURCE LIST
C
C  Data declarations:
C
	BYTE MDL(0:MDLHDL-1)			!MODEL LINE
	  INTEGER MDLJ(0:MDLHDL/4-1)
	  REAL MDLE(0:MDLHDL/4-1)
	  EQUIVALENCE (MDL,MDLJ,MDLE)
	LOGICAL LEDI				!EDIT (OR ADD)
	INTEGER SRCL(0:MXSRCL)			!SOURCE LIST TO DO
	INTEGER FTYP				!TYPE
	INTEGER LIDX				!LOCAL HEADER INDEX
	REAL SFAC(0:3)				!CALIBRATION FACTORS
	REAL MERAD(0:1)				!MERGE RADII (dl,dm)
	REAL MERADFLT(0:1)			!MERAD default
	REAL DEC0,FRQ0                          !Used in merge
	REAL MAXDL,MAXDM                        !Used in merge
C-
	LEDI=.FALSE.				!SET ADD
	GOTO 10
C
C EDIT
C
	ENTRY NMOAED
C
	LEDI=.TRUE.
	GOTO 10
C
C INIT
C
 10	CONTINUE
	IF (GDESJ(MDH_NSRC_J).GE.GDESJ(MDH_MODL_J)) THEN !ENOUGH SPACE?
	  IF (.NOT.NMOSLI(GDESJ(MDH_MODL_J)+100)) THEN !NO ADD
	    CALL WNCTXT(F_TP,'Error getting source area')
	    CALL WNCTXT(F_TP,'Source not added/edited')
C
	    RETURN
	  END IF
	END IF
C
C ADD SOURCES
C
	CALL WNGMVZ(MDLHDL,MDL)				!EMPTY LINE
	MDLE(MDL_ID_J)=GDESJ(MDH_NSRC_J)+1		!NEW ID
	IF (LEDI) THEN					!EDIT
	  IF (.NOT.WNDPAR('SOURCE_NUMBER',J,LB_J,
	1		J0,'""')) THEN			!GET SOURCE TO EDIT
	    IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	    GOTO 10					!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	    RETURN
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    RETURN
	  END IF
	  IF (J.LT.0 .OR. J.GT.GDESJ(MDH_NSRC_J)) GOTO 10 !NO SUCH SOURCE
	  CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET MODEL
	  CALL NMOEXT(MDL)				!EXTERNAL FORMAT
	  MDLE(MDL_ID_J)=MDLJ(MDL_ID_J)			!TRANSLATE ID
	END IF
	IF (.NOT.WNDPAR('SOURCE',MDLE,MDLHDL,J0,A_B(-A_OB),
	1		MDLE,13)) THEN			!GET SOURCE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 10					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  RETURN
	END IF
	IF (MDLE(MDL_I_E).EQ.0) THEN
	  IF (LEDI) THEN				!EDIT
	  ELSE						!ADD
	    RETURN					!READY
	  END IF
	END IF
	MDLJ(MDL_ID_J)=NINT(MOD(MDLE(MDL_ID_J),100000.)) !ID
	IF (MDLJ(MDL_ID_J).EQ.0) THEN
	  IF (LEDI) THEN				!EDIT
	    MDLJ(MDL_ID_J)=J				!NEW ID
	  ELSE						!ADD
	    MDLJ(MDL_ID_J)=GDESJ(MDH_NSRC_J)+1		!NEW ID
	  END IF
	END IF
	CALL NMOEXF(MDL)				!CONVERT FORMAT
	IF (LEDI) THEN					!EDIT
	  CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+MDLHDL*(J-1)-
	1		A_OB))				!SAVE SOURCE
	ELSE						!ADD
	  CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+MDLHDL*
	1		GDESJ(MDH_NSRC_J)-A_OB))	!SAVE SOURCE
	  GDESJ(MDH_NSRC_J)=GDESJ(MDH_NSRC_J)+1		!COUNT
	END IF
C
	GOTO 10						!SEE IF MORE
C
C DELETE
C
	ENTRY NMOADL
C
 20	CONTINUE
	SRCL(0)=0					!ASSUME LIST
	IF (.NOT.WNDPAR('SOURCE_LIST',SRCL(1),MXSRCL*LB_J,J0,
	1		'*')) THEN			!GET TO DELETE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 20					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  IF (.NOT.WNDPAR('SOURCE_RANGE',SRCL(1),2*LB_J,J0,
	1		'""')) THEN			!GET TO DELETE
	    IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	    GOTO 20					!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	    RETURN
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    SRCL(1)=1					!RANGE
	    SRCL(2)=GDESJ(MDH_NSRC_J)
	  END IF
	  IF (J0.EQ.1) SRCL(2)=SRCL(1)			!LIMIT RANGE
	  SRCL(0)=-1					!INDICATE RANGE
	END IF
	IF (SRCL(0).GE.0) THEN				!LIST
	  DO I=1,J0					!DELETE SOURCES
	    J=SRCL(I)					!TO DO
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMVZ(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!DELETE
	    END IF
	  END DO
	ELSE						!RANGE
	  DO J=SRCL(1),SRCL(2)				!DELETE SOURCES
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMVZ(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!DELETE
	    END IF
	  END DO
	END IF
	GOTO 20						!MORE?
C
C DELETE LOW LEVEL NON-CLEAN
C
	ENTRY NMOANC
C
 60	CONTINUE
	IF (.NOT.WNDPAR('DELETE_LEVEL',SFAC(0),LB_E,J0,
	1		'0.')) THEN			!GET DELETE VALUE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 60					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 60					!RETRY
	END IF
	DO J=1,GDESJ(MDH_NSRC_J)			!ALL SOURCES
	  CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET SOURCE
	  J1=MDL(MDL_TP_B)				!CLEAN TYPE
	  IF (IAND(MDLCLN_M,J1).EQ.0 .AND. MDLE(MDL_I_E).LT.SFAC(0)) THEN !DELETE
	    CALL WNGMVZ(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!DELETE
	  END IF
	END DO
C
	RETURN
C
C DELETE LOW LEVEL CLEAN
C
	ENTRY NMOACD
C
 70	CONTINUE
	IF (.NOT.WNDPAR('DELETE_LEVEL',SFAC(0),LB_E,J0,
	1		'0.')) THEN			!GET DELETE VALUE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 70					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 70					!RETRY
	END IF
	DO J=1,GDESJ(MDH_NSRC_J)			!ALL SOURCES
	  CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET SOURCE
	  J1=MDL(MDL_TP_B)				!CLEAN TYPE
	  IF (IAND(MDLCLN_M,J1).NE.0 .AND. MDLE(MDL_I_E).LT.SFAC(0)) THEN !DELETE
	    CALL WNGMVZ(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!DELETE
	  END IF
	END DO
C
	RETURN
C
C DELETE SOURCES IN AREA
C
	ENTRY NMOAAD
C
 80	CONTINUE
	IF (.NOT.WNDPAR('DELETE_AREA',SFAC(0),4*LB_E,J0,
	1		'0.,0.,0.,0.')) THEN		!GET DELETE AREA
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 80					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 80					!RETRY
	END IF
	DO J=1,GDESJ(MDH_NSRC_J)			!ALL SOURCES
	  CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET SOURCE
	  CALL NMOEXT(MDL)				!EXTERNAL FORMAT
	  IF ((MDLE(MDL_L_E).GT.SFAC(0)-0.5*SFAC(2) .AND.
	1		MDLE(MDL_L_E).LT.SFAC(0)+0.5*SFAC(2)) .AND.
	1		(MDLE(MDL_M_E).GT.SFAC(1)-0.5*SFAC(3) .AND.
	1		MDLE(MDL_M_E).LT.SFAC(1)+0.5*SFAC(3))) THEN ! DELETE
	    CALL WNGMVZ(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!DELETE
	  END IF
	END DO
C
	RETURN
C
C CALIBRATE
C
	ENTRY NMOADC
C
	SFAC(0)=1.					!ASSUME NO FACTORS
	SFAC(1)=0
	SFAC(2)=0
 50	CONTINUE
	SRCL(0)=0					!ASSUME LIST
	IF (.NOT.WNDPAR('SOURCE_LIST',SRCL(1),MXSRCL*LB_J,J0,
	1		'*')) THEN			!GET TO CALIBRATE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 50					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  IF (.NOT.WNDPAR('SOURCE_RANGE',SRCL(1),2*LB_J,J0,
	1		'""')) THEN			!GET TO CALIBRATE
	    IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	    GOTO 50					!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	    RETURN
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    SRCL(1)=1					!RANGE
	    SRCL(2)=GDESJ(MDH_NSRC_J)
	  END IF
	  IF (J0.EQ.1) SRCL(2)=SRCL(1)			!LIMIT RANGE
	  SRCL(0)=-1					!INDICATE RANGE
	END IF
 51	CONTINUE
	IF (.NOT.WNDPAR('SOURCE_FACTORS',SFAC,3*LB_E,J1,
	1		A_B(-A_OB),SFAC,3)) GOTO 50	!GET FACTORS
	IF (E_C.EQ.DWC_NULLVALUE) RETURN		!READY
	IF (E_C.EQ.DWC_WILDCARD) GOTO 51		!MUST SPECIFY
	IF (SRCL(0).GE.0) THEN				!LIST
	  DO I=1,J0					!CALIBRATE SOURCES
	    J=SRCL(I)					!TO DO
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET MODEL LINE
	      CALL NMOEXT(MDL)				!MAKE EXTERNAL FORMAT
	      MDLE(MDL_I_E)=SFAC(0)*MDLE(MDL_I_E)	!CALIBRATE AMPLITUDE
	      MDLE(MDL_L_E)=SFAC(1)+MDLE(MDL_L_E)	!CALIB. L
	      MDLE(MDL_M_E)=SFAC(2)+MDLE(MDL_M_E)	!CALIB. M
	      CALL NMOEXF(MDL)				!MAKE INTERNAL FORMAT
	      CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!SET MODEL LINE
	    END IF
	  END DO
	ELSE						!RANGE
	  DO J=SRCL(1),SRCL(2)				!CALIBRATE SOURCES
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET MODEL LINE
	      CALL NMOEXT(MDL)				!MAKE EXTERNAL FORMAT
	      MDLE(MDL_I_E)=SFAC(0)*MDLE(MDL_I_E)	!CALIBRATE AMPLITUDE
	      MDLE(MDL_L_E)=SFAC(1)+MDLE(MDL_L_E)	!CALIB. L
	      MDLE(MDL_M_E)=SFAC(2)+MDLE(MDL_M_E)	!CALIB. M
	      CALL NMOEXF(MDL)				!MAKE INTERNAL FORMAT
	      CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!SET MODEL LINE
	    END IF
	  END DO
	END IF
	GOTO 50						!MORE?
C
C MERGE
C
	ENTRY NMOAMG
C
	LIDX=-1						!GENERAL INDEX
	DO I=0,1
	    MERAD(I) = 0                                !merge radii
	END DO
	GOTO 40
C
C
	ENTRY NMOAM1(IDX)
C
	LIDX=IDX					!HEADER INDEX
	DO I=0,1
	    MERAD(I) = 0                                !merge radii
	END DO
	GOTO 40
C
	ENTRY NMOAM2(IDX,NST,NND)
C
	LIDX=IDX
	I3=NST						!1ST SOURCE
	I4=NND						!LAST SOURCE
	DO I=0,1
	    MERAD(I) = 0                                !merge radii
	END DO
	GOTO 41
C
	ENTRY NMOAMR
C
	LIDX=-1						!GENERAL INDEX
C
	DEC0 = GMDHD(MDH_DEC_D,-1)     ! DEC, circles
	FRQ0 = GMDHD(MDH_FRQ_D,-1)     ! Freq, MHz
	IF (FRQ0.NE.0) THEN
	    MERADFLT(0)=
     1         2*1.5/(3000.*PI2*FRQ0/CL/(1.E-6))*DEG*3600.
	    IF (DEC0.NE.0) THEN
	        MERADFLT(1)=MERADFLT(0)/ABS(SIN(DEC0*DPI2))     !GUESS
	    ELSE
	        MERADFLT(1) = MERADFLT(0)                      ! round 
	    ENDIF
	ELSE
	    MERADFLT(0) = 0                             !no default?
	    MERADFLT(1) = 0                             !no default?
 	ENDIF
	DO I=0,1
	    MERAD(I) = MERADFLT(I)                      !arcsec
	END DO
	IF (.NOT.WNDPAR('MERGE_RADIUS',MERAD,2*LB_E,J0,
	1	  A_B(-A_OB),MERAD,2)) THEN
	    RETURN					!READY
	ELSE IF (J0.EQ.0) THEN
	    RETURN					!READY
	ELSE IF (J0.LT.0) THEN		        !ASSUME DEFAULT
	    MERAD(0) = MERADFLT(0)
	    MERAD(1) = MERADFLT(1)
	END IF
	DO I=0,1
	    MERAD(I) = MERAD(I)*DPI2/(3600.*360.)       !radians
	END DO
	GOTO 40
C
C
 40	CONTINUE
	I3=1						!FIRST SOURCE
	I4=GMDHJ(MDH_NSRC_J,LIDX)			!LAST SOURCE
 41	CONTINUE
	I3=MAX(0,I3-1)					!LIMIT
	I4=MIN(I4-1,GMDHJ(MDH_NSRC_J,LIDX)-1)
	DO I=I3,I4					!ALL SOURCES
	  J=(GMDHJ(MDH_MODP_J,LIDX)+I*MDLHDL-A_OB)	!SOURCE
	  J1=J/LB_E
	  I2=A_B(J+MDL_BITS_B)				!BITS
	  IF (A_E(J1+MDL_I_E).NE.0 .AND.                !NOT DELETED 
     1            IAND(I2,1).EQ.0) THEN                 !NOT EXTENDED
	    MAXDL = MERAD(0)                            !MAX DELTA L
	    MAXDM = MERAD(1)                            !MAX DELTA M
	    J4 = MDL(MDL_TP_B) 				!CLEAN TYPE
	    IF (IAND(MDLCLN_M,J4).NE.0) THEN            !CLEAN COMPONENT 
	      MAXDL = 0                  !MERGE ONLY IF COINCIDES EXACTLY 
	      MAXDM = 0
	    ENDIF
	    DO I1=I+1,I4
	      J2=(GMDHJ(MDH_MODP_J,LIDX)+I1*MDLHDL-A_OB) !SOURCE
	      J3=J2/LB_E
	      I2=A_B(J2+MDL_BITS_B)			!BITS
	      IF (IAND(I2,1).EQ.0 .AND.			!NOT EXTENDED
     1		  A_B(J2+MDL_TP_B).EQ.A_B(J+MDL_TP_B) .AND. !SAME CLEAN
     1		  A_E(J3+MDL_I_E).NE.0 .AND.	        !NOT DELETED
     1		  ABS(A_E(J3+MDL_L_E)-A_E(J1+MDL_L_E))
     1                           .LE.MAXDL .AND.         !CLOSE ENOUGH IN L
     1		  ABS(A_E(J3+MDL_M_E)-A_E(J1+MDL_M_E))
     1                           .LE.MAXDM .AND.         !CLOSE ENOUGH IN M
     1		  A_E(J3+MDL_SI_E).EQ.A_E(J1+MDL_SI_E) .AND. !SAME S.I.
     1		  A_E(J3+MDL_RM_E).EQ.A_E(J1+MDL_RM_E)) THEN !SAME R.M.
C
		R0=A_E(J1+MDL_I_E)+A_E(J3+MDL_I_E)	         !NEW I
		A_E(J1+MDL_Q_E)=(A_E(J1+MDL_Q_E)*A_E(J1+MDL_I_E)+
	1		A_E(J3+MDL_Q_E)*A_E(J3+MDL_I_E))/R0      !NEW Q
		A_E(J1+MDL_U_E)=(A_E(J1+MDL_U_E)*A_E(J1+MDL_I_E)+
	1		A_E(J3+MDL_U_E)*A_E(J3+MDL_I_E))/R0      !NEW U
		A_E(J1+MDL_V_E)=(A_E(J1+MDL_V_E)*A_E(J1+MDL_I_E)+
	1		A_E(J3+MDL_V_E)*A_E(J3+MDL_I_E))/R0      !NEW V
		A_E(J1+MDL_I_E)=R0			         !MERGE I
		IF (A_E(J1+MDL_I_E).LT.A_E(J3+MDL_I_E)) THEN
		  A_E(J1+MDL_L_E) = A_E(J3+MDL_L_E)   ! USE L OF LARGEST
		  A_E(J1+MDL_M_E) = A_E(J3+MDL_M_E)   ! USE M OF LARGEST
		ENDIF
		A_E(J3+MDL_I_E)=0			     !SET DELETE
	      END IF
	    END DO
	  END IF
	END DO
C
	RETURN
C
C FIELD EDIT
C
	ENTRY NMOAFB(OFF)
C
	FTYP=-1						!BYTE
	GOTO 30
C
	ENTRY NMOAFJ(OFF)
C
	FTYP=0						!J
	GOTO 30
C
	ENTRY NMOAFE(OFF)
C
	FTYP=1						!E
	GOTO 30
C
 30	CONTINUE
	IF (.NOT.WNDPAR('EDIT_VALUE',R0,LB_E,J0,'""')) THEN !GET VALUE TO SET
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 30					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 30
	END IF
	SRCL(0)=0					!ASSUME LIST
	IF (.NOT.WNDPAR('SOURCE_LIST',SRCL(1),MXSRCL*LB_J,J0,
	1		'*')) THEN			!GET TO SET
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	  GOTO 30					!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	  RETURN
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  IF (.NOT.WNDPAR('SOURCE_RANGE',SRCL(1),2*LB_J,J0,
	1		'""')) THEN			!GET TO SET
	    IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READY
	    GOTO 30					!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN		!""
	    RETURN
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    SRCL(1)=1					!RANGE
	    SRCL(2)=GDESJ(MDH_NSRC_J)
	  END IF
	  IF (J0.EQ.1) SRCL(2)=GDESJ(MDH_NSRC_J)	!LIMIT RANGE
	  SRCL(0)=-1					!INDICATE RANGE
	END IF
	IF (SRCL(0).GE.0) THEN				!LIST
	  DO I=1,J0					!SET SOURCES
	    J=SRCL(I)					!TO DO
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET SOURCE
	      CALL NMOEXT(MDL)				!MAKE EXTERNAL FORMAT
	      IF (FTYP.LT.0) THEN			!B
		MDL(OFF)=MOD(NINT(R0),128)
	      ELSE IF (FTYP.EQ.0) THEN			!J
		MDLJ(OFF)=NINT(R0)
	      ELSE
		MDLE(OFF)=R0
	      END IF
	      CALL NMOEXF(MDL)				!MAKE INTERNAL FORMAT
	      CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!SET SOURCE
	    END IF
	  END DO
	ELSE						!RANGE
	  DO J=SRCL(1),SRCL(2)				!EDIT SOURCES
	    IF (J.GT.0 .AND. J.LE.GDESJ(MDH_NSRC_J)) THEN !CAN DO
	      CALL WNGMV(MDLHDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB),MDL)		!GET SOURCE
	      CALL NMOEXT(MDL)				!MAKE EXTERNAL FORMAT
	      IF (FTYP.LT.0) THEN			!B
		MDL(OFF)=MOD(NINT(R0),128)
	      ELSE IF (FTYP.EQ.0) THEN			!J
		MDLJ(OFF)=NINT(R0)
	      ELSE
		MDLE(OFF)=R0
	      END IF
	      CALL NMOEXF(MDL)				!MAKE INTERNAL FORMAT
	      CALL WNGMV(MDLHDL,MDL,A_B(GDESJ(MDH_MODP_J)+
	1		(J-1)*MDLHDL-A_OB))		!SET SOURCE
	    END IF
	  END DO
	END IF
	GOTO 30						!MORE?
C
C
	END