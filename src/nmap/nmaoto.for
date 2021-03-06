C+ NMAOTO.FOR
C  WNB 910403
C
C  Revisions:
C	WNB 911023	Correct APT type
C	WNB 930930	Use SMP
C	AXC 010709      Linux port - APT string
C
	SUBROUTINE NMAOTO
C
C  Load map data into old SMP file
C
C  Result:
C
C	CALL NMAOTO	will load map data in old SMP file from WMP file
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMA_DEF'
	INCLUDE 'GFH_O_DEF'			!GENERAL FILE HEADER
	INCLUDE 'MPH_O_DEF'			!MAP HEADER
	INCLUDE 'SMP_O_DEF'			!RMAP HEADER
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNFOP,WNFOPF			!OPEN FILE
	LOGICAL WNFRD				!READ DATA
	LOGICAL WNFWR				!WRITE DATA
	INTEGER WNFEOF				!FILE POSITION
	INTEGER WNCALN				!STRING LENGTH
	LOGICAL WNDLNK				!LINK SET
	LOGICAL WNDLNG,WNDLNF			!LINK GROUP
	CHARACTER*32 WNTTSG			!MAKE SUB-GROUP STRING
	LOGICAL NMASTG				!GET A MAP
C
C  Data declarations:
C
	INTEGER OMCA				!OUTPUT FILE
	INTEGER OFILN				!OUTPUT FILE #
	CHARACTER*160 OFILE			!OUTPUT FILE NMAE
	REAL LBUF(0:8191)			!DATA LINE
	INTEGER MPHP				!MAP HEADER POINTER
	BYTE MPH(0:MPHHDL-1)			!MAP HEADER
	  INTEGER*2 MPHI(0:MPHHDL/2-1)
	  INTEGER   MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  REAL*8 MPHD(0:MPHHDL/8-1)
	  EQUIVALENCE (MPH,MPHI,MPHJ,MPHE,MPHD)
	BYTE GFH(0:GFHHDL-1)			!GENERAL FILE HEADER
	  INTEGER GFHJ(0:GFHHDL/4-1)
	  EQUIVALENCE (GFH,GFHJ)
	BYTE OFH(0:SMP__L-1)			!OLD FILE HEADER
	  CHARACTER*(SMP__L) OFHC
	  INTEGER*2 OFHI(0:SMP__L/LB_I-1)
	  INTEGER   OFHJ(0:SMP__L/LB_J-1)
	  REAL OFHE(0:SMP__L/LB_E-1)
	  REAL*8 OFHD(0:SMP__L/LB_D-1)
	  EQUIVALENCE (OFH,OFHC,OFHI,OFHJ,OFHE,OFHD)
	CHARACTER*32 SETSTR			!GROUP NAME
C-
C
C INIT
C
	OFILN=0					!OUTPUT COUNT
 100	CONTINUE
	OFILN=OFILN+1				!NEXT OUTPUT
	CALL WNCTXS(OFILE,'!AS\.!6$ZJ',FILIN,OFILN) !MAKE FILE NAME
C
C GET A MAP
C
	IF (.NOT.NMASTG(FCAOUT,SETS,MPH,MPHP,SGNR)) GOTO 800 !NO MORE MAPS
	IF (.NOT.WNFOP(OMCA,OFILE,'W')) THEN
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'!/I/O error output file')
	  GOTO 800
	END IF
	IF (.NOT.WNFRD(FCAOUT,GFHHDL,GFH,0)) THEN !READ FILE HEADER
 11	  CONTINUE
	  CALL WNCTXT(F_TP,'!/I/O error input file')
	  GOTO 800
	END IF
C
C MAKE FILE HEADER
C
	CALL WNGMVZ(SMP__L,OFH)			!CLEAR HEADER
	CALL WNGMFS(4,'.SMP',OFH(SMP_ID_1))	!ID
	OFHI(SMP_LEN_I)=SMP__L			!LENGTH
	CALL WNGMV(32,GFH(GFH_CDAT_1),OFH(SMP_CRD_1)) !CREATION/UPD. DATES/TIMES
	OFHI(SMP_RVN_I)=GFHJ(GFH_RCNT_J)	!REV. COUNT
	OFHI(SMP_VER_I)=4			!VERSION
	OFHJ(SMP_MLH_J)=SMP__L			!LINK TO MAP
	SETSTR=WNTTSG(SGNR(0),0)		!GROUP NAME
	CALL WNCTXT(F_TP,'Map !AS being copied to !AS',
	1		SETSTR,OFILE)
	CALL WNGMV(MPH_FNM_N,MPH(MPH_FNM_1),OFH(SMP_FNM_1)) !FIELD NAME
	OFHE(SMP_EPO_E)=MPHE(MPH_EPO_E)		!EPOCH
	OFHD(SMP_RA_D)=MPHD(MPH_RA_D)		!RA
	OFHD(SMP_DEC_D)=MPHD(MPH_DEC_D)		!DEC
	OFHD(SMP_FRQ_D)=MPHD(MPH_FRQ_D)		!FREQ
	OFHD(SMP_BDW_D)=MPHD(MPH_BDW_D)		!BANDWIDTH
	OFHD(SMP_RAO_D)=MPHD(MPH_RAO_D)		!OBS. RA
	OFHD(SMP_DCO_D)=MPHD(MPH_DECO_D)	!OBS. DEC
	OFHD(SMP_FRO_D)=MPHD(MPH_FRQO_D)	!OBS. FREQ
	OFHI(SMP_ODY_I)=MPHI(MPH_ODY_I)		!OBS. DAY
	OFHI(SMP_OYR_I)=MPHI(MPH_OYR_I)		!OBS. YEAR
	OFHI(SMP_DCD_I)=MPHI(MPH_DCD_I)		!DATA CODE
	OFHI(SMP_PCD_I)=MPHI(MPH_PCD_I)		!PROG. CODE
	OFHD(SMP_SRA_D)=MPHD(MPH_SRA_D)		!SEP. RA
	OFHD(SMP_SDC_D)=MPHD(MPH_SDEC_D)	!SEP. DEC
	OFHD(SMP_SFR_D)=MPHD(MPH_SFRQ_D)	!SEP. FREQ
	OFHI(SMP_NRA_I)=MPHJ(MPH_NRA_J)		!# RA
	OFHI(SMP_NDC_I)=MPHJ(MPH_NDEC_J)	!# DEC
	OFHI(SMP_NFR_I)=MPHJ(MPH_NFRQ_J)	!# FREQ
	OFHI(SMP_ZRA_I)=MPHJ(MPH_ZRA_J)		!0 RA
	OFHI(SMP_ZDC_I)=MPHJ(MPH_ZDEC_J)	!0 DEC
	OFHI(SMP_ZFR_I)=MPHJ(MPH_ZFRQ_J)	!0 FREQ
	OFHI(SMP_MXR_I)=MPHJ(MPH_MXR_J)		!MAX. POS. RA
	OFHI(SMP_MXD_I)=MPHJ(MPH_MXD_J)		!MAX. POS. DEC
	OFHI(SMP_MXF_I)=MPHJ(MPH_MXF_J)		!MAX. POS. FREQ
	OFHI(SMP_MNR_I)=MPHJ(MPH_MNR_J)		!MIN. POS. RA
	OFHI(SMP_MND_I)=MPHJ(MPH_MND_J)		!MIN. POS. DEC
	OFHI(SMP_MNF_I)=MPHJ(MPH_MNF_J)		!MIN. POS. FREQ
	OFHE(SMP_MAX_E)=MPHE(MPH_MAX_E)		!MAX.
	OFHE(SMP_MIN_E)=MPHE(MPH_MIN_E)		!MIN.
	OFHD(SMP_SHR_D)=MPHD(MPH_SHR_D)		!SHIFT RA
	OFHD(SMP_SHD_D)=MPHD(MPH_SHD_D)		!SHIFT DEC
	OFHD(SMP_SHF_D)=MPHD(MPH_SHF_D)		!SHIFT FREQ
	OFHD(SMP_SUM_D)=MPHD(MPH_SUM_D)		!NORM. SUM
	OFHE(SMP_UNI_E)=MPHE(MPH_UNI_E)		!UNIT FACTOR
	CALL WNGMV(MPH_UCM_N,MPH(MPH_UCM_1),OFH(SMP_UCM_1)) !USER COMMENT
	OFHJ(SMP_NPT_J)=MPHJ(MPH_NPT_J)		!# OF POINTS
	OFHI(SMP_NBS_I)=MPHJ(MPH_NBL_J)		!# OF BASELINES
	OFHI(SMP_NST_I)=MPHJ(MPH_NST_J)		!# OF SETS
	CALL WNGMV(MPH_TYP_N,MPH(MPH_TYP_1),OFH(SMP_TYP_1)) !MAP TYPE
	IF (MPHI(MPH_TYP_1/LB_I).EQ.
	1	ICHAR('A')*256+ICHAR('P')) THEN	!SET APT
	    OFH(SMP_TYP_1)=ICHAR('A')
	    OFH(SMP_TYP_1+1)=ICHAR('P')
	    OFH(SMP_TYP_1+2)=ICHAR('T')
	    OFH(SMP_TYP_1+3)=ICHAR(' ')
	END IF
	CALL WNGMV(MPH_POL_N,MPH(MPH_POL_1),OFH(SMP_POL_1)) !POL. TYPE
	DO I=0,7				!TAPER TYPE ETC
	  OFHI(SMP_CD_I+I)=MPHI(MPH_CD_I+I)
	END DO
	OFHI(SMP_EPT_I)=MPHI(MPH_EPT_I)		!APP. TYPE
	OFHE(SMP_OEP_E)=MPHE(MPH_OEP_E)		!OBS. EPOCH
	OFHE(SMP_NOS_E)=MPHE(MPH_NOS_E)		!NOISE
	OFHE(SMP_FRA_E)=MPHE(MPH_FRA_E)		!FIELD RA
	OFHE(SMP_FDC_E)=MPHE(MPH_FDEC_E)	!FIELD DEC
	OFHE(SMP_FFR_E)=MPHE(MPH_FFRQ_E)	!FIELD FREQ
	CALL WNGMV(MPH_TEL_N,MPH(MPH_TEL_1),OFH(SMP_TEL_1)) !TEL. NAME
	OFHI(SMP_FSR_I)=MPHJ(MPH_FSR_J)		!FFT SIZE RA
	OFHI(SMP_FSD_I)=MPHJ(MPH_FSD_J)		!FFT SIZE DEC
	IF (.NOT.WNFWR(OMCA,SMP__L,OFH,0))
	1			THEN		!WRITE MAP HEADER
 12	  CONTINUE
	  CALL WNCTXT(F_TP,'!/Write error')
	  GOTO 800
	END IF
C
C COPY DATA
C
	DO I=0,MPHJ(MPH_NDEC_J)-1		!ALL LINES
	  IF (.NOT.WNFRD(FCAOUT,LB_E*MPHJ(MPH_NRA_J),LBUF,
	1		MPHJ(MPH_MDP_J)+I*LB_E*MPHJ(MPH_NRA_J)))
	1			GOTO 10		!READ LINE
	  IF (.NOT.WNFWR(OMCA,LB_E*MPHJ(MPH_NRA_J),LBUF,
	1		SMP__L+I*LB_E*MPHJ(MPH_NRA_J)))
	1			GOTO 12		!WRITE LINE
	END DO
	CALL WNFCL(OMCA)			!CLOSE OUTPUT
	GOTO 100				!TRY FOR MORE
C
 800	CONTINUE
	CALL WNFCL(FCAOUT)			!CLOSE INPUT
C
	RETURN					!READY
C
C
	END
