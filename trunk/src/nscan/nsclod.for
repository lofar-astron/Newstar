C+ NSCLOD.FOR
C  WNB 900219
C
C  Revisions:
C	WNB 910307	Add velocity information
C	WNB 910513	Correct for System 52 tape error
C	WNB 910513	Correct for LINOBS error if IQUV on tape
C	HjV 920520	HP does not allow extended source lines
C	WNB 920813	Set correct types NM OM GM in OH
C	WNB 920814	Add splitted OH
C	WNB 920815	Some more split changes
C	WNB 920816	Some more split changes
C	WNB 920817	Some more split changes
C	WNB 920818	Add Tsys in weights
C	WNB 920820	Typo Tsys: made wrong phi
C	WNB 920828	Correct MJD calculation for aborted obs. in Wbork
C	WNB 920828	Update velocity and frequency calculations for line
C	WNB 930604	Change TSYS definition for new weight/flag, add MTSYS
C	WNB 930618	Correct FWGT
C	WNB 930621	FWGT always UF correction
C	WNB 930803	New OFFSET definition for RECORDS
C	WNB 930819	Add DIPC in STH
C	WNB 930825	Add pol. codes
C	HjV 930907	Minor change for SUN/RUG use
C	WNB 931130	Add ACORM
C	CMV 931220	Changed parameters of call to NSCPFL
C	CMV 940223	Force conversion for VAX D-format to DECStation
C	CMV 940223	Option to make only a listing
C	CMV 940303	Correct date in listing
C	CMV 940317	Correct sequence number in listing
C       HjV 940407      Correct for 'older' tape-versions (i.e. <6)
C	CMV 940418	Print tapeversion as SJ, check Leiden tapes
C	CMV 940420	Select IF sets if requested
C	CMV 940516	LIST duration in stead of FD#
C	CMV 940518	No format messages in LOG during list
C	CMV 940518	Check on BSINT (should be 10)
C	CMV 940817	Correct OH setnr.s to bandnr.s (version <6)
C	CMV 940829	Message if too many channels in dataset
C	CMV 940929	Put gain-corrections into (deapply) OTHR
C	CMV 941012	Split list options off in NSCLLI
C	JPH 941213	No error message for wildcard disk labels
C	CMV 950123	Also pass FWDJ to NSCLLI, no longer pass IMCA
C	CMV 950509	Also pass OH-number to NSCLLI
C       HjV 950703	Change text when someone loads a Leiden tape
C	CMV 951128	Unique temp-file name
C	CMV 960122	Message if no labels found on disk-file (bug by JPH)
C       HjV 960618	Change length to read for OH
C       HjV 961107	Give original Online Version nr. as argument to NSCLLI
C
	SUBROUTINE NSCLOD(TYP)
C
C  Load WSRT data into SCN file
C
C  Result:
C
C	CALL NSCLOD(TYP_J:I)	will load WSRT data in SCN file if TYP is 0,
C				or list WSRT data if TYP is 1, 
C				or list and update Scissor if TYP is 2.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'FDW_O_DEF'			!FD BLOCK
	INCLUDE 'FDW_T_DEF'
	INCLUDE 'FDX_O_DEF'			!FDX BLOCK
	INCLUDE 'FDX_T_DEF'
	INCLUDE 'OHW_O_DEF'			!OH BLOCK
	INCLUDE 'OHW_T_DEF'
	INCLUDE 'SCW_O_DEF'			!SC BLOCK
	INCLUDE 'SCW_T_DEF'
	INCLUDE 'GFH_O_DEF'			!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'			!SUB-GROUP HEADER
	INCLUDE 'STH_O_DEF'			!SET HEADER
	INCLUDE 'IFH_O_DEF'			!IF-SET HEADER
	INCLUDE 'NSC_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER TYP				!Can be 0,1 or 2
C
C  Function references:
C
	LOGICAL WNFOP,WNFOPF			!OPEN FILE
	LOGICAL WNFRD				!READ DATA
	LOGICAL WNFWR				!WRITE DATA
	INTEGER WNFEOF				!FILE POSITION
	DOUBLE PRECISION WNGDNF			!NORM. ANGLE
	INTEGER WNCALN				!STRING LENGTH
	LOGICAL WNDLNG,WNDLNF			!LINK SUB-GROUP
	LOGICAL NSCLRD				!READ DATA
	LOGICAL NSCLWD				!WRITE DATA
	LOGICAL NSCLIF				!READ/WRITE IF SET
	CHARACTER*80 WNFTVL			!GET VOLUME HEADER
	CHARACTER*20 WNFFNM			!GET TEMP-NAME
C
C  Data declarations:
C
	LOGICAL OUT				!WRITE SCN FILE?
	CHARACTER*6 LTXT			!LABEL NAME
	CHARACTER*(OHW_FIELD_N) FNAM		!FIELD NAME
	INTEGER FDP(0:1),OHP(0:1),SCP(0:1)	!DATA LENGTH, POINTER
	INTEGER OHP1(-1:MXNMOS-1)		!SPLITTED OH/SC POINTERS
	LOGICAL FSC				!FIRST SC INDICATOR
	DOUBLE PRECISION MJDHA0			!MJD AT HA=0
	DOUBLE PRECISION RACMOS			!RA MOZAIC CENTRE
	DOUBLE PRECISION FRCMOS			!FREQ. MOZAIC CENTRE
	LOGICAL SPLIT				!GM OR NM
	LOGICAL FSPLIT				!FIRST CHANNEL
	INTEGER DWELT				!DWELL TIME
	LOGICAL THIS_CHAN			!LOAD THIS CHANNEL
	INTEGER I6
	REAL TSYS(0:1,0:STHTEL-1)		!1/TSYS
	REAL MTSYS				!MAX(1/TSYS)
	REAL FWGT				!MAX. WEIGHT
	CHARACTER*4 BECODE			!BE CODE
	INTEGER IXX,IYX,IXY,IYY			!CHAR CODES IN INTs
	INTEGER POLCD(0:3)			!POL. CODE
	  DATA POLCD/XX_P,XY_P,YX_P,YY_P/
C
	BYTE FDW(0:FDWHDL-1)			!FD
	  INTEGER*2 FDWI(0:FDWHDL/2-1)
	  INTEGER   FDWJ(0:FDWHDL/4-1)
	  EQUIVALENCE (FDW,FDWI,FDWJ)
	BYTE FDX(0:FDXHDL-1)			!FDX
	  INTEGER*2 FDXI(0:FDXHDL/2-1)
	  INTEGER   FDXJ(0:FDXHDL/4-1)
	  EQUIVALENCE (FDX,FDXI,FDXJ)
	BYTE OHW(0:OHWHDL-1)			!OH
	  INTEGER*2 OHWI(0:OHWHDL/2-1)
	  INTEGER   OHWJ(0:OHWHDL/4-1)
	  REAL OHWE(0:OHWHDL/4-1)
	  REAL*8 OHWD(0:OHWHDL/8-1)
	  EQUIVALENCE (OHW,OHWI,OHWJ,OHWE,OHWD)
	BYTE SCW(0:SCWHDL-1)			!SC
	  INTEGER*2 SCWI(0:SCWHDL/2-1)
	  INTEGER   SCWJ(0:SCWHDL/4-1)
	  REAL*4 SCWE(0:SCWHDL/4-1)
	  REAL*8 SCWD(0:SCWHDL/8-1)
	  EQUIVALENCE (SCW,SCWI,SCWJ,SCWE,SCWD)
	BYTE OHW1(0:OHWHDL-1)			!OH SPLITTED
	  INTEGER*2 OHW1I(0:OHWHDL/2-1)
	  INTEGER   OHW1J(0:OHWHDL/4-1)
	  REAL OHW1E(0:OHWHDL/4-1)
	  REAL*8 OHW1D(0:OHWHDL/8-1)
	  EQUIVALENCE (OHW1,OHW1I,OHW1J,OHW1E,OHW1D)
C
	BYTE STH(0:STHHDL-1)			!SET HEADER
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  INTEGER   STHJ(0:STHHDL/4-1)
	  REAL STHE(0:STHHDL/4-1)
	  REAL*8 STHD(0:STHHDL/8-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
	BYTE IFH(0:IFHHDL-1)			!IF-SET HEADER
	  INTEGER*2 IFHI(0:IFHHDL/2-1)
	  INTEGER   IFHJ(0:IFHHDL/4-1)
	  REAL IFHE(0:IFHHDL/4-1)
	  REAL*8 IFHD(0:IFHHDL/8-1)
	  EQUIVALENCE (IFH,IFHI,IFHJ,IFHE,IFHD)
C
C	Some buffers passed to lower level routines. 
C	Use of OBUF:	nsclif INTEGER*2 OBUF(2,0:1,0:STHTEL-1,0:*)
C			nscl?d REAL*4    OBUF(  0:1,0:STHTEL-1,0:*)
C
	INTEGER*2 TPBUF(2,0:1,0:STHTEL-1,0:MXDATN-1)	!BUFFER FOR IF-DATA
	INTEGER*2 OBUF(2,0:1,0:STHTEL-1,0:MXDATN-1)	!OUTPUT BUFFER
	INTEGER*2 DBUF(2,0:MXDATN-1)			!INPUT BUFFER
	INTEGER*2 TMPBUF(3,0:MXDATX-1)			!OUTPUT BUFFER
C
	INTEGER FCAT				!TMP FILE DESCRIPTOR
	INTEGER NCHT				!# OF CHANNELS DONE
	INTEGER FVERS                           !TAPE VERSION
	INTEGER ORG_VS				!ORIGINAL ONLINE SYSTEM VERSION
	INTEGER VS				!ONLINE SYSTEM VERSION
	INTEGER BINT				!BASIC INTEGRATION TIME
	INTEGER SFREQ                           !SFREQ FROM OH
	REAL OHAB				!START HA SCANS
	INTEGER ONS(6)				!INTEGRATION DATA
	INTEGER NPOL				!# OF POLARISATIONS FOUND
	INTEGER POLS(0:3)			!INDICATE POLS TO DO
	INTEGER NIFR				!INTERFEROMETER COUNT
	INTEGER IFRT(9,0:MXNIFR-1)		!INTERFEROMETER DESCRIPTION
	INTEGER F_XX				!OUTPUT FOR FORMATS
	CHARACTER*80 VOLUME			!VOLUME HEADER
C-
C
C INIT
C
	IXX=ICHAR('X')*256+ICHAR('X')
	IXY=ICHAR('X')*256+ICHAR('Y')
	IYX=ICHAR('Y')*256+ICHAR('X')
	IYY=ICHAR('Y')*256+ICHAR('Y')
C
	OUT=(TYP.EQ.0)				!ONLY SCN FILE IF TYP=0
	IF (OUT) THEN
	   F_XX=F_TP				!BOTH SCREEN AND LOG
	ELSE
	   F_XX=F_T				!ONLY SCREEN
	END IF
	IF (.NOT.WNFOP(FCAT,WNFFNM('NSC','TMP'),'WT')) THEN !OPEN TMP FILE
	  CALL WNCTXT(F_TP,'Cannot open TMP file (!XJ)',E_C)
	  GOTO 900
	END IF
	VOLUME=' '				!DEFAULT NO VOLUME
	IF (UNIT.NE.'D') VOLUME=WNFTVL(IMCA)	!GET VOLUME HEADER
C
	J1=0					!JOB COUNT
 30	CONTINUE
	J1=J1+1					!NEXT JOB
	IF (J1.GT.NJOB) GOTO 900		!READY
	J=0					!START LABEL INPUT
	IF (OUT) THEN
	  IF (.NOT.WNDLNG(GFH_LINKG_1,0,SGH_GROUPN_1,FCAOUT,SGPH(0),
	1		SGNR(0))) THEN
	    CALL WNCTXT(F_TP,'!/Cannot create sub-group')
	    GOTO 800				!NEXT JOB
	  END IF				!SUB-GROUP LINKED
	  CALL WNCTXT(F_P,'!_')			!NEW PAGE
	  CALL WNCTXT(F_TP,'!/Job !UJ\: Group !UJ',J1,SGNR(0))
	ELSE
	  CALL WNCTXT(F_TP,
	1	   'Label Seq.nr  Fieldname   Project '//
	1	   'hhmm TP SP yymmdd hhmm       RA         Dec   ')
	END IF
C
C DO A LABEL
C
 10	CONTINUE
	J=J+1					!COUNT INPUT LABEL
	IF (NLAB(J1).LT.0) THEN			!ALL LABELS ON TAPE
	  J0=J					!NEXT INPUT LABEL
	ELSE IF (J.LE.NLAB(J1)) THEN
	  J0=ILAB(J,J1)				!NEXT INPUT LABEL
	ELSE
	  GOTO 800				!READY WITH JOB
	END IF
C
C OPEN INPUT
C
	IF (UNIT.EQ.'D') THEN			!DISK INPUT
	  CALL WNCTXS(LTXT,'!6$ZJ',J0)		!MAKE LABEL NAME
	  IF (.NOT.WNFOP(IMCA,IFILE(1:WNCALN(IFILE))//'.'//LTXT,'R')) THEN
	    IF (NLAB(J1).GT.0) THEN
		CALL WNCTXT(F_XX,'Cannot find file !AS\.!AS',IFILE,LTXT)
	    ELSE
		CALL WNCTXT(F_XX,'No labels found for file !AS',IFILE)
	    END IF  
	    GOTO 800				!STOP JOB
	  END IF
	ELSE					!TAPE INPUT
	  IF (.NOT.WNFOPF(IMCA,' ','R',0,0,0,J0)) THEN
	    CALL WNCTXT(F_XX,'Cannot find label !UJ',J0)
	    GOTO 800				!NEXT JOB
	  END IF
	END IF
	IF (OUT) THEN
	  IF (.NOT.WNDLNG(SGPH(0)+SGH_LINKG_1,0,SGH_GROUPN_1,FCAOUT,
	1		SGPH(1),SGNR(1))) THEN	!LINK SUB-GROUP
	    CALL WNCTXT(F_TP,'!/Cannot create sub-group')
	    GOTO 700				!NEXT LABEL
	  END IF
	  CALL WNCTXT(F_TP,'!4CLabel !3$UJ: Sub-group !UJ\.!UJ',
	1		J0,SGNR(0),SGNR(1))
	END IF
C
C OPEN OUTPUT
C
C
C READ FD
C
	J2=0					!DATA POINTER
 20	CONTINUE
	IF (.NOT.WNFRD(IMCA,FDWHDL,FDW,J2)) THEN !READ FD BLOCK
	  CALL WNCTXT(F_XX,'Read error FD at !XJ',J2)
	  GOTO 700				!NEXT LABEL
	END IF
	IBMSW=.FALSE.				!ASSUME NON-IBM
	DECSW=.FALSE.				!ASSUME LOCAL
	IF (FDW(FDW_CBT_1).NE.ICHAR('F') .OR.
	1	FDW(FDW_CBT_1+1).NE.ICHAR('D')) THEN
	  IBMSW=.TRUE.				!ASSUME IBM
	  CALL WNTTIL(FDWHDL,FDW,FDW_T)		!TRANSLATE
	  IF (FDW(FDW_CBT_1).NE.ICHAR('F') .OR.
	1	FDW(FDW_CBT_1+1).NE.ICHAR('D')) THEN
 23	    CONTINUE
	    CALL WNCTXT(F_XX,'Cannot find FD block')
	    GOTO 700				!NEXT LABEL
	  END IF
	ELSE IF (FDWI(FDW_CBI_I).NE.32767) THEN
	  DECSW=.TRUE.				!ASSUME FROM DEC
	  CALL WNTTDL(FDWHDL,FDW,FDW_T)		!TRANSLATE
	  IF (FDWI(FDW_CBI_I).NE.32767) GOTO 23
C
C	DECStation/Alpha has the same swapping sequence as VAX D/G,
C	but uses IEEE floating point format. The test on FDW_CBI is
C	therefore not sufficient. Since raw data is assumed to be in
C	IBM (type -1) or VAX D (type 1) format, the following test is
C	safe and sufficient. 
C
	ELSE IF (PRGDAT.EQ.6) THEN
	  DECSW=.TRUE.				!ASSUME FROM DEC
	  CALL WNTTDL(FDWHDL,FDW,FDW_T)		!TRANSLATE
	  IF (FDWI(FDW_CBI_I).NE.32767) GOTO 23
	END IF
C
C REPAIR FD
C
	FVERS=FDWI(FDW_FVERS_I)
	IF (FVERS.EQ.-1) THEN
	   CALL WNCTXT(F_XX,'Leiden tape... please use option LEIDEN')
	   GOTO 700
	END IF
	IF (FVERS.LT.3) FDWI(FDW_STIM_I)=FDWI(FDW_STIM_I)*6
	IF (FVERS.LT.5) 
	1    CALL WNCTXT(F_P,'FVERS !SJ: all times are ST',FVERS)
	IF (FVERS.LT.7) FDWJ(FDW_MOH_J)=1
C
	IF (FDWI(FDW_LRCRD_I).NE.SRTRCL) THEN
	   CALL WNCTXT(F_XX,
	1	'WARNING: Recordlength of input tape seems incorrect '//
	1	'(!UJ set to !UJ)',FDWI(FDW_LRCRD_I),SRTRCL)
	   FDWI(FDW_LRCRD_I)=SRTRCL
	END IF
C
C READ FDX
C
	J2=J2+FDWI(FDW_LRCRD_I)			!POINT TO FDX
	I=(FDWJ(FDW_LFD_J)-1)*FDWI(FDW_LRCRD_I)	!LENGTH FDX
	IF (I.GT.FDXHDL) CALL WNCTXT(F_XX,'FDX block length (!UJ) '//
	1		'differs from expected (!UJ)',I,FDXHDL)
	I=MIN(I,FDXHDL)
	IF (.NOT.WNFRD(IMCA,I,FDX,J2)) THEN	!READ FDX
	  CALL WNCTXT(F_XX,'Read error FDX at !XJ',J2)
	  GOTO 700				!NEXT LABEL
	END IF
	IF (IBMSW) CALL WNTTIL(I,FDX,FDX_T)	!TRANSLATE
	IF (DECSW) CALL WNTTDL(I,FDX,FDX_T)
	FDP(0)=I+FDWI(FDW_LRCRD_I)		!TOTAL LENGTH FD
	IF (OUT) FDP(1)=WNFEOF(FCAOUT)		!POSITION TO WRITE
	FDWJ(FDW_LFD_J)=FDP(0)			!SAVE LENGTH
	IF (OUT) THEN
	  IF (.NOT.WNFWR(FCAOUT,FDWHDL,FDW(0),FDP(1))) THEN !WRITE FD
 22	    CONTINUE
	    CALL WNCTXT(F_XX,'!/Error writing to SCN file (!XJ)',E_C)
	    GOTO 900				!STOP
	  END IF
	  IF (.NOT.WNFWR(FCAOUT,FDP(0)-FDWHDL,
	1		FDX(0),FDP(1)+FDWHDL)) GOTO 22
	END IF
C
C READ OH
C
	J3=0					!OH COUNT
	J2=FDWI(FDW_LRCRD_I)*FDWJ(FDW_NOH_J)	!OH POINTER
	FSC=.TRUE.				!INDICATE FIRST SC
 50	CONTINUE
	J3=J3+1					!COUNT OH
	I=FDWI(FDW_LOH_I)*FDWI(FDW_LRCRD_I)	!LENGTH OH
	IF (I.GT.OHWHDL) THEN
	     CALL WNCTXT(F_XX,'OH block length (!UJ) '//
	1		'larger than expected (!UJ)',I,OHWHDL)
	     CALL WNCTXT(F_XX,'(probably there are '//
	1		'too many channels in the dataset)')
	END IF	
	I=MIN(I,OHWHDL)
	IF (.NOT.WNFRD(IMCA,I,OHW,J2)) THEN	!READ OH
	  CALL WNCTXT(F_XX,'Read error OH at !XJ',J2)
	  GOTO 700				!NEXT LABEL
	END IF
	IF (IBMSW) CALL WNTTIL(I,OHW,OHW_T)	!TRANSLATE
	IF (DECSW) CALL WNTTDL(I,OHW,OHW_T)
	IF (OHWI(OHW_CBI_I).NE.32767 .OR.
	1	OHW(OHW_CBT_1).NE.ICHAR('O') .OR.
	1	OHW(OHW_CBT_1+1).NE.ICHAR('H')) THEN
	  CALL WNCTXT(F_XX,'Cannot find OH block')
	  GOTO 600				!NEXT OH
	END IF
	OHP(0)=I				!TOTAL LENGTH OH
	IF (OUT) OHP(1)=WNFEOF(FCAOUT)		!POSITION TO WRITE
	OHWJ(OHW_LOH_J)=OHP(0)			!SAVE LENGTH
C
C	Test number of sets.
C
	IF (OHWI(OHW_NRSTS_I).GT.OHWSET) THEN
	   CALL WNCTXT(F_XX,
	1	'Indeed too many channels in the dataset '//
	1	'(!UI > > !UJ)',OHWI(OHW_NRSTS_I),OHWSET)
	   CALL WNCTXT(F_XX,'Using only first !UJ sets',OHWSET)
	   OHWI(OHW_NRSTS_I)=OHWSET		!TRUNCATE SETS
	END IF
C
C REPAIR OH
C
	IF (FVERS.LT.3) OHWI(OHW_OLSYS_I)=20
	ORG_VS=OHWI(OHW_OLSYS_I)		!ONLINE SYSTEM
	VS=OHWI(OHW_OLSYS_I)			!ONLINE SYSTEM
	IF (VS.EQ.52) OHWI(OHW_BSINT_I)=NINT(OHWI(OHW_BSINT_I)/10.) !SYSTEM 52
	IF (VS.LT.43.AND.OHWI(OHW_STOPAR_I).NE.0) OHWI(OHW_STOPAR_I)=0
	IF (OHWI(OHW_STOPAR_I).NE.0 .AND. OHWI(OHW_MSPAT_I).EQ.0)
	1		OHWI(OHW_MPOSN_I)=0	!MAKE SURE LINOBS ERROR CORRECT
	IF (FVERS.EQ.1) OHWI(OHW_VELC_I)=MOD(ABS(OHWI(OHW_MODE_I)),2)
	IF (FVERS.LT.2) OHWJ(OHW_VOLGNR_J)=OHWJ(OHW_VOLGNR_J)+
	1    OHWI(OHW_DATE_I+1)*100000
	IF (OHWJ(OHW_VOLGNR_J)/10000000.EQ.19) 
	1    OHWJ(OHW_VOLGNR_J)=OHWJ(OHW_VOLGNR_J)-190000000	!ERROR OLSYS 57
	IF (FVERS.LT.3) THEN
	  OHWI(OHW_STIM_I)=OHWI(OHW_STIM_I)*6
	  CALL WNCTXT(F_P,
	1    'FVERS !UJ: FREQ is fringe stopping freq, not middle of band',
	2    FVERS)
	END IF
C
	IF (FVERS.LT.6) THEN
	  OHWD(OHW_FREQ0_D)=0
	  IF (OHW(OHW_BECODE_1).NE.ICHAR('D')) THEN
	    CALL WNCTXT(F_XX,'Assuming DLB was used')
	    CALL WNGMFS(OHW_BECODE_N,'DLB ',OHW(OHW_BECODE_1))
	  END IF
	  DO I=0,OHWI(OHW_NRSTS_I)-1		    !ALL INPUT SETS
	    I1=(OHW_SET_1+I*OHWI(OHW_LENT_I))/LB_I  !OFFSET TABLE
	    OHWI(I1+SET_BANDNR_I)=(OHWI(I1+SET_BANDNR_I)-1)/4
	  END DO
	  CALL WNCTXT(F_XX,'Band-numbers corrected')
	END IF
C
	IF (OHWI(OHW_ETIM_I).LT.0) THEN		!FVERS < 7  NO ETIM
	  OHWI(OHW_ETIM_I)=OHWI(OHW_STIM_I)+
	1	(OHWD(OHW_HAEND_D)-OHWD(OHW_HAST_D))*DCRTSC/10+1
	END IF
	SFREQ=OHWI(OHW_SFREQ_I)
	CALL WNGMTS(OHW_BECODE_N,OHW(OHW_BECODE_1),BECODE) !BE CODE
C
C Check Basic integration time (errors found for FVERS 1, 1979)
C
	IF (OHWI(OHW_BSINT_I).GT.10) THEN
	   CALL WNCTXT(F_XX,
	1	'Basic int.time is !UI, changed to 10 sec',
	1	OHWI(OHW_BSINT_I))
	   OHWI(OHW_BSINT_I)=10
	END IF
C
C Check telescope positions (errors found for FVERS 1, 1979)
C
	IF (OHWJ(OHW_POST_J).EQ.OHWJ(OHW_POST_J+9)) THEN
	   CALL WNCTXT(F_XX,'Error in position of RT0, corrected')
	   OHWJ(OHW_POST_J)=OHWJ(OHW_POST_J+1)+
	1	(OHWJ(OHW_POST_J+1)-OHWJ(OHW_POST_J+2))	!ASSUME 0-1 = 1-2
	END IF
C
C SET PROPER SPLIT/UNSPLIT
C
	IF (FDWI(FDW_FVERS_I).EQ.6 .AND.
	1		(OHWI(OHW_OLSYS_I).EQ.60 .OR.
	1		OHWI(OHW_OLSYS_I).EQ.61)) THEN !UNSPLIT OLD FORMAT
	  OHWI(OHW_MSPAT_I)=-ABS(OHWI(OHW_MSPAT_I)) !INDICATE OM
	  OHWI(OHW_MSNP_I)=FDXI(807)		!PATTERN LENGTH
	ELSE IF (FDWI(FDW_FVERS_I).EQ.7 .AND.
	1		OHWI(OHW_OLSYS_I).LT.62) THEN !INDICATE GM
	  OHWI(OHW_MSNP_I)=FDXI(807)		!PATTERN LENGTH
	ELSE IF (OHWI(OHW_OLSYS_I).LT.60) THEN
	  OHWI(OHW_MSPAT_I)=0			!SET NM
	  OHWI(OHW_MSNP_I)=1			!NUMBER IN PATTERN
	  OHWI(OHW_MPOSN_I)=0			!PATTERN START
	END IF
	OHWI(OHW_MSNP_I)=MAX(1,MIN(MXNMOS,OHWI(OHW_MSNP_I))) !LIMIT
C
C FINAL OH CHANGES
C
	IF (FDWI(FDW_FVERS_I).LT.7) VS=MIN(VS,58) !CATER FOR ERROR
	OHWI(OHW_OLSYS_I)=VS
	BINT=OHWI(OHW_BSINT_I)			!BASIC INTEGRATION TIME (10 SEC)
	IF (BINT.GT.10) THEN
	   CALL WNCTXT(F_XX,
	1	'Basic int.time is !UJ, changed to 10 sec',BINT)
	   BINT=10
	END IF
C
C READ SC
C
	IF (FSC) THEN				!FIRST SC
	  J2=FDWJ(FDW_NSC_J)*FDWI(FDW_LRCRD_I)	!SC POINTER
	  FSC=.FALSE.				!NON-FIRST SC
	ELSE
	  J2=SCWJ(SCW_NSCN_J)*FDWI(FDW_LRCRD_I)	!POINTER NEXT SC
	END IF
	I=FDWI(FDW_LSC_I)*FDWI(FDW_LRCRD_I)	!LENGTH SC
	IF (I.GT.SCWHDL) CALL WNCTXT(F_XX,'SC block length (!UJ) '//
	1		'differs from expected (!UJ)',I,SCWHDL)
	I=MIN(I,SCWHDL)
	IF (.NOT.WNFRD(IMCA,I,SCW,J2)) THEN	!READ SC
	  CALL WNCTXT(F_XX,'Read error SC at !XJ',J2)
	  GOTO 700				!NEXT LABEL
	END IF
	IF (IBMSW) CALL WNTTIL(I,SCW,SCW_T)	!TRANSLATE
	IF (DECSW) CALL WNTTDL(I,SCW,SCW_T)
	IF (SCWI(SCW_CBI_I).NE.32767 .OR.
	1	SCW(SCW_CBT_1).NE.ICHAR('S') .OR.
	1	SCW(SCW_CBT_1+1).NE.ICHAR('C')) THEN
	  CALL WNCTXT(F_XX,'Cannot find SC block')
	  GOTO 600				!NEXT OH
	END IF
C
C REPAIR SC
C
	IF (VS.LT.26) SCWD(SCW_FRO_D)=0         !ASSUME 0
	IF (VS.LT.43) THEN
	  SCWI(SCW_SYNF_I)=1                    !ASSUME 1
	  SCWE(SCW_CEXT_E)=1                    !ASSUME NO EXTINCTION
	  SCWE(SCW_GLAT_E)=52.7317357/360.	!Lattitude in circles
	END IF
	IF (FVERS.LT.3) THEN
	  SCWI(SCW_STIM_I)=SCWI(SCW_STIM_I)*6
	  SCWE(SCW_C2X2_E)=0.0                  !NO V.VLECK CORR. KNOWN
	  SCWE(SCW_C2X3_E)=0.0
	  SCWE(SCW_C2X4_E)=0.0
	  SCWE(SCW_C4X3_E)=0.0
	  SCWE(SCW_C4X4_E)=0.0
	  SCWE(SCW_JDCP_E)=0.0                  !NO CLOCK CORR. STORED
	  SCWE(SCW_CLOCK_E)=0.0
	  SCWE(SCW_CLCOFF_E)=0.0
	  SCWE(SCW_DCLOCK_E)=0.0
	  SCWI(SCW_YEAR_I)=0
	  SCWI(SCW_MONTH_I)=0
	  SCWI(SCW_DAY_I)=0
	END IF
	IF (FVERS.LT.5) SCWD(SCW_CUTST_D)=0.0   !ST instead of UT
	SCP(0)=I				!TOTAL LENGTH SC
	SCP(1)=OHP(1)+OHP(0)			!POSITION TO WRITE
	SCWJ(SCW_LSC_J)=SCP(0)			!SAVE LENGTH
	MTSYS=0					!MAX. TSYS
	DO I=0,STHTEL-1				!GET TSYS ALL TEL.
	  DO I1=0,1				!X, Y
	    TSYS(I1,I)=1./MAX(15.,MIN(300.,
	1		SCWE(SCW_TSYSI_E+2*I+I1))) !LIMITED 1/TSYS
	    MTSYS=MAX(MTSYS,TSYS(I1,I))		!MAX. 1/TSYS
	  END DO
	END DO
	DO I=0,STHTEL-1				!LIMIT 1/TSYS TO 256
	  DO I1=0,1				!X, Y
	    TSYS(I1,I)=256.*TSYS(I1,I)/MTSYS
	  END DO
	END DO
	CVUTST=SCWD(SCW_CUTST_D)+1D0		!UT TO ST CONVERSION
C
C CHECK IF OH WANTED
C
	SPLIT=OHWI(OHW_MSPAT_I).GE.0		!SPLITTED DATA
	IF (NPTC(J1).LT.0) THEN			!DO ALL OH'S (*)
	ELSE IF (OHWI(OHW_MSPAT_I).LE.0) THEN	!DO IF OM OR NM TYPE
	ELSE
	  DO I=1,NPTC(J1)
	    IF (IPTC(I,J1).EQ.J3) GOTO 51	!THIS ONE
	  END DO
	  GOTO 600				!TRY NEXT OH
	END IF
C
C WRITE SC/OH
C
 51	CONTINUE
	IF (SPLIT) THEN				!NM OR GM
	  IF (OUT) THEN
	    IF (.NOT.WNFWR(FCAOUT,OHP(0),OHW(0),OHP(1))) GOTO 22 !WRITE OH
	  END IF
	  RACMOS=0				!NO CENTRE RA NEEDED
	  FRCMOS=0				!NO CENTRE FREQ. NEEDED
	ELSE					!OM
	  DO I1=1,OHWI(OHW_MSNP_I)		!ALL FIELD CENTRA
	    IF (NPTC(J1).LT.0) THEN		!ALL
	    ELSE
	      DO I=1,NPTC(J1)			!TEST
		IF (IPTC(I,J1).EQ.I1) GOTO 60	!THIS ONE ASKED
	      END DO
	      GOTO 61				!TRY NEXT
	    END IF
C
C MAKE OH/SC FOR OM TYPE
C
 60	    CONTINUE
	    CALL WNGMV(OHWHDL,OHW,OHW1)		!COPY INITIAL OH
	    I2=MOD((I1-1)+OHW1I(OHW_MPOSN_I),OHW1I(OHW_MSNP_I)) !TABLE #
	    OHW1D(OHW_RA0_D)=SCWJ((SCW_MOZP_1+I2*SCW_MOZP_N)/LB_J+
	1		MOZP_RA1_J)/D2T32	!RA APPARENT
	    OHW1D(OHW_DEC0_D)=SCWJ((SCW_MOZP_1+I2*SCW_MOZP_N)/LB_J+
	1		MOZP_DEC1_J)/D2T32	!DEC APPARENT
	    OHW1D(OHW_RA1_D)=SCWJ((SCW_MOZP_1+I2*SCW_MOZP_N)/LB_J+
	1		MOZP_RA0_J)/D2T32	!RA EPOCH
	    OHW1D(OHW_DEC1_D)=SCWJ((SCW_MOZP_1+I2*SCW_MOZP_N)/LB_J+
	1		MOZP_DEC0_J)/D2T32	!DEC EPOCH
	    IF (VS.GE.62) THEN			!CAN DO FREQUENCY
	      OHW1D(OHW_FREQ_D)=SCWD((SCW_MOZP_1+I2*SCW_MOZP_N)/LB_D+
	1		MOZP_FREQ1_D)
	    ELSE
	      CALL WNCTXT(F_XX,'FREQ not corrected for mosaic position')
	    END IF
	    OHW1D(OHW_HAST_D)=OHWD(OHW_HAST_D)+OHWD(OHW_RA0_D)-
	1		OHW1D(OHW_RA0_D)	!UPDATE HA START
	    OHW1D(OHW_HAST_D)=WNGDNF(OHW1D(OHW_HAST_D)) !NORM.
	    OHW1D(OHW_HAEND_D)=OHWD(OHW_HAEND_D)+OHWD(OHW_RA0_D)-
	1		OHW1D(OHW_RA0_D)	!UPDATE HA END
	    OHW1D(OHW_HAEND_D)=WNGDNF(OHW1D(OHW_HAEND_D)) !NORM.
	    RACMOS=OHWD(OHW_RA0_D)		!RA CENTRE MOSAIC APP.
	    FRCMOS=OHWD(OHW_FREQ_D)		!FREQ. CENTRE MOSAIC
	    CALL WNGMTS(OHW_FIELD_N,OHW(OHW_FIELD_1),FNAM) !FIELD NAME
	    DO I3=OHW_FIELD_N,1,-1		!CHECK .
	      IF (FNAM(I3:I3).EQ.'.') THEN	!FOUND
	        CALL WNCTXS(FNAM(I3+1:),'!UJ',I2)
	        GOTO 62
	      END IF
	    END DO
	    I3= MIN(WNCALN(FNAM),OHW_FIELD_N-4)	!NAME
	    CALL WNCTXS(FNAM(I3+1:),'.!UJ',I2)
 62	    CONTINUE
	    CALL WNGMFS(OHW_FIELD_N,FNAM,OHW1(OHW_FIELD_1)) !SET NAME
	    IF (OUT) THEN
	      OHP1(-1)=OHP(0)
	      OHP1(I1-1)=WNFEOF(FCAOUT)		!WHERE TO WRITE
	      IF (.NOT.WNFWR(FCAOUT,OHP(0),OHW1(0),
	1			OHP1(I1-1))) GOTO 22 !WRITE OH
	      IF (.NOT.WNDLNF(SGPH(1)+SGH_LINKG_1,I2,SGH_GROUPN_1,FCAOUT,
	1		SGPH(2),SGNR(2))) THEN	!FIND/CREATE SUB-GROUP
	        CALL WNCTXT(F_TP,'!/Cannot link sub-group')
	        GOTO 600			!NEXT OH
	      END IF
	      CALL WNCTXT(F_TP,'!6C\OH  !4$UJ: !UJ\.!UJ\.!UJ'//
	1		'!32C\RA= !10$DPF15.5 Dec= !10$DAF15.5',
	1		I1,SGNR(0),SGNR(1),SGNR(2),
	1		OHW1D(OHW_RA1_D),OHW1D(OHW_DEC1_D))
	    ELSE
	      CALL NSCLLI(TYP,VOLUME,J0,I1,FVERS,ORG_VS,
	1		FDW,FDWI,FDWJ,
	1	        STH,STHI,STHJ,STHE,STHD,
	1		OHW1,OHW1I,OHW1J,OHW1E,OHW1D,
	1		SCW, SCWI, SCWJ, SCWE, SCWD)
	    END IF
 61	    CONTINUE
	  END DO
	  IF (OUT) SCP(1)=WNFEOF(FCAOUT)		!FORGET OH
	  OHP(1)=0
	END IF
	IF (OUT) THEN
	  IF (.NOT.WNFWR(FCAOUT,SCP(0),SCW(0),SCP(1))) GOTO 22 !WRITE SC
	END IF
C
C GET POLARISATIONS TO DO
C
	DO I=0,3				!SET POL. TO DO
	  IF (IAND(POLCD(I),POL(J1)).NE.0) THEN
	    POLS(I)=0				!SET WANTED
	  ELSE
	    POLS(I)=-1				!SET NOT
	  END IF
	END DO
	DO I=0,OHWI(OHW_NRSTS_I)-1		!CHECK POL.
	  I1=OHW_SET_1+SET_DATYP_1+I*OHWI(OHW_LENT_I) !OFFSET TABLE ENTRY
	  I2=I1/LB_I
	  IF (OHWI(I2).EQ.IXX) THEN
	    IF (POLS(0).GE.0) POLS(0)=1		!SET PRESENT
	  ELSE IF (OHWI(I2).EQ.IXY) THEN
	    IF (POLS(1).GE.0) POLS(1)=1		!SET PRESENT
	  ELSE IF (OHWI(I2).EQ.IYX) THEN
	    IF (POLS(2).GE.0) POLS(2)=1		!SET PRESENT
	  ELSE IF (OHWI(I2).EQ.IYY) THEN
	    IF (POLS(3).GE.0) POLS(3)=1		!SET PRESENT
	  END IF
	END DO
	NPOL=0					!CNT POL
	DO I=0,3
	  IF (POLS(I).GT.0) NPOL=NPOL+1
	END DO
C
C MAKE SET HEADER TEMPLATE
C
	CALL WNGMVZ(STHHDL,STH(0))		!CLEAR
	STHI(STH_LEN_I)=STHHDL			!LENGTH
	STHI(STH_VER_I)=STHHDV			!VERSION
	STHI(STH_BEC_I)=IAND('0000ffff'X,OHWJ(OHW_CONFNR_J)) !BACKEND CODE
	STHJ(STH_DIPC_J)=0			!DIPOLE SETTING
	I1=OHWI(OHW_POLC_I)
	DO I=0,STHTEL-1
	  IF (I.LT.10) THEN			!WEST TEL.
	    STHJ(STH_DIPC_J)=STHJ(STH_DIPC_J)+ISHFT(I1/4,2*I)
	  ELSE					!EAST TEL
	    STHJ(STH_DIPC_J)=STHJ(STH_DIPC_J)+ISHFT(MOD(I1,4),2*I)
	  END IF
	END DO
	STHI(STH_PTS_I)=OHWI(OHW_MPOSN_I)	!POINTING SET #
	STHJ(STH_VNR_J)=OHWJ(OHW_VOLGNR_J)	!VOLG NUMBER
	STHI(STH_PLN_I)=NPOL			!# OF POLARISATIONS
	CALL WNGMV(STH_FIELD_N,OHW(OHW_FIELD_1),STH(STH_FIELD_1)) !FIELD NAME
	STHD(STH_RA_D)=OHWD(OHW_RA0_D)		!APP. RA
	STHD(STH_DEC_D)=OHWD(OHW_DEC0_D)	!APP. DEC
	STHD(STH_RAE_D)=OHWD(OHW_RA1_D)		!RA EPOCH
	STHD(STH_DECE_D)=OHWD(OHW_DEC1_D)	!DEC EPOCH
	STHE(STH_OEP_E)=OHWD(OHW_JUCEN_D)*100.+1900. !OBS. DATE IN JUL. YEARS
	STHE(STH_EPO_E)=1900.+OHWI(OHW_DATE_I)	!EPOCH
	STHI(STH_OBS_I)=OHWI(OHW_SDAY_I)	!DAY
	STHI(STH_OBS_I+1)=OHWI(OHW_DATE_I+1)	!YEAR
	DO I=0,STHTEL-1				!TEL. POSITIONS
	  STHE(STH_RTP_E+I)=(OHWJ(OHW_POST_J+I)-OHWJ(OHW_POST_J))/65536.
	END DO
	STHJ(STH_VELC_J)=OHWI(OHW_VELC_I)+1	!VELOCITY CODE
	STHE(STH_VELR_E)=1000.*OHWE(OHW_VLCTY_E) !REF. VEL.
	STHD(STH_FRQC_D)=OHWD(OHW_FREQ_D)	!REF. FREQUENCY
	STHD(STH_FRQ0_D)=OHWD(OHW_FREQ0_D)	!REST FREQ.
	STHD(STH_FRQV_D)=STHD(STH_FRQC_D)	!CHANNEL FREQ. FOR NOW
	IF (OHWI(OHW_FREQC_I).LT.10 .OR.
	1		STHJ(STH_VELC_J).GT.4 .OR.
	1		BECODE(1:2).EQ.'DC') THEN !CONTINUUM OR UNKNOWN
	  STHJ(STH_VELC_J)=0			!SET CONTINUUM
	  STHE(STH_VELR_E)=0			!REF. VELOCITY
	END IF
	STHE(STH_VEL_E)=STHE(STH_VELR_E)	!VEL. FOR NOW
	STHJ(STH_NFD_J)=FDP(0)			!FD BLOCK
	STHJ(STH_FDP_J)=FDP(1)
	STHJ(STH_NOH_J)=OHP(0)			!OH BLOCK
	STHJ(STH_OHP_J)=OHP(1)
	STHJ(STH_NSC_J)=SCP(0)			!SC BLOCK
	STHJ(STH_SCP_J)=SCP(1)
	CALL NSCCLP(FCAOUT,STH(0),STHE(STH_PHI_E)) !GET PREC. ROTATION ANGLE
	STHE(STH_PHI_E)=STHE(STH_PHI_E)/PI2	!MAKE CIRCLES
	STHD(STH_UTST_D)=1.+SCWD(SCW_CUTST_D)	!UT/ST DAY
	IF (SCWI(SCW_GCODE_I).EQ.0) STHJ(STH_ACORM_J)=1 !SET AMPL. CORR. METHOD
	MJDHA0=OHWD(OHW_JDAY_D)+40000D0-0.5D0	!MJD MIDDLE OBS.
	D0=OHWI(OHW_STIM_I)/360.D0/24D0		!START TIME
	IF (D0.GT.MOD(MJDHA0,1D0))		!PREVIOUS DAY
	1		MJDHA0=MJDHA0-1D0
	MJDHA0=MJDHA0-MOD(MJDHA0,1D0)+D0	!MJD START TIME
	MJDHA0=MJDHA0-(OHWD(OHW_HAST_D)-5D0/3600D0/24D0) !MJD AT HA0
	IF (SPLIT) THEN				!GM, NM
	  I=STHI(STH_PTS_I)			!POINTING SET #
	  IF (OUT) THEN
	    IF (.NOT.WNDLNF(SGPH(1)+SGH_LINKG_1,I,SGH_GROUPN_1,FCAOUT,
	1		SGPH(2),SGNR(2))) THEN	!FIND/CREATE SUB-GROUP
	      CALL WNCTXT(F_TP,'!/Cannot link sub-group')
	      GOTO 600				!NEXT OH
	    END IF
	    CALL WNCTXT(F_TP,'!6C\OH  !4$UJ: !UJ\.!UJ\.!UJ'//
	1		'!32C\RA= !10$DPF15.5 Dec= !10$DAF15.5',
	1		J3,SGNR(0),SGNR(1),SGNR(2),
	1		STHD(STH_RAE_D),STHD(STH_DECE_D))
	  ELSE
	      CALL NSCLLI(TYP,VOLUME,J0,J3,FVERS,ORG_VS,
	1		FDW,FDWI,FDWJ,
	1	        STH,STHI,STHJ,STHE,STHD,
	1		OHW,OHWI,OHWJ,OHWE,OHWD,
	1		SCW,SCWI,SCWJ,SCWE,SCWD)
	  END IF
	END IF
C
C MAKE TMP FILE
C
	NCHT=0					!COUNT SELECTED
	I3=-1					!TEST CHANNEL
	NIFR=0					!NO IFR SEEN
	FWGT=0					!MAX. WEIGHT
	FSPLIT=.TRUE.				!FIRST SPLIT CHANNEL
	DO I=0,OHWI(OHW_NRSTS_I)-1		!ALL INPUT SETS
	  I1=OHW_SET_1+I*OHWI(OHW_LENT_I)	!OFFSET TABLE
	  I2=I1/LB_I
	  I4=I1/LB_J
C
C Select Channel
C
	  THIS_CHAN=.TRUE.			!ASSUME WANTED
	  IF (NCHAN(J1).LT.0) THEN		!ALL CHANNELS SELECTED
	  ELSE
	    DO I5=1,NCHAN(J1)			!SEE IF TO DO
	      IF (OHWI(I2+SET_BANDNR_I).EQ.CHAN(I5,J1)) GOTO 41 !SELECTED CHAN.
	    END DO
	    THIS_CHAN=.FALSE.			!NOT WANTED
	  END IF
 41	  CONTINUE
C
C Select IF (DCB: one per band, select requested bands only, 
C	     DLB/DXB: only one, so select always)
C
	  IF (OUT.AND.OHWI(I2+SET_DATYP_1/LB_I).EQ.
	1	ICHAR('I')*256+ICHAR('F')
	1	.AND. (BECODE(1:3).NE.'DCB'.OR.THIS_CHAN)) THEN
C
C Anything left from previous polarisation set?
C
	      IF (NIFR.GT.0) THEN		!SOME TO WRITE
		IF (FWGT.NE.0) THEN
		  STHE(STH_WFAC_E)=1.-FWGT/255.*4.*OINT*
	1			MTSYS*MTSYS/2.56/2.56
		  FWGT=255./FWGT		!TO MAKE < 256
	        ELSE
		  STHE(STH_WFAC_E)=1.-FWGT
		END IF
		IF (.NOT.NSCLWD(FCAT,ONS,OHAB,NIFR,IFRT,POLS,
	1		BINT,STH(0),MJDHA0,SPLIT,J1,
	1		OHWI(OHW_MSNP_I),OHP1,I3,DWELT,FSPLIT,
	1		RACMOS,FRCMOS,FWGT,OBUF,TMPBUF)) GOTO 600
	      END IF
	      I3=-1				!NO CHANNELS YET
	      ONS(1)=-1				!NEW CHANNEL
	      NIFR=0				!NO IFRS YET
	      FWGT=0				!NO WEIGHT YET
C
C Copy fixed information from the OH/SC-block
C
	     IFHI(IFH_CHAN_I)=OHWI(I2+SET_BANDNR_I)	!BAND NUMBER
	     IFHI(IFH_GCODE_I)=SCWI(SCW_GCODE_I)	!PRINCIPAL CORRECTION
	     DO I5=0,2*STHTEL-1				!VALUES FOR DLB/DBC-0
	       IFHI(IFH_GNCAL_I+I5)=SCW(SCW_GNCAL_1+I5)
	       IFHE(IFH_TSYSI_E+I5)=SCWE(SCW_TSYSI_E+I5)
	       IFHE(IFH_RGAINI_E+I5)=SCWE(SCW_RGAINI_E+I5)
	       IFHE(IFH_TNOISEI_E+I5)=SCWE(SCW_TNOISI_E+I5)
	     END DO
	     IF (BECODE(1:3).EQ.'DCB'.AND.
	1	IFHI(IFH_CHAN_I).GE.1.AND.
	1	IFHI(IFH_CHAN_I).LE.8) THEN		!DCB 1..8
	       I6=SCW_BCOR_1+(IFHI(IFH_CHAN_I)-1)*BCORHDL !OFFSET TABLE
	       DO I5=0,2*STHTEL-1
	         IFHI(IFH_GNCAL_I+I5)=SCWI(I6/LB_I+BCOR_GNCL_I+I5)
	         IFHE(IFH_TSYSI_E+I5)=SCWE(I6/LB_E+BCOR_TSYS_E+I5)
	         IFHE(IFH_RGAINI_E+I5)=SCWE(I6/LB_E+BCOR_GAIN_E+I5)
	         IFHE(IFH_TNOISEI_E+I5)=SCWE(I6/LB_E+BCOR_NOIS_E+I5)
	       END DO
	     END IF
C
C Read and possibly write IF data, if written: save pointer in SCH
C
	     IF (.NOT.NSCLIF(IMCA,OHWJ(I4+SET_NSH_J),IFHJ,IFHE,STHJ,
	1		  VS,FVERS,BECODE,SFREQ,BINT,
	1		  TPBUF,DBUF,OBUF)) GOTO 600	!READ AND WRITE IF SET
	     IF (IFSETS.GT.0) 
	1	CALL WNCTXT(F_TP,'!7C\Ch. !3$UI: IF data written',
	1	               IFHI(IFH_CHAN_I))
	  END IF
C
C If channel not selected: do not load polarisation sets
C
	  IF (.NOT.THIS_CHAN) GOTO 40
C
C Select Polarisation
C
	  IF ((OHWI(I2+SET_DATYP_1/LB_I).EQ.IXX .AND. POLS(0).GT.0) .OR.
	1	(OHWI(I2+SET_DATYP_1/LB_I).EQ.IXY .AND. POLS(1).GT.0) .OR.
	1	(OHWI(I2+SET_DATYP_1/LB_I).EQ.IYX .AND. POLS(2).GT.0) .OR.
	1	(OHWI(I2+SET_DATYP_1/LB_I).EQ.IYY .AND. POLS(3).GT.0)) THEN
	    IF (I3.EQ.OHWI(I2+SET_BANDNR_I)) THEN !MORE POL.
	      IF (.NOT.NSCLRD(IMCA,OHWJ(I4+SET_NSH_J),FCAT,
	1		VS,FVERS,BECODE,SFREQ,BINT,INTOFF(J1),ONS,
	1		OHAB,NIFR,IFRT,STHJ,STHE,STHD,DWELT,SPLIT,
	1		TSYS,FWGT,IFHJ,IFHE,TPBUF,OBUF,
	1		DBUF,TMPBUF)) GOTO 600 !READ A CHAN.
	    ELSE				!NEW CHANNEL
	      IF (NIFR.GT.0) THEN		!SOME TO WRITE
		IF (FWGT.NE.0) THEN
		  STHE(STH_WFAC_E)=1.-FWGT/255.*4.*OINT*
	1			MTSYS*MTSYS/2.56/2.56
		  FWGT=255./FWGT		!TO MAKE < 256
	        ELSE
		  STHE(STH_WFAC_E)=1.-FWGT
		END IF
		IF (.NOT.NSCLWD(FCAT,ONS,OHAB,NIFR,IFRT,POLS,
	1		BINT,STH(0),MJDHA0,SPLIT,J1,
	1		OHWI(OHW_MSNP_I),OHP1,I3,DWELT,FSPLIT,
	1		RACMOS,FRCMOS,FWGT,OBUF,TMPBUF)) GOTO 600
	      END IF
	      I3=OHWI(I2+SET_BANDNR_I)		!NEW TEST VALUE
	      ONS(1)=-1				!NEW CHANNEL
	      NIFR=0				!NO IFRS YET
	      FWGT=0				!NO WEIGHT YET
	      IF (.NOT.NSCLRD(IMCA,OHWJ(I4+SET_NSH_J),FCAT,
	1		VS,FVERS,BECODE,SFREQ,BINT,INTOFF(J1),ONS,
	1		OHAB,NIFR,IFRT,STHJ,STHE,STHD,DWELT,SPLIT,
	1		TSYS,FWGT,IFHJ,IFHE,TPBUF,OBUF,
	1		DBUF,TMPBUF)) GOTO 600 !READ A CHAN.
	    END IF
	  END IF
 40	  CONTINUE
	END DO
	IF (NIFR.GT.0) THEN			!SOME TO WRITE
	  IF (FWGT.NE.0) THEN
	    STHE(STH_WFAC_E)=1.-FWGT/255.*4.*OINT*
	1			MTSYS*MTSYS/2.56/2.56
	    FWGT=255./FWGT			!TO MAKE < 256
	  ELSE
	    STHE(STH_WFAC_E)=1.-FWGT
	  END IF
	  IF (.NOT.NSCLWD(FCAT,ONS,OHAB,NIFR,IFRT,POLS,
	1		BINT,STH(0),MJDHA0,SPLIT,J1,
	1		OHWI(OHW_MSNP_I),OHP1,I3,DWELT,FSPLIT,
	1		RACMOS,FRCMOS,FWGT,OBUF,TMPBUF)) GOTO 600
	END IF
C
C FINISH OH
C
 600	CONTINUE
	IF (VS.GE.59 .AND. OHWJ(OHW_NOHN_J).GT.0) THEN !MORE OH
	  J2=OHWJ(OHW_NOHN_J)*FDWI(FDW_LRCRD_I)	!POINTER NEXT OH
	  IF (NPTC(J1).LT.0) THEN		!DO ALL OH'S
	    GOTO 50
	  ELSE
	    DO I=1,NPTC(J1)
	      IF (IPTC(I,J1).GT.J3) GOTO 50	!STILL MORE TO DO
	    END DO
	  END IF
	END IF
C
C FINISH LABEL
C
 700	CONTINUE
	CALL WNFCL(IMCA)			!CLOSE LABEL
	GOTO 10					!NEXT LABEL
C
C FINISH JOB
C
 800	CONTINUE
	GOTO 30					!NEXT JOB
C
C READY
C
 900	CALL WNFCL(IMCA)			!CLOSE INPUT
	CALL WNFDMO(IMCA)			!DISMOUNT INPUT
	CALL WNFCL(FCAT)			!CLOSE/DELETE TMP FILE
	IF (OUT) THEN
	   CALL NSCPFH(F_TP,FCAOUT)		!SHOW FILE HEADER
	   CALL NSCPFL(F_TP,FCAOUT,NODOUT,.FALSE.)	!SHOW LAYOUT
	   CALL WNFCL(FCAOUT)			!CLOSE OUTPUT
	END IF
C
	RETURN					!READY
C
C
	END