C+ NSCPFL.FOR
C  WNB 900810
C
C  Revisions:
C	HjV 930311	Change some text
C	CMV 931220	Add overview option (basically taken 
C			  from NCOOVV but without the pointers)
C	WNB 931222	Correct overview heading
C	CMV 940314	Add OVERVIEW for overview-level
C	CMV 940317	Rather not ask OVERVIEW if LAYOUT requested
C	CMV 940427	Correct bug in Group overview (why did nobody notice?)
C	CMV 940601	Change format for printing CPOL
C	JPH 940824	Improve text of layout list.
C			Remove OVERVIEW default. (Now in NCOMM.PEF)
C	CMV 950122	Mention layout values are maxima.
C	JPH 960126	Add ALTOBS
C	JPH 960518	Widen field for abs. sector number from 3 to 5
C	JPH 960612	Bug fixes: Right-justify SETNAM and shift right by 2 pos C	JPH 960614	Format fine-tuning to maximise space for Field
C
	SUBROUTINE NSCPFL(PTYPE,INFCA,NODIN,OVV)
C
C  Show SCN file layout
C
C  Result:
C
C	CALL NSCPFL ( PTYPE_J:I, INFCA_J:I, NODIN_C(*):I, OVV_L:I)
C					Show on output PTYPE the file layout
C					of file INFCA (if OVV is .false.) or
C					give an overview (if OVV is .true.)
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
	INCLUDE 'OHW_O_DEF'		!OH BLOCK
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (f_p, f_t ETC)
	INTEGER INFCA			!FILE DESCRIPTOR
	CHARACTER NODIN*(*)		!NODE NAME
	LOGICAL OVV			!OVERVIEW (else LAYOUT)?
C
C  Function references:
C
	LOGICAL WNDPAR			!Get input
	LOGICAL WNFRD			!READ DATA
	INTEGER WNFEOF			!GET FILE POINTER
	INTEGER WNCALN			!Length of string
	LOGICAL NSCSTG			!GET DATASET
	CHARACTER*32 WNTTSG		!MAKE SET NAME
C
C  Data declarations:
C
	CHARACTER*10 LVL		!OVERVIEW level
	INTEGER SET(0:7,0:1)		!ALL SETS
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER STHP			!SET POINTER
	INTEGER	PSNAM(0:7)		!previous sector name
	CHARACTER*19 CSNAM		!ASCII sector name
	REAL HAE			!end HA
	INTEGER CCHN,CFLD,CSEC,CPOL	!Channel, field, sector and pol. count
	INTEGER FCHN,FFLD,FSEC		!Count for first one only
	CHARACTER*80 LINE		!Buffer for Group Overview
	CHARACTER*12 CATEG,OBSDATE	!Items from OH block
	INTEGER*2 PROJECT		! idem
C
 	BYTE GFH(0:GFHHDL-1)		!FILE HEADER
	BYTE SGH(0:SGHHDL-1,0:7)	!SUB-GROUP HEADER
	  INTEGER SGHJ(0:SGHHDL/4-1,0:7)
	  EQUIVALENCE(SGH,SGHJ)
C
	BYTE STH(0:STHHDL-1)	!SET HEADER
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  INTEGER  STHJ(0:STHHDL/4-1)
	  REAL STHE(0:STHHDL/4-1)
	  DOUBLE PRECISION STHD(0:STHHDL/8-1)
	  EQUIVALENCE 	(STH,STHI,STHJ,STHE,STHD)
	BYTE OHW(0:OHWHDL-1)			!OH
	  INTEGER*2 OHWI(0:OHWHDL/2-1)
	  INTEGER   OHWJ(0:OHWHDL/4-1)
	  REAL OHWE(0:OHWHDL/4-1)
	  REAL*8 OHWD(0:OHWHDL/8-1)
	  EQUIVALENCE (OHW,OHWI,OHWJ,OHWE,OHWD)
C-
C
C For overview, ask level; default to Observations/Groups
C
	IF (OVV) THEN
	   IF (.NOT.WNDPAR('OVERVIEW',LVL,LEN(LVL),J)) THEN
	      LVL='O'
	   ELSE IF (E_C.EQ.DWC_NULLVALUE.OR.
	1	    E_C.EQ.DWC_WILDCARD) THEN
	      LVL='O'
	   END IF
	END IF
C
C INIT
C
	DO I=0,7				!SET SET *.*.*.*.*.*.*
	  DO I1=0,1
	    SET(I,I1)=0
	  END DO
	  SET(I,1)=-1				!1 LINE
	END DO
	SET(0,0)=1				!1 LINE
C
C SHOW NAME AND SIZE
C
	IF (NODIN.EQ.' ') THEN
          IF (.NOT.WNFRD(INFCA,GFHHDL,GFH,0)) THEN
	     CALL WNCTXT(PTYPE,
	1	'!/File description of SCN node (!UJ bytes):!/',
	1	WNFEOF(INFCA))
	  ELSE
	     CALL WNCTXT(PTYPE,
	1	'!/File description of SCN node !AD\ (!UJ bytes):!/',
	1       GFH(GFH_NAME_1),GFH_NAME_N,WNFEOF(INFCA))
	  END IF
	ELSE
	  CALL WNCTXT(PTYPE,
	1	'!/File layout of SCN node !AS (!UJ bytes):!/',
	1	NODIN,WNFEOF(INFCA))
	END IF
C
C SHOW LAYOUT
C
	IF (.NOT.OVV) THEN
	  DO WHILE(NSCSTG(INFCA,SET,STH,STHP,SNAM))
	    DO I=0,7				!CLEAR LEVEL COUNT
	      SGHJ(SGH_HEADH_J-SGH_LINKG_J,I)=0
	    END DO
	    I=SET(1,0)-1			!CURRENT LEVEL
	    IF (.NOT.WNFRD(INFCA,SGHHDL-SGH_LINKG_1,SGH(0,I),
	1	SET(3,0)+SGH_LINKG_1)) THEN 	!READ TOP
 10	      CONTINUE
	      CALL WNCTXT(PTYPE,'Error reading file')
	      RETURN
	    END IF
	    DO WHILE(I.GT.0)			!READ LEVELS
	      I=I-1
	      IF (.NOT.WNFRD(INFCA,SGHHDL-SGH_LINKG_1,SGH(0,I),
	1	SGHJ(SGH_HEADH_J-SGH_LINKG_J,I+1))) GOTO 10
	    END DO
	    SNAM(2)=-1				!ONLY grp and obs LEVELS
	    CALL WNCTXT(PTYPE,'!AS!12C: !4$UJ fields * !4$UJ '//
	1		'channels * !4$UJ sectors for !AD',
	1		WNTTSG(SNAM(0),0),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,1),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,2),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,3),
	1		STH(STH_FIELD_1),STH_FIELD_N)
	    IF (.NOT.WNFRD(INFCA,SGHHDL,SGH(0,0),SET(3,0))) GOTO 10 
						!READ CURRENT
	    DO WHILE (SET(1,0).GT.2)		!DECREASE LEVEL
	      SET(1,0)=SET(1,0)-1		!DECREASE LEVEL
	      SET(3,0)=SGHJ(SGH_HEADH_J,0)-SGH_LINKG_1+SGH_LINK_1 !LOWER HEADER
	      IF (.NOT.WNFRD(INFCA,SGHHDL,SGH(0,0),SET(3,0))) GOTO 10 !CURRENT
	      SET(4,0)=SGHJ(SGH_HEADH_J,0)	!NEW LOWER HEAD
	    END DO
	  END DO
	  CALL WNCTXT(PTYPE,'NOTE: Values are maxima, '//
	1	'65 channels means highest channel is 64')
	  CALL WNCTXT(PTYPE,' ')
	ELSE
C
C Else print summary of contents
C  Heading first
C
	  IF (LVL(1:1).EQ.'O') THEN		! OBS overview
	    CALL WNCTXT (F_TP,			!print heading
	1	'grp.obs Obs SP/TP'//		!SNAM, TYPE
	1	' !-12$AS'//			!field
	1	' !-7$AS'//			!volgnr
	1	' !6$AS'//			!Project
	1	'  !11$AS'//			!Date
	1	'  Chn  Fld Sect Pol!/',
	1	'Field','Volgnr','Proj.','Date/Time')
C
	  ELSE					! sector overview
	    CALL WNCTXT (F_TP,			!print heading
	1	'grp.obs.fld.chn.sec!5$AS'//	!SNAM, SETN
	1	' !-11$AS'//			!field
	1	' !7$AS'//			!volgnr
	1	' !7$AS!7$AS'//			!FREQ, BAND
	1	' !5$AS!5$AS'//			!HAB, HAE
	1	'!4$AS'//			!SCN
	1	' !-4$AS'//			!NIFR,PLN
	1	'!/',
	1	'(#)',
	1	'Field',
	1	'Volgnr',
	1	'FREQ', 'BAND',
	1	'HAB','HAE',
	1	'SCN',
	1	'IF P')
	  END IF
C
	  DO I=0,7				!No sets printed yet
	     PSNAM(I)=-999
	  END DO
C
C  Loop over all sectors
C
	  DO WHILE (NSCSTG(INFCA,SET,STH,STHP,SNAM))!all sets
	    HAE= STHE(STH_HAB_E) + (STHJ(STH_SCN_J)-1)*STHE(STH_HAI_E)
	    IF (LVL(1:1).NE.'O') THEN
C
C  Field/channel/sector list: lots of output
C
	      CSNAM=WNTTSG(SNAM,3)		!get "." set name 
	      DO I=0,7
	        IF (SNAM(I).NE.PSNAM(I)) GOTO 20 !compare against previous
	      END DO
 20	      CONTINUE
	      IF (I.GT.0) CSNAM(1:4*I)=' '	!blank out components that have
C						! not changed
C
	      IF (SNAM(0).NE.PSNAM(0) .OR. 	! new group
	1	  ( LVL(1:1).EQ.'A' .AND.
	1	  	(SNAM(1).NE.PSNAM(1) )).OR.! ALTOBS with new obs
	1	  ( LVL(1:1).EQ.'F' .AND.	! FIELD level with new field
	1	  	(SNAM(1).NE.PSNAM(1).OR.
	1		 SNAM(2).NE.PSNAM(2) )).OR.
	1	  ( LVL(1:1).EQ.'C' .AND.	! CHANNEL level with new channel
	1	    	(SNAM(3).NE.PSNAM(3).OR.
	1		 SNAM(2).NE.PSNAM(2).OR.
	1	  	 SNAM(1).NE.PSNAM(1) )).OR.
	1	  LVL(1:1).EQ.'S') THEN		! SECTOR level
	        CALL WNCTXT (F_TP,
	1	  '!19$AS!5$UJ5'//		!SNAM, SETN
	1	  ' !-11$AD'//			!field
	1	  ' !7$UJ7'//			!volgnr
	1	  ' !7$D7.2!7$E7.3'//		!FREQ, BAND
	1	  ' !5$EAF5.1!5$EAF5.1'//	!HAB, HAE
	1	  '!4$UJ4'//			!SCN
	1	  ' !2$UJ2 !1$UI1',		!NIFR,PLN
	1	  CSNAM,STHJ(STH_SETN_J),
	1	  STH(STH_FIELD_1),STH_FIELD_N,
	1	  STHJ(STH_VNR_J),
	1	  STHD(STH_FRQ_D),STHE(STH_BAND_E),
	1	  STHE(STH_HAB_E),HAE,
	1	  STHJ(STH_SCN_J),
	1	  STHJ(STH_NIFR_J),STHI(STH_PLN_I))
	      END IF
C
C  Group list: only after all channels/pointing centra counted
C
	    ELSE
	      IF (SNAM(0).NE.PSNAM(0).OR.SNAM(1).NE.PSNAM(1)) THEN
	         IF (PSNAM(0).NE.-999) CALL WNCTXT(F_TP,
	1		'!AS !4$UJ4 !4$UJ4 !4$UJ4 !3$UJ3',
	1	  	LINE(:57),CCHN,CFLD,CSEC,CPOL)
C
C  Prepare for next observation
C
	         CSNAM=WNTTSG(SNAM,3)		!get "." set name 
	         CCHN=1				!Only this channel so far
	         CFLD=1				!Only this field so far
	         CSEC=1				!Only this sector so far
	         CPOL=STHI(STH_PLN_I)		!Number of pol.s
		 FCHN=SNAM(3)			!Count for first sector
		 FFLD=SNAM(2)			!  only
		 FSEC=SNAM(4)
		 CATEG='??? ??/??'		!Reset type
	         PROJECT= -1			!Reset project
	         OBSDATE='??????/????'		!Observation date
		 IF (STHJ(STH_OHP_J).NE.0) THEN	!Read New OH if any
	           IF (WNFRD(INFCA,STHJ(STH_NOH_J),OHW,
	1	 	   STHJ(STH_OHP_J))) THEN
		      PROJECT=OHWI(OHW_PROJECT_I)
		      IF (OHW(OHW_CATEG_1).EQ.ICHAR('I')) THEN
	                CALL WNCTXS(CATEG,'Cal !AF/!AF',
	1	           OHW(OHW_SPEFU_1),OHW_SPEFU_N,  !Special functions
	1	           OHW(OHW_TYPE_1), OHW_TYPE_N)   !Obs. type-code
		      ELSE
	                CALL WNCTXS(CATEG,'Src !AF/!AF',
	1	           OHW(OHW_SPEFU_1),OHW_SPEFU_N,  !Special functions
	1	           OHW(OHW_TYPE_1), OHW_TYPE_N)   !Obs. type-code
		      END IF
		      CALL WNCTXS(OBSDATE,
	1		'!2$ZI!2$ZI!2$ZI/!2$ZI!2$ZI',
	1		OHWI(OHW_DATE_I+1),OHWI(OHW_DATE_I+2),
	1		OHWI(OHW_DATE_I+3),
	1		OHWI(OHW_DATE_I+4),OHWI(OHW_DATE_I+5))
	           END IF
	         END IF
	         CALL WNCTXS(LINE,
	1	      '!7$AS !9$AS'//		!SNAM, TYPE
	1	      ' !-12$AD'//		!field
	1	      ' !7$UJ'//		!volgnr
	1	      ' !6$UI'//		!Project
	1	      ' !11$AS',		!Date
	1	      CSNAM(1:7),CATEG(1:9),
	1	      STH(STH_FIELD_1),STH_FIELD_N,
	1	      STHJ(STH_VNR_J),PROJECT,OBSDATE(1:11))
C
C  If same observation: just count
C
	      ELSE
	        IF (SNAM(3).EQ.FCHN.AND.SNAM(4).EQ.FSEC.AND.
	1	    SNAM(2).NE.FFLD) CFLD=CFLD+1
	        IF (SNAM(3).NE.FCHN.AND.SNAM(4).EQ.FSEC.AND.
	1	    SNAM(2).EQ.FFLD) CCHN=CCHN+1
	        IF (SNAM(3).EQ.FCHN.AND.SNAM(4).NE.FSEC.AND.
	1	    SNAM(2).EQ.FFLD) CSEC=CSEC+1
	      END IF
	    END IF
C
	    DO I=0,7
	       PSNAM(I)=SNAM(I)
	    END DO
	  END DO
C
C  Print last group
C
	  IF (LVL(1:1).EQ.'O') THEN
	     IF (PSNAM(0).NE.-999) CALL WNCTXT(F_TP,
	1		'!AS !4$UJ4 !4$UJ4 !4$UJ4 !3$UJ3',
	1	  	LINE(:57),CCHN,CFLD,CSEC,CPOL)
	  END IF
	  CALL WNCTXT(PTYPE,' ')
C
	END IF
C
	RETURN
C
C
	END
