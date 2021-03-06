C+ WNPSET.FOR
C  WNB 911121
C
C  Revisions:
C
C  Set parameter routines
C
	LOGICAL FUNCTION WQSPLI(IX)
C
C  Result:
C
C	WQSPLI( IX_J:I)		Set polyline index
C	WQSPMI( IX_J:I)		Set polymark index
C	WQSFAI( IX_J:I)		Set fill area index
C	WQSTXI( IX_J:I)		Set text index
C	WQSTXH( FR_E:I)		Set text height
C	WQSTXU( UP_E(2):I)	Set text direction
C	WQSTXX( FR_E:I)		Set text expansion
C	WQSTXP( IX_J:I)		Set text path
C	WQSTXS( FR_E:I)		Set text spacing
C	WQSPID( IX_J:I)		Set pick id
C	WQSCLP( CLP_L:I)	Set clip indicator
C	WQSPSZ( UP_E(2):I)	Set pattern size
C	WQSPRP( UP_E(2):I)	Set pattern reference point
C	WQSPLR( ID_J:I, NIX_J:I, TP_J:I, SC_E:I) Polyline represent.
C	WQSPLR_IC( ID_J:I, NIX_J:I, TP_J:I, SC_E:I, COL_J:I) Polyline colour
C	WQSPMR( ID_J:I, NIX_J:I, TP_J:I, SC_E:I) Polymark represent.
C	WQSPMR_IC( ID_J:I, NIX_J:I, TP_J:I, SC_E:I, COL_J:I) Polymark colour
C	WQSTXR( ID_J:I, NIX_J:I, TP_J:I, PR_J:I) Text represent.
C	WQSTXR_IC( ID_J:I, NIX_J:I, TP_J:I, PR_J, COL_J:I_J:I) Text colour
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE 'WQD_O_DEF'		!DEVICE AREA
C
C  Entry points:
C
	LOGICAL WQSPMI			!SET POLYMARK INDEX
	LOGICAL WQSFAI			!SET FILL AREA INDEX
	LOGICAL WQSTXI			!SET TEXT INDEX
	LOGICAL WQSTXH			!SET TEXT HEIGHT
	LOGICAL WQSTXU			!SET TEXT DIRECTION
	LOGICAL WQSTXX			!SET TEXT EXPANSION
	LOGICAL WQSTXP			!SET TEXT PATH
	LOGICAL WQSTXS			!SET TEXT SPACING
	LOGICAL WQSPID			!SET PICK ID
	LOGICAL WQSCLP			!SET CLIP INDICATOR
	LOGICAL WQSPSZ			!SET PATTERN SIZE
	LOGICAL WQSPRP			!SET PATTERN REFERENCE POINT
	LOGICAL WQSPLR			!SET POLYLINE REPRESENTATION
	LOGICAL WQSPLR_IC		!... WITH COLOUR
	LOGICAL WQSPMR			!SET POLYMARK REPRESENTATION
	LOGICAL WQSPMR_IC		!... WITH COLOUR
	LOGICAL WQSTXR			!SET TEXT REPRESENTATION
	LOGICAL WQSTXR_IC		!... WITH COLOUR
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER IX			!POLYLINE INDEX
	REAL FR				!FACTOR
	REAL UP(2)			!TEXT DIRECTION
	LOGICAL CLP			!CLIP INDICATOR
	INTEGER ID			!DEVICE ID
	INTEGER NIX			!INDEX
	INTEGER TP			!LINE TYPE
	REAL SC				!LINE SCALE
	INTEGER PR			!PATTERN
	INTEGER COL			!COLOUR
C
C  Function references:
C
	LOGICAL WNPCID			!CHECK DEVICE ID
	LOGICAL WQOPEN			!OPEN SYSTEM
C
C  Data declarations:
C
	LOGICAL LCOL			!INCLUDE COLOUR
C-
C
C SPLI
C
	WQSPLI=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSPLI=WQOPEN()		!OPEN SYSTEM
	IF (WQSPLI) THEN
	  IF (IX.LE.0) THEN
	    E_C=80
	    WQSPLI=.FALSE.
	  ELSE
	    WQG_CPOLLIX=IX				!SET INDEX
	  END IF
	END IF
C
	RETURN
C
C SPMI
C
	ENTRY WQSPMI(IX)
C
	WQSPMI=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSPMI=WQOPEN()		!OPEN SYSTEM
	IF (WQSPMI) THEN
	  IF (IX.LE.0) THEN
	    E_C=80
	    WQSPMI=.FALSE.
	  ELSE
	    WQG_CPOLMIX=IX				!SET INDEX
	  END IF
	END IF
C
	RETURN
C
C SFAI
C
	ENTRY WQSFAI(IX)
C
	WQSFAI=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSFAI=WQOPEN()		!OPEN SYSTEM
	IF (WQSFAI) THEN
	  IF (IX.LE.0) THEN
	    E_C=91
	    WQSFAI=.FALSE.
	  ELSE
	    WQG_CFILAIX=IX				!SET INDEX
	  END IF
	END IF
C
	RETURN
C
C STXI
C
	ENTRY WQSTXI(IX)
C
	WQSTXI=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXI=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXI) THEN
	  IF (IX.LE.0) THEN
	    E_C=86
	    WQSTXI=.FALSE.
	  ELSE
	    WQG_CTXTIX=IX				!SET INDEX
	  END IF
	END IF
C
	RETURN
C
C STXH
C
	ENTRY WQSTXH(FR)
C
	WQSTXH=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXH=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXH) THEN
	  IF (FR.LE.0) THEN
	    E_C=60
	    WQSTXH=.FALSE.
	  ELSE
	    WQG_CTXHT=FR				!SET HEIGHT
	  END IF
	END IF
C
	RETURN
C
C STXU
C
	ENTRY WQSTXU(UP)
C
	WQSTXU=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXU=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXU) THEN
	  IF (UP(1).EQ.0 .AND. UP(2).EQ.0) THEN
	    E_C=61
	    WQSTXU=.FALSE.
	  ELSE
	    WQG_CTXUP(0)=UP(1)				!SET DIRECTION
	    WQG_CTXUP(1)=UP(2)
	    R0=ATAN2(UP(1),UP(2))
	    WQG_CTXCS(0)=SIN(R0)
	    WQG_CTXCS(1)=COS(R0)
	  END IF
	END IF
C
	RETURN
C
C STXX
C
	ENTRY WQSTXX(FR)
C
	WQSTXX=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXX=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXX) THEN
	  IF (FR.LE.0) THEN
	    E_C=62
	    WQSTXX=.FALSE.
	  ELSE
	    WQG_CTXXP=FR				!SET EXPANSION
	  END IF
	END IF
C
	RETURN
C
C STXP
C
	ENTRY WQSTXP(IX)
C
	WQSTXP=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXP=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXP) THEN
	  WQG_CTXPA=IAND(3,IX)				!SET PATH
	END IF
C
	RETURN
C
C STXS
C
	ENTRY WQSTXS(FR)
C
	WQSTXS=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSTXS=WQOPEN()		!OPEN SYSTEM
	IF (WQSTXS) THEN
	  WQG_CTXSP=FR					!SET SPACING
	END IF
C
	RETURN
C
C SPID
C
	ENTRY WQSPID(IX)
C
	WQSPID=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSPID=WQOPEN()		!OPEN SYSTEM
	IF (WQSPID) THEN
	  WQG_CPID=IX					!SET INDEX
	END IF
C
	RETURN
C
C SCLP
C
	ENTRY WQSCLP(CLP)
C
	WQSCLP=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSCLP=WQOPEN()		!OPEN SYSTEM
	IF (WQSCLP) THEN
	  IF (CLP) THEN
	    WQG_CLIP=1
	  ELSE
	    WQG_CLIP=0
	  END IF
	END IF
C
	RETURN
C
C SPSZ
C
	ENTRY WQSPSZ(UP)
C
	WQSPSZ=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSPSZ=WQOPEN()		!OPEN SYSTEM
	IF (WQSPSZ) THEN
	  IF (UP(1).LE.0 .OR. UP(2).LE.0) THEN
	    E_C=63
	    WQSPSZ=.FALSE.
	  ELSE
	    WQG_CPTSZ(0)=UP(1)				!SET PATTERN SIZE
	    WQG_CPTSZ(1)=UP(2)
	  END IF
	END IF
C
	RETURN
C
C SPRP
C
	ENTRY WQSPRP(UP)
C
	WQSPRP=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LE.0) WQSPRP=WQOPEN()		!OPEN SYSTEM
	IF (WQSPRP) THEN
	  WQG_CPTRP(0)=UP(1)				!SET PATTERN REF. PT.
	  WQG_CPTRP(1)=UP(2)
	END IF
C
	RETURN
C
C SPLR
C
	ENTRY WQSPLR(ID,NIX,TP,SC)
C
	LCOL=.FALSE.
	GOTO 10
C
C SPLR_IC
C
	ENTRY WQSPLR_IC(ID,NIX,TP,SC,COL)
C
	LCOL=.TRUE.
	GOTO 10
C
 10	CONTINUE
	WQSPLR=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!ILLEGAL STATE
	  WQSPLR=.FALSE.
	END IF
	IF (WQSPLR) THEN
	  IF (.NOT.WNPCID(ID)) THEN
	    E_C=20					!WRONG ID
	    WQSPLR=.FALSE.
	  END IF
	END IF
	J0=(ID-A_OB)/LB_J				!POINTER
	IF (WQSPLR) THEN
	  IF (IAND(4,A_J(J0+WQD_TYP_J)).NE.0) THEN	!DISS
	    E_C=35
	    WQSPLR=.FALSE.
	  END IF
	END IF
	IF (WQSPLR) THEN
	  IF (IAND(8,A_J(J0+WQD_TYP_J)).NE.0 .AND.
	1	IAND(2,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!META INPUT
	    E_C=32
	    WQSPLR=.FALSE.
	  END IF
	END IF
	IF (WQSPLR) THEN
	  IF (IAND(1,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!INPUT
	    E_C=34
	    WQSPLR=.FALSE.
	  END IF
	END IF
	IF (WQSPLR) THEN
	  IF (NIX.LE.0 .OR. NIX.GT.A_J(J0+WQD_NPLIX_J)+1) THEN
	    E_C=80					!ILLEGAL INDEX
	    WQSPLR=.FALSE.
	  END IF
	END IF
	J=NIX-1						!TABLE POINTER
	IF (WQSPLR) THEN
	  IF (TP.LE.0 .OR. TP.GT.A_J(J0+WQD_NPLT_J)) THEN
	    E_C=82					!ILLEGAL TYPE
	    WQSPLR=.FALSE.
	  ELSE
	    A_E(J0+WQD_PLIX_E+3*J+0)=TP			!LINE TYPE
	  END IF
	END IF
	IF (WQSPLR) THEN
	  A_E(J0+WQD_PLIX_E+3*J+1)=SC			!LINE SCALE
	END IF
	IF (WQSPLR .AND. LCOL) THEN
	  IF (COL.LT.0 .OR. COL.GT.A_J(J0+WQD_NCLIX_J)+1) THEN
	    E_C=96					!ILLEGAL COLOUR
	    WQSPLR=.FALSE.
	  ELSE
	    A_E(J0+WQD_PLIX_E+3*J+2)=COL		!LINE COLOUR
	  END IF
	END IF
C
	RETURN
C
C SPMR
C
	ENTRY WQSPMR(ID,NIX,TP,SC)
C
	LCOL=.FALSE.
	GOTO 11
C
C SPMR_IC
C
	ENTRY WQSPMR_IC(ID,NIX,TP,SC,COL)
C
	LCOL=.TRUE.
	GOTO 11
C
 11	CONTINUE
	WQSPMR=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!ILLEGAL STATE
	  WQSPMR=.FALSE.
	END IF
	IF (WQSPMR) THEN
	  IF (.NOT.WNPCID(ID)) THEN
	    E_C=20					!WRONG ID
	    WQSPMR=.FALSE.
	  END IF
	END IF
	J0=(ID-A_OB)/LB_J				!POINTER
	IF (WQSPMR) THEN
	  IF (IAND(4,A_J(J0+WQD_TYP_J)).NE.0) THEN	!DISS
	    E_C=35
	    WQSPMR=.FALSE.
	  END IF
	END IF
	IF (WQSPMR) THEN
	  IF (IAND(8,A_J(J0+WQD_TYP_J)).NE.0 .AND.
	1	IAND(2,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!META INPUT
	    E_C=32
	    WQSPMR=.FALSE.
	  END IF
	END IF
	IF (WQSPMR) THEN
	  IF (IAND(1,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!INPUT
	    E_C=34
	    WQSPMR=.FALSE.
	  END IF
	END IF
	IF (WQSPMR) THEN
	  IF (NIX.LE.0 .OR. NIX.GT.A_J(J0+WQD_NPMIX_J)+1) THEN
	    E_C=83					!ILLEGAL INDEX
	    WQSPMR=.FALSE.
	  END IF
	END IF
	J=NIX-1						!TABLE POINTER
	IF (WQSPMR) THEN
	  IF (TP.LE.0 .OR. TP.GT.A_J(J0+WQD_NPMT_J)) THEN
	    E_C=85					!ILLEGAL TYPE
	    WQSPMR=.FALSE.
	  ELSE
	    A_E(J0+WQD_PMIX_E+3*J+0)=TP			!LINE TYPE
	  END IF
	END IF
	IF (WQSPMR) THEN
	  A_E(J0+WQD_PMIX_E+3*J+1)=SC			!LINE SCALE
	END IF
	IF (WQSPMR .AND. LCOL) THEN
	  IF (COL.LT.0 .OR. COL.GT.A_J(J0+WQD_NCLIX_J)+1) THEN
	    E_C=96					!ILLEGAL COLOUR
	    WQSPMR=.FALSE.
	  ELSE
	    A_E(J0+WQD_PMIX_E+3*J+2)=COL		!LINE COLOUR
	  END IF
	END IF
C
	RETURN
C
C STXR
C
	ENTRY WQSTXR(ID,NIX,TP,PR)
C
	LCOL=.FALSE.
	GOTO 12
C
C STXR_IC
C
	ENTRY WQSTXR_IC(ID,NIX,TP,PR,COL)
C
	LCOL=.TRUE.
	GOTO 12
C
 12	CONTINUE
	WQSTXR=.TRUE.					!ASSUME OK
	IF (WQG_STATE.LT.2) THEN
	  E_C=7						!ILLEGAL STATE
	  WQSTXR=.FALSE.
	END IF
	IF (WQSTXR) THEN
	  IF (.NOT.WNPCID(ID)) THEN
	    E_C=20					!WRONG ID
	    WQSTXR=.FALSE.
	  END IF
	END IF
	J0=(ID-A_OB)/LB_J				!POINTER
	IF (WQSTXR) THEN
	  IF (IAND(4,A_J(J0+WQD_TYP_J)).NE.0) THEN	!DISS
	    E_C=35
	    WQSTXR=.FALSE.
	  END IF
	END IF
	IF (WQSTXR) THEN
	  IF (IAND(8,A_J(J0+WQD_TYP_J)).NE.0 .AND.
	1	IAND(2,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!META INPUT
	    E_C=32
	    WQSTXR=.FALSE.
	  END IF
	END IF
	IF (WQSTXR) THEN
	  IF (IAND(1,A_J(J0+WQD_TYP_J)).EQ.0) THEN	!INPUT
	    E_C=34
	    WQSTXR=.FALSE.
	  END IF
	END IF
	IF (WQSTXR) THEN
	  IF (NIX.LE.0 .OR. NIX.GT.A_J(J0+WQD_NTXIX_J)+1) THEN
	    E_C=86					!ILLEGAL INDEX
	    WQSTXR=.FALSE.
	  END IF
	END IF
	J=NIX-1						!TABLE POINTER
	IF (WQSTXR) THEN
	  IF (TP.LE.0 .OR. TP.GT.WQG_NFONT) THEN
	    E_C=89					!ILLEGAL FONT
	    WQSTXR=.FALSE.
	  ELSE
	    A_E(J0+WQD_TXIX_E+3*J+0)=TP			!FONT
	  END IF
	END IF
	IF (WQSTXR) THEN
	  A_E(J0+WQD_TXIX_E+3*J+1)=IAND(PR,3)		!PRECISION
	END IF
	IF (WQSTXR) THEN
	  IF (COL.LT.0 .OR. COL.GT.A_J(J0+WQD_NCLIX_J)+1) THEN
	    E_C=96					!ILLEGAL COLOUR
	    WQSTXR=.FALSE.
	  ELSE
	    A_E(J0+WQD_TXIX_E+3*J+2)=COL		!LINE COLOUR
	  END IF
	END IF
C
	RETURN
C
C
	END
