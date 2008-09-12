C+ NSCREG.FOR
C  WNB 900820
C
C  Revisions:
C	WNB 920828	Change completely for different meaning
C	JPH 930610	Comments. Labels 202-204 at the end, rearrange code to
C			 clarify loop structure. Meaningful variable names.
C	WNB 930803	Change to SCN_DEF
C	JPH 930827	Report new old and new indices
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C
	SUBROUTINE NSCREG
C
C  Create new job with new SGH hierarchy for data in SCN file
C
C  Result:
C
C	CALL NSCREG	will create new groups
C
C PIN references:
C
C	INPUT_SCAN
C	SETS
C	SET_PATTERN
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NSC_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
	INCLUDE 'GFH_O_DEF'			!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'			!SUB-GROUP HEADER
	INCLUDE 'SCN_DEF'			!subgroup levels
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDNOD				!GET NODE NAME
	LOGICAL WNFOP				!OPEN FILE
	LOGICAL WNDSTA				!GET SETS TO DO
	LOGICAL NSCSTG				!GET A SET
	LOGICAL WNDLNG,WNDLNF			!LINK SUB-GROUP
	CHARACTER*32 WNTTSG			!SUB-GROUP NAME
C
C  Data declarations:
C
	INTEGER STHP				!SUB-GROUP POINTER
	INTEGER SNAM(0:7),SNAMN(0:7)		!SET NAME
	INTEGER SETNEW(0:7,0:1)			!SET PATTERN
	INTEGER LVL				!subgroup level
	BYTE		STH(0:STHHDL-1)		!SET HEADER
	  INTEGER	STHJ(0:STHHDL/4-1)
	  INTEGER*2	STHI(0:STHHDL/2-1)
	  REAL		STHE(0:STHHDL/4-1)
	  EQUIVALENCE	(STH,STHJ,STHI,STHE)
C-
C******************************************************************************
C		Get user's parameters
C******************************************************************************
C
C GET NODE
C
 100	CONTINUE
	IF (.NOT.WNDNOD('INPUT_SCN_NODE',' ',
	1	'SCN','R',NODIN,IFILE)) THEN	!NODE
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN	!READY WITH REGROUP
	  CALL WNCTXT(F_TP,'Node does not exist')
	  GOTO 100
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  RETURN				!END
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 100				!MUST SPECIFY
	ELSE
	  IF (.NOT.WNFOP(FCAIN,IFILE,'U')) THEN !OPEN SCN FILE
	    CALL WNCTXT(F_TP,'Cannot open file attached to node')
	    GOTO 100
	  END IF
C
C GET SETS specification
C
	  DO LVL=0,7
	    SNAMN(LVL)=-1
	  ENDDO
 200	  CONTINUE
	  IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN))
	1	GOTO 204			!GET SETS TO DO
	  IF (SETS(0,0).EQ.0) GOTO 204		!NONE
	  IF (.NOT.WNDSTA('SCN_SET_PATTERN',1,SETNEW,FCAIN))
	1	GOTO 204			!GET SETS TO MAKE
	  IF (SETNEW(0,0).EQ.0) GOTO 204	!NONE

C******************************************************************************
C DO SETS
C******************************************************************************
C
	  DO WHILE (NSCSTG(FCAIN,SETS,STH,	!loop over sectors
	1	STHP,SNAM))
	    CALL WNDSTI(FCAIN,SNAM)		!MAKE PROPER NAME
	    DO LVL=SCN_GRP,SCN_CHN		!MAKE OUTPUT NAME
	      IF (SETNEW(LVL,1).LT.0) THEN	!COPY FIELD
		SNAMN(LVL)=SNAM(LVL)
	      ELSE
		SNAMN(LVL)=SETNEW(LVL,1)
	      END IF
	    END DO
	    DO LVL=SCN_GRP,SCN_CHN		!CHECK FOR LOOP
	      IF (SNAMN(LVL).NE.SNAM(LVL)) GOTO 203!CAN DO
	    END DO
	    GOTO 201				!CANNOT DO
 203	      CONTINUE
	      IF (.NOT.WNDLNF(0+GFH_LINKG_1,
	1	SNAMN(SCN_GRP),
	1	SGH_GROUPN_1,FCAIN,SGPH(0),
	1	SGNR(0))) GOTO 202		!find/create group
	      DO LVL=SCN_OBS,SCN_CHN		!find/create
		IF (.NOT.WNDLNF			! observn, field, channel
	1	(SGPH(LVL-1)+SGH_LINKG_1,
	1	SNAMN(LVL),
	1	SGH_GROUPN_1,FCAIN,SGPH(LVL),
	1	SGNR(LVL))) GOTO 202
	      ENDDO
	      IF (.NOT.WNDLNG			!link STH at first free sector
	1	(SGPH(SCN_CHN)+SGH_LINKG_1,STHP,! number
	1	SGH_GROUPN_1,FCAIN,SGPH(SCN_SCT),
	1	SGNR(SCN_SCT))) GOTO 202
		SNAMN(SCN_SCT)=SGNR(SCN_SCT)
	        CALL WNCTXT(F_TP,'Creating new index !AS for sector !AS',
	1		WNTTSG(SNAMN,0),WNTTSG(SNAM,0) )
 201	    CONTINUE
	  ENDDO					!end sectors loop
	ENDIF
	GOTO 200				!prompt for more sectors
C
C EXITS 
C
 202	  CONTINUE				!all errors here
	  CALL WNCTXT(F_TP,'!/Cannot create sub-group')
 204	  CONTINUE				!all exits here
	  CALL WNFCL(FCAIN)			!CLOSE DATASET
	  GOTO 100				!FINISH
	END
