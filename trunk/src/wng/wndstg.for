C+ WNDSTG.FOR
C  WNB 910327
C
C  Revisions:
C	WNB 910826	Add loop definitions to #
C	WNB 910909	Minor changes
C	JPH 930513	Comments
C	WNB 931015	Split off STR, STS; use SSH_DEF
C	WNB 931115	Improve speed for average find by factor 2
C	JPH 940826	Make 'new version' message generic
C	JPH 941005	WNGSTD
C
C
	LOGICAL FUNCTION WNDSTG(FCA,SETS,HDV,SSHP,SNAM)
C
C  Get next set
C  Note: "Set" refers to the generic "set" concept in Newstar, which may be a 
C SCN-file sector, a .WMP-file map or whatever. The only assumption made is
C that the offsets are as defined in the PARAMETER statements below.
C
C  Result:
C
C	WNDSTG_L = WNDSTG( FCA_J:I, SETS_J(0:*,0:*):IO, HDV_J:I,
C				SSHP_J:O, SNAM_J(0:*):O)
C				Get next set in file FCA, using the
C				specification in SETS (see WNDSTA).
C				WNDSTG will be .false. if no more sets.
C				HDV is the number of the current program
C				version header. SSHP the
C				diskpointer. SNAM is the full name of the
C				group, coded. A check is made for the right
C				version.
C
C	WNDSTH_L = WNDSTH( FCA_J:I, SETS_J(0:*,0:*):IO, HDV_J:I,
C				SSHP_J:O, SNAM_J(0:*):O)
C				Same, but no check for version
C
C	WNDSTL_L = WNDSTL( FCA_J:I, SETS_J(0:*,0:*):IO, HDV_J:I,
C				SSHP_J:O, SNAM_J(0:*):O,
C				OFFSET_J(0:*):I)
C				As WNDSTG, but the check in the set list SETS
C				is done with offsets OFFSET.
C			
C	WNDSTD_L = WNDSTS( FCA_J:I, SETS_J(0:*,0:*):IO, HDV_J:I,
C				SSHP_J:O, SNAM_J(0:*):O,
C				OFFSET_J(0:*):I)
C				As WNDSTL, but remove the link between the last
C				SGH block and the data header so the data become
C				'invisible' 
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET RELATED
	INCLUDE 'GFH_O_DEF'		!FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
C
C  Parameters:
C
C
C  Entry points:
C
	LOGICAL WNDSTH			! NO VERSION CHECK
	LOGICAL WNDSTL			! OFFSET FOR LOOPS
	LOGICAL WNDSTD			! delete link
C
C  Arguments:
C
	INTEGER FCA			!FILE TO SEARCH
	INTEGER SETS(0:SOF__N-1,0:*)	!SETS TO DO
	INTEGER HDV			!HEADER VERSION TO ACCEPT
	INTEGER SSHP			!POINTER TO SET HEADER
	INTEGER SNAM(0:SOF__N-1)	!FULL SET NAME
	INTEGER OFFSET(0:SOF__N-1)	!CHECK OFFSET FOR LOOPS
C
C  Function references:
C
	LOGICAL WNFRD, WNFWR		! READ/write DISK
C
C  Data declarations:
C
	LOGICAL DODEL			! 'delete' switch
	LOGICAL DOCH			!DO CHECK SWITCH
	INTEGER CHSET(0:SOF__N-1)	!OFFSET FOR LOOP CHECK
	BYTE SGH(0:SGH__L-1)		!SUB-GROUP HEADER
	  INTEGER SGHJ(0:SGH__L/LB_J-1)
	  EQUIVALENCE (SGH,SGHJ)
	BYTE SSH(0:SSH__L-1)		!LOCAL PART SET HEADER
	  INTEGER*2 SSHI(0:SSH__L/LB_I-1)
	  EQUIVALENCE (SSH,SSHI)
C-
	WNDSTG=.TRUE.					!ASSUME OK
	DOCH=.TRUE.					!CHECK VERSION
	DODEL=.FALSE.
	GOTO 11
C
C WNDSTH
C
	ENTRY WNDSTH(FCA,SETS,HDV,SSHP,SNAM)
C
	WNDSTH=.TRUE.
	DOCH=.FALSE.					!NO CHECK VERSION
	DODEL=.FALSE.
	GOTO 11
C
C CLEAR OFFSET
C
 11	CONTINUE
	
	DO I=0,SOF__N-1					!NO OFFSETS
	  CHSET(I)=0
	END DO
	GOTO 10
C
C WNDSTL
C
	ENTRY WNDSTL(FCA,SETS,HDV,SSHP,SNAM,OFFSET)
C
	WNDSTL=.TRUE.
	DOCH=.TRUE.					!CHECK VERSION
	DODEL=.FALSE.
	GOTO 9
C
C WNDSTD
C
	ENTRY WNDSTD(FCA,SETS,HDV,SSHP,SNAM,OFFSET)
C
	WNDSTD=.TRUE.
	DOCH=.FALSE.					!CHECK VERSION
	DODEL=.TRUE.
	GOTO 9
C
 9	CONTINUE
	DO I=0,SOF__N-1					!SET OFFSETS
	  CHSET(I)=OFFSET(I)
	END DO
	GOTO 10

C NEXT CHECK LINE
C
 10	CONTINUE
	IF (SETS(SOF_0_LEVEL,0).EQ.0) THEN		!NEW LINE
	  SETS(SOF_0_CLINE,0)=SETS(SOF_0_CLINE,0)+1	!NEXT LINE
	  IF (SETS(SOF_0_CLINE,0).GT.SETS(SOF_0_NLINE,0)) GOTO 900 !NO MORE LINES
	  IF (IAND(SETS(SOF_L_DEF,SETS(SOF_0_CLINE,0)),SOF_M_HI).EQ.
	1			SOF_M_SLOOP) GOTO 10	!SKIP LOOP DEFINITION
	  SETS(SOF_0_LEVEL,0)=1				!LEVEL 1
	  IF (SETS(SOF_SPEC,SETS(SOF_0_CLINE,0)).EQ.SOF_M_SPEC) THEN !#
	    SETS(SOF_0_CSET,0)=GFH_LINK_1		!CURRENT SET
	    SETS(SOF_0_CLH,0)=GFH_LINK_1		!CURRENT LINK HEAD
	  ELSE						!GROUPS
	    SETS(SOF_0_CSET,0)=GFH_LINKG_1		!CURRENT GROUP
	    SETS(SOF_0_CLH,0)=GFH_LINKG_1		!CURRENT LINK HEAD
	  END IF
	END IF
C
C READ CURRENT GROUP
C
 21	CONTINUE
	IF (.NOT.WNFRD(FCA,SGH__L,SGH(0),
	1		SETS(SOF_0_CSET,0))) GOTO 900	!READ CURRENT
 20	CONTINUE
	IF (SGHJ(SGH_LINK_J).EQ.SETS(SOF_0_CLH,0)) THEN	!END OF LIST
 22	  CONTINUE
	  SETS(SOF_0_LEVEL,0)=SETS(SOF_0_LEVEL,0)-1	!DECREASE LEVEL
	  IF (SETS(SOF_0_LEVEL,0).EQ.0) GOTO 10		!LOWEST, NEXT LINE
	  SETS(SOF_0_CSET,0)=
	1	SGHJ(SGH_HEADH_J)-SGH_LINKG_1+SGH_LINK_1 !LOWER HEADER ADDR
	  IF (.NOT.WNFRD(FCA,SGH__L,SGH(0),
	1		SETS(SOF_0_CSET,0))) GOTO 900	!READ CURRENT
	  SETS(SOF_0_CLH,0)=SGHJ(SGH_HEADH_J)		!NEW LOWER HEAD
	  GOTO 20					!RETRY
	END IF
	SETS(SOF_0_CSET,0)=SGHJ(SGH_LINK_J)		!NEXT ENTRY
	IF (.NOT.WNFRD(FCA,SGH__L,SGH(0),
	1		SETS(SOF_0_CSET,0))) GOTO 900	!READ CURRENT
	IF (SETS(SOF_SPEC,SETS(SOF_0_CLINE,0)).EQ.SOF_M_SPEC) THEN !#
	  IF (SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0)).NE.
	1		SOF_M_ALL) THEN			!CHECK * OR VALUE
	    IF (IAND(SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0)),SOF_M_HI).EQ.
	1			SOF_M_LOOP) THEN	!LOOP PRESENT
	      I=IAND(SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0)),SOF_M_LO)	!LOOP DEF. LINE
	      IF (SGHJ(SSH_SETN_J).LT.
	1		SETS(SOF_L_START,I)+CHSET(SETS(SOF_0_LEVEL,0)-1))
	1			GOTO 20			!BEFORE LOOP START
	      IF (MOD(SGHJ(SSH_SETN_J)-
	1		(SETS(SOF_L_START,I)+
	1		CHSET(SETS(SOF_0_LEVEL,0)-1)),
	1			SETS(SOF_L_INC,I)).NE.0) GOTO 20 !NOT IN LOOP
	      IF (SETS(SOF_L_END,I).NE.SOF_M_ALL) THEN	!NOT * END
		IF (SGHJ(SSH_SETN_J).GT.
	1		SETS(SOF_L_END,I)+CHSET(SETS(SOF_0_LEVEL,0)-1))
	1			GOTO 22			!BEYOND END
	      END IF
	    ELSE					!VALUE
	      I0=SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0))+
	1		CHSET(SETS(SOF_0_LEVEL,0)-1)	!CURRENT LINE SET
	      IF (SGHJ(SSH_SETN_J).LT.I0) THEN		!BEFORE LINE SET
		GOTO 20
	      ELSE IF (SGHJ(SSH_SETN_J).GT.I0) THEN	!BEYOND LINE SET
		GOTO 22
	      END IF
	    END IF
	  END IF
	  SSHP=SETS(SOF_0_CSET,0)			!SET HEADER
	ELSE
	  IF (SETS(SETS(SOF_0_LEVEL,0)-1,SETS(SOF_0_CLINE,0)).NE.
	1		SOF_M_ALL) THEN			!NOT *
	    IF (IAND(SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0)),SOF_M_HI).EQ.
	1			SOF_M_LOOP) THEN	!LOOP PRESENT
	      I=IAND(SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0)),SOF_M_LO)	!LOOP DEF. LINE
	      IF (SGHJ(SGH_GROUPN_J).LT.SETS(SOF_L_START,I)+
	1		CHSET(SETS(SOF_0_LEVEL,0)-1))
	1			GOTO 20			!BEFORE LOOP START
	      IF (MOD(SGHJ(SGH_GROUPN_J)-(SETS(SOF_L_START,I)+
	1		CHSET(SETS(SOF_0_LEVEL,0)-1)),
	1			SETS(SOF_L_INC,I)).NE.0) GOTO 20 !NOT IN LOOP
	      IF (SETS(SOF_L_END,I).NE.SOF_M_ALL) THEN	!NOT * END
		IF (SGHJ(SGH_GROUPN_J).GT.
	1		SETS(SOF_L_END,I)+CHSET(SETS(SOF_0_LEVEL,0)-1))
	1			GOTO 22			!BEYOND END
	      END IF
	    ELSE
	      I0=SETS(SETS(SOF_0_LEVEL,0)-1,
	1		SETS(SOF_0_CLINE,0))+
	1		CHSET(SETS(SOF_0_LEVEL,0)-1)	!LINE SET
	      IF (SGHJ(SGH_GROUPN_J).LT.I0) THEN	!BEFORE LINE SET
	        GOTO 20					!NOT SELECTED
	      ELSE IF (SGHJ(SGH_GROUPN_J).GT.I0) THEN	!BEYOND LINE SET
		GOTO 22
	      END IF
	    END IF
	  END IF
	  SSHP=SGHJ(SGH_DATAP_J)			!DATA POINTER
	  IF (SSHP.EQ.0) THEN				!NEXT LEVEL
	    IF (SGHJ(SGH_LINKG_J).EQ.
	1		SETS(SOF_0_CSET,0)+SGH_LINKG_1) GOTO 20 !NO NEXT L.
	    SETS(SOF_0_LEVEL,0)=SETS(SOF_0_LEVEL,0)+1	!NEXT LEVEL
	    IF (SETS(SOF_0_LEVEL,0).GT.SOF__N) GOTO 900	!TOO MANY LEVELS
	    SETS(SOF_0_CLH,0)=SETS(SOF_0_CSET,0)+SGH_LINKG_1 !NEW HEADER PTR
	    SETS(SOF_0_CSET,0)=SETS(SOF_0_CLH,0)	!NEXT CURRENT
	    GOTO 21					!CONTINUE
	  END IF
	END IF
C
	IF (DODEL) THEN
	  SGHJ(SGH_DATAP_J)=0
	  IF (.NOT.WNFWR(FCA,SGH__L,SGH(0),
	1		SETS(SOF_0_CSET,0))) GOTO 900	!WRITE last SGH
	ENDIF
C
C gET SET HEADER
C
	IF (.NOT.WNFRD(FCA,SSH__L,SSH(0),SSHP)) GOTO 900 !READ SET HEADER
	IF (DOCH) THEN					!CHECK VERSION
	  IF (SSHI(SSH_VER_I).LT.HDV) THEN		!WRONG VERSION
	    CALL WNCTXT(F_TP,
	1'!/Old version: First update your input file with the NVS option!/')
	    GOTO 900
	  END IF
	END IF
C
C SET SET NAME
C
	IF (SETS(SOF_SPEC,SETS(SOF_0_CLINE,0)).EQ.SOF_M_SPEC) THEN !#
	  SNAM(0)=SGHJ(SSH_SETN_J)			!SET NUMBER
	  SNAM(SOF_SPEC)=SOF_M_SPEC			!INDICATE #
	ELSE
	  DO I=0,SOF__N-1				!COPY NAME
	    SNAM(I)=SGHJ(SGH_FGROUP_J+I)
	  END DO
	END IF
C
	RETURN
C
C ERROR
C
 900	CONTINUE
	DO I=1,SOF__N-1
	  SETS(I,0)=0					!RESET SEARCH
	END DO
	WNDSTG=.FALSE.					!NO MORE
C
	RETURN
C
C
	END