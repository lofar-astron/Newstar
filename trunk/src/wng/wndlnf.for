C+ WNDLNF.FOR
C  WNB 900306
C
C  Revisions:
C	JPH 920118	Detailed comments; informative variable names
C	JPH 930527	Detailed comments; informative variable names
C	WNB 930803	Remove gfh_eqv, sgh_eqv
C	JPH 941005	Signal existing/new SGH through E_C
C
C
	LOGICAL FUNCTION WNDLNF(PLHDP,NEWID,IDOFF,FCA,SGHP,SGHN)
C
C  Find/create a set-group in a file
C
C  Result:
C
C	WNDLNF_L = WNDLNF
C		(PLHDP_J:I, NEWID_J:I, IDOFF_J:I, FCA_J:I, SGHP_J:O, SGHN_J:O)
C
C		Find/create an SGH area with identifier NEWID on disk in a
C		linked list with parent listhead PLHDP in file FCA. 
C		If IDOFF>0 an identification is set.
C		The file address of the SGH created is returned in SGHP
C		its ID in SGHN
C		Upon successful exit, E_C is set to show whether an existing SGH
C		was found (E_C=0) or a new one had to be created (E_C=1)
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'GFH_O_DEF'			!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'			!SUB-GROUP HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PLHDP				!DISK POINTER LINK PLHDP
	INTEGER NEWID				!SUB-GROUP TO FIND
	INTEGER IDOFF				!OFFSET TO AN ID
	INTEGER FCA				!FILE CONTROL AREA
	INTEGER SGHP				!POINTER TO SUB-GROUP CREATED
	INTEGER SGHN				!SUB-GROUP # CREATED
C
C  Function references:
C
	LOGICAL WNFRD				!READ FILE
	LOGICAL WNDLNG				!LINK IN SUB-GROUP LIST
C
C  Data declarations:
C
	INTEGER SGHJ(0:SGHHDL/4-1)		!SUB-GROUP HEADER
	INTEGER SGH1J(0:SGHHDL/4-1)		!SUB-GROUP HEADER
C-
	WNDLNF=.TRUE.				!ASSUME OK
C
	IF (.NOT.WNFRD(FCA,SGHHDL,SGHJ(0),
	1	PLHDP-SGH_LHD_1+SGH_LINK_1))
	1	GOTO 900			!read parent SGH 
C
C LLEN in this SGH contains the ID of the next child SGH to be created, so
C  if NEWID is >= it, it does not yet exist 
	IF (NEWID.GE.SGHJ(SGH_LLEN_J)) THEN
C
C Create the missing SGHs 
	  DO I=SGHJ(SGH_LLEN_J),NEWID
	    IF (.NOT.WNDLNG(PLHDP,0,IDOFF,FCA,
	1	SGHP,SGHN)) GOTO 900
	  END DO
	  E_C=1					! signal "new SGH created"
	  RETURN
	ELSE
C
C Follow the queue to the requested SGH. (NOTE: This could be done more directly
C  through a DO I=0, NEWID-1 if we assume that the sequence of ID numbers is
C  intact. The more cumbersome implementation below provides a partial check
C  on this.)
	  SGHP=SGHJ(SGH_LHD_J)			!Listhead ptr to 1st element
	  DO I=0,SGHJ(SGH_LLEN_J)-1
	    IF (.NOT.WNFRD(FCA,SGHHDL,
	1	SGH1J(0),SGHP)) GOTO 900
	    IF (SGH1J(SGH_NAME_J).EQ.NEWID)
	1	THEN				!FOUND 
	      SGHN=NEWID
	      E_C=0				! signal "existing SGH found"
	      RETURN
	    END IF
	    SGHP=SGH1J(SGH_LINK_J)		!TRY NEXT
	  END DO
	  GOTO 900				!NOT FOUND, file corrupted
	END IF
C
C ERROR
C
 900	CONTINUE
	WNDLNF=.FALSE.				!ERROR
	RETURN
C
C
	END
