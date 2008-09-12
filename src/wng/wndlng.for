C+ WNDLNG.FOR
C  WNB 900306
C
C  Revisions:
C	JPH 920118	Detailed comments, more informative variable names
C	JPH 930527	Detailed comments, more informative variable names
C	WNB 930803	Remove _eqv
C
C
	LOGICAL FUNCTION WNDLNG(PLHDP,DATAP,IDOFF,FCA,SGHP,SGHN)
C
C  Create and link a new subgroup header
C
C  Result:
C
C	WNDLNG_L =
C	   WNDLNG( PLHDP_J:I, DATAP_J:I, IDOFF_J:I, FCA_J:I, SGHP_J:O, SGHN_J:O)
C			Create an SGH on disk for data at DATAP and link it
C			at the tail of the queue headed by PLHDP in file FCA.
C			If IDOFF>0 an identification is set.
C			The file address of the new SGH is returned in SGHP,
C			its ID in SGHN.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'GFH_O_DEF'			!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'			!SUB-GROUP HEADER
C
C  Parameters:
	INTEGER F,B,N				!FIELDS
	PARAMETER (F=0, B=1, N=2)		!forw/backw link, list length
C
C  Arguments:
C
	INTEGER PLHDP				!File address of listhead
	INTEGER DATAP				!File address of data
						!(0 if none)
	INTEGER IDOFF				!Offset to ID field in new SGH
	INTEGER FCA				!FILE CONTROL AREA
	INTEGER SGHP				!POINTER TO SUB-GROUP CREATED
	INTEGER SGHN				!SUB-GROUP # CREATED
C
C  Function references:
C
	LOGICAL WNFRD				!READ FILE
	LOGICAL WNFWR				!WRITE FILE
	INTEGER WNFEOF				!FILE POSITION
	LOGICAL WNDLNK				!LINK IN LIST
C
C  Data declarations:
	INTEGER LISTHD(F:N)			!Listhead buffer
	INTEGER NEW(F:B)			!New area's LINK field
	INTEGER TAIL(F:B)			!Queue tail's LINK field
	INTEGER SGHJ(0:SGHHDL/4-1)		!SGH buffer
	INTEGER PSGHJ(0:SGHHDL/4-1)!
	INTEGER SGHJP				!pointer to SGH
C-
	WNDLNG=.TRUE.				!ASSUME OK
	CALL WNGMVZ(SGHHDL,SGHJ(0))		!EMPTY HEADER
	SGHJP=WNFEOF(FCA)			!WHERE TO WRITE
C
C Create SGH area 
	SGHJ(SGH_LHD_J)=SGHJP+SGH_LHD_1		!Listhead LHD of as yet empty
	SGHJ(SGH_LHD_J+1)=SGHJ(SGH_LHD_J)	!queue: points to itself
	SGHJ(SGH_PLHD_J)=PLHDP			!Back pointer to parent listhead
	SGHJ(SGH_STHP_J)=DATAP			!DATA POINTER
	IF (.NOT.WNFWR(FCA,SGHHDL,SGHJ(0),SGHJP))
	1	 GOTO 900			!WRITE HEADER
C
C Link it into the queue headed by PLHDP, set ID in GROUPN field,
C  then read it back in (SGHJP is the disk address)
	IF (.NOT.WNDLNK(PLHDP,SGHJP,SGH_NAME_1,FCA))
	1	GOTO 900
	IF (.NOT.WNFRD(FCA,SGHHDL,SGHJ(0),SGHJP)) 
	1	GOTO 900
C
C If listhead coincides with LHD field of GFH it is at level 0:
	IF (PLHDP.EQ.GFH_LINKG_1) THEN
C
C  Clear full name
	  DO I=0,7
	    SGHJ(SGH_FNAME_J+I)=-1
	  END DO
	ELSE
C
C  Read the parent SGH and copy its full name 
	  IF (.NOT.WNFRD(FCA,SGHHDL,PSGHJ(0),
	1	PLHDP-SGH_LHD_1+SGH_LINK_1)) GOTO 900
	  DO I=0,7
	    SGHJ(SGH_FNAME_J+I)=PSGHJ(SGH_FNAME_J+I)
	  END DO
	END IF
C
C Extend the full name with the ID of the present SGH
	DO I=0,7				!EXTEND NAME
	  IF (SGHJ(SGH_FNAME_J+I).EQ.-1) THEN
	    SGHJ(SGH_FNAME_J+I)=SGHJ(SGH_NAME_J)
	    GOTO 10
	  END IF
	END DO
 10	CONTINUE
C
	IF (.NOT.WNFWR(FCA,SGHHDL,SGHJ(0),SGHJP)) 
	1	GOTO 900			!REWRITE SGH
	SGHP=SGHJP				!RETURN its file address
	SGHN=SGHJ(SGH_NAME_J)			!and its ID
	RETURN					!READY
C
C ERROR
C
 900	CONTINUE
	WNDLNG=.FALSE.				!ERROR
	RETURN
C
C
	END
