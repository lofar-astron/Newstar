C+ NGCPFL.FOR
C  WNB 920820
C
C  Revisions:
C	CMV 931220	Add OVERVIEW option
C	JPH 940809	'WMP node' --> 'NGF node'
C
C
	SUBROUTINE NGCPFL(PTYPE,INFCA,NODIN,OVV)
C
C  Show NGF file layout
C
C  Result:
C
C	CALL NGCPFL ( PTYPE_J:I, INFCA_J:I, NODIN_C*(*):I, OVV_L:I)
C					Show on output PTYPE the file layout
C					of file INFCA (if OVV is .false.) or
C					give ann overview (if OVV is .true.).
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGF_O_DEF'		!PLOT HEADER
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
C
C  Parameters:
C
	INTEGER MAXCMT			!Max. number of comments to remember
	PARAMETER(MAXCMT=25)
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (F_P, F_T ETC)
	INTEGER INFCA			!FILE DESCRIPTOR
	CHARACTER NODIN*(*)		!NAME OF NODE
	LOGICAL OVV			!OVERVIEW (else layout)?
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	INTEGER WNFEOF			!GET FILE POINTER
	LOGICAL NGCSTG			!GET DATASET
	INTEGER WNCALN			!Length of string
	CHARACTER*32 WNTTSG		!MAKE SET NAME
C
C  Data declarations:
C
	INTEGER SET(0:7,0:1)		!ALL SETS
	INTEGER SNAM(0:7)		!SET NAME
	CHARACTER CNAM*23,TNAM*32	!IN CHARACTERS
	INTEGER NGFP			!SET POINTER
C
	INTEGER CGROUP			!Current group
	INTEGER CFIELD			!Current field
	INTEGER CCHAN			!Current channel
	INTEGER LCHAN			!Last channel
	CHARACTER CCMT*40		!Current comment
	CHARACTER CMT(MAXCMT)*40	!List of comments
	INTEGER NCMT			!Number of unique comments
	LOGICAL DO_PRINT		!Print line for map
C
	BYTE GFH(0:GFHHDL-1)		!FILE HEADER
	BYTE SGH(0:SGHHDL-1,0:7)	!SUB-GROUP HEADER
	  INTEGER SGHJ(0:SGHHDL/4-1,0:7)
	  EQUIVALENCE(SGH,SGHJ)
C
	BYTE NGF(0:NGFHDL-1)		!PLOT HEADER
	  INTEGER NGFJ(0:NGFHDL/4-1)
	  INTEGER*2 NGFI(0:NGFHDL/2-1)
	  REAL NGFE(0:NGFHDL/4-1)
	  EQUIVALENCE (NGF,NGFJ,NGFI,NGFE)
C-
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
	1	'!/File layout of NGF node (!UJ bytes):!/',
	1	WNFEOF(INFCA))
	  ELSE
	     CALL WNCTXT(PTYPE,
	1	'!/File layout of NGF node !AD (!UJ bytes):!/',
	1       GFH(GFH_NAME_1),GFH_NAME_N,WNFEOF(INFCA))
	  END IF
	ELSE
	  CALL WNCTXT(PTYPE,
	1	'!/File layout of NGF node !AS (!UJ bytes):!/',
	1	NODIN,WNFEOF(INFCA))
	END IF
C
C SHOW LAYOUT
C
	IF (.NOT.OVV) THEN
	  DO WHILE(NGCSTG(INFCA,SET,NGF,NGFP,SNAM)) !GET SETS
	    DO I=0,7				!CLEAR LEVEL COUNT
	      SGHJ(SGH_HEADH_J-SGH_LINKG_J,I)=0
	    END DO
	    I=SET(1,0)-1				!CURRENT LEVEL
	    IF (.NOT.WNFRD(INFCA,SGHHDL-SGH_LINKG_1,SGH(0,I),
	1		SET(3,0)+SGH_LINKG_1)) THEN !READ TOP
 10	      CONTINUE
	      CALL WNCTXT(PTYPE,'Error reading file')
	      RETURN
	    END IF
	    DO WHILE(I.GT.0)			!READ LEVELS
	      I=I-1
	      IF (.NOT.WNFRD(INFCA,SGHHDL-SGH_LINKG_1,SGH(0,I),
	1		SGHJ(SGH_HEADH_J-SGH_LINKG_J,I+1))) GOTO 10
	    END DO
	    SNAM(1)=-1				!ONLY LOWER LEVELS
	    CALL WNCTXT(PTYPE,'!AS!10C contains !4$UJ fields, !4$UJ '//
	1		'channels, !4$UJ pol.s and !4$UJ '//
	1		'tel/ifrs for !AD',
	1		WNTTSG(SNAM(0),0),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,0),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,1),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,2),
	1		SGHJ(SGH_LINKGN_J-SGH_LINKG_J,3),
	1		NGF(NGF_NAM_1),NGF_NAM_N)
	    IF (.NOT.WNFRD(INFCA,SGHHDL,SGH(0,0),SET(3,0))) GOTO 10 !READ CURRENT
	    DO WHILE (SET(1,0).GT.1)			!DECREASE LEVEL
	      SET(1,0)=SET(1,0)-1				!DECREASE LEVEL
	      SET(3,0)=SGHJ(SGH_HEADH_J,0)-SGH_LINKG_1+SGH_LINK_1 !LOWER HEADER
	      IF (.NOT.WNFRD(INFCA,SGHHDL,SGH(0,0),SET(3,0))) GOTO 10 !CURRENT
	      SET(4,0)=SGHJ(SGH_HEADH_J,0)		!NEW LOWER HEAD
	    END DO
	  END DO
	  CALL WNCTXT(PTYPE,' ')
C
C	Else print summary of contents
C
	ELSE
C
	  CALL WNCTXT(PTYPE,'grp.fld.chn.pol.ifr.cut  (#) '//
     &	                  'Ifr Pol  Field        Type')
C
	  CGROUP=-1				!Group unknown so far
	  CFIELD=-1				!Field unknown so far
	  LCHAN=-1				!No channel printed yet
C
C Loop over all sectors
C
	  DO WHILE (NGCSTG(INFCA,SET,NGF,NGFP,SNAM))!all sets
C
C	If this is a new group, make sure we print the final 
C	channel of the previous group and that we print the first 
C	channel of this group
C
	   IF (CGROUP.NE.SNAM(0)) THEN
	      IF (LCHAN.NE.-1) THEN
	         CALL WNCTXT(PTYPE,'      - !3$UJ',LCHAN)
	         LCHAN=-1
	      ENDIF
	      CCHAN=SNAM(2)			!Print this channel
	      NCMT=0				!No comments yet
	   END IF
C
C	We do not print a continuous range of channels
C
	   DO_PRINT=(SNAM(2).NE.CCHAN+1)
C
C	Unless they have a different comment
C
	   CALL WNGMTS(40,NGF(NGF_TYP_1),CCMT)	!Current comment
	   TNAM=WNTTSG(SNAM,0)			!Set name
	   I2=1
	   DO WHILE (TNAM(I2:I2).NE.'.')	!Strip group
	      I2=I2+1
	   END DO
	   TNAM=TNAM(I2+1:)
	   I2=WNCALN(TNAM)
	   I1=INDEX(CCMT,TNAM(:I2))		!Comment contains set name?
	   IF (I1.NE.0)	THEN			!Then replace by ...
	      CCMT=CCMT(:I1-1)//'...'//CCMT(I1+I2:)
	   END IF
C
C	Check all previous comments
C
	   I=1
	   DO WHILE (I.LE.NCMT.AND.CCMT.NE.CMT(I))
	     I=I+1
	   END DO
	   IF (I.GT.NCMT) THEN
	      DO_PRINT=.TRUE.
	      IF (NCMT.EQ.MAXCMT) THEN		!End of buffer
	        CMT(2)=CCMT			!Overwrite second comment
	      ELSE
	        NCMT=NCMT+1			!Fill buffer
	        CMT(NCMT)=CCMT
	      ENDIF
	   ENDIF
C
C	If we do need to print this one, do so now, else keep channel
C
	   IF (DO_PRINT) THEN
	      CNAM=WNTTSG(SNAM,3)		!ONLY 23 CHARS (4*6-1)
	      IF (CGROUP.EQ.SNAM(0)) THEN
		  CNAM(1:4)=' '				!Wipe group
	          IF (CFIELD.EQ.SNAM(1)) CNAM(5:8)=' '	!Wipe field
	      END IF
	      CALL WNCTXT(PTYPE,
     &	         '!23$AS !3$UJ  !-4$AL4 !AL2  !-12$AL12 !AL40',
     &	          CNAM,NGFJ(NGF_SETN_J),
     &	          NGF(NGF_IFR_1),NGF(NGF_POL_1),NGF(NGF_NAM_1),CCMT)
	   END IF
C
	   CGROUP=SNAM(0)			!Keep group for check
	   CFIELD=SNAM(1)			!Keep field for check
	   CCHAN=SNAM(2)			!Keep channel for check
	   LCHAN=CCHAN				!Channel may be printed later
	   IF (DO_PRINT) LCHAN=-1		!Channel has been printed
C
	  END DO
C
	  IF (LCHAN.NE.-1) CALL WNCTXT(PTYPE,'      - !3$UJ',LCHAN)
	  CALL WNCTXT(PTYPE,' ')
C
	END IF
C
	RETURN
C
C
	END
