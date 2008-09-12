C+ NPLSST.FOR
C  JPH 960305
C
C  Revisions:
C  JPH 960522	Remove spurious print statement
C  JPH 960619	Add monotony option
C  JPH 960806	Straighten out. - Init ST=0 for monotony mode
C  JPH 960808	Bug fix: ST0 real i.s.o. integer
C  JPH 961118	Remove HA jumps
C
C
C
	SUBROUTINE NPLSST(STHD, HA, SNAM, ST)
C
C  Calculate sidereal time
C
C  Result:
C	NPLSST (STH_D(0:*):I, HA_R:I, SNAM_J(0:*):I, ST_R:O)
C		Calculate ST in circles from HA and STHD(STH_RA_D).
C		If ST_INIT#0, set ST_INIT to 0 and save ST and STHD(STH_MJD_D)
C		 in ST0 and MJD0; if common ST_INIT<0 set STPREV for monotony
C		else use these saved values to resolve 1-circle ambiguity
C	 	 by requiring that STHD(STH_MJD_D)-MJD0 be almost equal
C		 to ST-ST0.
C		In monotony mode, ST must always be larger than previous; this 
C		 is achieved by predicting ST from the ST and HA length of the 
C		 previous sector and using it when appropriate. Moreover, an ST
C		 gap is created to mark the transitions to a new grp, obs, fld;
C		 these are detected by comparing SNAM with the previous one 
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NPL_DEF'
	INCLUDE 'STH_O_DEF'
C
C  Parameters:
C
	INTEGER NNAM				! # of SNAM comp. of interest
	PARAMETER (NNAM=3)			! grp,obs,fld
C
C  Arguments:
C
	DOUBLE PRECISION STHD(0:*)		! sector header
	REAL		 HA			! hour angle (circles)
	INTEGER		 SNAM(0:NNAM-1)		! sector name
 	REAL		 ST			! sidereal time (circles)
C
C
C  Entry points:
C
C
C  Function references:
C
	REAL		NPLSSR, NPLSSS
C
C  Data declarations:
	DOUBLE PRECISION MJD0			! reference MJD
	REAL		 ST0			! reference ST
	REAL		 STPRED			! predicted ST
	LOGICAL		 MONOT			! monotony-mode flag
	INTEGER		 PNAM(0:NNAM-1)		! saved sector name
 	SAVE		 STPRED, ST0, MJD0, MONOT, PNAM
C
C  Equivalences:
C
C
C  Commons:
C
C-
C Initialisation: ST_INIT ><0
C
 	ST=STHD(STH_RA_D)+HA
 	IF (ST_INIT .GT.0) THEN			! init ST mode
 	  MJD0= STHD(STH_MJD_D)
	  ST0= ST				! reference ST
 	  MONOT=.FALSE.
	ELSEIF (ST_INIT.LT.0) THEN		! init monotonous mode
	  STPRED=0
	  MONOT=.TRUE.
	  DO I=0,NNAM-1
	    PNAM(I)=-1				! init name
	  ENDDO
	ENDIF
 	ST_INIT=0
C
C Running: ST_INIT =0
C	   
	IF (.NOT.MONOT) THEN			! ST mode
	  ST= ST +INT( (STHD(STH_MJD_D)-MJD0)	! eliminate 1-day jumps 
	1	*STHD(STH_UTST_D) -ST+ST0 )
 	ELSE					! monotonous mode
 	  ST=STPRED 
!!	  DO I=0,NNAM-1				! extra space for new group,
!!	    IF (PNAM(I).NE.SNAM(I)) THEN	!  obs. or field
!!	      ST=ST+(NNAM-I)*NPLSSS(STHD)
!!	      GOTO 10
!!	    ENDIF
!!	  ENDDO
 10	  CONTINUE
 	  STPRED=ST+NPLSSR(STHD,STHD)		! predict next		
	  DO I=0,NNAM-1
	    PNAM(I)=SNAM(I)			! save name
	  ENDDO
	ENDIF
C
 	RETURN
	END
C
C
	REAL FUNCTION NPLSSR(STHE,STHJ)		! auxiliary function
C
	INCLUDE		'STH_O_DEF'
	REAL 		STHE(0:*)
	INTEGER 	STHJ(0:*)
C
	NPLSSR=STHE(STH_HAI_E)*STHJ(STH_SCN_J)
	RETURN
C 
	END
C
C
	REAL FUNCTION NPLSSS(STHE)		! auxiliary function
C
	INCLUDE		'STH_O_DEF'
	REAL 		STHE(0:*)
C
	NPLSSS=STHE(STH_HAI_E)
	RETURN
C 
	END
