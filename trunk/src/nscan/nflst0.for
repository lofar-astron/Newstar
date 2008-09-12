C+ NFLST0.FOR
C  JEN940418
C
C  Revisions:

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLST0 (ACTION,NAME,NVAL,IVAL,RVAL)
C
C  Store legend information for display of statistics:
C
C  Result:
C
C	CALL NFLST0 (ACTION_C(*):I,NAME_C(*):I,
C                    NVAL_J:I,IVAL(*)_J:I,RVAL(*)_R:I)
C
C       CALL NFLST0 ('INIT',' ',0,0,0)
C
C       CALL NFLST0 ('SET','IFRTABLE',nifrtable,ifra,0.)
C       CALL NFLST0 ('SET','BASEL',nifr,0,basel)
C       CALL NFLST0 ('SET','DIPOS',nifr,0,ang)
C       CALL NFLST0 ('ADD','HACIR',1,0,HACIR)
C       CALL NFLST0 ('ADD','HADEG',1,0,HACIR)
C       CALL NFLST0 ('ADD','CHAN',1,ICHAN,0)
C
C       CALL NFLST0 ('SECTOR',<g.o.f.c.s>,0,0,0)
C
C       CALL NFLST0 ('GET','IFRTABLE',nifrtable,ifrtable,0.)
C       CALL NFLST0 ('GET','BASEL',nifr,0,basel)
C       CALL NFLST0 ('GET','DIPOS',nifr?,0,ang)
C       CALL NFLST0 ('GET','HARANGE',2,0,HADEG)          !min,max
C       CALL NFLST0 ('GET','CHRANGE',2,ICHAN,0)          !min,max
C
C       CALL NFLST0 ('SHOW','RANGES',ncslin,0,0)   ! show HA-range etc
C            NB: The printed strings is enclosed in hashes (#).
C            NB: ncslin indicates the position of the closing hash (#).
C
C PIN references:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
	INTEGER XX,XY,YX,YY		!POL. POINTERS
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
        INTEGER IHAMIN,IHAMAX           !HA-BIN NRS (1 degr wide)
          PARAMETER (IHAMIN=-180, IHAMAX=180)
C
        INTEGER ICHMIN,ICHMAX           !CHANNEL NRS 
          PARAMETER (ICHMIN=0, ICHMAX=256)

        CHARACTER*79 SEPAR              !Separator string (see SHOW)
          PARAMETER (SEPAR=
     1          '########################################'//     
     1          '#######################################')     
C
C  Arguments:
C
        CHARACTER ACTION*(*)            !ACTION TO BE PERFORMED
        CHARACTER NAME*(*)              !CLOSER SPECIFICATION OF ACTION
	INTEGER NVAL			!NR OF VALUES IN IVAL,RVAL
	INTEGER IVAL(*)			!INPUT VALUE(S)
	REAL RVAL(*)			!INPUT VALUE(S)
	CHARACTER*80 ARGSTR
C
C  Function references:
C
C  Data declarations:
C
        CHARACTER*2 POLNAME(0:3)              !POL NAMES (XX, XY ETC)
          DATA POLNAME /'XX','XY','YX','YY'/    !
C
        CHARACTER*1 TELNAME(0:STHTEL-1)       !TEL NAMES (0,1,2,A, ETC)
          DATA TELNAME /'0','1','2','3','4','5','6',
     1                '7','8','9','A','B','C','D'/
C
C  Variables:
C
        INTEGER      N
        LOGICAL      SELPOL(-1:3)       !POL SELECTION (SHOW)
        CHARACTER*80 TXT80              !GENERAL TEXT BUFFER
C
        INTEGER      IFRTABLE(2*STHIFR) !IFR TABLE 
	INTEGER      NIFRTABLE          !NR OF INTEGERES IN IFRTABLE
	INTEGER	     NBASEL             !NR OF REALS IN BASEL
	REAL	     BASEL(STHIFR)	!BASELINE LENGTHS (M)
	INTEGER	     NANG               !NR OF REALS IN ANG
	REAL         ANG(STHIFR)	!DIPOLE POSITION ANGLES (?)
        REAL         HARAN(0:1)         !HA-RANGE OF TESTED SCANS
        INTEGER      CHRAN(0:1)         !CHANNEL-RANGE OF TESTED SECTORS
C
C Common:
C
	COMMON /NFLST0COMMON/ NIFRTABLE,IFRTABLE,
     1                        NBASEL,BASEL,
     1                        NANG,ANG,
     1                        HARAN,CHRAN
C-
C******************************************************************************
C
        IF (ACTION(:4).EQ.'INIT') THEN
	  NIFRTABLE = 0
	  NBASEL = 0
	  NANG = 0
          HARAN(0) = +181                     !DEGR
          HARAN(1) = -181
          CHRAN(0) = ICHMAX+1
          CHRAN(1) = ICHMIN-1
C
C******************************************************************************
C
        ELSE IF (ACTION(:3).EQ.'SET') THEN
C
          IF (NAME(:4).EQ.'IFRT') THEN
	    IF (NVAL.LE.0.OR.NVAL.GT.2*STHIFR) THEN
	       ARGSTR='NFLST0 SET: '
     1              //' NVAL out of range: !UJ (!UJ) '//NAME
               CALL WNCTXT (F_TP,ARGSTR,,NVAL,2*STHIFR)
	    ELSE
	      DO I=1,NVAL
	        IFRTABLE(I) = IVAL(I)                !SAVE IN COMMON
	      END DO
	      NIFRTABLE = NVAL
	    END IF
C
          ELSE IF (NAME(:5).EQ.'BASEL') THEN
	    DO I=1,NVAL
	      BASEL(I) = RVAL(I)
	    END DO
	    NBASEL = NVAL
C
          ELSE IF (NAME(:5).EQ.'DIPOS') THEN
	    DO I=1,NVAL
	      ANG(I) = RVAL(I)
	    END DO
	    NANG = NVAL
C
	  ELSE
	    ARGSTR='NFLST0 ('//ACTION(:3)//'):'//' Name not recognised: '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
	  END IF
C
C******************************************************************************
C
        ELSE IF (ACTION(:3).EQ.'ADD') THEN
C
          IF (NAME(:5).EQ.'HADEG') THEN
            HARAN(0) = MIN(HARAN(0),RVAL(1))       !HA-RANGE (DEGR)
            HARAN(1) = MAX(HARAN(1),RVAL(1))       !HA-RANGE
C
          ELSE IF (NAME(:5).EQ.'HACIR') THEN
            HARAN(0) = MIN(HARAN(0),RVAL(1)*360)   !HA-RANGE (DEGR)
            HARAN(1) = MAX(HARAN(1),RVAL(1)*360)   !HA-RANGE
C
          ELSE IF (NAME(:4).EQ.'CHAN') THEN
            CHRAN(0) = MIN(CHRAN(0),IVAL(1))       !CHAN-RANGE (NR)
            CHRAN(1) = MAX(CHRAN(1),IVAL(1))       !CHAN-RANGE
C
	  ELSE
	    ARGSTR='NFLST0 ('//ACTION(:3)//'):'//' Name not recognised: '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
	  END IF
C
C******************************************************************************
C
        ELSE IF (ACTION(:6).EQ.'SECTOR') THEN
C
C******************************************************************************
C
        ELSE IF (ACTION(:3).EQ.'GET') THEN
C
          IF (NAME(:4).EQ.'IFRT') THEN
	    IF (NVAL.LE.0.OR.NIFRTABLE.NE.NVAL) THEN
	       ARGSTR='NFLST0 GET: '
     1              //' NVAL out of range: !UJ (!UJ) '//NAME
               CALL WNCTXT (F_TP,ARGSTR,NVAL,NIFRTABLE)
	    ELSE
	      DO I=1,NIFRTABLE
	        IVAL(I) = IFRTABLE(I)                !
	      END DO
	    END IF
C
          ELSE IF (NAME(:5).EQ.'BASEL') THEN
	    DO I=1,NBASEL
	      RVAL(I) = BASEL(I)
	    END DO
C
          ELSE IF (NAME(:5).EQ.'DIPOS') THEN
	    DO I=1,NANG
	      RVAL(I) = ANG(I)
	    END DO
C
	  ELSE
	    ARGSTR='NFLST0 ('//ACTION(:3)//'):'//
     1           ' Name not recognised: '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
	  END IF
C
C******************************************************************************
C
        ELSE IF (ACTION(:4).EQ.'SHOW') THEN
C
          IF (NAME(:5).EQ.'RANGE') THEN
	    CALL WNCTXT (F_TP,'!AS'
     1           //' HA-range= !F6.2:!F6.2 degr '
     1           //' channels= !UJ:!UJ '
     1           //' !#C!AS'
     1           ,'#',HARAN(0),HARAN(1),CHRAN(0),CHRAN(1)
     1           ,NVAL,'#')
C
	  ELSE
            ARGSTR='NFLST0 ('//ACTION(:3)//'):'//
     1           ' Name not recognised: '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
	  END IF
C
C******************************************************************************
C
        ELSE
          CALL WNCTXT (F_TP,'NFLST0 Action '//ACTION(:3)
     1                 //': not recognised')
        END IF                                    !ACTION
C
C******************************************************************************
C
 900	CONTINUE
	RETURN
	END







