c+ WNGU2S.FOR
C  HjV 950124	Made from routines used at Westerbork
C
C  Revisions:
C
	SUBROUTINE WNGU2S (DIR, YEAR, UTDAY, UT, STDAY, ST) 
C
C  Convert UT<-->ST
C
C  Result:
C
C	CALL WNGU2S( DIR_J:I, YEAR_J:I, UTDAY_J:IO, UT_D:IO, STDAY_J:IO, ST_D:IO)
C			Convert ST <--> UT
C	CALL WNGJVS( DIR_J:I, JD_D:IO, LMST_D:IO)
C			Calculate Sidereal time <--> Universal time in 1950 system
C	CALL WNGJUL( DIR_J:I, YEAR_J:IO, UTDAY_J:IO, UT_D:IO, JD_D:IO)
C       		Julian day/time moment <--> Civil day/time
C	CALL WNGSTL( DIR:J:I, LMST_D:IO, LSD_J:IO, ST_D:IO)
C			Calculate LSD nr and time in day fraction <--> LMST moment
C
C  Include files:
C
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER   DIR				!DIRECTION TO CONVERT
	INTEGER   YEAR				!CIVIL YEAR
	INTEGER   UTDAY				!UNIVERSAL DAY OF YEAR
	REAL*8    UT				!UT - TIME IN FRACTIONS
	INTEGER   STDAY				!SIDEREAL DAY FROM 1900
	REAL*8    ST				!ST - TIME IN FRACTIONS
C
C  Function references:
C
C
C  Data declarations:
C
	REAL*8    LMST				!LOCAL MEAN SIDEREAL MOMENT
	INTEGER   LSD				!LOCAL SIDEREAL DAY
	REAL*8    JD				!JULIAN DAY MOMENT
C
C-
C
C WNGU2S
C 
C   History  : Original version UTOST.F (BK 31-03-93)
C	       08-02-95 BK Correction of the relative STDAY of year
C   Subtitle : "UT VERSUS ST"
C 
C   In :       dir   -   direction: dir >= 0 : UT to ST 
C                                   dir <  0 : ST to UT 
C   In :       year  -   civil year 
C   Update:    utday -   universal day of year
C   Update:    ut    -   UT -  time in fractions
C   Update:    stday -   Siderial day from 1900 
C   Update:    st    -   ST -  time in fractions
C 
	IF (DIR. LT. 0) THEN			! convert st to ut
	   LSD = YEAR*366+(YEAR-1)/4+1725757+STDAY
	   CALL WNGSTL(-1, LMST, LSD, ST)
	   CALL WNGJVS(-1, JD, LMST) 
	   CALL WNGJUL(-1, YEAR, UTDAY, UT, JD)
	ELSE					! convert ut to st
	   CALL WNGJUL( 1, YEAR,UTDAY, UT, JD)	! convert to julian day 
	   CALL WNGJVS( 1, JD, LMST) 		! convert jd to lmst
	   CALL WNGSTL( 1, LMST, LSD, ST) 	! convert lmst to lsd+lst 
	   STDAY = LSD - (YEAR*366+(YEAR-1)/4+1725757)
	ENDIF 
C 
	RETURN
	END
C
C
C WNGJVS
C
	SUBROUTINE WNGJVS (DIR, JD, LMST)
C
C   History  : Original version JDVST.F
C   Subtitle : "JD VERSUS LMST "
C 
C   Calculate Sidereal time from Universal time in 1950 system.
C   Calculate Universal time from Sidereal time in 1950 system.
C 
C     Parameters are ;
C       DIR  - Direction in I4. 
C              Positive : UT to ST. 
C              Negative : ST to UT. 
C       JD   - Julian day moment in R8. 
C       LMST - Local mean sidereal moment in R8.
C 
C   Used is the method described in SRZM note 143. 
C 
C   S = 2421632.7952643056 + 1.002737909265*(J - 2415020) +
C       0.8063*10**-15*(J - 2415020)**2. 
C 
C   J = 2415019.2069071108 + 0.997269566414*(S - 2421632) -
C       0.7994*10**-15*(S - 2421632)**2. 
C 
C   NOTE : The coefficients of the J = formula have been improved
C          in this routine, in order to get a better reversal of 
C          the S = formula !!!!!!!!! 
C 
C   For the clarity these formulas are changed into ;
C 
C   LMST = 0.7769193890 + LWSRT + 1.002737909265*(JD   - JD0) +
C          0.8063*10**-15*(JD   - JD0)**2 + SD0. 
C   JD   =-0.7747479726 - LWSRT + 0.997269566414*(LMST - SD0) -
C          0.7994*10**-15*(LMST - SD0)**2 + JD0. 
C 
C         LMST  - Local mean sidereal day and time for the WSRT.
C         LWSRT - WSRT longitude (6.60417 Degr or 0.0183449166) 
C         JD0   - Offset from JD to 1900, 0 JAN 12.00 UT, equal to 2415020. 
C         SD0   - Offset from LMST to 1900 equal to 2421632.
C 
C
C  Arguments:
C
	INTEGER   DIR				!DIRECTION TO CONVERT
	REAL*8    JD				!JULIAN DAY MOMENT
	REAL*8    LMST				!LOCAL MEAN SIDEREAL MOMENT
C
C  Data declarations:
C
        REAL*8 X,JD0,SD0
        REAL*8 AJ,BJ,CJ,AS,BS,CS,LWSRT
C 
        DATA LWSRT / 0.0183449166D0      /
        DATA AJ    / -0.77474797263057D0 /
        DATA BJ    / 0.99726956641441D0  /
        DATA CJ    / -0.7997133713334D-15/
        DATA AS    / 0.7769193890D0      /
        DATA BS    / 1.002737909265D0    /
        DATA CS    / 0.8063D-15          /
        DATA SD0   / 2421632D0           /
        DATA JD0   / 2415020D0           /
C 
        IF (DIR.GE.0) THEN
          X = JD - JD0
          LMST = (CS*X + BS)*X + AS + LWSRT 
          LMST = LMST + SD0 
        ELSE
          X = LMST - SD0
          JD = (CJ*X + BJ)*X + AJ - LWSRT 
          JD = JD + JD0 
        ENDIF 
C 
	RETURN
	END
C
C
C WNGJUL
C
	SUBROUTINE WNGJUL (DIR, YEAR, UTDAY, UT, JD)
C
C   History  : Original version JULDA.F
C   Subtitle : "YEAR,DAY,TIME VS JULIAN DAY "
C
C   Calculate the Julian day moment from the civil year YEAR,
C   the day of the year DAY, and the universal time UT in
C   fraction of the day.
C   Calculate the civil year YEAR, the day of the year DAY,
C   and the universal time UT in fraction of the day from the
C   Julian day moment.
C   The routine is correct after 1 januari 1901 (Because the year 1900
C   is not a leap year), and will be correct until the year 2400.
C   Parameters are ;
C       D     - Direction in I4.
C               Positive : YEAR,DAY,UT to JD.
C               Negative : JD to YEAR,DAY,UT.
C       YEAR  - Civil year in I4. I.e. 1989.
C       UTDAY - UT day of the year in I4. 1, 2, ....,365, OR 366.
C       UT    - UT time in day fraction in R8. 0 <= UT < 1.
C       JD    - Julian day moment in R8.
C
C  Arguments:
C
	INTEGER   DIR				!DIRECTION TO CONVERT
	INTEGER   YEAR				!CIVIL YEAR
	INTEGER   UTDAY				!UNIVERSAL DAY OF YEAR
	REAL*8    UT				!UT - TIME IN FRACTIONS
	REAL*8    JD				!JULIAN DAY MOMENT
C
C  Data declarations:
C
	INTEGER   YY,LD
	INTEGER   ND,JD1900
	REAL*8    D0
C
	DATA JD1900 /2415020/		! JULIAN DAY ON 1900 0 JAN 12.00 UT
C
        IF (DIR.GE.0) THEN
           YY = YEAR - 1900                      ! NR OF YEARS SINCE 1900
           ND = YY * 365                         ! NR OF DAYS SINCE 1900
           ND = ND + (YY-1)/4                    ! NR OF LEAP DAYS TO ADD
           ND = ND + UTDAY                       ! ADD DAY OF THE YEAR
           ND = ND + JD1900                      ! MAKE INTEGER JULIAN DAY
           JD = DBLE(ND) + UT - 0.5D0            ! ADD TIME MINUS 0.5
	ELSE
           D0 = JD + 0.5D0                       ! ADD CORRECTION
           ND = IDINT(D0)                        ! MAKE INTEGER DAYS
           UT = D0 - DBLE(ND)                    ! MAKE UT TIME IN FRACTION
           ND = ND - JD1900                      ! NR OF DAYS SINCE 1900
           LD = ND / 1461                        ! NR OF LEAP DAYS
           YY = 1900 + LD*4                      ! YEAR TO START LEAP YEAR
           UTDAY = ND - LD * 1461 + 1            ! DAYS SINCE START LEAP YEAR
           IF (UTDAY .EQ. 366) GOTO 900          ! LAST DAY OF THE LEAP YEAR
           IF (UTDAY .GT. 366) THEN              ! NOT A LEAP YEAR
             YY = YY + 1                         ! INCREMENT YEAR
             UTDAY = UTDAY - 366                 ! SUBTRACT DAYS IN LEAP YEAR
           ENDIF
           YEAR = YY + (UTDAY-1)/365             ! FIND CORRECT YEAR
           UTDAY = MOD(UTDAY-1, 365) + 1         ! FIND DAY
	ENDIF
C
 900	RETURN
	END
C
C
C WNGSTL
C
	SUBROUTINE WNGSTL (DIR, LMST, LSD, LST)
C
C   History  : Original version JULDA.F
C   SUBTITLE : "LMST VERSUS LSD,LST "
C 
C   Calculate LSD nr and time in day fraction from LMST. 
C   Calculate LMST moment from day and fraction. 
C 
C   Parameters are ; 
C     DIR  - Direction in I4
C            Positive : LMST to LSD,LST.
C            Negative : LSD,LST to LMST.
C     LMST - Local mean sidereal time moment in R8. 
C     LSD  - Local sidereal day nr in I4. 
C     LST  - Local sidereal time in day fraction in R8. 
C 
C  Arguments:
C
	INTEGER   DIR				!DIRECTION TO CONVERT
	REAL*8    LMST				!LOCAL MEAN SIDEREAL MOMENT
	INTEGER   LSD				!LOCAL SIDEREAL DAY
	REAL*8    LST				!LOCAL SIDEREAL TIME IN DAY FRACTIONS
C
C  Data declarations:
C
	IF (DIR.GE.0) THEN
           LSD = IDINT(LMST)
           LST = LMST - DBLE(LSD) 
           IF (LST.LT.0D0) LST = LST + 1D0
	ELSE 
           LMST = DBLE(LSD) + LST 
	ENDIF
C 
	RETURN
	END
