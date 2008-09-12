C+ NATRPF.FOR
C  WNB 920428
C
C  Revisions:
C     RPN 29/9/88: major changes for IF axis and tables
C
C     rpn 9/11/88  major change in treatment of if's. For multi-IF data,
C                rpfits should be called once per IF (i.e. several  
C                times per integration), with a formal parameter if_no
C                varying from 1 to n_if.
C                A new group will be written for each IF, and needn't
C                be the same length. THUS GRPLENGTH CAN NOW VARY FROM
C                GROUP TO GROUP 
C                PTI data will continue to be written with nstok = 2
C                Also added parameters FLAG, BIN, if_no, and source no
C                and FG and SU tables
C     rpn 8/2/89:  changed dates from AEST to UT
C     rpn 17/2/89: major mods: (1) changed to use FORTRAN BLK routines
C                          (2) changed record length from 512 to 2560,
C                              but included common RECL  so
C                              that old data can still be read.
C                          (3) Put IN_ and OUT_RECNO and RP_IOSTAT in 
C                              INDEX common, and got rid of 
C                              OLD_RPFITSIN
C     rpn 8/5/89:  Allow use of either blk_read or AT_READ by editing
C                the logical USE_BLK in the include 
C                file RPFITS_SEL.INC
C     hm 9/5/89    Call routines (dummies on VAX) to translate
C                real and integer numbers from VAX format as they are 
C                read from the RPFITS file, and before they are 
C                written to the RPFITS file.
C     rpn 23/5/89  Fixed bugs in use of VAXI4, etc.
C     rpn 10/10/89 Allow FG table to be at the end of the data
C     rpn 10/10/89 Equivalence m(80) to buffer so FG table can be read 
C                from data buffer (Yes this does work on the VAX)
C     rpn 20/3/90  Write IF tabel even if n_if=1
C                Don't bother writing ANTENNA, TEMP, etc. cards
C                Introduced n_complex
C     rpn 21/3/90  MAJOR MODS: introduced syscal data group into 
C                RPFITSIN and RPFITSOUT
C     hm 10/5/90   Split routines into separate files and made
C                mods necessary for compilation on SUNs.
C     hm 21/5/90   Added write_wt tests.
C     hm 28/5/90   Eliminated need for rpfits_sel.inc
C     hm 19/6/90   End of scan check changed.
C     hm 9/8/90    Recover from illegal randon parameters, possibly
C                caused by missing blocks.
C     rpn 16/11/90 Changed SIMPLE test to work on buffer instead of m
C     rpn 17/11/90 Tidied up code by using routine getparm
C     rpn 17/11/90 Tidied up group synch tests
C     hm  13/12/90 Made OK for AIPS:
C                 . no tabs in col 1
C                 . no code past col 72
C                 . 'C' not 'c' for comments
C     JER 04/01/91 GETPARM was being called with 4 more actual args
C                than formal args. Remove SC_UT, SC_ANT, SC_IF and
C                SC_Q from calling arg list: they're in common anyway.
C     JER 04/01/91 Initialise IF_CSTOK to blanks if no IF table found.
C     HM  03/02/91 Add more checks for bad data to illbase.
C     HM  15/11/91 Initialize new new IF entries if missing.
C     NEBK 08/01/92 Reworked conversion of floating point buffer value
C                  into integer baseline to avoid problems with
C                  arithmetic exceptions on wildly corrupt values
C                  CHanges in GETPARM, SKIPTHRU and ILLBASE for this
C     HM  19/02/92 For SYSCAL data - Put source number into sc_srcno .
C                  Note that for syscal, it is not returned as argument
C                  sorceno.
C
	LOGICAL FUNCTION NATRPO(FCA,FCAPT)
C
C  Read RPFITS tape. Based on RPFITSIN
C
C  Result:
C
C	NATRPO_L = NATRPO( FCA_J:I, FCAPT_J:O)
C				Start a file  ON FCA and read first header.
C				FCAPT is the current file pointer
C	NATRPH_L = NATRPH( FCA_J:I, FCAPT_J:IO)
C				Read next header
C	NATRPD_L = NATRPD( FCA_J:I, FCAPT_J:IO, VIS_X(*):O,
C				BASEL_J:O, UT_E:O, U_E:O, V_E:O, W_E:O,
C				FLAG_J:O, BIN_J:O, IFNO_J:O, SRCNO_J:O)
C				Read next data group
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'RPF_DEF'	!RPFITS OUTPUT
C
C  Parameters:
C
	INTEGER ILLEGAL		!ILLEGAL DATA ENTITY
	  PARAMETER (ILLEGAL=32768)
	INTEGER SS$_ENDOFFILE	!EOF INDICATOR
	  PARAMETER (SS$_ENDOFFILE='870'X)
C
C  Arguments:
C
	INTEGER FCA		!FILE CONTROL AREA
	INTEGER FCAPT		!FILE POINTER
	COMPLEX VIS(*)		!VISIBILITIES
	INTEGER BASEL		!IFR DATA
	REAL UT			!UT
	REAL U,V,W		!U,V,W
	INTEGER FLAG		!FLAG DATA
	INTEGER BIN		!PULSAR BIN
	INTEGER IFNO		!IF NUMBER
	INTEGER SRCNO		!SOURCE NUMBER
C
C  Entry points:
C
	LOGICAL NATRPH,NATRPD
C
C  Function references:
C
	INTEGER NATXSF		!CHECK HEADER/TABLE FG
	LOGICAL WNFRD		!READ A FILE
	INTEGER NATXCJ		!MAKE J
	REAL NATXCE		!MAKE E
C
C  Data declarations:
C
	LOGICAL ENDHDR,STARTHDR		!HEADER SEARCH
	INTEGER BUFPTR			!PTR TO NEXT VIS. IN GROUP
	INTEGER BUFLEFT			!# OF WORDS STILL TO BE READ IN BUF.
	INTEGER BUFLEFT3
	INTEGER GRPLENGTH		!# VIS. IN GRP
	INTEGER GRPPTR			!PTR TO NEXT VIS. IN GROUP
	REAL GRPHDR(9)
	  INTEGER I_GRPHDR(9)
	  EQUIVALENCE (GRPHDR,I_GRPHDR)
	REAL REVIS
	INTEGER N_COMPLEX
	LOGICAL NEW_ANTENNA
	  DATA NEW_ANTENNA/.FALSE./
	LOGICAL ENDSCAN
	REAL CRPIX4
	REAL VELREF
	INTEGER PCOUNT
	INTEGER ICHAR,NCHAR
	REAL SC_BUF(MAX_SC*MAX_IF*ANT_MAX)
	  EQUIVALENCE (SC_BUF,SC_CAL)
	INTEGER I_BUFF(640)		!DATA BUFFER
	  REAL BUFFER(640)
	  CHARACTER*80 M(32)
	  EQUIVALENCE (I_BUFF,BUFFER,M)
	INTEGER ICARD
	REAL LAST_GOOD_UT
	  DATA LAST_GOOD_UT/0/
	INTEGER K
C
C  Common:
C
	COMMON /BUFPAR/ BUFPTR,BUFFER
	  DATA BUFPTR/0/		!START FRESH
C-
C
C RPO
C
	NATRPO=.TRUE.				!ASSUME OK
	FCAPT=0					!START AT BEGIN
	GOTO 10
C
C RPH
C
	ENTRY NATRPH(FCA,FCAPT)
C
	NATRPH=.TRUE.				!ASSUME OK
 10	CONTINUE
C
C READ HEADER
C
	ENDHDR=.FALSE.
	STARTHDR=.FALSE.
	BUFPTR=0
	ICARD=1
	IF (NCARD.LT.0) NCARD=-1
	N_IF=0
	AN_FOUND=.FALSE.
	IF_FOUND=.FALSE.
	SU_FOUND=.FALSE.
	FG_FOUND=.FALSE.
	NX_FOUND=.FALSE.
	MT_FOUND=.FALSE.
	CU_FOUND=.FALSE.
	LAST_GOOD_UT=0.
C
C LOOK FOR START OF NEXT HEADER
C
	DO WHILE (.NOT.STARTHDR)
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) THEN	!READ A BLOCK
 21	    CONTINUE
	    IF (E_C.EQ.SS$_ENDOFFILE) THEN		!EOF SEEN
	      E_C=3
	      GOTO 20
	    END IF
	    CALL WNCTXT(F_TP,'Unable to read header block or data')
	    E_C=-1					!ERROR TYPE
 20	    CONTINUE
	    NATRPH=.FALSE.
C
	    RETURN
	  END IF
	  FCAPT=FCAPT+2560				!READ POINTER
	  IF (M(1)(1:8).EQ.'SIMPLE') THEN
	    STARTHDR=.TRUE.
	  ELSE IF (M(1)(1:8).EQ.'TABLE FG') THEN
	    CALL NATRRT(FCA,FCAPT,M,-1,ENDHDR)		!READ FG
	    E_C=4					!ERROR TYPE
	    GOTO 20
	  END IF
	END DO
C
C SCAN THROUGH HEADER, GETTING THE INTERESTING BITS
C
	DO WHILE (.NOT.ENDHDR)
	  IF (.NOT.STARTHDR) THEN
	    IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	    FCAPT=FCAPT+2560				!UPDATE PTR
	  END IF
	  STARTHDR=.FALSE.
	  VERSION=' '
	  DO I=1,32
	    IF(M(I)(1:8).EQ.'NAXIS2') THEN
	      READ (M(I)(11:30),'(I20)')N_COMPLEX
	    ELSE IF (M(I)(1:8).EQ.'NAXIS3') THEN
	      READ (M(I)(11:30),'(I20)')NSTOK
	    ELSE IF (M(I)(1:8).EQ.'NAXIS4') THEN
	      READ (M(I)(11:30),'(I20)')NFREQ
	    ELSE IF (M(I)(1:8).EQ.'NAXIS7') THEN
C
C Note fudge for intermediate format PTI data
C
	      READ (M(I)(11:30),'(I20)')NSTOK
	    ELSE IF (M(I)(1:8).EQ.'GCOUNT') THEN
	      READ (M(I)(11:30),'(I20)')NCOUNT
	    ELSE IF (M(I)(1:8).EQ.'PCOUNT') THEN
	      READ (M(I)(11:30),'(I20)')PCOUNT
	    ELSE IF (M(I)(1:8).EQ.'SCANS ') THEN
	      READ (M(I)(11:30),'(I20)')NSCAN
	    ELSE IF (M(I)(1:8).EQ.'INTIME') THEN
	      READ (M(I)(11:30),'(I20)')INTIME
	    ELSE IF (M(I)(1:8).EQ.'CRPIX4') THEN
	      READ (M(I)(11:30),'(G20.12)')CRPIX4
	    ELSE IF (M(I)(1:8).EQ.'CRVAL4') THEN
	      READ (M(I)(11:30),'(G20.12)')FREQ
	    ELSE IF (M(I)(1:8).EQ.'CDELT4') THEN
	      READ (M(I)(11:30),'(G20.12)')DFREQ
	    ELSE IF (M(I)(1:8).EQ.'CRVAL5') THEN
	      READ (M(I)(11:30),'(G20.12)')RA
	    ELSE IF (M(I)(1:8).EQ.'CRVAL6') THEN
	      READ (M(I)(11:30),'(G20.12)')DEC
	    ELSE IF (M(I)(1:8).EQ.'RESTFREQ') THEN
	      READ (M(I)(11:30),'(G20.12)')RFREQ
	    ELSE IF (M(I)(1:8).EQ.'VELREF  ') THEN
	      READ (M(I)(11:30),'(G20.12)')VELREF
	    ELSE IF (M(I)(1:8).EQ.'ALTRVAL ') THEN
	      READ (M(I)(11:30),'(G20.12)')VEL1
	    ELSE IF (M(I)(1:8).EQ.'OBJECT  ') THEN
	      READ (M(I)(12:30),'(A16)')OBJECT
	    ELSE IF (M(I)(1:8).EQ.'INSTRUME') THEN
	      READ (M(I)(12:30),'(A16)')INSTRUMENT
	    ELSE IF (M(I)(1:8).EQ.'CAL     ') THEN
	      READ (M(I)(12:30),'(A16)')CAL
	    ELSE IF (M(I)(1:8).EQ.'OBSERVER') THEN
	      READ (M(I)(12:30),'(A16)') RP_OBSERVER
	    ELSE IF (M(I)(1:8).EQ.'VERSION ') THEN
	      READ (M(I)(12:30),'(A8)') VERSION
	    ELSE IF (M(I)(1:8).EQ.'DATE-OBS') THEN 
	      READ (M(I)(12:40),'(A8,16X,A4)') DATOBS,DATSYS
	    ELSE IF (M(I)(1:8).EQ.'DATE    ') THEN
	      READ (M(I)(12:30),'(A8)')DATWRIT
	    ELSE IF (M(I)(1:8).EQ.'EPOCH') THEN
	      READ (M(I)(12:30),'(A8)')COORD
	    ELSE IF (M(I)(1:5).EQ.'PRESS') THEN
	      READ (M(I)(6:40),'(I2,4X,G20.12)') K,MT_PRESS(K)
	    ELSE IF (M(I)(1:5).EQ.'TEMPE') THEN 
	      READ (M(I)(6:40),'(I2,4X,G20.12)') K,MT_TEMP(K)
	    ELSE IF (M(I)(1:5).EQ.'HUMID') THEN
	      READ (M(I)(6:40),'(I2,4X,G20.12)') K,MT_HUMID(K)
	    ELSE IF (M(I)(1:5).EQ.'EPHEM') THEN
	      READ (M(I)(6:40),'(I2,4X,G20.12)') K,RP_C(K)
	    ELSE IF (M(I)(1:8).EQ.'DEFEAT  ') THEN
	      READ (M(I)(11:30),'(I20)') RP_DEFEAT
	    ELSE IF (M(I)(1:8).EQ.'UTCMTAI ') THEN
	      READ (M(I)(11:30),'(G20.12)') RP_UTCMTAI
	    ELSE IF (M(I)(1:8).EQ.'DJMREFP ') THEN
	      READ (M(I)(11:30),'(G20.12)') RP_DJMREFP
	    ELSE IF (M(I)(1:8).EQ.'DJMREFT ') THEN
	      READ (M(I)(11:30),'(G20.12)') RP_DJMREFT
	    ELSE IF (M(I)(1:6).EQ.'TABLE ') THEN
C
C SORT OUT TABLES 
C
	      CALL NATRRT(FCA,FCAPT,M,I,ENDHDR)
	    ELSE IF (M(I)(1:8).EQ.'END     ') THEN
C
C END card.
C
	      ENDHDR=.TRUE.
	    END IF
C
C Write into "cards" array if necessary
C
	    IF (NCARD.GT.0) THEN
	      DO J=1,NCARD
		NCHAR=0
		DO ICHAR=1,12
		  IF (CARD(J)(ICHAR:ICHAR).NE.' ') NCHAR=ICHAR
		END DO
		IF (M(I)(1:NCHAR).EQ.CARD(J)(1:NCHAR)) CARD(J)=M(I)
	      END DO
	    ELSE IF (NCARD.LT.0) THEN
	      IF (ICARD.LE.MAX_CARD .AND. .NOT.ENDHDR) THEN
		CARD(-NCARD)=M(I)
		ICARD=ICARD+1
		NCARD=NCARD-1
	      END IF
	    END IF
C
C Read antenna parameters (a) OLD FORMAT
C
	    IF(M(I)(1:8).EQ.'ANTENNA:') THEN
	      IF (.NOT.NEW_ANTENNA) THEN
		NANT=0
		NEW_ANTENNA=.TRUE.
	      END IF
	      READ (M(I)(12:71),900) K,X(K),Y(K),Z(K),STA(K)
 900	      FORMAT(I1,4X,G13.6,' Y=',G13.6,' Z=',G13.6,' STA=',A3)
	      NANT=NANT+1
	    END IF
C
C Read antenna parameters (b) NEW FORMAT
C
	    IF (M(I)(1:8).EQ.'ANTENNA ') THEN
	      IF (.NOT.NEW_ANTENNA) THEN
		NANT=0
		NEW_ANTENNA=.TRUE.
	      END IF
	      READ (M(I)(11:80),910) K,STA(K),X(K),Y(K),Z(K)
 910	      FORMAT(i1,1X,A3,3X,G17.10,3X,G17.10,3X,G17.10)
	      NANT=NANT+1
	    END IF
	    IF (ENDHDR) GO TO 2400
	  END DO
 2400	  CONTINUE
	END DO
	NCARD=ABS(NCARD)
C
C See whether WEIGHT is to be written
C
	IF (N_COMPLEX.EQ.3) THEN
	  WRITE_WT=.TRUE.
	ELSE
	  WRITE_WT=.FALSE.
	END IF
C
C Insert default values into table commons if tables weren't found
C
	IF (.NOT.IF_FOUND) THEN 
	  N_IF=1
	  IF_FREQ(1)=FREQ
	  IF_INVERT(1)=1
	  IF_BW(1)=NFREQ*DFREQ
	  IF_NFREQ(1)=NFREQ
	  IF_NSTOK(1)=NSTOK
	  IF_REF(1)=CRPIX4
	  DO I=1,4
	    IF_CSTOK(I,1)=' '
	  END DO
	  IF_SIMUL(1)=1
	  IF_CHAIN(1)=1
	ELSE
	  FREQ=IF_FREQ(1)
	  NFREQ=IF_NFREQ(1)
C
C hm 18may90 added -1 below
C
	  IF (IF_NFREQ(1).GT.1) THEN
	    DFREQ=IF_BW(1)/(IF_NFREQ(1)-1)
	  ELSE
	    DFREQ=IF_BW(1)/IF_NFREQ(1)
	  END IF
	  NSTOK=IF_NSTOK(1)
	END IF
	IF (.NOT.SU_FOUND) THEN
	  N_SU=1
	  SU_NAME(1)=OBJECT
	  SU_RA(1)=RA
	  SU_DEC(1)=DEC
	ELSE
	  OBJECT=SU_NAME(1)
	  RA=SU_RA(1)
	  DEC=SU_DEC(1)
	END IF
C
C Tidy up
C
	IF (INSTRUMENT.EQ.'PTI') N_IF=1
	N_IF=MAX(N_IF,1)
	IVELREF=VELREF+0.5
	NEW_ANTENNA=.FALSE.
	BUFPTR=0
C
	RETURN					!READY
C
C RPD
C
	ENTRY NATRPD(FCA,FCAPT,VIS,BASEL,UT,U,V,W,
	1			FLAG,BIN,IFNO,SRCNO)
C

	NATRPD=.TRUE.				!ASSUME OK
C
C READ DATA GROUP HEADER
C
C THE FOLLOWING POINTERS AND COUNTERS ARE USED HERE:
C     GRPLENGTH      No. of visibilities in group
C     GRPPTR         Pointer to next visibility in group to be read
C     BUFPTR         Pointer to next word to be read in current buffer
C     BUFLEFT        No. of words still to be read from current buffer
C
C Note that data are read in blocks of 5 records=640 (4byte) words
C
	GRPPTR=1
	IFNO=1 
	IF (WRITE_WT) THEN				!WRITE WEIGHT
	  N_COMPLEX=3
	ELSE
	  N_COMPLEX=2
	END IF
	IF (BUFPTR.EQ.0 .OR. BUFPTR.EQ.641) THEN
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	  FCAPT=FCAPT+2560				!UPDATE PTR
	  E_C=NATXSF(FCA,FCAPT,BUFFER)
	  IF (E_C.NE.0) THEN
	    FCAPT=FCAPT-2560				!RE-READ
	    GOTO 20					!ERROR
	  END IF
	  BUFPTR=1
	END IF
C
C READ PARAMETERS FROM FITS FILE
C FORMAT FROM RPFITS IS:
C      ------ VIS data -------------      ----------- SYSCAL data ----
C      (baseline > 0)                         (baseline = -1)
C      param 1=u in m                         0.0
C      param 2=v in m                         0.0
C      param 3=w in m                         0.0
C      param 4=baseline number                -1.0
C      param 5=UT in seconds                  sc_ut: UT in seconds
C      param 6= flag (if present)             sc_ant
C      param 7= bin  (if present)             sc_if
C      param 8= ifno (if present)             sc_q
C      param 9=sourceno (if present)          sc_srcno
C
 3100	CONTINUE
	BUFLEFT=641-BUFPTR
C
C ---------check for end of scan -------------
C This is indicated by buffer being padded out with reserved 
C operands. 
C
C Old rpfits files may be padded with zeros, so check for u, 
C baseline no and UT all zero. Assume that if next vis 
C incomplete at end of buffer, next buffer will be all zeros.
C Mod by HM 19jun90
C
	ENDSCAN=.FALSE.
	IF (BUFLEFT.GE.PCOUNT) THEN
	  IF (NATXCJ(I_BUFF(BUFPTR)).EQ.0
     1			.AND. NATXCJ(I_BUFF(BUFPTR+3)).EQ. 0
     1			.AND. NATXCJ(I_BUFF(BUFPTR+4)).EQ. 0) THEN
	    ENDSCAN=.TRUE.
	  END IF
	END IF
	IF (NATXCJ(I_BUFF(BUFPTR)).EQ.ILLEGAL .OR. ENDSCAN ) THEN
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	  FCAPT=FCAPT+2560			!FILE PTR
	  E_C=NATXSF(FCA,FCAPT,BUFFER)
	  IF (E_C.NE.0) THEN
	    FCAPT=FCAPT-2560			!RE-READ
	    GOTO 20				!ERROR
	  END IF
	  E_C=5
	  GOTO 20
	END IF
C
C ------------NOW READ DATA -------------
C
	IF (BUFLEFT.GE.PCOUNT) THEN 
C
C If it will all fit in current buffer, then things are easy
C
	  CALL NATRGP(FCA,FCAPT,BUFFER,I_BUFF,BUFPTR,BUFPTR,BUFFER,
	1		PCOUNT,U,V,W,BASEL,
	1		UT,FLAG,BIN,IFNO,SRCNO)
	  IF (E_C.EQ.-2) GOTO 3100
	  IF (E_C.NE.0) GOTO 20
	  BUFPTR=BUFPTR+PCOUNT
	ELSE
C
C We can recover only part of the group header.
C dispose of what we have, then read the remainder from
C the next batch of data (pcount blocks).
C
	  DO I=1,BUFLEFT
	    GRPHDR(I)=BUFFER(BUFPTR+I-1)
	  END DO
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	  FCAPT=FCAPT+2560				!UPDATE PTR
	  E_C=NATXSF(FCA,FCAPT,BUFFER)
	  IF (E_C.NE.0) THEN
	    FCAPT=FCAPT-2560				!RE-READ
	    GOTO 20
	  END IF
	  BUFPTR=PCOUNT-BUFLEFT
C
C Extract bufptr items from the next buffer
C
	  DO I=1,BUFPTR
	    GRPHDR(I+BUFLEFT)=BUFFER(I)
	  END DO
	  CALL NATRGP(FCA,FCAPT,GRPHDR,I_GRPHDR,1,BUFPTR,BUFFER,
	1		PCOUNT,U,V,W,BASEL,
	1		UT,FLAG,BIN,IFNO,SRCNO)
	  IF (E_C.EQ.-2) GOTO 3100
	  IF (E_C.NE.0) GOTO 20
C
C Set bufptr to the first visibility in the new buffer.
C
	  BUFPTR=BUFPTR+1
	END IF
C
C Determine GRPLENGTH
C
	IF (BASEL.EQ.-1) THEN
	  GRPLENGTH=SC_Q*SC_IF*SC_ANT
	ELSE IF (IFNO.GT.1) THEN
	  GRPLENGTH=IF_NFREQ(IFNO)*IF_NSTOK(IFNO)
	ELSE
	  GRPLENGTH=NSTOK*NFREQ
	END IF
	IF (BASEL.EQ.-1) GO TO 4000 
C
C----------------------READ VIS DATA GROUP -----------------------------
C
C READ DATA FROM FITS FILE, FORMAT FROM RPFITS IS:
C     word 1= Re(vis)
C     word 2= Imag(vis)
C     word 3= weight (if n_complex=3)
C
 3500	CONTINUE
	LAST_GOOD_UT=UT
	BUFLEFT=641-BUFPTR
	IF (BUFLEFT.GE.(N_COMPLEX*(GRPLENGTH-GRPPTR+1))) THEN 
C
C If entire group can be filled from existing buffer then do so
C
	  DO I=GRPPTR,GRPLENGTH
	    VIS(I)=CMPLX(NATXCE(BUFFER(BUFPTR)),
	1		NATXCE(BUFFER(BUFPTR+1)))
C	    IF (WRITE_WT) THEN
C	      WEIGHT(I)=NATXCE(BUFFER(BUFPTR+2))
C	    END IF
	    BUFPTR=BUFPTR+N_COMPLEX
	  END DO
	  E_C=0
C
	  RETURN
	ELSE
C
C Otherwise things are a bit more complicated, first read
C complete visibilities in old buffer.
C
	  BUFLEFT3=BUFLEFT/N_COMPLEX
	  DO I=1,BUFLEFT3
	    VIS(GRPPTR+I-1)=CMPLX(NATXCE(BUFFER(BUFPTR)),
	1		NATXCE(BUFFER(BUFPTR+1)))
C	    IF (WRITE_WT) THEN
C	      WEIGHT(GRPPTR+I-1)=NATXCE(BUFFER(BUFPTR+2))
C	    END IF
	    BUFPTR=BUFPTR+N_COMPLEX
	  END DO
	  GRPPTR=GRPPTR+BUFLEFT3
C
C Read the fraction of a visibility left in old buffer
C
	  BUFLEFT=BUFLEFT-N_COMPLEX*BUFLEFT3
	  IF (BUFLEFT.EQ.1) REVIS=NATXCE(BUFFER(640))
	  IF (N_COMPLEX.EQ.3 .AND. BUFLEFT.EQ.2) VIS(GRPPTR)=
	1		CMPLX(NATXCE(BUFFER(639)),NATXCE(BUFFER(640)))
C
C Now read in a new buffer
C
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	  FCAPT=FCAPT+2560				!UPDATE PTR
	  E_C=NATXSF(FCA,FCAPT,BUFFER)
	  IF (E_C.NE.0) THEN
	    FCAPT=FCAPT-2560				!RE-READ
	    GOTO 20
	  END IF
C
C Fill any incomplete visibility
C
	  IF (BUFLEFT.EQ.0) THEN
	    BUFPTR=1
	  ELSE IF (BUFLEFT.EQ.1) THEN 
	    VIS(GRPPTR)=CMPLX(REVIS,NATXCE(BUFFER(1)))
C	    IF (WRITE_WT) THEN
C	      WEIGHT(GRPPTR)=NATXCE(BUFFER(2))
C	    ENDIF
	    GRPPTR=GRPPTR+1
	    BUFPTR=N_COMPLEX
	  ELSE IF (BUFLEFT.EQ.2 .AND. N_COMPLEX.EQ.3) THEN
C	    IF (WRITE_WT) THEN
C	      WEIGHT(GRPPTR)=NATXCE(BUFFER(1))
C	    END IF
	    GRPPTR=GRPPTR+1
	    BUFPTR=2
	  END IF
C
C Return to pick up the rest of the group
C
	END IF
	GO TO 3500
C
C----------------------READ SYSCAL DATA GROUP --------------------------
C
C READ DATA FROM FITS FILE
C note that in this conmtext GRPLENGTH is in units of words,
C not visibilities .
C
 4000	CONTINUE
	BUFLEFT=641-BUFPTR
	IF (BUFLEFT.GE.(GRPLENGTH-GRPPTR+1)) THEN 
C
C If entire group can be filled from existing buffer then do so
C
	  DO I=GRPPTR,GRPLENGTH
	    SC_BUF(I)=NATXCE(BUFFER(BUFPTR))
	    BUFPTR=BUFPTR+1
	  END DO
	  E_C=0
C
	  RETURN
	ELSE
C
C Otherwise read complete visibilities in old buffer
C
	  DO I=1,BUFLEFT
	    SC_BUF(GRPPTR+I-1)=NATXCE(BUFFER(BUFPTR))
	    BUFPTR=BUFPTR+1
	  END DO
	  GRPPTR=GRPPTR+BUFLEFT
C
C Then read in a new buffer
C
	  IF (.NOT.WNFRD(FCA,2560,BUFFER,FCAPT)) GOTO 21 !READ A BLOCK
	  FCAPT=FCAPT+2560				!UPDATE PTR
	  E_C=NATXSF(FCA,FCAPT,BUFFER)
	  IF (E_C.NE.0) THEN
	    FCAPT=FCAPT-2560				!RE-READ
	    GOTO 20
	  END IF
	  BUFPTR=1
C
C and then return to pick up the rest of the group
C
	END IF
	GO TO 4000
C
C
	END
