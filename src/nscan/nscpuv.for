C+ NSCPUV.FOR
C  WNB 910226
C
C  Revisions:
C	JPH 941213	No error message for wildcard disk labels 
C	CMV 950130	Print summary listing if more than one label
C
	SUBROUTINE NSCPUV
C
C  Formatted print UVFITS
C
C  Result:
C
C	CALL NSCPUV		will print a formatted FITS file
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NSC_DEF'
C
C  Parameters:
C
	INTEGER LL,LLD2,LR		!RECORD AND CARD IMAGE LENGTHS
	  PARAMETER (LL=80,LLD2=LL/2,LR=2880)
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNFOP,WNFOPF		!OPEN FILE
	LOGICAL WNFRD			!READ FILE
	INTEGER WNCALN			!STRING LENGTH
	LOGICAL WNCASA			!TEST IF ALPHABETIC
	INTEGER WNCAJ			!GET INTEGER FROM STRING
C
C  Data declarations:
C
	INTEGER CLAB			!INDEX FOR LABEL
	CHARACTER*160 INFILE		!FILE NAME
	INTEGER BTP			!BITPIX
	LOGICAL EXTS			!EXTENSION SEEN
	INTEGER EXTC			!EXTENSION COUNT
	INTEGER EXTN			!FIELD WIDTH
	INTEGER EXTR			!# OF ENTRIES
	INTEGER EP(0:30)		!BUFFER POINTER/AXIS LENGTH
	CHARACTER CN(0:30)*8		!AXIS NAMES
	DOUBLE PRECISION CR(1:3,0:30)	!REFERENCE POSITION
	CHARACTER*300 EXTTP		!OUTPUT CODE
	INTEGER*2 EXTCD(0:1,0:30)	!TRANSLATE EXTENSION
	CHARACTER*256 EXTF		!EXTENSION FORMAT
	INTEGER*2 TRBUF(0:3)		!TRANSL. BUF
	  DATA TRBUF/2,LLD2,0,0/
	CHARACTER*(LL) BUFTC
	  BYTE BUFTCB(LL)
	  EQUIVALENCE (BUFTC,BUFTCB)
	  DATA BUFTCB /LL*0/
	BYTE BUF(0:LR-1)
	  CHARACTER*(LR) BUFC
	  INTEGER*2 BUFI(0:LR/2-1)
	  INTEGER BUFJ(0:LR/4-1)
	  REAL BUFE(0:LR/4-1)
	  REAL BUFD(0:LR/8-1)
	  EQUIVALENCE(BUF,BUFC,BUFI,BUFJ,BUFE,BUFD)
C-
C
C PRELIMINARIES
C
C
	CLAB=1						!Nothing done yet
	IF (NLAB(1).NE.1) THEN
	   CALL WNCTXT(F_TP,' Lbl    Object             '//
	1	'BtP Axes  Comment')
	END IF
C
  100	CONTINUE					!Loop over labels
	IF (NLAB(1).LE.0) THEN				!Wildcard
	  J0=CLAB					!All labels
	ELSE IF (CLAB.LE.NLAB(1)) THEN
	  J0=ILAB(CLAB,1)
	ELSE
	  GOTO 900
	END IF
	CLAB=CLAB+1					!Count this one
C
	IF (UNIT.EQ.'D') THEN				!DISK
	  CALL WNCTXS(INFILE,'!AS\.!6$ZJ',IFILE,J0)
	  IF (.NOT.WNFOP(IMCA,INFILE(1:WNCALN(INFILE)),'R')) THEN
	    IF (NLAB(1).GT.0)
	1	CALL WNCTXT(F_TP,'Cannot find file !AS',INFILE)
	    GOTO 900					!ERROR END
	  END IF
	ELSE						!TAPE
	  IF (.NOT.WNFOPF(IMCA,' ','R',0,0,0,J0)) THEN
	    CALL WNCTXT(F_TP,'Cannot find label !UJ',J0)
	    GOTO 900
	  END IF
	END IF
C
C Long listing if single label
C
	IF (NLAB(1).EQ.1) THEN
	  CALL WNCFHD(F_P,3,'!40C\Listing of !AS, label !UJ',IFILE,J0)
	  CALL WNCFHD(F_P,4,' ')
	  CALL WNCFHD(F_P,5,' ')
	  CALL WNCTXT(F_P,'!^')
	  J=0						!DATA POINTER
	  EXTS=.FALSE.					!NO EXTENSION SEEN
C
C DO DATA
C
	  DO WHILE(WNFRD(IMCA,LL,BUF,J))		!READ LINE
	    J=J+LL
 10	    CONTINUE
	    IF (BUFC(8:8).EQ.'=' .OR. BUFC(1:4).EQ.'END ' .OR.
	1	BUFC(1:4).EQ.'HIST' .OR. BUFC(1:4).EQ.'COMM') THEN !TEXT
	       CALL WNCTXT(F_P,'!4$XJ/!4$XJ !AL80',
	1		MOD(J-LL,LR),(J-LL)/LR,BUF)
	      IF (BUFC(1:8).EQ.'XTENSION') THEN		!EXTENSION LINES
	        EXTS=.TRUE.				!SET SEEN
	        EXTC=0					!EMPTY FORMAT
	        EP(0)=0					!OFFSET
	        EXTTP=' '				!CONVERSION
	      END IF
	      IF (EXTS) THEN				!EXTENSION SEEN
	        IF (BUFC(1:5).EQ.'TFORM') THEN		!FORMAT
	   	  I1=12					!POINTER
	 	  I1=I1-1
		  I=WNCAJ(BUFC(1:LL),LL,I1)		!REPEAT FACTOR
		  I1=I1+1
		  IF (I.GT.0) THEN			!SHOULD DO
		    EXTCD(1,EXTC)=I			!SET TRANSLATION
		    IF (BUFC(I1:I1).EQ.'A') THEN
		      EXTCD(0,EXTC)=9
		      EP(EXTC+1)=EP(EXTC)+I*1
		      I1=WNCALN(EXTTP)+2
		      CALL WNCTXS(EXTTP(I1:),'!!\!UJ$AL!UJ',I,I)
		    ELSE IF (BUFC(I1:I1).EQ.'I') THEN
		      EXTCD(0,EXTC)=2
		      EP(EXTC+1)=EP(EXTC)+I*2
		      I1=WNCALN(EXTTP)+2
		      CALL WNCTXS(EXTTP(I1:),'!!\!UJ\SI',I)
		    ELSE IF (BUFC(I1:I1).EQ.'J') THEN
		      EXTCD(0,EXTC)=3
		      EP(EXTC+1)=EP(EXTC)+I*4
		      I1=WNCALN(EXTTP)+2
		      CALL WNCTXS(EXTTP(I1:),'!!\!UJ\SJ',I)
		    ELSE IF (BUFC(I1:I1).EQ.'E') THEN
		      EXTCD(0,EXTC)=4
		      EP(EXTC+1)=EP(EXTC)+I*4
		      I1=WNCALN(EXTTP)+2
		      CALL WNCTXS(EXTTP(I1:),'!!\!UJ\E',I)
		    ELSE IF (BUFC(I1:I1).EQ.'D') THEN
		      EXTCD(0,EXTC)=5
		      EP(EXTC+1)=EP(EXTC)+I*8
		      I1=WNCALN(EXTTP)+2
		      CALL WNCTXS(EXTTP(I1:),'!!\!UJ\D',I)
		    END IF
		    EXTC=EXTC+1
		    EXTCD(0,EXTC)=0
		    EXTCD(1,EXTC)=0
		  END IF
	        END IF
	        IF (BUFC(1:6).EQ.'NAXIS1') THEN		!FIELD WIDTH
	  	  I1=12
		  CALL WNCASB(BUFC,I1)
		  CALL WNCACJ(BUFC,I1,10,EXTN)
	        END IF
	        IF (BUFC(1:6).EQ.'NAXIS2') THEN		!# OF ENTRIES
	  	  I1=12
		  CALL WNCASB(BUFC,I1)
		  CALL WNCACJ(BUFC,I1,10,EXTR)
	        END IF
	      END IF
	    ELSE IF (BUFC(1:LL).EQ.BUFTC) THEN		!0 LINE
	      CALL WNCTXT(F_P,'!4$XJ/!4$XJ lines with zeroes',
	1	  	MOD(J-LL,LR),(J-LL)/LR)
	      DO WHILE (WNFRD(IMCA,LL,BUF,J))
	        J=J+LL
	        IF (BUFC(1:LL).NE.BUFTC) GOTO 10
	      END DO
	    ELSE IF (.NOT.EXTS) THEN			!DATA
	      J1=0					!CNT LINES
	      CALL WNTTTL(LL,BUFI,TRBUF,5)		!MAKE FORMAT
	      CALL WNCTXT(F_P,'!132$11Q1!4$XJ/!4$XJ !6$#SI',
	1		MOD(J-LL,LR),(J-LL)/LR,LL/2,BUFI)
	      J1=J1+1
 20	      CONTINUE
	      DO WHILE (WNFRD(IMCA,LL,BUF,J))
	        J=J+LL
	        IF (BUFC.EQ.BUFTC) GOTO 10
	        IF (BUFC(8:8).EQ.'=' .OR. BUFC(1:4).EQ.'END ' .OR.
	1	    BUFC(1:4).EQ.'HIST' .OR. BUFC(1:4).EQ.'COMM') GOTO 10 !TEXT
	        IF (J1.LT.10) THEN
	           CALL WNTTTL(LL,BUFI,TRBUF,5)		!MAKE FORMAT
	          CALL WNCTXT(F_P,'!132$11Q1!4$XJ/!4$XJ !6$#SI',
	1		MOD(J-LL,LR),(J-LL)/LR,LL/2,BUFI)
	        ELSE IF (J1.EQ.10) THEN
	          CALL WNCTXT(F_P,'!4$XJ/!4$XJ ....',
	1		MOD(J-LL,LR),(J-LL)/LR)
	        END IF
	        J1=J1+1
	      END DO
	    ELSE					!EXTENSION
	      J1=J-LL					!REPOSITION
	      DO I=1,MIN(2,EXTR)			!2 FIELDS
	        IF (.NOT.WNFRD(IMCA,EXTN,BUF(0),J1)) GOTO 900
	        J1=J1+EXTN
	        CALL WNTTTL(EXTN,BUF,EXTCD,5)		!CORRECT FORMAT
	        CALL WNCTXT(F_P,'!132$11Q!4$XJ/!4$XJ'//EXTTP,
	1		MOD(J-LL,LR),(J-LL)/LR,
	1		BUF(EP(0)),BUF(EP(1)),BUF(EP(2)),BUF(EP(3)),
	1		BUF(EP(4)),BUF(EP(5)),BUF(EP(6)),BUF(EP(7)),
	1		BUF(EP(8)),BUF(EP(9)),BUF(EP(10)),BUF(EP(11)),
	1		BUF(EP(12)),BUF(EP(13)),BUF(EP(14)),BUF(EP(15)),
	1		BUF(EP(16)),BUF(EP(17)),BUF(EP(18)),BUF(EP(19)))
	      END DO
	      EXTS=.FALSE.
	      J1=10
	      GOTO 20					!CONTINUE
	    END IF					!END TEXT READ
	  END DO					!END READ
C
C More than one label: shortlist
C
	ELSE
	  J=0						!DATA POINTER
	  EXTS=.FALSE.					!NO EXTENSION SEEN
	  EXTC=0
	  EXTF='Unknown'
	  EXTTP=' '
	  DO I=1,30
	    EP(I)=0
	    CN(I)='Unknown'
	    CR(1,I)=0
	    CR(2,I)=0
	    CR(3,I)=0
	  END DO
C
	  DO WHILE(WNFRD(IMCA,LL,BUF,J))		!READ LINE
	    J=J+LL
C
	    IF ((BUFC(1:8).EQ.'COMMENT '.OR.
	1	 BUFC(1:8).EQ.'HISTORY ').AND.EXTTP.EQ.' ') THEN
	      I=11					!CHECK IF FILLED
	      DO WHILE (I.LT.80.AND. .NOT.WNCASA(BUFC,I))
	         I=I+1
	      END DO
	      IF (I.LT.80) EXTTP=BUFC(10:)		!FIRST COMMENT
C
	    ELSE IF (BUFC(8:8).EQ.'=') THEN		!TEXT
C
	      IF (BUFC(1:8).EQ.'XTENSION') THEN		!EXTENSION LINES
C
	        IF (EXTC.EQ.0) THEN			!NOT YET PRINTED
	           CALL WNCTXT(F_TP,
	1		'!/!Q1!4$UJ !20$AS !3$UJ !3$UJ !AS',
	1		   J0,EXTF,BTP,EP(0),EXTTP)
		   DO I=1,EP(0)
	              CALL WNCTXT(F_TP,
	1	'        Axis !3$UJ: !-8$AS  !4$UJ pts !D = !D (!D)',
	1		I,CN(I),EP(I),CR(1,I),CR(2,I),CR(3,I))
		   END DO
	        END IF
C
	        EXTS=.TRUE.				!SET SEEN
	        EXTC=EXTC+1				!AND COUNT
	        EXTF=BUFC(11:21)			!SAVE TYPE
	      ELSE IF (EXTS) THEN			!FIND EXTN. NAME
	         IF (BUFC(1:7).EQ.'EXTNAME') THEN
	            CALL WNCTXT(F_TP,
	1			'     Extension !3$UJ - !AS (!AS)',
	1			EXTC,BUFC(11:21),EXTF)
	         ENDIF
	         EXTS=.FALSE.				!SET PRINTED
C
	      ELSE IF (BUFC(1:6).EQ.'BITPIX') THEN	!BITS/PIXEL
	        I1=10
	        CALL WNCACJ(BUFC,I1,10,BTP)
	      ELSE IF (BUFC(1:6).EQ.'NAXIS ') THEN	!NUMBER OF AXES
	        I1=10
	        CALL WNCACJ(BUFC,I1,10,EP(0))
	      ELSE IF (BUFC(1:5).EQ.'NAXIS') THEN	!AXIS LENGTH
	        I1=6
	        CALL WNCACJ(BUFC,I1,10,I2)
	        IF (I2.GT.0.AND.I2.LE.30) THEN		!AT MOST 30 AXES
	           I1=10
	           CALL WNCACJ(BUFC,I1,10,EP(I2))
	        END IF
	      ELSE IF (BUFC(1:5).EQ.'CTYPE') THEN	!AXIS NAME
	        I1=6
	        CALL WNCACJ(BUFC,I1,10,I2)
	        IF (I2.GT.0.AND.I2.LE.30) THEN
	           CN(I2)=BUFC(12:19)
	        END IF
	      ELSE IF (BUFC(1:5).EQ.'CRPIX') THEN	!REFERENCE PIXEL
	        I1=6
	        CALL WNCACJ(BUFC,I1,10,I2)
	        IF (I2.GT.0.AND.I2.LE.30) THEN
	           I1=10
	           CALL WNCACD(BUFC,I1,10,CR(1,I2))
	        END IF
	      ELSE IF (BUFC(1:5).EQ.'CRVAL') THEN	!REFERENCE VALUE
	        I1=6
	        CALL WNCACJ(BUFC,I1,10,I2)
	        IF (I2.GT.0.AND.I2.LE.30) THEN
	           I1=10
	           CALL WNCACD(BUFC,I1,10,CR(2,I2))
	        END IF
	      ELSE IF (BUFC(1:5).EQ.'CDELT') THEN	!SPACING
	        I1=6
	        CALL WNCACJ(BUFC,I1,10,I2)
	        IF (I2.GT.0.AND.I2.LE.30) THEN
	           I1=10
	           CALL WNCACD(BUFC,I1,10,CR(3,I2))
	        END IF

	      ELSE IF (BUFC(1:6).EQ.'OBJECT') THEN	!OBJECT NAME
		EXTF=BUFC(11:31)
	      END IF
	   END IF
	  END DO					!END READ
C
	        IF (EXTC.EQ.0) THEN			!NOT YET PRINTED
	           CALL WNCTXT(F_TP,
	1		'!/!Q1!4$UJ !20$AS !3$UJ !3$UJ !AS',
	1		   J0,EXTF,BTP,EP(0),EXTTP)
		   DO I=1,EP(0)
	              CALL WNCTXT(F_TP,
	1	'        Axis !3$UJ: !-8$AS  !4$UJ pts !D = !D (!D)',
	1		I,CN(I),EP(I),CR(1,I),CR(2,I),CR(3,I))
		   END DO
	        END IF
C
	END IF
	GOTO 100
C
C END
C
 900	CONTINUE
	CALL WNFCL(IMCA)
	CALL WNCFHD(F_P,-3,' ')
C
	RETURN
C
C
	END