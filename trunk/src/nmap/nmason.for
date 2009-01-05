C+ NMASON.FOR
C  WNB 910304
C
C  Revisions:
C	WNB 950809	Add BUFPP
C       WNB 950817	Larger BUFPP
C
	SUBROUTINE NMASON(FCA,BAD,BUF,BUFPP)
C
C  Finish UV sorting
C
C  Result:
C
C	CALL NMASON( FCA_J:I, BAD_J(4,0:*):IO, BUF_J(0:*):I, BUFPP_J:I)
C				Finish UV data sorting. BAD is the buffer
C				administration, BUF the buffers, FCA the
C				output. BUFPP points to FT Pol. Int buffer.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMA_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE CONTROL
	INTEGER BAD(4,0:*)		!BUFFER ADMINISTRATION
	INTEGER BUF(0:MXSBJ-1,0:*)	!BUFFERS
	INTEGER BUFPP			!POL INT FT BUFFER
C
C  Function references:
C
	INTEGER WNFEOF			!FILE END
	LOGICAL WNFWR			!WRITE DISK
	LOGICAL WNFPUR			!PURGE BUFFERS TO FILE
C
C  Data declarations:
C
C-
	DO I3=0,NBIN-1				!EMPTY BUFFERS
	  I1=BAD(2,I3)				!CURRENT BUF #
	  IF (BAD(1,I3).GT.0) THEN		!BUF NOT EMPTY
	    IF (BAD(1,I3).LT.BAD(3,I3)) THEN	!BUFFER NOT FULL
	      BUF(BAD(1,I3)+2,I1)=0		!SET APD=0 TO INDICATE END
	    END IF
	    J=WNFEOF(FCA)			!CURRENT FILE END
	    IF (.NOT.WNFWR(FCA,MXSBB,BUF(0,I1),J)) THEN !WRITE
 10	      CONTINUE
	      CALL WNCTXT(F_TP,'Write error sorted UV data')
	      CALL WNGEX			!LEAVE PROGRAM
	    END IF
	    BAD(4,I3)=J				!SAVE LAST
	  END IF
	END DO
C
	IF (.NOT.WNFPUR(FCA)) GOTO 10		!WRITE ALL TO DISK
C
	CALL WNGFVM(NBIN*MXSBB,BINBUF)		!RELEASE BUFFERS
	CALL WNGFVA((3*LB_X+2*LB_E+LB_J)*NPTRF,BUFPP)
C
	RETURN
C
C
	END