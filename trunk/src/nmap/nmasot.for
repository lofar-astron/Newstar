C+ NMASOT.FOR
C  WNB 910304
C
C  Revisions:
C	WNB 911009	Cater for multiple polarisations correctly
C
	SUBROUTINE NMASOT(FCA,BAD,BUFR,BUFJ,NI,NP,CSD,UVD,APD)
C
C  Output sorted UV data
C
C  Result:
C
C	CALL NMASOT (FCA_J:I, BAD_J(4,0:*):IO, BUFR_E(0:*,0:*):IO,
C				BUFJ_J(0:*,0:*):IO,
C				NI_J:I, NP_J:I, CSD_X(0:*,0:3):I,
C				UVD_E(0:1,0:*):I, APD_E(0:*):I)
C			Output sorted UV data to temporary file.
C			CALL NMASOI to prepare buffers first, NMASON to
C			finish after last.
C			FCA is the file control, BAD the buffer admini-
C			stration, BUF the output buffers, NI the number of
C			input points, NP the number of polarisations,
C			CSD, UVD, APD resp. the data, the UV-
C			coordinates and the antenna pattern weight.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'NMA_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!OUTPUT FILE CONTROL
	INTEGER BAD(4,0:*)		!BUFFER ADMINISTRATION
	REAL BUFR(0:MXSBJ-1,0:*)	!OUTPUT BUFFERS
	INTEGER BUFJ(0:MXSBJ-1,0:*)	!OUTPUT BUFFERS
	INTEGER NI			!# OF INPUT POINTS
	INTEGER NP			!# OF POLARISATIONS
	REAL CSD(0:1,0:STHIFR-1,0:*)	!DATA
	REAL UVD(0:1,0:*)		!UV COORDINATES
	REAL APD(0:*)			!ANTENNA PATTERN WEIGHT
C
C  Function references:
C
	INTEGER WNFEOF			!CURRENT FILE END
	LOGICAL WNFWR			!DISK WRITING
C
C  Data declarations:
C
C-
	DO I=0,NI-1				!DO ALL INPUT POINTS
	 IF (APD(I).NE.0) THEN			!PRESENT
	  I3=INT(UVD(0,I)/BINSIZ)		!GET BIN
	  IF (I3.LT.NBIN) THEN			!CAN FIT
	    I2=BAD(1,I3)			!PTR IN BIN BUF
	    I1=BAD(2,I3)			!CURRENT BUF
	    BUFR(I2,I1)=UVD(0,I)		!SET U,V
	    BUFR(I2+1,I1)=UVD(1,I)
	    BUFR(I2+2,I1)=APD(I)		!SET ANTENNA WEIGHT
	    DO I4=0,NP-1
	      BUFR(I2+3+2*I4,I1)=CSD(0,I,I4)	!SET DATA
	      BUFR(I2+4+2*I4,I1)=CSD(1,I,I4)
	    END DO
	    BAD(1,I3)=BAD(1,I3)+3+2*NP		!UPDATE PTR IN BUF
	    IF (BAD(1,I3).GE.BAD(3,I3)) THEN	!BUF FULL
	      J=WNFEOF(FCA)			!CURRENT FILE END
	      IF (.NOT.WNFWR(FCA,MXSBB,BUFJ(0,I1),J)) THEN !WRITE
		CALL WNCTXT(F_TP,'Write error sorted UV data')
		CALL WNGEX			!END PROGRAM
	      END IF
	      BAD(1,I3)=0			!BUF PTR
	      BUFJ(MXSBJ-1,I1)=J		!POINT TO PREVIOUS
	      BAD(4,I3)=J			!SAVE LAST
	    END IF
	  END IF
	 END IF
	END DO
C
	RETURN
C
C
	END
