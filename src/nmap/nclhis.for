C+ NCLHIS.FOR
C  WNB 910809
C
C  Revisions:
C	WNB 920103	Add HID
C	WNB 920131	Change accumulation
C	JPH 940224	Comments
C
C
	SUBROUTINE NCLHIS(MPHP,APHP)
C
C  Determine histograms
C
C  Result:
C
C	CALL NCLHIS ( MPHP_J:I, APHP_J:I)
C				Determine the beam and map histograms.
C				The MAP is assumed at MPHP, the beam
C				at APHP. Dynamic memmory allocated for the 
C				histograms is pointed at by
C				COMMON variables BMHAD and MPHAD, which are
C				indices into the array A_J
C	CALL NCLHID ( MPHP_J:I, APHP_J:I)
C				Determine histograms as above plus the beam
C				patch size and associated parameters in
C				COMMON variables:
C				  BEMPAT	beam-patch size
c				  MAPLIM	map-data limit in patch
C				  MAPPAT	nr of pts in map patch
C				  CLBXLM	max. corrn. outside patch
C	CALL NCLHIE ( MPHP_J:I, APHP_J:I)
C				Determine beam patch size (BEMPAT) and 
C				 associated parameters
C	CALL NCLHIX ( MPHP_J:I, APHP_J:I)
C				Deallocate beam histogram buffer
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'		!MAP HEADER
	INCLUDE 'NCL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER MPHP			!MAP POINTER
	INTEGER APHP			!AP POINTER
C
C  Function references:
C
C
C  Data declarations:
C
	LOGICAL LHID			!HID SWITCH
	INTEGER BMSIZ,MPSIZ		!HISTO SIZES
	INTEGER BMPTR,MPPTR		!HISTO POINTERS
	REAL MPMAX			!MAP HISTO DATA
C-
C
C HIS
C
	LHID=.FALSE.					!NOT PATCH
	GOTO 10
C
C HID
C
	ENTRY NCLHID(MPHP,APHP)
C
	LHID=.TRUE.
	GOTO 10
C
C make BEAM HISTOGRAM
C
 10	CONTINUE
	IF (APHP.NE.0) THEN
	  CALL NCLHB0(BMHAD,APHP)		!make histogram of entire beam,
						! BMHAD points to histogram bufr
	  IF (LHID) 
	1	CALL WNMHB6(BMHAD,BMSIZ,BMPTR)	!make cumulative BEAM histogram,
						! set size and pointer for it,
						! but do nothing with it yet
	END IF
C
C make MAP HISTOGRAM
C
	IF (MPHP.NE.0) THEN			!MAKE MAP HISTO
	  CALL NCLHM0(MPHAD,MPHP)		!MPHAD points to histogram bufr
	END IF
	GOTO 30
C
C HIE
C
	ENTRY NCLHIE(MPHP,APHP)
C
	LHID=.TRUE.
	GOTO 30
C
C GET PATCH
C NOTE: THe lower-case comments in this section are tentative,
C  it is not really clear what happers here
C
 30	CONTINUE
	IF (LHID .AND. MPHP.NE.0 .AND. APHP.NE.0) THEN
	  CALL WNMHB7(BMHAD,BMSIZ,BMPTR)	!GET len and ptr to beam hgram
	  CALL WNMHS6(MPHAD,MPSIZ,MPPTR,	!GET len and ptr to cumulative 
						! map hgram. plus the maximum
	1	MPHMXI,MPMAX)			!  value found and the range
						!   of the histogram 
	  J=MNBPAT				!MIN. BEAM PATCH
C
C MNBPAT (=3) defines the smallest patch size allowed
C MXBPAT (=...) defines an absolute largest patch size
C The patch can not be larger than the beam and is further limited by the
C horizontal and vertical sizes of the union of all selected areas
C
	  DO I=MNBPAT,MIN(BMSIZ-1,MXBPAT,
	1	MAX(TAREA(2,0),TAREA(3,0)))	!try successively larger patches
	    R0=A_E(BMPTR+I)			!BEAM FRACTION TO CATER = 
						! largest beam value within
						! patch size being tried
	    I0=INT(R0*MPSIZ*MPHMXI/MPMAX)	!CORRESPONDING MAP PTR
	    IF ((2*I+1)*(I+1)+2*A_J(MPPTR+I0)
	1	.GT.MEMSIZ/LB_E) GOTO 20 	!TOO LARGE
	    J=I					!CAN FIT MORE
	    IF ((I0*MPMAX)/MPSIZ
	1	.LT.MPDEP*MPHMXI) GOTO 20 	!DEEP ENOUGH
	  END DO
C
 20	  CONTINUE
	  BEMPAT=J
C
C We now have a beam patch size
C
	  I0=INT(A_E(BMPTR+J)*MPSIZ*MPHMXI/MPMAX)!MAP HISTO PTR
	  IF (BEMPAT.EQ.MNBPAT) THEN		!LIMIT TO CYCLE DEPTH
	    DO WHILE(I0.LT.MPSIZ .AND.
	1	(I0*MPMAX)/MPSIZ.LT.
	1	MPDEP*MPHMXI)
	      I0=I0+1
	    END DO
	    I0=MAX(0,I0-1)			!GO ONE BACK
	  END IF
	  MAPLIM=(I0*MPMAX)/MPSIZ		!MAP LIMIT
	  MAPPAT=A_J(MPPTR+I0)			!# OF POINTS IN BUF
	  CLBXLM=A_E(BMPTR+J+1)			!MAX. CORRECTION OUTSIDE
	END IF
C
C CLEAR HISTOGRAMS
C
	IF (MPHP.NE.0) THEN
	  CALL NCLHM9(MPHAD)			!free MAP HISTO buffer
	END IF
	GOTO 40
C
C HIX
C
	ENTRY NCLHIX(MPHP,APHP)
C
	LHID=.FALSE.
	GOTO 40
C
C CLEAR BEAM HISTOGRAM
C
 40	CONTINUE
	IF (APHP.NE.0 .AND. .NOT.LHID) THEN
	  CALL NCLHB9(BMHAD)			!free BEAM HISTO buffer
	END IF
C
	RETURN
C
C
	END
