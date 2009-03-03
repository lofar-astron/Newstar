C+ NCLHIM.FOR
C  WNB 910809
C
C  Revisions:
C	JPH 940224	Comments
C	CMV 950616	Account for DATAFAC in map limits
C
C
	SUBROUTINE NCLHM0(LPHAD,MPHP)
C
C  Determine histograms
C
C  Result:
C
C	CALL NCLHM0( LPHAD_J:O, MPHP_J:I)
C				Determine the map histogram for selected areas.
C				The map header is assumed at MPHP, and the
C				histogram area is returned in LPHAD.
C	CALL NCLHM9( LPHAD_J:IO)
C				Clear the map histogram
C	CALL NCLHB0( LPHAD_J:O, MPHP_J:I)
C				Determine the beam histogram.
C				The beam header is assumed at MPHP, and the
C				histogram area is returned in LPHAD.
C	CALL NCLHB9( LPHAD_J:IO)
C				Clear the beam histogram
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
	INTEGER LPHAD			!HISTOGRAM AREA
	INTEGER MPHP			!MAP HEADER POINTER
C
C  Function references:
C
	LOGICAL WNFRD			!READ DISK
	CHARACTER*32 WNTTSG		!MAP NAME
C
C  Data declarations:
C
	BYTE MPH(0:MPHHDL-1)		!MAP HEADER
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  EQUIVALENCE (MPH,MPHJ,MPHE)
	REAL BUF(0:8191)		!MAP LINE
C-
C
C HM0
C
C
C GET MAP HEADER
C
	IF (.NOT.WNFRD(FCAMAP,MPHHDL,MPH,MPHP)) THEN
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'Error reading map')
	  CALL WNGEX				!STOP PROGRAM
	END IF
C
C Allocate histogram buffer, initialise its range with max. abs. value in map
C
	CALL WNMHS8(LPHAD,+1,MAX(ABS(MPHE(MPH_MAX_E)*DATAFAC),
	1		ABS(MPHE(MPH_MIN_E)*DATAFAC)))	!GET HISTO BUFFER
C
C Fill histogram with all lines in selected areas
C
	DO I=TAREA(2,1),TAREA(3,1)		!ALL LINES
	  IF (.NOT.WNFRD(FCAMAP,LB_E*MPHJ(MPH_NRA_J),
	1		BUF,MPHJ(MPH_MDP_J)+
	1		(I+MPHJ(MPH_NDEC_J)/2)*LB_E*MPHJ(MPH_NRA_J)))
	1			GOTO 10		!ERROR
	  IF (DATAFAC.NE.1.) THEN		!NEED TO MULTIPLY
	    DO I1=1,MPHJ(MPH_NRA_J)
	       BUF(I1)=BUF(I1)*DATAFAC		!MULTIPLY BUFFER
	    END DO
	  END IF
	  J2=-32768				!START POINT
	  DO I1=1,NAREA				!ALL AREAS
	    IF (I.GE.PAREA(2,I1,1) .AND. I.LE.PAREA(3,I1,1)) THEN !THIS LINE
	      J2=MAX(J2,PAREA(0,I1,1))		!START POINT
	      J1=PAREA(1,I1,1)-J2+1		!LENGTH
	      CALL WNMHS1(LPHAD,J1,
	1		BUF(J2+MPHJ(MPH_NRA_J)/2)) !HISTO DATA
	      J2=PAREA(1,I1,1)+1		!NEXT START POINT
	    END IF				!END SUB-AREA
	  END DO				!END SUB AREAS
	END DO					!END LINES
C
C SHOW HISTO DATA
C
	CALL WNCFHD(F_P,4,'Node: !AS   Map: !AS(#!UJ)   Field:  !AL12',
	1		NODMAP,WNTTSG(MAPNAM,0),
	1		MPH(MPH_SETN_1),MPH(MPH_FNM_1))
	CALL WNCFHD(F_P,5,' ')
	CALL WNMHS3(LPHAD,1,F_P)		!SHOW HISTOGRAM
	CALL WNMHS4(LPHAD,R0,F_P)		!GET NOISE
	CALL WNCFHD(F_P,-4,' ')			!DELETE HEADER
	CALL WNCFHD(F_P,-5,' ')
C
	RETURN
C
C HM9
C
	ENTRY NCLHM9(LPHAD)
C
	CALL WNMHS9(LPHAD)			!CLEAR HISTOGRAM
C
	RETURN
C
C HB0
C
	ENTRY NCLHB0(LPHAD,MPHP)
C
C GET beam HEADER: MPHP is the beam header pointer
C
	IF (.NOT.WNFRD(FCAMAP,MPHHDL,MPH,MPHP)) THEN
 20	  CONTINUE
	  CALL WNCTXT(F_TP,'Error reading beam')
	  CALL WNGEX				!STOP PROGRAM
	END IF
C
C Allocate a histogram buffer whose length is 1/2* the minimum of the
C  horizontal size and the vertical size of the beam
C
	CALL WNMHB0(LPHAD,+1,
	1		MIN(MPHJ(MPH_NRA_J),MPHJ(MPH_NDEC_J))/2) !GET BUFFER
C
C DO ALL LINES in the lower half of the beam
C
	I1=MIN(2*TAREA(2,0),MPHJ(MPH_NRA_J))	!LENGTH LINE
	DO I=-MIN(TAREA(3,0),MPHJ(MPH_NDEC_J)/2),0 !ALL NEGATIVE LINES
	  IF (.NOT.WNFRD(FCAMAP,LB_E*MPHJ(MPH_NRA_J),
	1		BUF,MPHJ(MPH_MDP_J)+
	1		(I+MPHJ(MPH_NDEC_J)/2)*LB_E*MPHJ(MPH_NRA_J)))
	1			GOTO 20		!ERROR
	  CALL WNMHB1(LPHAD,I1,
	1		BUF(-I1/2+MPHJ(MPH_NRA_J)/2),I) !HISTO DATA
	END DO					!END LINES
C
C SHOW HISTO DATA
C
	CALL WNCFHD(F_P,4,'Node: !AS   Beam: !AS(#!UJ)   Field:  !AL12',
	1		NODAP,WNTTSG(APNAM,0),
	1		MPH(MPH_SETN_1),MPH(MPH_FNM_1))
	CALL WNCFHD(F_P,5,' ')
	CALL WNMHB2(LPHAD,F_P)			!SHOW HISTOGRAM
	CALL WNCFHD(F_P,-4,' ')			!DELETE HEADER
	CALL WNCFHD(F_P,-5,' ')

C
	RETURN
C
C HB9
C
	ENTRY NCLHB9(LPHAD)
C
	CALL WNMHB9(LPHAD)			!CLEAR HISTOGRAM
C
	RETURN
C
C
	END