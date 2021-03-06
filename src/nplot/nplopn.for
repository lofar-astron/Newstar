C+ NPLOPN.FOR
C  HjV 931108
C       Combined parts of old version of NPLTEL and NPLRES
C
C  Revisions:
C       HjV 940224      Add mosaik test
C       HjV 940428	Add plotting of IF data
C       HjV 940503	Change PLOT SCALE for IF data
C       HjV 940530	Plot different datatypes on one page
C	HjV 960415	Option to stop during loop or more plots per page
C	JPH 960730	Data types AGAIN and PGAIN: Scales % and deg
C	JPH 9611..	Plot annotation
C	JPH 970129	Move annotation to left side, Sector: closer to plot
C
C
	SUBROUTINE NPLOPN (LDATTP,IPOL,NHV,IFRS)
C
C  Open plot and plot heading
C
C  Result:
C
C	CALL NPLOPN		Open plot and plot heading
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NPL_DEF'
	INCLUDE 'WND_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER LDATTP			!CURRENT DATA TYPE
	INTEGER IPOL
	INTEGER NHV(0:1)		!# OF PAGES
	INTEGER IFRS(0:1)		!CURRENT IFR'S
C
C  Function references:
C
	LOGICAL WQ_MPAGE		!OPEN PLOT DEVICE
	CHARACTER*32 WNTTSG             !SHOW SET NAME
        LOGICAL WNDSTM                  !DECODE SET SPECIFICATION
	INTEGER WNCALN                  !STRING LENGTH
C
C  Data declarations:
C
	CHARACTER*6 CORTXT(0:7)		!CORRECTION TEXT
	  DATA CORTXT /' ','R','A','R+A','O','R+O','A+O','R+A+O'/
	  CHARACTER*6 CORT
	CHARACTER*2 POLNAM(0:3)		!NAME OF POLARISATION
	  DATA POLNAM/'XX','XY','YX','YY'/
	INTEGER MXNHV(0:1)		!MAX. # OF PAGES
	INTEGER XINTPP,YINTPP           !SIZE PER PLOT
	INTEGER XOFF,YOFF               !PLOT POSITION
	CHARACTER*2 IFRNM               !NAME IFR
	CHARACTER*32 DUSER              !USERNAME
	CHARACTER*80 LINE               !DATATYPE OR DECODED SETS
C
C TAREA = SPACE FOR PLOT + AXIS-INFO
C PAREA = SPACE FOR PLOT
C-
C
C MORE PLOTS ON ONE PAGE ???
C
	IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	  MXNHV(0)=1		                        !SET MAX. # OF PAGES
	  MXNHV(1)=1
	  PG(1,1)=0.					!TOTAL AREA
	  PG(2,1)=0.
	  PG(1,2)=XWND
	  PG(2,2)=YWND
	  XINTPP=INT(XWND/PPP(1))
	  YINTPP=INT((YWND-YFAC*20.+1)/PPP(2))
	  PPPNR=PPPNR+1
	  IF (PPPNR.EQ.(PPP(1)*PPP(2))) THEN
	     CALL NPLCLO(DQID,NHV)
	     IF (NO_MORE) RETURN			!USER SAID: STOP
	     PPPNR=0
	  ENDIF
	  XOFF=MOD(PPPNR,PPP(1))
	  YOFF=INT(PPPNR/PPP(1))
	  TAREA(0)=XOFF*XINTPP
	  TAREA(1)=(PPP(2)-YOFF-1)*YINTPP+5.
	  TAREA(2)=(XOFF+1)*XINTPP
	  TAREA(3)=(PPP(2)-YOFF)*YINTPP-5.
	  PAREA(1)=TAREA(1)
	  PAREA(2)=TAREA(2)
	  PAREA(3)=TAREA(3)
	  IF (PLOTAP) THEN
	    PAREA(0)=TAREA(0)+70./PPP(1)
	    IF (IFR_MODE.EQ.'BAND') THEN
	      PAREA(2)=TAREA(2)-220./PPP(1)
	    ELSE
	      PAREA(2)=PAREA(0)+360.*(HARA(1)-HARA(0))/HASC*XFAC
	    END IF
	  ELSE
	    PAREA(0)=TAREA(0)+50./PPP(1)
	    IF (IFR_MODE.EQ.'BAND') THEN
	      PAREA(1)=TAREA(1)+50/PPP(2)*YFAC
	    ELSE
	      PAREA(1)=TAREA(1)-1.-(50./PPP(2)+360*(HARA(1)-HARA(0))/HASC)*YFAC
	    END IF
	  ENDIF
	ELSE
C
C MAKE PAGES AND OVERLAP CROSSES
C
	  XINTPP=0
	  YINTPP=0
	  XOFF=0
	  YOFF=0
	  PPPNR=0
	  IF (PLOTAP) THEN
	    MXNHV(0)=MXNPAG				!SET MAX. # OF PAGES
	    MXNHV(1)=1
	    PG(1,1)=0.					!TOTAL AREA
	    PG(2,1)=0.
	    IF (IFR_MODE.EQ.'BAND') THEN
	      PG(1,2)=XWND-220.
	    ELSE
	      PG(1,2)=360.*(HARA(1)-HARA(0))/HASC*XFAC
	    END IF
	    PG(2,2)=YWND
	  ELSE
	    MXNHV(0)=1					!MAX. # OF PAGES
	    MXNHV(1)=MXNPAG
	    PG(1,1)=0					!TOTAL AREA
	    IF (IFR_MODE.EQ.'BAND') THEN
	      PG(2,1)=0.
	    ELSE
	      PG(2,1)=YWND-1.-(50.+360.*(HARA(1)-HARA(0))/HASC)*YFAC
	    END IF
	    PG(1,2)=XWND-1.
	    PG(2,2)=YWND
	  END IF
	  TAREA(0)=PG(1,1)                              !PLOT AREA.
	  TAREA(1)=PG(2,1)+1.
	  TAREA(2)=PG(1,2)
	  TAREA(3)=PG(2,2)-YFAC*20.+1.
	  IF (PLOTAP) THEN
	    PAREA(0)=TAREA(0)+70.
	    PAREA(2)=TAREA(2)+70.
	  ELSE
	    PAREA(0)=TAREA(0)+50.
	    PAREA(2)=TAREA(2)
	  ENDIF
	  PAREA(1)=TAREA(1)
	  PAREA(3)=TAREA(3)
	END IF
C
C  OPEN PLOT
C
	IF (PPPNR.EQ.0) THEN
	  IF (.NOT.WQ_MPAGE(DQID,NHV,PLDEV,MXNHV,780.,PG(1,1))) THEN
	    CALL WNCTXT(F_TP,'Cannot find plotter')
	    CALL WNGEX					!STOP PROGRAM
	  END IF
C
C  PLOT HEADING
C
	  CALL WNGSGU(DUSER)                            !GET USER
	  CALL WQ_MPLR(DQID,NHV,1,1,1.,0)		!NORMAL UNITS
	  CALL WQSTXH(TXTHGT)				!TEXT HEIGHT
	  CORT=CORTXT(IAND(CORAP,7))
	  IF (((PPP(1).GT.1).OR.(PPP(2).GT.1)).AND.(NDATTP.GT.1)) THEN
	     LINE=' '
	  ELSE
	     LINE=DATTYP(LDATTP)
	  END IF
	  IF (OPT.EQ.'TEL') THEN
	    IF (IF_MODE.EQ.' ') THEN			!No IF data
	      CALL WNCTXS(TEXT,'     !AS (!AS) corrections by !AS    ',
	1		LINE,CORT,DUSER)
	    ELSE
	      CALL WNCTXS(TEXT,'     !AS (!AS) !AS by !AS    ',
	1		LINE,CORT,IF_MODE,DUSER)
	    END IF
	  ELSE IF (OPT.EQ.'RES') THEN
	    CALL WNCTXS(TEXT,'     !AS (!AS) residuals by !AS    ',
	1		LINE,CORT,DUSER)
	  ELSE IF (OPT.EQ.'DAT') THEN
	    CALL WNCTXS(TEXT,'     !AS (!AS) data by !AS    ',
	1		LINE,CORT,DUSER)
	  ELSE IF (OPT.EQ.'INT') THEN
	    CALL WNCTXS(TEXT,'     !AS (!AS) ifr.corrections by !AS ',
	1		LINE,CORT,DUSER)
	  ELSE IF (OPT.EQ.'MOD') THEN
	    CALL WNCTXS(TEXT,'     !AS (!AS) model data by !AS    ',
	1		LINE,CORT,DUSER)
	  END IF
	  CALL WQ_MDATE(DQID,NHV,TEXT)			!DATE MESSAGE
	  CALL WNCTXS(TEXT,'Node:   !AS !50CFile: !AS',NODIN,FILIN)
	  TXTXY(1)=0.					!WRITE HEADING
	  TXTXY(2)=PAREA(3)+15*YFAC
	  CALL WQTEXT(TXTXY,TEXT)			!TEXT
	  CALL WNCTXS(TEXT,'Field:  !AS !50CObs. yy.day: !2$UI\.!3$ZI',
	1      FNAM,OBSDY(2),OBSDY(1))
	  TXTXY(1)=0.					!WRITE HEADING
	  TXTXY(2)=PAREA(3)+12*YFAC
	  CALL WQTEXT(TXTXY,TEXT)			!TEXT
	  TXTXY(1)=0.					!WRITE HEADING
	  TXTXY(2)=PAREA(3)+9*YFAC
	  CALL WQTEXT(TXTXY,ANNOTN)
C
C PLOT SCALE
C
	  IF (.NOT.PLOTAP) THEN
	    IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	      I2=14
	    ELSE
	      I2=13
	    END IF
	    POINXY(1,1)=210*XFAC			!PLOT 1CM LINE BEGIN (Y)
	    POINXY(2,1)=PAREA(3)+(I2+1)*YFAC
	    POINXY(1,2)=POINXY(1,1)
	    POINXY(2,2)=PAREA(3)+(I2-1)*YFAC
	    CALL WQPOLL(2,POINXY)			!POLYLINE
	    POINXY(1,1)=210*XFAC			!PLOT 1CM. LINE (X)
	    POINXY(2,1)=PAREA(3)+I2*YFAC
	    POINXY(1,2)=220*XFAC
	    POINXY(2,2)=POINXY(2,1)
	    CALL WQPOLL(2,POINXY)			!POLYLINE
	    POINXY(1,1)=220*XFAC			!PLOT 1CM. LINE END (Y)
	    POINXY(2,1)=PAREA(3)+(I2+1)*YFAC
	    POINXY(1,2)=POINXY(1,1)
	    POINXY(2,2)=PAREA(3)+(I2-1)*YFAC
	    CALL WQPOLL(2,POINXY)			!POLYLINE
	    I2=0
	    I1=12					!Y-POSITION
	    IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1).AND.
	1	 (DATTYP(LDATTP)(1:1).EQ.'P')) I2=1
	    IF (I2.EQ.0) THEN
	      IF (OPT.EQ.'TEL') THEN
		IF (IF_MODE(1:4).EQ.'TSYS' .OR. 
	1	      IF_MODE(1:5).EQ.'TNOIS') THEN
	          CALL WNCTXS(TEXT,'= !E9.2 K',SCAL(1)*10.)
		ELSE IF (IF_MODE.NE.' ') THEN
	          CALL WNCTXS(TEXT,'= !E9.2 units',SCAL(1)*10.)
		ELSE
	          CALL WNCTXS(TEXT,'= !E9.2 %',SCAL(1)*10.)
		END IF
	      ELSEIF (OPT.EQ.'RES'.AND.DATTYP(LDATTP)(1:2).EQ.'AG') THEN
	        CALL WNCTXS(TEXT,'= !E9.2 %',SCAL(1)*10.)
	      ELSE
	        CALL WNCTXS(TEXT,'= !E9.2 W.U.',SCAL(1)*10.)
	      ENDIF
	      TXTXY(1)=222.*XFAC			!PLOT RULE
	      TXTXY(2)=PAREA(3)+I1*YFAC
	      CALL WQTEXT(TXTXY,TEXT)			!TEXT
	      I1=15					!Y-POSITION
	    END IF
	    I2=0
	    IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1).AND.
	1	 (DATTYP(LDATTP)(1:1).NE.'P')) I2=1
	    IF (I2.EQ.0) THEN
	      IF (OPT.EQ.'DAT' .OR. OPT.EQ.'MOD' .OR.
	1	  OPT.EQ.'INT' .OR. OPT.EQ.'TEL' .OR.
	1	  OPT.EQ.'RES'.AND.DATTYP(LDATTP)(1:2).EQ.'PG') THEN
	        CALL WNCTXS(TEXT,'= !E9.2 degrees (Ph)',SCAL(2)*10.)
	      ELSE
	        CALL WNCTXS(TEXT,'= !E9.2 W.U. (Ph)',SCAL(2)*10.)
	      END IF
	      TXTXY(1)=222.*XFAC			!PLOT RULE
	      TXTXY(2)=PAREA(3)+I1*YFAC
	      CALL WQTEXT(TXTXY,TEXT)			!TEXT
	    END IF
	  END IF
C
C DRAW PLOTS-SEPERATOR
C
	  IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	    CALL WQSPLI(3)                              !DOTTED
	    DO I0=1,PPP(1)-1
	      POINXY(1,1)=I0*XINTPP
	      POINXY(2,1)=0.
	      POINXY(1,2)=POINXY(1,1)
	      POINXY(2,2)=YWND-20*YFAC
	      CALL WQPOLL(2,POINXY)
	    END DO
	    DO I1=1,PPP(2)-1
	      POINXY(1,1)=0.
	      POINXY(2,1)=I1*YINTPP
	      POINXY(1,2)=XWND
	      POINXY(2,2)=POINXY(2,1)
	      CALL WQPOLL(2,POINXY)
	    END DO
	    CALL WQSPLI(1)                              !NORMAL
	  END IF
	ENDIF
C
C WRITE PLOT INFO PER PLOT IN HEADER
C
	IF (MOSAIK.AND.(XPOFF(0,1,0).EQ.1).AND.
	1      (XPOFF(0,0,1).EQ.1).AND.(XPOFF(1,0,1).EQ.0)) THEN
	  JS=WNDSTM(SETS,LINE)                          !DECODE SETS
	ELSE
	  LINE=WNTTSG(SETNAM,0)                         !SET NAME
	END IF
	I2=WNCALN(LINE)                                 !LENGTH LINE
	IF (IFR_MODE.EQ.'NORMAL' .OR.
	1   IFR_MODE.EQ.'INVERT' .OR.
	1   IFR_MODE.EQ.'SORT') THEN                    !SCN FILE DATA: (HA,IFRS)
	  IF (PLOTAP) THEN
	    IFRNM=TXT(1)(1:1)//TXT(2)(1:1)
	    CALL WNCTXS (TEXT,'Sector: !AS (!AS-!AS)',
	1	 LINE(1:I2),IFRNM,POLNAM(IPOL))	        !SET NAME + IFR + POL
	  ELSE
	    CALL WNCTXS (TEXT,'Sector: !AS (!AS-!AS)',
	1	 LINE(1:I2),POLNAM(IPOL),DATTYP(LDATTP)(1:3)) !SET NAME + POL + DATTYP
	  END IF
	ELSE IF (IFR_MODE.EQ.'SPECTRAL') THEN   	!SCN FILE DATA: (HA,CHAN)
	  IF (OPT.EQ.'TEL') THEN
	    IFRNM=TELNAM(IFRS(0)+1:IFRS(0)+1)//POLNAM(IPOL)(1:1)
	    CALL WNCTXS (TEXT,'Tel:    !AS (!AS)',IFRNM,
	1	 DATTYP(LDATTP)(1:3))			!TEL + POL + DATTYP
	  ELSE
	    IFRNM=TELNAM(IFRS(0)+1:IFRS(0)+1)//
	1	   TELNAM(IFRS(1)+1:IFRS(1)+1)
	    CALL WNCTXS (TEXT,'Ifr:    !AS (!AS-!AS)',
	1      IFRNM,POLNAM(IPOL),DATTYP(LDATTP)(1:3))	!IFR + POL + DATTYP
	  END IF
	ELSE IF (IFR_MODE.EQ.'BAND') THEN       	!SCN FILE DATA: (CHAN,IFR)
	  IF (PLOTAP) THEN
	    IFRNM=TXT(1)(1:1)//TXT(2)(1:1)
	    CALL WNCTXS (TEXT,'HA:    !6$E6.2 - !6$E6.2 (!AS-!AS)',
	1      HARA(0)*360.,HARA(1)*360.,IFRNM,POLNAM(IPOL)) 
C						!SECTOR + HA + IFR + POL
	  ELSE
	    CALL WNCTXS (TEXT,'HA:    !6$E6.2 - !6$E6.2 (!AS-!AS)',
	1	HARA(0)*360.,HARA(1)*360.,POLNAM(IPOL),
	2	DATTYP(LDATTP)(1:3))		!SECTOR + HA + POL + DATTYP
	  END IF
	END IF
	IF (PPP(1).GT.4) THEN
	  CALL WQSTXH(TXTHGT/1.5)			!TEXT HEIGHT
	ELSE
	  CALL WQSTXH(TXTHGT)				!TEXT HEIGHT
	END IF
	TXTXY(1)=XOFF*XINTPP
	TXTXY(2)=PG(2,2)-(15+YOFF*3)*YFAC
	CALL WQTEXT(TXTXY,TEXT)				!TEXTC
C
C WRITE PLOT INFO PER PLOT 
C
	IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	  IF (.NOT.PLOTAP) THEN
	    IF (IFR_MODE.EQ.'NORMAL'.OR.
	1   IFR_MODE.EQ.'INVERT' .OR.
	1     IFR_MODE.EQ.'SORT') THEN			!SCN FILE DATA: (HA,IFRS)
	        TEXT=POLNAM(IPOL)//'-'//DATTYP(LDATTP)(1:1)
	      CALL WNCTXS (TEXT,'!AS-!AS',POLNAM(IPOL),DATTYP(LDATTP)(1:1))
	    ELSE IF (IFR_MODE.EQ.'SPECTRAL') THEN   	!SCN FILE DATA: (HA,CHAN)
	      IF (OPT.EQ.'TEL') THEN
	        TEXT=TELNAM(IFRS(0)+1:IFRS(0)+1)//POLNAM(IPOL)(1:1)//
	1	   '-'//DATTYP(LDATTP)(1:1)
	      ELSE
	        TEXT=TELNAM(IFRS(0)+1:IFRS(0)+1)//
	1	   TELNAM(IFRS(1)+1:IFRS(1)+1)//POLNAM(IPOL)//
	2	   '-'//DATTYP(LDATTP)(1:1)
	      END IF
	    ELSE IF (IFR_MODE.EQ.'BAND') THEN       	!SCN FILE DATA: (CHAN,IFR)
	      TEXT=POLNAM(IPOL)//'-'//DATTYP(LDATTP)(1:1)
	    END IF
	    CALL WQSTXH(TXTHGT/PPP(2))			!TEXT HEIGHT
	    TXTXY(1)=TAREA(0)
	    TXTXY(2)=PAREA(3)-9./PPP(2)*YFAC
	    CALL WQTEXT(TXTXY,TEXT(1:6))		!TEXTC
	    CALL WQSTXH(TXTHGT)				!TEXT HEIGHT
	  END IF
	END IF
C
	END
