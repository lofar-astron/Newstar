	BLOCK DATA CPL_BLOCK
C	Created by BLDDEF from: USER5:[VANDIEPEN.WORK.NEW]CPL.DEF; on 17-MAR-92
C	HjV 921208	Removed all equivalence and add names to common block
C	HjV 930427	Change size CPL$WRKBUF from 2000 to 2500
C	HjV 930613	Change size CPL$WRKBUF from 2500 to 5000
C	HjV 940829	Change size CPL$WRKBUF from 5000 to 10000
C       AXC 010709      Linux port - data initiialisation
C
C
C
	INTEGER*4 CPL$LENGTH
	INTEGER*4 CPL$TYPE
	INTEGER*4 CPL$SRCLUN
	CHARACTER*64 CPL$SRCNAME
	CHARACTER*255 CPL$SRCBUF
	LOGICAL*1 CPL$FILLER1
	INTEGER*4 CPL$SRCLBUF
	INTEGER*4 CPL$SRCLNR
	INTEGER*4 CPL$SRCPTR	
	INTEGER*4 CPL$OBJLUN
	CHARACTER*64 CPL$OBJNAME
	INTEGER*4 CPL$OBJLREC
	INTEGER*4 CPL$OBJNREC
	CHARACTER*10000 CPL$WRKBUF
	INTEGER*4 CPL$WRKLBUF
	INTEGER*4 CPL$WRKSTART(32)
	INTEGER*4 CPL$WRKEND(32)
	INTEGER*4 CPL$WRKLNR(0:32)
	INTEGER*4 CPL$ERRBUF(100,2)
	INTEGER*4 CPL$ERRNERR
	INTEGER*4 CPL$ERRNWARN
	INTEGER*4 CPL$ERRNTOT
C
	COMMON /CPL/ CPL$LENGTH, CPL$TYPE, CPL$SRCLUN, CPL$SRCNAME,
     *		CPL$SRCBUF, CPL$FILLER1, CPL$SRCLBUF, CPL$SRCLNR,
     *		CPL$SRCPTR, CPL$OBJLUN, CPL$OBJNAME, CPL$OBJLREC,
     *		CPL$OBJNREC, CPL$WRKBUF, CPL$WRKLBUF, CPL$WRKSTART,
     *		CPL$WRKEND, CPL$WRKLNR, CPL$ERRBUF, CPL$ERRNERR,
     *		CPL$ERRNWARN, CPL$ERRNTOT
C
	DATA CPL$SRCLUN /-1/
	DATA CPL$SRCNAME /' '/
	DATA CPL$SRCBUF /' '/
	DATA CPL$SRCLBUF /0/
	DATA CPL$SRCLNR /0/
	DATA CPL$SRCPTR /0/
	DATA CPL$OBJLUN /-1/
	DATA CPL$OBJNAME /' '/
	DATA CPL$OBJLREC /0/
	DATA CPL$OBJNREC /0/
	DATA CPL$ERRNTOT /0/
	DATA CPL$WRKBUF /' '/
	DATA CPL$WRKLBUF /0/
	DATA CPL$WRKSTART /32*0/
	DATA CPL$WRKEND /32*0/
	DATA CPL$WRKLNR /33*0/
	DATA CPL$ERRBUF /200*0/
	DATA CPL$ERRNERR /0/
	DATA CPL$ERRNWARN /0/
C
	END











