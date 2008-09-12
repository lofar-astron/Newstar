C+ Created from wnc.dsc on 970828 at 16:56:53 at daw18
C  WNC_BD.FOR
C  WNB 970828
C
C  Revisions:
C
C       WNB 930803      Make use of WNTINC
C       WNB 890716      Original version
C                                                                             
	BLOCK DATA WNC_BD
C
C  Result:
C
C       Initialisation of wnc.def
C
C  WNC.DEF is an INCLUDE file for the WNC I/O routines.
C  	Initialisation is done in WNC_BD.FOR, generated automatically.
C                                                                             
C
C  Parameters:
C                                                                             
	INTEGER CMPH                            ! MAX. # OF HEADERS
	  PARAMETER (CMPH=16)
	INTEGER CDPL                            ! DEFAULT PAGE LENGTH
	  PARAMETER (CDPL=60)
	INTEGER CDLL                            ! DEFAULT LINE LENGTH
	  PARAMETER (CDLL=132)
	INTEGER CMLL                            ! MAX. LINE LENGTH
	  PARAMETER (CMLL=132)
C
C  WNC common data:
C                                                                             
	INTEGER CEXH(1:6)                       ! EXIT HANDLER BLOCK
	  DATA CEXH /6*0/
	INTEGER CLUN(-1:16)                     ! LUN
	  DATA CLUN /18*0/
	INTEGER CPC(-1:16)                      ! PAGE COUNT
	  DATA CPC /18*0/
	INTEGER CLC(-1:16)                      ! LINE COUNT
	  DATA CLC /18*0/
	INTEGER CPL(-1:16)                      ! PAGE LENGTH
	  DATA CPL /0,17*CDPL/
	INTEGER CLL(-1:16)                      ! LINE LENGTH
	  DATA CLL /80,17*CDLL/
	INTEGER*2 CHPH(-1:16)                   ! MAX. HEADER LINE SET
	  DATA CHPH /18*0/
	INTEGER CDIS(-1:16)                     ! DISPOSITION (NOT USED)
	  DATA CDIS /18*1/
	CHARACTER*80 CFN(-1:16)                 ! FILE NAME USED
	  DATA CFN /18*' '/
	CHARACTER*80 CFFN(-1:16)                ! FINAL FILE NAME
	  DATA CFFN /18*' '/
	CHARACTER*132 CPH(1:16,-1:16)           ! HEADER LINES
	CHARACTER*1 CSPH(1:16,-1:16)            ! HEADER LINE SET
C
C  WNC common block:
C                                                                             
	COMMON /WNC_COM/ CEXH,CLUN,CPC,CLC,
	1              CPL,CLL,CHPH,CDIS,
	1              CFN,CFFN,CPH,CSPH
C
C
	END
C-                                                                            
