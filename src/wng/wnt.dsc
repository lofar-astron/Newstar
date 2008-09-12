!+ WNT.DSC
!  WNB 930801
!
!  Revisions:
!
%REVISION=WNB=931216="New edit default for D/E unformatted items"
%REVISION=WNB=930902="Make 32 array indices, 64 field length"
%REVISION=WNB=930801="Original version"
!
!  Description:
!
%COMMENT="WNT.DSC describes the include files (WNT_O.DEF[.inc],"
%COMMENT="	WNT.DEF [.inc]) for the WNTINC program."
%COMMENT="	Most WNTI* routines need:"
%COMMENT="	INCLUDE 'WNT_O_DEF' and"
%COMMENT="	INCLUDE 'WNT_DEF'"
!
!	Standard data
!
%VERSION=1			!Version
%SYSTEM=1			!System version
%USER=WNB			!Author
%%DATE				!Date of compilation
%%NAME				!Name of files
!
%ALIGN				!Align structures and common blocks
!
!	Program parameters
!
.PARAMETER
!
!  Program variable sizes
!
	MXDINC	J	/8/			!Max. include depth
	MXSLIN	J	/132/			!Single line length
	MXTLIN	J	/4096/			!Composite max. line length
%GLOBAL=MXLPAR=8				!Length % parameter values
%GLOBAL=MXLNAM=64				!Max. length name/value fields
%GLOBAL=MXNARR=32				!Max. # of array indices
	COMPOS	J	/42/			!Comment position
!
! Type indicators
!
	AT	A:	/DEFINE,BEGIN/			!Data area types
	BT	A:	/PARAM,DATA,SDATA, \		!Data block types
				COMMON,BEGIN,EBGIN,DEFINE, \
				EDFINE,MAP,EMP,DCMMON/
	FT	A:(-1)	/CONTIN,NULL,DATA,BEGIN, \	!Format block types
				DEFINE,END,MAP,DCMMON/
	OP	A:	/LB,PL,MI,MU,DV,SP,SM/		!Operators:
							!LBracket  PLus
							!MInus	   MUltiply
							!DIvide    SinglePlus
							!SingleMinus
!
! Data structures
!
.STRUCTURE=WNTB					!General buffer administration
  .PARAMETER
	INCCNT	J	/16/			!Start # of entities to allocate
  .DATA
	CCNT	J				!Current # allocated
	CNT	J				!Current # filled
	ELEN	J				!Length data element (bytes)
	BPTR	J				!Pointer to start data (A_B)
	JPTR	J				!Pointer to start data (A_J)
    .ALIGN=8
.END						!WNTB
!
.STRUCTURE=WNTI					!Input line definition
	FTYP	J				!Format type
	LCOM	J				!Length comment (or 0)
	PCOM	J				!Pointer to comment block
	PFOR	J				!Pointer to format block
  .ALIGN=8
.END						!WNTI
!
.STRUCTURE=WNTV					!Local/global value
	NAM	C16				!Variable name
	TYP	J				!Type:
						!+: local	-: global
						!1: integer	2: string
	VAL	J				!Value
	STR	C64				!Value as string
   .ALIGN=8
.END						!WNTV
!
.STRUCTURE=WNTF					!Format data block
	BTYP	J				!Block type (BT_)
	ALEN	J				!Align length
	DTP	J				!Data type (T_)
	ULEN	J				!Length one unit (bytes)
	SLEN	J				!String length
	DIM	J				!# of dimensions
	REFP	J				!Pointer to reference line
	NINI	J				!# of initialisation values
	INIP	J				!Pointer to first init. value
	EDIP	J				!Edit information pointer
	TLEN	J				!Total length (entities) entry
	OFF	J				!Offset of this entry
	ENT	J				!Pointer to input line entry
	SREF	J				!Pointer to structure definition
	IND	J(0:1,0:MXNARR-1)		!Low bound, length array index
	NAM	C(MXLNAM)			!Name of variable
  .ALIGN=8
.END						!WNTF
!
.STRUCTURE=WNTD					!Data initialisation information
	REP	J				!Repetition factor
	STR	C36				!Initialisation information
  .ALIGN=8
.END						!WNTD
!
.STRUCTURE=WNTE					!Edit data
	EDIT	J				!Edit allowed (0)
	PAT	C12				!I/O format pattern
	UNIT	C12				!Units
	SPEC	C12				!Special information
  .ALIGN=8
.END						!WNTD
!
! Variables
!
.DEFINE
  .DATA
!
! Known names
!
	P	A:	/NAME,DATE,USER,VERSION, \	!Known (%) parameters
				SYSTEM,LIST,NOLIST,INSERT, \
				LOCAL,GLOBAL,INCLUDE,REVISION, \
				COMMENT,FORTRAN,CC,PRINT, \
				NOPRINT,ALIGN,NOALIGN/
%LOCAL=P_SYS=5						!Position in P_ list
	PN	A:	/DEFINE,BEGIN,END,PARAMETER, \	!Known (.) names
				DATA,COMMON,OFFSET,STRUCTURE, \
				ALIGN,MAP,UNION/
!
! Data type information
!
	CD1	C1(T__N-1)	/B,C,L,I,J,K,E,D,X,Y,A,S/ !Letter codes for all
							!  T_ data types
	CD2	J(T__N-1)	/LB_B,LB_C,LB_L,LB_I, \	!Length for all
					LB_J,LB_K,LB_E,LB_D, \ !  T_ data types
					LB_X,LB_Y,LB_A,LB_S/
	CD	C*(T__N-1)	/BYTE,CHARACTER, \	!Code for all T_
					LOGICAL,"INTEGER*2", \ !  data types
					INTEGER,"INTEGER*4", \ !  in Fortran
					REAL,"DOUBLE PRECISION", \
					COMPLEX,"DOUBLE COMPLEX", \
					CHARACTER,BYTE/
	UD	C*(T__N-1)	/"char","char", \	!Code for all T_
					"unsigned int","short", \ !  data types
					"int","long int", \	!  in C
					"float","double", \
					"float","double", \
					"char","struct"/
	TCD	J(T__N-1)	/9,1,3,2,3,3,4,5,14,15,2,10/ !Translation types
							!  for all T_ data types
	ECD	C10(T__N-1)	/UB,AL,LL,SI,SJ,SK, \	!Default edit types for
					E12.6,D12.8, \	!  all T_ types
					EC12.6,DC12.8,AL,UB/
!
! Common block
!
  .COMMON
	DEP	J				!Structure depth
	LSTON	L				!List lines in log
	PRTON	L				!Print comments
	ALGON	L				!Align data
	CATP	J				!Current area type
	CALN	J				!Line were current area defined
	CBTP	J				!Current block type
	COFF	J				!Current offset
	CALEN	J				!Current structure align length
	DEFSN	L				!.DEFINE seen
	BEGSN	L				!.BEGIN (=.STRUCT) seen
	PARSN	L				!.PARAMETER seen
	CINSN	L				!.COMMIN initialisation seen
	UNID	J				!Counter for unique name
	OINFIL	C160				!Original input file
	INFIL	C160				!Current input file
	PARM	C(MXLPAR)(P_SYS)		!Program parameters
!
! Buffer descriptors
!
	IBDES	S:WNTB				!Input lines
	CBDES	S:WNTB				!Comment on lines
	VBDES	S:WNTB				!Global/local values
	RBDES	S:WNTB				!%REVISION data
	CMDES	S:WNTB				!%COMMENT data
	FMDES	S:WNTB				!%FORTRAN data
	CCDES	S:WNTB				!%CC data
	FEDES	S:WNTB				!Embedded %FORTRAN data
	CEDES	S:WNTB				!Embedded %CC data
	XFDES	S:WNTB				!Formatted data
	DFDES	S:WNTB				!Initialisation data
	EFDES	S:WNTB				!Edit data
!
.END						!DEFINE
