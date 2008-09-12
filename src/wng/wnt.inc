/*+ Created from wnt.dsc on 970828 at 17:01:48 at daw18
..  WNT.INC
..  WNB 970828
..
..  Revisions:
..
..      WNB 931216      New edit default for D/E unformatted items
..      WNB 930902      Make 32 array indices, 64 field length
..      WNB 930801      Original version
..                                                                            */
/*
..  Result:
..
..  WNT.DSC describes the include files (WNT_O.DEF[.inc],
..  	WNT.DEF [.inc]) for the WNTINC program.
..  	Most WNTI* routines need:
..  	INCLUDE 'WNT_O_DEF' and
..  	INCLUDE 'WNT_DEF'
..                                                                            */
/*
..  Data declarations:
..                                                                            */
	static char p__txt[20][9] = {"NAME ",   /* Known (%) parameters */
	       "DATE ","USER ","VERSION ",
	       "SYSTEM ","LIST ","NOLIST ",
	       "INSERT ","LOCAL ","GLOBAL ",
	       "INCLUDE ","REVISION ","COMMENT ",
	       "FORTRAN ","CC ","PRINT ",
	       "NOPRINT ","ALIGN ","NOALIGN ",
	       " "};
	static char pn__txt[12][10] = {         /* Known (.) names */
	       "DEFINE ","BEGIN ","END ",
	       "PARAMETER ","DATA ","COMMON ",
	       "OFFSET ","STRUCTURE ","ALIGN ",
	       "MAP ","UNION "," "};
	static char cd1[12][1] = {"B ",         /* Letter codes for all */
	       "C ","L ","I ","J ","K ",        /* T_ data types */
	       "E ","D ","X ","Y ","A ",
	       "S "};
	static int cd2[12] = {1,1,4,2,4,        /* Length for all */
	       4,4,8,8,16,2,1};                 /* T_ data types */
	static char cd[12][17] = {"BYTE ",      /* Code for all T_ */
	       "CHARACTER ","LOGICAL ",         /* data types */
	       "INTEGER*2 ","INTEGER ",         /* in Fortran */
	       "INTEGER*4 ","REAL ","DOUBLE PRECISION ",
	       "COMPLEX ","DOUBLE COMPLEX ",
	       "CHARACTER ","BYTE "};
	static char ud[12][13] = {"char ",      /* Code for all T_ */
	       "char ","unsigned int ",         /* data types */
	       "short ","int ","long int ",     /* in C */
	       "float ","double ","float ",
	       "double ","char ","struct "};
	static int tcd[12] = {9,1,3,2,3,        /* Translation types */
	       3,4,5,14,15,2,10};               /* for all T_ data types */
	static char ecd[12][10] = {"UB ",       /* Default edit types for */
	       "AL ","LL ","SI ","SJ ",         /* all T_ types */
	       "SK ","E12.6 ","D12.8 ",
	       "EC12.6 ","DC12.8 ","AL ",
	       "UB "};
/*
..  WNT common data:
..                                                                            */
struct wnt_com {
	int dep;                                /* Structure depth */
	unsigned int lston;                     /* List lines in log */
	unsigned int prton;                     /* Print comments */
	unsigned int algon;                     /* Align data */
	int catp;                               /* Current area type */
	int caln;                               /* Line were current area defined */
	int cbtp;                               /* Current block type */
	int coff;                               /* Current offset */
	int calen;                              /* Current structure align length */
	unsigned int defsn;                     /* .DEFINE seen */
	unsigned int begsn;                     /* .BEGIN (=.STRUCT) seen */
	unsigned int parsn;                     /* .PARAMETER seen */
	unsigned int cinsn;                     /* .COMMIN initialisation seen */
	int unid;                               /* Counter for unique name */
	char oinfil[160];                       /* Original input file */
	char infil[160];                        /* Current input file */
	char parm[5][8];                        /* Program parameters */
	struct wntb ibdes;                      /* Input lines */
	struct wntb cbdes;                      /* Comment on lines */
	struct wntb vbdes;                      /* Global/local values */
	struct wntb rbdes;                      /* %REVISION data */
	struct wntb cmdes;                      /* %COMMENT data */
	struct wntb fmdes;                      /* %FORTRAN data */
	struct wntb ccdes;                      /* %CC data */
	struct wntb fedes;                      /* Embedded %FORTRAN data */
	struct wntb cedes;                      /* Embedded %CC data */
	struct wntb xfdes;                      /* Formatted data */
	struct wntb dfdes;                      /* Initialisation data */
	struct wntb efdes;                      /* Edit data */
};
/*
..  WNT common block:
..                                                                            */
extern struct wnt_com wnt_com_ ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
