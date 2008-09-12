/*+ Created from wnt.dsc on 970828 at 17:01:49 at daw18
..  WNT_O.INC
..  WNB 970828
..
..  Revisions:
..
..      WNB 931216      New edit default for D/E unformatted items
..      WNB 930902      Make 32 array indices, 64 field length
..      WNB 930801      Original version
..                                                                            */
/*
..  Given statements:
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
..  Parameters:
..                                                                            */
#define MXDINC 8                                /* Max. include depth */
#define MXSLIN 132                              /* Single line length */
#define MXTLIN 4096                             /* Composite max. line length */
#define MXLPAR 8                                /* Length % parameter values */
#define MXLNAM 64                               /* Max. length name/value fields */
#define MXNARR 32                               /* Max. # of array indices */
#define COMPOS 42                               /* Comment position */
#define AT_DEF 1                                /* Data area types */
#define AT_BEG 2
#define AT__N 3
#define AT__L 1
#define AT__H 2
#define AT__I 1
#define BT_PAR 1                                /* Data block types */
#define BT_DAT 2
#define BT_SDA 3
#define BT_COM 4
#define BT_BEG 5
#define BT_EBG 6
#define BT_DEF 7
#define BT_EDF 8
#define BT_MAP 9
#define BT_EMP 10
#define BT_DCM 11
#define BT__N 12
#define BT__L 1
#define BT__H 11
#define BT__I 1
#define FT_CON -1                               /* Format block types */
#define FT_NUL 0
#define FT_DAT 1
#define FT_BEG 2
#define FT_DEF 3
#define FT_END 4
#define FT_MAP 5
#define FT_DCM 6
#define FT__N 9
#define FT__L -1
#define FT__H 6
#define FT__I 1
#define OP_LB 1                                 /* Operators: */
	                                        /* LBracket  PLus */
	                                        /* MInus	   MUltiply */
	                                        /* DIvide    SinglePlus */
	                                        /* SingleMinus */
#define OP_PL 2
#define OP_MI 3
#define OP_MU 4
#define OP_DV 5
#define OP_SP 6
#define OP_SM 7
#define OP__N 8
#define OP__L 1
#define OP__H 7
#define OP__I 1
#define INCCNT 16                               /* Start # of entities to allocate */
#define P_NAM 1                                 /* Known (%) parameters */
#define P_DAT 2
#define P_USE 3
#define P_VER 4
#define P_SYS 5
#define P_LIS 6
#define P_NOL 7
#define P_INS 8
#define P_LOC 9
#define P_GLO 10
#define P_INC 11
#define P_REV 12
#define P_COM 13
#define P_FOR 14
#define P_CC 15
#define P_PRI 16
#define P_NOP 17
#define P_ALI 18
#define P_NOA 19
#define P__N 20
#define P__L 1
#define P__H 19
#define P__I 1
#define PN_DEF 1                                /* Known (.) names */
#define PN_BEG 2
#define PN_END 3
#define PN_PAR 4
#define PN_DAT 5
#define PN_COM 6
#define PN_OFF 7
#define PN_STR 8
#define PN_ALI 9
#define PN_MAP 10
#define PN_UNI 11
#define PN__N 12
#define PN__L 1
#define PN__H 11
#define PN__I 1
/*
..  WNTB structure definitions:
..                                                                            */
#define WNTBHDL 24                              /* Length */
#define WNTBHDV 1                               /* Version */
#define WNTBHDS 1                               /* System */
#define WNTB__L 24                              /* Length */
#define WNTB__V 1                               /* Version */
#define WNTB__S 1                               /* System */
/*
..  WNTB Offsets:
..                                                                            */
struct wntb {                                   /* General buffer administration */
	int ccnt;                               /* Current # allocated */
	int cnt;                                /* Current # filled */
	int elen;                               /* Length data element (bytes) */
	int bptr;                               /* Pointer to start data (A_B) */
	int jptr;                               /* Pointer to start data (A_J) */
	char wntb__0000[4];
};                                              /* WNTB */
/*
..  WNTI structure definitions:
..                                                                            */
#define WNTIHDL 16                              /* Length */
#define WNTIHDV 1                               /* Version */
#define WNTIHDS 1                               /* System */
#define WNTI__L 16                              /* Length */
#define WNTI__V 1                               /* Version */
#define WNTI__S 1                               /* System */
/*
..  WNTI Offsets:
..                                                                            */
struct wnti {                                   /* Input line definition */
	int ftyp;                               /* Format type */
	int lcom;                               /* Length comment (or 0) */
	int pcom;                               /* Pointer to comment block */
	int pfor;                               /* Pointer to format block */
};                                              /* WNTI */
/*
..  WNTV structure definitions:
..                                                                            */
#define WNTVHDL 88                              /* Length */
#define WNTVHDV 1                               /* Version */
#define WNTVHDS 1                               /* System */
#define WNTV__L 88                              /* Length */
#define WNTV__V 1                               /* Version */
#define WNTV__S 1                               /* System */
/*
..  WNTV Offsets:
..                                                                            */
struct wntv {                                   /* Local/global value */
	char nam[16];                           /* Variable name */
	int typ;                                /* Type: */
	                                        /* +: local	-: global */
	                                        /* 1: integer	2: string */
	int val;                                /* Value */
	char str[64];                           /* Value as string */
};                                              /* WNTV */
/*
..  WNTF structure definitions:
..                                                                            */
#define WNTFHDL 376                             /* Length */
#define WNTFHDV 1                               /* Version */
#define WNTFHDS 1                               /* System */
#define WNTF__L 376                             /* Length */
#define WNTF__V 1                               /* Version */
#define WNTF__S 1                               /* System */
/*
..  WNTF Offsets:
..                                                                            */
struct wntf {                                   /* Format data block */
	int btyp;                               /* Block type (BT_) */
	int alen;                               /* Align length */
	int dtp;                                /* Data type (T_) */
	int ulen;                               /* Length one unit (bytes) */
	int slen;                               /* String length */
	int dim;                                /* # of dimensions */
	int refp;                               /* Pointer to reference line */
	int nini;                               /* # of initialisation values */
	int inip;                               /* Pointer to first init. value */
	int edip;                               /* Edit information pointer */
	int tlen;                               /* Total length (entities) entry */
	int off;                                /* Offset of this entry */
	int ent;                                /* Pointer to input line entry */
	int sref;                               /* Pointer to structure definition */
	int ind[32][2];                         /* Low bound, length array index */
	char nam[64];                           /* Name of variable */
};                                              /* WNTF */
/*
..  WNTD structure definitions:
..                                                                            */
#define WNTDHDL 40                              /* Length */
#define WNTDHDV 1                               /* Version */
#define WNTDHDS 1                               /* System */
#define WNTD__L 40                              /* Length */
#define WNTD__V 1                               /* Version */
#define WNTD__S 1                               /* System */
/*
..  WNTD Offsets:
..                                                                            */
struct wntd {                                   /* Data initialisation information */
	int rep;                                /* Repetition factor */
	char str[36];                           /* Initialisation information */
};                                              /* WNTD */
/*
..  WNTE structure definitions:
..                                                                            */
#define WNTEHDL 40                              /* Length */
#define WNTEHDV 1                               /* Version */
#define WNTEHDS 1                               /* System */
#define WNTE__L 40                              /* Length */
#define WNTE__V 1                               /* Version */
#define WNTE__S 1                               /* System */
/*
..  WNTE Offsets:
..                                                                            */
struct wnte {                                   /* Edit data */
	int edit;                               /* Edit allowed (0) */
	char pat[12];                           /* I/O format pattern */
	char unit[12];                          /* Units */
	char spec[12];                          /* Special information */
};                                              /* WNTD */
/*-                                                                           */
