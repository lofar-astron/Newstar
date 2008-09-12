/*+ Created from wng.dsc on 970828 at 16:58:00 at daw18
..  WNG.INC
..  WNB 970828
..
..  Revisions:
..
..      HJV 940217      Change data type T (remove blank line)
..      WNB 931130      Add BKJY, DBKJY
..      JPH 930825      Comment
..      WNB 930803      Use new WNTINC options
..      WNB 930727      Add T_S, LB_S, T_ALL
..      WNB 930527      Add A data type
..      WNB 921222      Add WSRT/ATNF longitude and latitude
..      HJV 920626      Add comment for prgdat (8 = HP station)
..      HJV 920501      Type of JS changed to L from J
..      WNB 890427      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  WNG.DSC is a general include file. WNG_DEF (wng_inc) should be
..  	included as the first executable statement after the
..  	routine definition. In WNGLOGIN.COM an assignment to
..  	WNG_DEF is present, so use it as:
..  	INCLUDE 'WNG_DEF'
..  	Initialisation is done in WNGIN (via NLINK)
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define DPI 3.1415926535897932385               /* PI */
#define PI DPI
#define DEE 2.7182818284590452353               /* E */
#define EE DEE
#define DPI2 6.2831853071795864769              /* 2*PI */
#define PI2 DPI2
#define DRAD 0.0174532925199432958              /* PI/360 */
#define RAD DRAD
#define DDEG 57.2957795130823208768             /* 360/PI */
#define DEG DDEG
#define DCRTSC 240.*360.                        /* CIRCLES TO SECONDS */
#define RCRTSC DCRTSC
#define DCL 2.997925D8                          /* C IN M/S */
#define CL DCL
#define DBKJY 1380.54                           /* K IN JY.M^2/K */
#define BKJY DBKJY
#define LATW 52.9169                            /* LAT WSRT (DEG) */
#define LONGW 6.604167                          /* LONG WSRT (DEG) */
#define SLATW 0.797762                          /* SIN(LAT) WSRT */
#define CLATW 0.602973                          /* COS(LAT) WSRT */
#define LATA -30.31445                          /* LAT ATNF (DEG) */
#define LONGA 149.566928                        /* LONG ATNF (DEG) */
#define SLATA -0.504745                         /* SIN(LAT) ATNF */
#define CLATA 0.863268                          /* COS(LAT) ATNF */
#define E2T8 2.**8                              /* 2**8 */
#define E2T16 2.**16                            /* 2**16 */
#define E2T32 2.**32                            /* 2**32 */
#define D2T16 2D0**16                           /* 2**16 */
#define D2T32 2D0**32                           /* 2**32 */
#define F_FILN 16                               /* # OF FILES */
#define F_T 1
	                                        /* Type BIT */
	                                        /* Print BIT */
	                                        /* Prefix BIT: puts a ">" */
#define F_P 2
#define F_P1 128
#define F_0 256
#define F_1 512
#define F_2 1024
#define F_3 2048
#define F_4 4096
#define F_5 8192
#define F_6 16384
#define F_7 32768
#define F_8 65536
#define F_9 131072
#define F_10 262144
#define F_11 524288
#define F_12 1048576
#define F_13 2097152
#define F_14 4194304
#define F_15 8388608
#define F__N 25
#define F__L 1
#define F__H 8388608
#define F__I 2
#define F_TP 3                                  /* TYPE/PRINT BITS */
#define F_ALL -F_0+F_15+F_15+F_TP               /* ALL FILE BITS */
#define F_NO 0                                  /* NO DISPOSITION */
#define F_YES 1                                 /* KEEP FILE */
#define F_SP 2                                  /* SPOOL FILE */
#define F_CAT 3                                 /* CONCATENATE FILE */
#define F_LC 1                                  /* LINE COUNT CODE */
#define F_PC 2                                  /* PAGE COUNT CODE */
#define F_LL 3                                  /* LINE LENGTH CODE */
#define F_PL 4                                  /* PAGE LENGTH CODE */
#define F_DIS 5                                 /* DISPOSITION CODE */
#define IUND -32768                             /* UNDEFINED WSRT VALUE */
#define DWC_ENDOFLOOP 134448144                 /* ^Z PARAMETER REPLY */
#define DWC_NULLVALUE 134448161                 /* "" PARAMETER REPLY */
#define DWC_WILDCARD 134448169                  /* *  PARAMETER REPLY */
#define L_B 8                                   /* LENGTH IN BITS OF */
	                                        /* DATA TYPES */
#define L_C 8
#define L_L 32
#define L_I 16
#define L_J 32
#define L_K 32
#define L_E 32
#define L_D 64
#define L_X 64
#define L_Y 128
#define L_A 16
#define L_S 8
#define L__N 13
#define LB_B 1                                  /* LENGTH IN BYTES OF */
	                                        /* DATA TYPES */
#define LB_C 1
#define LB_L 4
#define LB_I 2
#define LB_J 4
#define LB_K 4
#define LB_E 4
#define LB_D 8
#define LB_X 8
#define LB_Y 16
#define LB_A 2
#define LB_S 1
#define LB__N 13
#define T_B 1                                   /* TYPE CODES OF */
#define T_C 2
#define T_L 3
#define T_I 4
#define T_J 5
#define T_K 6
#define T_E 7
#define T_D 8
#define T_X 9
#define T_Y 10
#define T_A 11
#define T_S 12
#define T__N 13
#define T__L 1
#define T__H 12
#define T__I 1
#define T_ALL BCLIJKEDXYAS                      /* KNOWN DATA TYPES */
/*
..  Data declarations:
..                                                                            */
	int j;                                  /* POINTERS */
	int j0;
	int j1;
	int j2;
	int j3;
	int j4;
	int j5;
	int i;                                  /* LOOPS */
	int i0;
	int i1;
	int i2;
	int i3;
	int i4;
	int i5;
	unsigned int js;                        /* ERROR */
	float r0;                               /* SOME HELP */
	float r1;
	double d0;
	double d1;
	unsigned int l0;
	unsigned int l1;
	char b0;
	char b1;
/*
..  WNG common data:
..                                                                            */
struct wng_com {
  union {
	double a_y[1][2];                       /* DUMMY ARRAYS */
	char a_b[1];
	short a_i[1];
	int a_j[1];
	long int a_k[1];
	unsigned int a_l[1];
	float a_e[1];
	double a_d[1];
	float a_x[1][2];
  } a_y;
	int e_c;                                /* MOST RECENT ERROR CODE */
	int a_ob;                               /* ARRAY OFFSETS */
	int a_oi;
	int a_oj;
	int a_ok;
	int a_ol;
	int a_oe;
	int a_od;
	int a_ox;
	int a_oy;
	int logcd;                              /* LOG CODE */
	int prgdat;                             /* DATA TYPE */
	                                        /* 1= VAX, D_FORMAT */
	                                        /* 2= VAX, G_FORMAT */
	                                        /* 3= ALLIANT */
	                                        /* 4= CONVEX */
	                                        /* 5= IEEE */
	                                        /* 6= DEC station */
	                                        /* 7= SUN station */
	                                        /* 8= HP  station */
	char prgnam[9];                         /* PROGRAM NAME */
	char prgver[6];                         /* PROGRAM VERSION */
};
/*
..  WNG common block:
..                                                                            */
extern struct wng_com wng_com_ ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
