/*+ Created from wnc.dsc on 970828 at 16:56:52 at daw18
..  WNC.INC
..  WNB 970828
..
..  Revisions:
..
..      WNB 930803      Make use of WNTINC
..      WNB 890716      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  WNC.DEF is an INCLUDE file for the WNC I/O routines.
..  	Initialisation is done in WNC_BD.FOR, generated automatically.
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define CMPH 16                                 /* MAX. # OF HEADERS */
#define CDPL 60                                 /* DEFAULT PAGE LENGTH */
#define CDLL 132                                /* DEFAULT LINE LENGTH */
#define CMLL 132                                /* MAX. LINE LENGTH */
/*
..  Data declarations:
..                                                                            */
/*
..  WNC common data:
..                                                                            */
struct wnc_com {
	int cexh[6];                            /* EXIT HANDLER BLOCK */
	int clun[18];                           /* LUN */
	int cpc[18];                            /* PAGE COUNT */
	int clc[18];                            /* LINE COUNT */
	int cpl[18];                            /* PAGE LENGTH */
	int cll[18];                            /* LINE LENGTH */
	short chph[18];                         /* MAX. HEADER LINE SET */
	int cdis[18];                           /* DISPOSITION (NOT USED) */
	char cfn[18][80];                       /* FILE NAME USED */
	char cffn[18][80];                      /* FINAL FILE NAME */
	char cph[18][16][132];                  /* HEADER LINES */
	char csph[18][16][1];                   /* HEADER LINE SET */
};
/*
..  WNC common block:
..                                                                            */
extern struct wnc_com wnc_com_ ;
/*
..  External initialisation:
..                                                                            */
	extern wnc_bd_() ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
