/*+ Created from wnd.dsc on 970828 at 16:57:18 at daw18
..  WND.INC
..  WNB 970828
..
..  Revisions:
..
..      CMV 940224      Add MODELB
..      WNB 940215      Add XMEM
..      WNB 930803      Remove .INCLUDE
..      WNB 920607      Add XUFLAG
..      WNB 910913      Add for DATAB, INFIX, RUN, loops
..      WNB 910909      Add RUNCD
..      WNB 900130      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  WND.DEF is an INCLUDE file for the DWARF interface routines
..  
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define MXNLOP 8                                /* MAX. # OF LOOP INDICES */
/*
..  Data declarations:
..                                                                            */
/*
..  WND common data:
..                                                                            */
struct wnd_com {
	char datab[80];                         /* CURRENT DATABASE */
	char prefix[80];                        /* CURRENT PREFIX */
	unsigned int runcd;                     /* RUN (.TRUE.) INDICATOR */
	int xcap;                               /* CORR. TO APPLY */
	int xcdap;                              /* CORR. TO DE-APPLY */
	int xpoff[9][2][8];                     /* LOOP OFFSETS GIVEN */
	int xlsav[9][8];                        /* SAVED OFFSETS PER INDEX */
	int xlcnt[9];                           /* INDEX COUNT */
	int xlptr;                              /* CURRENT INDEX LEVEL */
	int xuflag;                             /* CURRENT UFLAG OPTIONS */
	int xmem;                               /* MEMORY CHUNK SIZE */
	char modelb[80];                        /* DIRECTORY FOR MODELS */
};
/*
..  WND common block:
..                                                                            */
extern struct wnd_com wnd_com_ ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
