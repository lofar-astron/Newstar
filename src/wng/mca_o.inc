/*+ Created from mca.dsc on 000922 at 11:09:09 at duw01
..  MCA_O.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930811      Add some names
..      WNB 930803      Use WNTINC features
..      WNB 890724      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  MCA.DSC defines the MCA (Magnetic tape Control Area)
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define MCA_M_OUT 1                             /* TAPE MASKS FOR: */
	                                        /* OUTPUT, UNLABELED TAPE */
	                                        /* BLOCKED OPERATION */
	                                        /* ALLOCATED,ASSIGNED, */
	                                        /* MOUNTED BY PROGRAM */
#define MCA_M_UNL 2
#define MCA_M_BLK 4
#define MCA_M_ALL 8
#define MCA_M_ASS 16
#define MCA_M_MOU 32
#define MCA_M__N 7
#define MCA_M__L 1
#define MCA_M__H 32
#define MCA_M__I 2
#define MCA_V_OUT 0                             /* TAPE MASK BITS FOR: */
	                                        /* OUTPUT, UNLABELED TAPE */
	                                        /* BLOCKED OPERATION */
	                                        /* ALLOCATED,ASSIGNED, */
	                                        /* MOUNTED BY PROGRAM */
#define MCA_V_UNL 1
#define MCA_V_BLK 2
#define MCA_V_ALL 3
#define MCA_V_ASS 4
#define MCA_V_MOU 5
#define MCA_V__N 7
#define MCA_V__L 0
#define MCA_V__H 5
#define MCA_V__I 1
#define MCA__IRG 60                             /* INTERREC. GAP IN .01 INCHES */
#define MCA__TML 150                            /* TAPE MARK LENGTH IN .01 INCHES */
/*
..  MCA structure definitions:
..                                                                            */
#define MCAHDL 320                              /* Length */
#define MCAHDV 1                                /* Version */
#define MCAHDS 1                                /* System */
#define MCA__L 320                              /* Length */
#define MCA__V 1                                /* Version */
#define MCA__S 1                                /* System */
/*
..  MCA Offsets:
..                                                                            */
struct mca {
	int link;                               /* LINK, MUST BE AT 0 */
	int tid;                                /* ID. 0=FCA, 1=MCA, MUST BE AT 4 */
	int size;                               /* SIZE OF BLOCK */
	int chan;                               /* ASSIGNED CHANNEL */
	int iosb[2];                            /* IO STATUS BLOCK */
	int bits;                               /* BITS */
	                                        /* THE ABOVE SHOULD BE SAME FOR FCA & MCA */
	int dens;                               /* DENSITY (BPI) */
	int undes[2];                           /* UNIT DESCRIPTOR */
	char unit[32];                          /* UNIT NAME */
	int fca;                                /* BELONGING FCA (OR 0) */
	int magf;                               /* FILE POSITION */
	char vol[80];                           /* VOLUME LABEL */
	char hd1[80];                           /* HDR1 LABEL */
	char hd2[80];                           /* HDR2 LABEL */
};                                              /* END DEFINITION */
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
