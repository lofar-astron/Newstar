/*+ Created from fbc.dsc on 000922 at 11:09:15 at duw01
..  FBC_O.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930803      Rearrange text
..      WNB 890724      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  FBC.DSC defines the FBC (File Buffer Controlblock)
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define FBC_M_WRITE 1                           /* REWRITE BUFFER */
#define FBC_V_WRITE 0
/*
..  FBC structure definitions:
..                                                                            */
#define FBCHDL 32                               /* Length */
#define FBCHDV 1                                /* Version */
#define FBCHDS 1                                /* System */
#define FBC__L 32                               /* Length */
#define FBC__V 1                                /* Version */
#define FBC__S 1                                /* System */
/*
..  FBC Offsets:
..                                                                            */
struct fbc {
	int bqa[2];                             /* ADDRESS LINK, MUST BE AT 0 */
	int bqt[2];                             /* TIME LINK, MUST BE AT 8 */
	int addr;                               /* BUFFER ADDRESS */
	int disk;                               /* START DISK ADDRESS IN BUF */
	int disknd;                             /* END DISK ADDRESS+1 IN BUF */
	int bits;                               /* BITS */
};                                              /* END DEFINITION */
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
