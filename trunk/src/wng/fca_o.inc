/*+ Created from fca.dsc on 000922 at 11:09:12 at duw01
..  FCA_O.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930811      Add some names; change FIB length; add FAT*
..      WNB 930803      Use WNTINC options
..      WNB 890724      Original version
..      JPH 930415      FCA_x_WRT --> FCA_x_WRTAPE
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  FCA.DSC defines the FCA (File Control Area)
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define FIB__L 80                               /* FIB LENGTH (SYSTEM DEPENDENT) */
#define ATR__L 12                               /* ATTRIBUTE CONTROL BLK LENGTH */
#define RATR__L 32                              /* RECORD ATTRIBUTE LENGTH */
#define FCA__FNL 80                             /* FILE NAME LENGTH */
#define FCA_M_ASS 1                             /* MASKS FOR: */
	                                        /* CHANNEL ASSIGNED, FILE ACCESSED */
	                                        /* READ/WRITE ACTIVE */
	                                        /* SEQUENTIAL FILE */
	                                        /* TEMP. FILE, MAGNETIC TAPE */
	                                        /* OLD FILE */
#define FCA_M_ACC 2
#define FCA_M_ACT 4
#define FCA_M_SEQ 8
#define FCA_M_TMP 16
#define FCA_M_MAG 32
#define FCA_M_OLD 128
#define FCA_M__N 10
#define FCA_M__L 1
#define FCA_M__H 128
#define FCA_M__I 2
#define FCA_V_ASS 0                             /* BITS FOR: */
	                                        /* CHANNEL ASSIGNED, FILE ACCESSED */
	                                        /* READ/WRITE ACTIVE */
	                                        /* SEQUENTIAL FILE */
	                                        /* TEMP. FILE, MAGNETIC TAPE */
	                                        /* OLD FILE */
#define FCA_V_ACC 1
#define FCA_V_ACT 2
#define FCA_V_SEQ 3
#define FCA_V_TMP 4
#define FCA_V_MAG 5
#define FCA_V_OLD 7
#define FCA_V__N 10
#define FCA_V__L 0
#define FCA_V__H 7
#define FCA_V__I 1
#define FCA_M_WRTAPE 64                         /* MAGNETIC TAPE WRITE */
#define FCA_V_WRTAPE 6
#define FCA_M_WRITE 256                         /* WRITE ALLOWED */
#define FCA_V_WRITE 8
#define FAT_EFBLKL_1 10
#define FAT_EFBLKH_1 8
#define FAT_HIBLKL_1 6
#define FAT_HIBLKH_1 4
#define FAT_FFBYTE_1 12
#define FAT_EFBLKL_I 5
#define FAT_EFBLKH_I 4
#define FAT_HIBLKL_I 3
#define FAT_HIBLKH_I 2
#define FAT_FFBYTE_I 6
/*
..  FCA structure definitions:
..                                                                            */
#define FCAHDL 356                              /* Length */
#define FCAHDV 1                                /* Version */
#define FCAHDS 1                                /* System */
#define FCA__L 356                              /* Length */
#define FCA__V 1                                /* Version */
#define FCA__S 1                                /* System */
/*
..  FCA Offsets:
..                                                                            */
struct fca {
	int link;                               /* LINK, MUST BE AT 0 */
	int tid;                                /* ID. 0=FCA, 1=MCA, MUST BE AT 4 */
	int size;                               /* SIZE OF BLOCK */
	int chan;                               /* ASSIGNED CHANNEL */
  union {
	int iosb[2];                            /* IO STATUS BLOCK */
	short iosbi[4];
  } iosb;
	int bits;                               /* BITS */
	                                        /* THE ABOVE SHOULD BE SAME FOR FCA & MCA */
	int fibdes[2];                          /* FIB DESCRIPTOR */
	int did[2];                             /* DIRECTORY ID */
  union {
	int atrj[3];                            /* ATTRIBUTE CONTROL BLOCK */
	char atr[12];
  } atrj;
	int bqt[2];                             /* TIME ORDERED BUFFER QUEUE */
	int bqa[2];                             /* ADDRESS ORDERED BUFFER QUEUE */
	int blen;                               /* BUFFER LENGTH */
	int bcp;                                /* BUFFER CONTROL AREA POINTER */
	int fea[2];                             /* ACTIVE FILE ELEMENT QUEUE */
	int fee[2];                             /* EMPTY FILE ELEMENT QUEUE */
	int fep;                                /* ELEMENT AREA POINTER */
	int fel;                                /* CURRENT ELEMENT POINTER */
	int mca;                                /* PTR TO MCA */
	int hiblk;                              /* LOWEST NOT ALLOCATED ADDRESS */
	int eof;                                /* FIRST BYTE BEYOND EOF */
	int rad;                                /* LAST READ ADDRESS */
	int dad;                                /* DISK ADDRESS */
	int bad;                                /* BUFFER ADDRESS */
	int len;                                /* LENGTH TO READ/WRITE */
	int aclen;                              /* ACTUAL LENGTH READ/WRITTEN */
	int ef;                                 /* IO EF */
	int efa;                                /* ACTIVITY EF */
	int err;                                /* FINAL IO ERROR */
	int map;                                /* MAG TAPE POINTER */
	int mab;                                /* MAG TAPE BLOCK */
	int maw;                                /* MAG TAPE WRITE POSITION */
	int fnaml;                              /* FILE NAME LENGTH */
  union {
	int fibj[20];                           /* FIB */
	short fibi[40];
	char fib[80];
  } fibj;
	char fnam[80];                          /* FILE NAME */
  union {
	int recatrj[8];                         /* RECORD ATTRIBUTES */
	char recatr[32];
  } recatrj;
};                                              /* END DEFINITION */
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
