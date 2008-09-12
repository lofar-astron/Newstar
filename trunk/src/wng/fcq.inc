/*+ Created from fcq.dsc on 000922 at 11:09:06 at duw01
..  FCQ.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 890724      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  FCQ.DSC defines the FCA (File Control Area) queue and the
..  FCA exit handler block.
..                                                                            */
/*
..  Parameters:
..                                                                            */
/*
..  Data declarations:
..                                                                            */
/*
..  FCQ common data:
..                                                                            */
struct fcq_com {
	int fcaque;                             /* FCA QUEUE HEAD */
	int fcaexh[6];                          /* EXIT HANDLER BLOCK */
};
/*
..  FCQ common block:
..                                                                            */
extern struct fcq_com fcq_com_ ;
/*
..  External initialisation:
..                                                                            */
	extern fcq_bd_() ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
