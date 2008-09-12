C+DWCBLOCK.FOR
C CMV 940131
C
C       010709   AXC      linux port, data init
C
C	Block data for DWC.DEF
C
	BLOCK DATA DWC_BLOCK
C
C
C  General common block for DWARF (not to be confused with DWARF_4.DEF)
C
	INTEGER		DWLOG		!Printfiles for messages
	CHARACTER*130	DWMSG		!Message string 
C
	COMMON 		/DWC_COM/DWLOG,DWMSG
C
	  DATA DWLOG/1/				!Terminal (==F_T)
	  DATA DWMSG/' '/
C
	END
