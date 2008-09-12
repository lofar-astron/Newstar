$!+wngfex_nfra.com
$!
$! File with system dependent commands for Newstar site NFRA (VAX version)
$!
$! Called by wngfex.com with the command in P1, the name of the 
$! input file in P2 and the name of the spool-file in P3.
$! If the file has to be deleted after spooling, P4 will be equal to "D"
$!
$ if (P1 .eqs. "SP") then COPY 'P2' RZMVX5::LPA0:'P3'
$!
$ if (P1 .eqs. "QM") 
$ then 
$       IF P4-"D" .NES. P4
$       THEN
$         PRINT/QUEUE=QMSQ/PASSALL/DELETE 'P2'
$       ELSE
$         PRINT/QUEUE=QMSQ/PASSALL 'P2'
$       ENDIF
$ endif
$!
$ if (p1 .eqs. "PS") 
$ then
$       IF P4-"D" .NES. P4
$       THEN
$         PRINT/QUEUE=CMPS/DELETE 'P2'
$       ELSE
$         PRINT/QUEUE=CMPS 'P2'
$       ENDIF
$ endif
$!
$ if (p1 .eqs. "A3")
$ then
$       IF P4-"D" .NES. P4
$       THEN
$         PRINT/QUEUE=CMPS/DELETE 'P2'
$       ELSE
$         PRINT/QUEUE=CMPS 'P2'
$       ENDIF
$ endif
$!
$ if (P1 .eqs. "LA") 
$ then  
$   COPY 'P2' RZMVX4::TXA4:'P2'
$   IF P4-"D" .NES. P4 THEN DELETE 'P2'
$ endif
$!
$!-

