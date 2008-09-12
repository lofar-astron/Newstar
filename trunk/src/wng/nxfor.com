$!#  nxfor.ssc
$!# 	WNB 921214
$!# 
$!#  Revisions:
$!#	WNB 921215	Add ifndef
$!#	WNB 921218	Add wn_site__
$!#	WNB 921230	Creation message; make SSC
$!#	WNB 930108	Correct unix part for site dependencies
$!#	WNB 930302	Add /bin/csh
$!#	WNB 930330	Add wn_gipsy__ and wn_pgplot__
$!#	WNB 930428	Correct Unix gipsy test
$!#			Delete pgplot reference
$!#			Make Unix awk
$!#	WNB 930429	Make VAX awk
$!#	WNB 930615	Proper delete .TMP
$!# 
$!# 	Convert the standard input to standard output
