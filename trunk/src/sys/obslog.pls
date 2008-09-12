#!/local/bin/perl -I/local/lib/perl5
#
# obslog.pls created by hjv 
#
#+ obslog.pls
#
# This script will have one argument: MMMYYYYOBS
# The program will look for the file MMMYYYYOBS.TXT
#
# It will do the following steps:
# 1 - Read logbook from wsrt00 (MMMYYYYOBS.TXT) line for line
#	every line should have at least 7 arguments:
#	1. sequencenr.
#	2. logdate (ddMmmyy)
#	3. logtime (hh:mm:ss)
#	4. name of person who added entry
#	5. category
#	6. subsystem
#	7. remarks
#	N.B. For the following combination of (category,subsystem)
#	     6 arguments are enough:
#	  (TRO,PH) - tropospherische fase fluctuaties
#	  (SUN,A.) or (SUN,C.) - zonnestoring
# read the values for every field and
# try to extract stuff from "remarks" and put the new values
# 2 - 	into the fields "HARANGE" and "DESCRIPTION"
# 3 - 	into the fields "CHANNELS" and "DESCRIPTION"
# 4 - 	into the fields "IFRS" and "DESCRIPTION"
#
# Output will be written in wsrtlog.MMMYYYYOBS
#
# HjV 970814	Created
#
# Preamble
#
#
unless (defined $VMS) {
  $VMS=0;
  if ($ENV{"SHELL"}) {				# aid routines unix
     unshift(@INC,$ENV{'n_src'}.'/sys');
  }
  unless (require "c2aid.pls") {		# general aid routines
     print "Fatal: Cannot load c2aid.pls properly";
     exit;
  }
  &ENV_IMPORT;					# get environment
  $argv=join(" ",@ARGV);			# get command arguments
}

#
$nrargs=scalar(@ARGV);
if ($nrargs != 1) {
   &echo("","One argument needed: ");
   &echo("","\tmonth and year to process on format: MMMYYYY");
   &echo("","Program stop.");
   exit;
}
($What=@ARGV[0]) =~ tr/a-z/A-Z/;

#
# Initialise
#
$Logfile="tmp.log";

#
# Construct the date/time strings, define logfile
#
$Myname=  &Pipe("p$$.tmp00", &awk( "-F:" , '{ if ($1 == "' . $USER 
                        .'") print $5 }' ,  "/etc/passwd" , "p$$.tmp00" ) ) 
                        ; 
if ( &eq(   $Myname ,  '' ) ) { $Myname=  &Pipe("p$$.tmp00", &whoami( 
                        "p$$.tmp00" ) ) ; } 
$dt=  &Pipe("p$$.tmp00", &date( "p$$.tmp00" ) ) ; 
if ( &peq(   (split(' ',$dt)) [ 3 -1 ] , "[1-9]" )	# day
                        ) { @dt=split(' ',$dt); splice(@dt, "3" -1,1,   
                        (split(' ',$dt)) [ 3 -1 ] ); $dt=join(' ',@dt); } 
$Umc= "JAN" .' '. "FEB" .' '. "MAR" .' '. "APR" .' '. "MAY" .' '. "JUN" 
                        .' '. "JUL" .' '. "AUG" .' '. "SEP" .' '. "OCT" 
                        .' '. "NOV" .' '. "DEC" ; 
$lmc= "jan" .' '. "feb" .' '. "mar" .' '. "apr" .' '. "may" .' '. "jun" 
                        .' '. "jul" .' '. "aug" .' '. "sep" .' '. "oct" 
                        .' '. "nov" .' '. "dec" ; 
$mc= "Jan" .' '. "Feb" .' '. "Mar" .' '. "Apr" .' '. "May" .' '. "Jun" 
                        .' '. "Jul" .' '. "Aug" .' '. "Sep" .' '. "Oct" 
                        .' '. "Nov" .' '. "Dec" ; 
$days= "31" .' '. "28" .' '. "31" .' '. "30" .' '. "31" .' '. "30" 
                        .' '. "31" .' '. "31" .' '. "30" .' '. "31" 
                        .' '. "30" .' '. "31" ; 
$mnr= "01" .' '. "02" .' '. "03" .' '. "04" .' '. "05" .' '. "06" .' '.
      "07" .' '. "08" .' '. "09" .' '. "10" .' '. "11" .' '. "12" ;
@mnr = split(' ',$mnr);
@days = split(' ',$days);
for $mm__x (split(' ',join(' ' , "01" , "02" , "03" , "04" , "05" , "06" 
                        , "07" , "08" , "09" , "10" , "11" , "12" ))) { 
                        $mm=$mm__x ; 
  if ( &eq(   (split(' ',$dt)) [ 2 -1 ] ,		# month
                        (split(' ',$mc)) [ $mm -1 ] ) ) { last ; } 
} 
$yy= (split(' ',$dt)) [ &vn($dt) -1 ] - 1900 ;		# year
$mh=  &Pipe("p$$.tmp00", &echo( '' ,  &fn(		# hh mm ss
                        (split(' ',$dt)) [ 4 -1 ] ) , "p$$.tmp01" ) , &tr( 
                        "-s" ,  ":" ,  " " , "p$$.tmp01" , "p$$.tmp00" ) ) 
                        ; 
$C_Date=   $yy . $mm . (split(' ',$dt)) [ 3 -1 ] ;	# date: yymmdd
$C_Time=   (split(' ',$mh)) [ 1 -1 ] .			# time: hhmm
                        (split(' ',$mh)) [ 2 -1 ] ; 
undef $dt;
system("rm -f p*.tmp*");				# Remove temp. file
#
#
#
# Step 1 - Read "MMMYYYYOBS.TXT" line for line
#	read the values for every field
#	try to extract stuff from "REMARKS" and put the new values
#		into the fields "CHANNELS", "IFRS", "HARANGE" and "DESCRIPTION"
$Infile="${What}.TXT";
$Outfile="wsrtlog.${What}";
open(IN,"$Infile") || die "Cannot open $Infile";;
open(OUT,">$Outfile") || die "Cannot open $Outfile";;
# Read the lines
$linenr=2;
$ha=$hb=$hc=$hd=$he=$hf=$ho=0;
$ca=$cb=$cc=$cd=$ce=$cf=$co=0;
$ia=$ib=$ic=$id=$ie=$ig=$ih=$ii=$ij=$ik=$io=0;
$lcom=$lerror=$linenr=0;
while ( $line = <IN>) {
    $linenr++;						# lines read
    if (substr($line,0,1) eq "#") { $lcom++; next; }	# comment line
    @line=split(' ',$line);				# delimited with space
    $ifrs=$harng=$chan=$descr="";
    $seqnr=@line[0];
    $logdate=@line[1]; 
    $logtime=@line[2]; 
    $name=@line[3]; 
    $cat=@line[4]; 
    $ssy=@line[5];
    $error="Line $linenr has not enough arguments (minimal 7 needed):";
    if (($nr_arg=scalar(@line)) < 6) {
	&echo ("","$error \n\t$line");
	$lerror++;
	next;
    }
    elsif (($nr_arg=scalar(@line)) == 6) {
	if ((($cat eq "TRO") && ($ssy eq "PH")) ||
	    (($cat eq "SUN") && (($ssy eq "A.") || ($ssy eq "C.")))) {
	    goto SCHRIJF;
	} else {
	    &echo ("","$error \n\t$line");
	    $lerror++;
	    next;
	}
    }
    for ($i=6;$i<=$nr_arg-1;$i++) {
	$descr=$descr . " " . @line[$i]; 
    }

AGAIN: {
#
#
#
# Step 2 - try to extract stuff from "remarks" and put the new values
#     	into the fields "HARANGE" and "DESCRIPTION"
#
# Select "< HA <"
    if ($descr =~ /.*( [+~-]?[0-9]+[\.[0-9]*]?[ ]?[<>][ ]?HA[ ]?[<>][ ]?[+~-]?[0-9]+[\.[0-9]*]?).*/i) {
	if ($harng eq "") {
	    $harng = $1;
	} else {
	    $harng = $1 . "," . $harng;
	}
	@newdescr=split(/$harng/,$descr);
	$descr=join(' ',@newdescr);
	$ha++;
    } 
# Select "< HA"
    elsif ($descr =~ /.*( [~]?[ ]?[+-]?[0-9]*[\.[0-9]*]?[<>][ ]?HA).*/i) {
	if ($harng eq "") {
	    $harng = $1;
	} else {
	    $harng = $1 . "," . $harng;
	}
	@newdescr=split(/$harng/,$descr);
	$descr=join(' ',@newdescr);
	$hb++;
    }
# Select "at HA ,"
    elsif ($descr =~ /.*(at HA[ ]?[ ~+<>-]+[ ]?[-+]?[0-9]*[\.[0-9]*]?[, ]?[+-]?[0-9]*[\.[0-9]*]?).*/i) {
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	($tmp=$1) =~ s/(at )(.*)/$2/i;			# remove "at "
	if ($harng eq "") {
	    $harng = $tmp;
	} else {
	    $harng = $tmp . "," . $harng;
	}
	$he++;
    }
# Select "HA <"
    elsif ($descr =~ /.*(HA[ ]?[<>][ ]?[~]?[ ]?[-+]?[0-9]*[\.[0-9]*]?).*/i) {
	$harng = $1;
	@newdescr=split(/$harng/,$descr);
	$descr=join(' ',@newdescr);
	$hc++;
    }
# Select "HA TO"
    elsif ($descr =~ /.*(HA [~]?[ ]?[+-]?[0-9]*[\.[0-9]*]? TO [+-]?[0-9]*[\.[0-9]*]?).*/i) {
	if ($harng eq "") {
	    $harng = $1;
	} else {
	    $harng = $1 . "," . $harng;
	}
	@newdescr=split(/$harng/,$descr);
	$descr=join(' ',@newdescr);
	$hd++;
    }
# Select "HA ,"
    elsif ($descr =~ /.*(HA[ ]?[ ~+-]+[ ]?[+-]?[0-9]*[\.[0-9]*]?)([\[,\]?\[ \]?\[+-\]?\[0-9\]*\[\.\[0-9\]*\]?]*).*/i) {
	$tmp = $1 . $2;
	if ($harng eq "") {
	    $harng = $tmp;
	} else {
	    $harng = $tmp . "," . $harng;
	}
	@newdescr=split(/$tmp/,$descr);
	$descr=join(' ',@newdescr);
	$he++;
    }
    else {
	$ho++;
    }
#
#
#
# Step 2 - try to extract stuff from "remarks" and put the new values
#  	   into the fields "CHANNELS" and "DESCRIPTION"
# Select Freq.channels
    if ($descr =~ /.*(F[0-9][0-9][0-9]*-F[0-9][0-9][0-9]*).*/i) {
	$chan = $1;
	@newdescr=split(/$chan/,$descr);
	$descr=join(' ',@newdescr);
	$ca++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(F[0-9]-F[0-9][0-9][0-9]*).*/i) {
	$chan = $1;
	@newdescr=split(/$chan/,$descr);
	$descr=join(' ',@newdescr);
	$cb++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(F[0-9]-F[0-9]).*/i) {
	$chan = $1;
	@newdescr=split(/$chan/,$descr);
	$descr=join(' ',@newdescr);
	$cc++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(F[0-9][0-9]* TO F[0-9][0-9]*).*/i) {
	while ($descr =~ /.*(F[0-9][0-9]* TO F[0-9][0-9]*).*/i) {
	    if ($chan eq "") {
		$chan = $1;
	    } else {
		$chan = $1 . "," . $chan;
	    }
	    @newdescr=split(/$1/,$descr);
	    $descr=join(' ',@newdescr);
	    $cd++;
	}
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(F[0-9][0-9]+).*/i) {
	while ($descr =~ /.*(F[0-9][0-9]+).*/i) {
	    if ($chan eq "") {
		$chan = $1;
	    } else {
		$chan = $1 . "," . $chan;
	    }
	    @newdescr=split(/$1/,$descr);
	    $descr=join(' ',@newdescr);
	    $ce++;
	    redo AGAIN;
	}
    } 
    elsif ($descr =~ /.*(F[0-9]).*/i) {
	while ($descr =~ /.*(F[0-9]).*/i) {
	    if ($chan eq "") {
		$chan = $1;
	    } else {
		$chan = $1 . "," . $chan;
	    }
	    @newdescr=split(/$1/,$descr);
	    $descr=join(' ',@newdescr);
	    $cf++;
	    redo AGAIN;
	}
    } 
    else {
	$co++;
    }
    $chan =~ tr/F//d;
#
#
#
# Step 4 - try to extract stuff from "remarks" and put the new values
# 	   into the fields "IFRS" and "DESCRIPTION"
#
# Select Interferometers
    if ($descr =~ /.*(RT[0-9][ ]?).*/i) {
	if ($ifrs eq "") {
	    $ifrs = substr($1,2,1) . "***";
	} else {
	    $ifrs = substr($1,2,1) . "***" . "," . $ifrs;
	}
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ia++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(RT[A-D][ ]?).*/) {		# only in capitals
	if ($ifrs eq "") {
	    $ifrs = "**" . substr($1,2,1) . "*";
	} else {
	    $ifrs = "**" . substr($1,2,1) . "*," . $ifrs;
	}
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ib++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(RT[0-9][A-D]).*/) {		# only in capitals
	if ($ifrs eq "") {
	    $ifrs = substr($1,2,1) . "*" . substr($1,3,1) . "*";
	} else {
	    $ifrs = substr($1,2,1) . "*" . substr($1,3,1) . "*," . $ifrs;
	}
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ic++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*(RT[s]? [0-9A-D,]+).*/) {	# only in capitals
	@ifrs = split(/ /,$1);
	$ifrs = @ifrs[1];
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ik++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*([0-9*][XY*][A-D*][XY*]).*/i) {
	while ($descr =~ /.*([0-9*][XY*][A-D*][XY*][,]?).*/i) {
	    ($tmp=$1) =~ tr/*/%/;
	    $ifrs = $1 . $ifrs;
	    @newdescr=split(/$tmp/,$descr);
	    $descr=join(' ',@newdescr);
	    if (index($tmp,"%") > -1) { last; }
	}
	$id++;
	if (index($tmp,"%") == -1) { redo AGAIN; }
    } 
    elsif ($descr =~ /.*([0-9][XY][XY][,]?).*/i) {
	if ($ifrs eq "") {
	    $ifrs = substr($1,0,2) . "**," . substr($1,0,1) .substr($1,2,1) . "**";
	} else {
	    $ifrs = substr($1,0,2) . "**," . substr($1,0,1) .substr($1,2,1) . "**" . $ifrs;
	}
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ie++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*([A-D][XY][XY][,]?).*/i) {
	if ($ifrs eq "") {
	    $ifrs = "**" . substr($1,0,2) . ",**" . substr($1,0,1) . substr($1,2,1);
	} else {
	    $ifrs = "**" . substr($1,0,2) . ",**" . substr($1,0,1) . substr($1,2,1) . $ifrs;
	}
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ig++;
	redo AGAIN;
    } 
    elsif ($descr =~ /.*([0-9][XY] TO [0-9][XY]).*/i) {
	$ifrs = $1;
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ih++;
    } 
    elsif ($descr =~ /.*([A-D][XY] TO [A-D][XY]).*/i) {
	$ifrs = $1;
	@newdescr=split(/$1/,$descr);
	$descr=join(' ',@newdescr);
	$ii++;
    } 
    elsif ($descr =~ /.*([0-9|A-D][XY]).*/) {		# only Capitals
	while ($descr =~ /.*([0-9|A-D][XY][,]?).*/) {
	    if ($ifrs eq "") {
		$ifrs = $1;
	    } else {
		if (index($1,",") == -1) {
		    $ifrs = $1 . "," . $ifrs;
		} else {
		    $ifrs = $1 . $ifrs;
		}
	    }
	    @newdescr=split(/$1/,$descr);
	    $descr=join(' ',@newdescr);
	}
	$ij++;
    }
    else {
	$io++;
    }
}						# end AGAIN
#
#
#
#
# Remove leading blanks from description
# Remove some dots
# replace the =-sign with the text " is "
    $descr =~ s/^  \x2F (.*)/$1/;		# remove "  / "
    $descr =~ s/^[ ]*(.*)/$1/;			# remove blanks at begin line
    $name  =~ s/[\.]*//g;			# remove dots
    $cat   =~ s/[\.]*//g;			# remove dots
    $ssy   =~ s/[\.]*//g;			# remove dots
    $descr =~ s/=/ is /g;			# replace = with " is "

SCHRIJF:
# Date format in: dd/mm/yyyy
    for ($mm=1;$mm<=12;$mm++) {
	if ( substr($logdate,2,3) eq (split(' ',$mc)) [ $mm-1 ] ) {
	    $logdate=substr($logdate,0,2) . "/" . @mnr[$mm-1] . "/19" . substr($logdate,5,2);
	    last;
	}
    }
# Write into new file
    print OUT "PUT=OBSLOG SEQNUMBER=${seqnr} LOGDATE=${logdate} LOGTIME=${logtime} name=${name} CATEGORY=${cat} SUBSYSTEM=${ssy} CHANNELS=${chan} INTERFEROMETERS=${ifrs} HA_RANGE=${harng} DESCRIPTION=${descr}\n";
}
close IN;
close OUT;
#
#
# Some statistics
#
&echo ("","\nRead $linenr lines, of which $lcom comment-lines and $lerror with not enough arguments");
&echo ("","\n\nHour-Angle         ");
&echo ("","1. <HA< a    -- $ha times");
&echo ("","2. 0-9 HA    -- $hb times");
&echo ("","3. HA 0-9    -- $hc times");
&echo ("","4. HA a-z to -- $hd times");
&echo ("","5. HA a-z ,  -- $he times");
&echo ("","   Other     -- $ho times");
&echo ("","\n\nChannels           ");
&echo ("","1. Fxx-Fxx   -- $ca times");
&echo ("","2. Fx-Fxx    -- $cb times");
&echo ("","3. Fx-Fx     -- $cc times");
&echo ("","4. Fx to Fx  -- $cd times");
&echo ("","5. Fxx       -- $ce times");
&echo ("","6. Fx        -- $cf times");
&echo ("","   Other     -- $co times");
&echo ("","\n\nInterferometers    ");
&echo ("","1. RTf       -- $ia times");
&echo ("","2. RTm       -- $ib times");
&echo ("","3. RTfm      -- $ic times");
&echo ("","4. fxmy      -- $id times");
&echo ("","5. fxy       -- $ie times");
&echo ("","6. mxy       -- $ig times");
&echo ("","7. f. to f.  -- $ih times");
&echo ("","8. m. to m.  -- $ii times");
&echo ("","9. f. of m.  -- $ij times");
&echo ("","10.RTs x,x,x -- $ik times");
&echo ("","   Other     -- $io times");
#
# Postamble
#
&ENV_EXPORT;					# save environment
#
