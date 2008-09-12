#+ CSH2P.PLS
#  WNB 940318
#
#  Revisions:
#	WNB 940531	Isolate eq, ne
#	WNB 940613	Correct elm switches
#
# perl csh2p.pls file
#	will translate csh script file to a perl script with name.pls
#-
#
#  Intro
#
if ($ENV{'SHELL'}) { unshift(@INC,$ENV{'n_src'}.'/sys');}
else { unshift(@INC,'N_SRC:[SYS]');}
unless (require 'c2aid.pls') {		# some general routines
  print 'Fatal: Errors in loading c2aid.pls'; exit;}
&ENV_IMPORT;				# get environment
#
# Compilation constants
#
$C2_npos=2;				# spaces per indent level
$C2_ncom=56;				# position of comment
$C2_nlen=78;				# length line
$C2_nind=24;				# minimum indent for continuation
$C2_nstr=44;				# maximum string bit length
$OUT_code=';';				# statement end character
%C2_cmode=(	's',	'M_sw',		# command arguments modes
		'i',	'M_in',
		'o',	'M_out',
		't',	'M_int',
		'w',	'M_inw',
		'f',	'M_inf');
%C2_cmd=(	'ar',		'two',	# commands with s(witch),i(n),o(ut) etc
		'awk',		'stio',
		'cat',		'sio',
		'cd',		'i',
		'chdir',	'i',
		'chmod',	'ti',
		'cmp',		'wo',
		'compress',	'wo',
		'cp',		'sw',
		'date',		'o',
		'diff',		'swo',
		'domainname',	'o',
		'echo',		'swo',
		'elm',		'swi',
		'exit',		't',
		'find',		'io',
		'ftp',		'sio',
		'grep',		'stio',
		'head',		'sio',
		'ln',		'si',
		'ls',		'sio',
		'mem',		'i',
		'mkdir',	'i',
		'more',		'sio',
		'mv',		'si',
		'nm',		'io',
		'pr',		'sio',
		'ranlib',	'i',
		'remsh',	'tiio',
		'rm',		'si',
		'rsh',		'tiio',
		'sed',		'stio',
		'sort',		'sio',
		'strip',	'i',
		'tail',		'sio',
		'tar',		'tio',
		'stty',		't',
		'tee',		'sii',
		'touch',	'si',
		'tr',		'sttio',
		'uncompress',	'io',
		'wc',		'sio',
		'what',		'sio',
		'whoami',	'o');
#
# Check for secondary file
#
unless ($ARGV[0]=~/^\-/) { $C2_mrout=1;}	# set main routine
#
# Do all files
#
ALL:
while ($C2_in=&fn(shift(@ARGV))) {	# input files
  local($C2_cnt);			# line count
  local($C2_ifd);			# if depth
  local($C2_incid);			# post increment depth if
  local($C2_ford);			# for/while depth
  local($C2_incfd);			# post increment depth for/while
  local($C2_labd,%C2_labdp);		# label depth
  local($C2_nocom);			# no comment seen indicator
  local($C2_tcnt);			# << text count
  local($C2_acnt)='1000';		# metacharacters count
#
# Open files
#
  $C2_in="$C2_in.csh" unless &fp('e',$C2_in);
  $C2_innam=&fp('t',&fp('r',$C2_in));
  open(C2_IN,$C2_in) || print "Fatal: Cannot open input file $C2_in\n",
			next ALL;
  $C2_out=&fp('r',$C2_in).'.pls';	# output file
  open(C2_OUT,">$C2_out") || print "Fatal: Cannot open output file $C2_out",
			close(C2_IN),next ALL;
#
# Preamble
#
  print C2_OUT "#+ $C2_out\n#   created by $ENV{'USER'} on $ENV{'HOST'} at ".
		&date."\n#-\n";
#
#  Do all lines
#
FILE:
  while (&R_line) {
    &P_line(&DO_line('',@C2_line));
  }
#
# Finish input file
#
  print C2_OUT "#\n#+ Postamble\n#\n";
  print C2_OUT "#\n# Finish main routine\n#\n";
  $C2_ifd--; &P_line(''); $C2_ifd++;
  print C2_OUT "#\n# Call main routine\n#\n";
  print C2_OUT "eval('&${C2_innam}__pls');\n1;\n";
  print C2_OUT "#-\n";
  close(C2_IN); close(C2_OUT);
  if ($C2_ifd) { print "Error: Unclosed if statement found\n";}
  if ($C2_ford) { print "Error: Unclosed for/while statement found\n";}
  if ($C2_labd) { print "Error: Unclosed subroutine statement found\n";}
  print "$C2_out produced from $C2_in\n";
#
# Next file
#
}
#
# Clear apostrophe and other meta counts
#
sub C_acnt {

  undef %C2_acnt; $C2_acnt='1000';}		# counts
#
# Strip command line of superfluous characters
#
sub P_strip {

  local(@line)=@_;

  for ($i=1; $i<=$#line; $i++) {		# strip superfluous char
    if ($line[$i] eq ';' && ($line[$i-1]=~/\}$/ ||
		$line[$i-1]=~/\{$/ || $line[$i-1]=~/;$/)) {
      splice(@line,$i,1); $i--; next;}
    if ($line[$i] eq ',' && $line[$i-1]=~/,$/) {
      splice(@line,$i,1); $i--; next;}
    if ($line[$i] eq ')' && $line[$i-1] eq ',') {
      splice(@line,$i-1,1); $i--; next;}
  }
  @line;
}
#
# Print resulting line (@line)
#
sub P_line {

  local(@line)=@_;				# line parts
  local($t)=(&C2_indent);			# indentation
  local($tl)=length($t);			# length indent
  local($com);					# current comment
  local($len)=($C2_ncom);			# current line length
  local($p,$pl,$i,$j);				# current part and length

  for ($i=$C2_labd;$i>0;$i--) {			# check label subroutines
    if ($C2_ifd+$C2_ford<$C2_labdp{$i}) {	# end label subroutine
      $C2_ifd++; &P_line('&exit(\'\');}'); $C2_labd--; $C2_ifd--;
      $t=&C2_indent;}
  }
  @line=&P_strip(@line);			# strip superfluous characters
  $p=$t;					# line part indent
  while (@line) {
    if (@C2_com) { ($com=shift(@C2_com))=~s/^#//; $len=$C2_ncom;} # more comment
    else {$com=''; $len=$C2_nlen;}
    $p.=shift(@line).' ';			# first part
    $pl=length($p)+1;
    while (@line) {
      if ((($i=length($j=shift(@line))+1)+$pl)<$len) {
        $p.="$j "; $pl+=$i;}
      else {unshift(@line,$j); last;}
    }    
    if ($com) { $j=(($i=$C2_ncom-$pl)>1 ? ' ' x $i : ' ' )."#$com";}
    else {$j='';}
    print C2_OUT "$p$j\n" unless "$p$j"=~/^\s*$/; # print part
    $p=' ' x ((($i=$tl+2*$C2_npos)>$C2_nind) ? $i : $C2_nind); #reset
  }
  while (@C2_com) { ($com=shift(@C2_com))=~s/^#//; # more comment
    print C2_OUT (' ' x $C2_ncom)."#$com\n";}
}
#
# Calculate current indentation
#
sub C2_indent {				# output indentation

  ('  ' x ($C2_ifd+$C2_ford+$C2_labd),$C2_ifd+=$C2_incid,
		$C2_ford+=$C2_incfd,$C2_incid=$C2_incfd=0)[0];}
#
# line = Replace \char with proper code (line)
#
sub G_non {

  local($line)=@_;

  while ($line=~s/\\(.)/\@\@n$C2_acnt/) {  # isolate \char
    $C2_acnt{"n$C2_acnt"}=$1; $C2_acnt++;}
  $line;}
#
# line = Replace line with proper code (line)
#
sub G_word {				# isolate line

  local($line)=@_;

  $line=~s/^(.*)$/\@\@w$C2_acnt/;
  $C2_acnt{"w$C2_acnt"}=$1; $C2_acnt++;
  $line;}
#
# line = Replace `` strings with proper code (line)
#
sub G_pipe {				# isolate `` strings

  local($line)=@_;

  while ($line=~s/`([^`]*)`/\@\@p$C2_acnt/) { # isolate `
    $C2_acnt{"p$C2_acnt"}=($C2_acnt++,&G_apo($1));} # isolate ',"
  $line;}
#
# line = Replace '' and "" strings with proper code (line)
#
sub G_apo {				# Get apostrophes

  local($line)=@_;

  while ($line=~/(['"])/) {
    if ($1 eq '"') {
      if ($line=~s/"([^"]*)"/\@\@d$C2_acnt/) {
          $C2_acnt{"d$C2_acnt"}=$1; $C2_acnt++;}
      else {$line=~s/"/'"'/;}
    }
    else {
      if ($line=~s/'([^']*)'/\@\@s$C2_acnt/) {
        $C2_acnt{"s$C2_acnt"}=$1; $C2_acnt++;}
      else {$line=~s/'/"'"/;}
    }
  }
  $line;
}					# end G_apo
#
# @line=replace () string with proper special code (@line)
#
sub G_paren {				# get ()

  local(@line)=@_;
  local($i,$cnt,$pb)=(0,0,0);
  local(@pm);

  for ($i=0;$i<=$#line;$i++) {
    if ($line[$i] eq '(') {
      $pb=$i unless $cnt; $cnt++;}
    elsif ($line[$i] eq ')') {
      unless ($cnt) {$cnt++; last;}
      $cnt--;
      unless ($cnt) { @pm=splice(@line,$pb,$i-$pb+1,"\@\@e$C2_acnt");
        shift(@pm); pop(@pm); $C2_acnt{"e$C2_acnt"}=join(' ',@pm);
        $C2_acnt++; $i=$pb;}
    }
  }
  print "Unmatched () in line $C2_cnt:\n@line\n" if $cnt;
  @line;}
#
# line=replace ${} or [] string with proper code (line)
#
sub G_sparen {

  local($t)=@_;
  local($i,$cnt,$pb,$l,$ps)=(0,0,0,0,0);
  local($pm)=('');

  while ($t=~/\$\{/) { $l=length($t); $pb=index($t,'${'); $cnt=1;
    for ($i=$pb+2; $i<=$l; $i++) {
      if (substr($t,$i,1) eq '{') { $cnt++;}
      elsif (substr($t,$i,1) eq '}') { $cnt--;
        unless ($cnt) { $pm=substr($t,$pb+2,$i-$pb-2);
	  substr($t,$pb,$i-$pb+1)="\@\@c$C2_acnt";
	  $C2_acnt{"c$C2_acnt"}=$pm; $C2_acnt++; last;}
      }
    }
    if ($cnt) { print "Unmatched \${} in line $C2_cnt: $t\n"; last;}
  }
  while ($t=~/\$\w+\[/) { $ps=length($`); $l=length($t);
    $pb=$ps+index($&,'['); $cnt=1;
    for ($i=$pb+1; $i<=$l; $i++) {
      if (substr($t,$i,1) eq '[') { $cnt++;}
      elsif (substr($t,$i,1) eq ']') { $cnt--;
	unless ($cnt) { $pm=substr($t,$pb+1,$i-$pb-1);
	  substr($t,$pb,$i-$pb+1)="\@\@q$C2_acnt";
	  $C2_acnt{"q$C2_acnt"}=$pm; $C2_acnt++; last;}
      }
    }
    if ($cnt) { print "Unmatched [] in line $C2_cnt: $t\n"; last;}
  }
  $t;
}
#
# line=replace $name[]:l string with proper code (line)
#
sub G_vname {

  local($t)=@_;

  while ($t=~s/\$[\w<]\w*(\@\@q\d\d\d\d)?(:[a-z])?/\@\@c$C2_acnt/) {
    $C2_acnt{"c$C2_acnt"}=substr($&,1); $C2_acnt++;}
  $t;
}
#
# string=Obtain a special isolated '"`() string(name)
#
sub G_obtain {

  local($name)=@_;

  delete $C2_acnt{$name};}		# get string
#
# string=Get a special isolated '"`() string with '"`()(name)
#
sub G_get {

  local($name)=@_;
  local($str,$i);

  $str=delete $C2_acnt{$name};		# get string
  if (($i=substr($name,0,1)) eq 'p') {	# `
    $str="`$str`";}
  elsif ($i eq 's') {
    $str="'$str'" unless $str eq '"';}
  elsif ($i eq 'd') {
    $str="\"$str\"" unless $str eq "'";}
  elsif ($i eq 'e') { $str="( $str )";}
  elsif ($i eq 'n') { $str="\\$str";}
  elsif ($i eq 'w') {}
  elsif ($i eq 'c') { $str="\${$str}";}
  elsif ($i eq 'q') { $str="[$str]";}
  else {$str='';};
  $str;}
#
# line= restore all special '"`() strings with symbols (line)
#
sub G_all {

  local($line)=@_;
  local($i);

  while($line=~/\@\@([a-z]\d\d\d\d)/) {
    $i=&G_get($1); $line=~s/\@\@([a-z]\d\d\d\d)/$i/;}
  $line;}
#
# @line= split into words (line)
#
sub L_word {

  local($line)=@_;
  local(@line,@res);

  $line=&G_non($line);				# remove all \char
  @line=split(' ',$line);			# split on blank space
  @res=();					# built new
  for (@line) {					# split ; ( ) < << | || |&
    push(@res,split(/([;\(\)]|\$<|<=|<<?|\|[\&\|]?)/));}
  @line=();
  for (@res) {					# split >... & &&
    push(@line,split(/(>[=>]?\&?\!?|[^\|]+\&\&?)/));}
  @line;
}  
#
# file= Make next STDOUT file name (file)
#
sub N_file {

  local($file)=@_;
  local(@t);

  if ($file) {@t=reverse(split(//,$file)); $t[0]++;
    join('',reverse(@t));}
  else {'p$$.tmp00';}
}
#
# Error=Read a line into @C2_line and @C2_com
#
sub R_line {

  local($eod);					# << seen
  local($line,$i);

  &C_acnt;					# reset '"`() cnts
  @C2_line=@C2_com=();				# no output
L1:
  while (<C2_IN>) {				# read lines
    chop;$C2_cnt++; @C2_com=();			# count input line; no comments
    if (/^\s*#/) {print C2_OUT "$_\n"; next L1;} # full comment line
    if (/^\s*$/) {next L1;}			# empty line
    unless ($C2_nocom) { $C2_nocom=1;		# skipped all initial comments
      print C2_OUT "#+\n# Preamble\n#\n";
      &P_line(scalar(@C2_com=" check for environment",
			'unless (defined $VMS) {')); $C2_ifd++;
      &P_line(scalar(@C2_com=" aid routines unix",
			'if ($ENV{"SHELL"}) {')); $C2_ifd++;
      &P_line('unshift(@INC,$ENV{\'n_src\'}.\'/sys\');}'); $C2_ifd--;
      &P_line(scalar(@C2_com=" aid routines VMS",
			'else {')); $C2_ifd++;
      &P_line('unshift(@INC,\'N_SRC:[SYS]\');}'); $C2_ifd--;
      &P_line('unless (require \'c2aid.pls\') {'); $C2_ifd++;
      &P_line('print "Fatal: Cannot load c2aid.pls properly"; exit;}');
      $C2_ifd--;
      &P_line(scalar(@C2_com=" get environment",
			'&ENV_IMPORT;'));
      &P_line(scalar(@C2_com=" get command arguments",
			'$argv=join(\' \',@ARGV);}'));$C2_ifd--;
      if ($C2_mrout) {				# main routine
        @C2_com=" renew main routine";
        &P_line('if (&ft("e",&fp("r","$0").".csh") &&',
			'(&ft("M","$0") > &ft("M","$n_src/sys/csh2p.pls") ||',
			'&ft("M","$0") > &ft("M",&fp("r","$0").".csh"))) {');
        $C2_ifd++;
        &P_line('$status=&system("perl ".&fnp("$n_src/sys/csh2p.pls")." ".',
			'&fp("r","$0"));}'); $C2_ifd--;
      }
      print C2_OUT "#\n# Start translated script\n#-\n";
      &P_line("sub ${C2_innam}__pls {");
      $C2_labd++; $C2_labdp{$C2_labd}=$C2_ifd+$C2_ford;}
    s/\s+$//;					# remove trailing blanks
    if (($i=chop) eq "\\") {			# continuation line
      $line.=$_;}				# restore state and add line
    else { $line.="$_$i"; last L1;}
  }
  $line=&G_apo(&G_pipe($line));			# isolate '"`
  if ($line=~/([^\\\$\{]#)/) {	        	# split off comments
    push(@C2_com,&G_all(substr($line,$i=index($line,$1)+1)));
    $line=substr($line,0,$i);}
  push(@C2_line,&L_word($line));		# make line into words
  for ($i=0;$i<=$#C2_line;$i++) {		# find if << data
    if ($C2_line[$i] eq '<<') {
      if (defined $C2_line[$i+1]) {
        if ($C2_line[$i+1]=~/^[a-z_A-Z]\w*$/) {
	  $C2_line[$i+1]=&G_word($C2_line[$i+1]);} # make recognisable
        if ($C2_line[$i+1]=~/^\@\@([psdw]\d\d\d\d)$/) { # correct <<
	  $eod=&G_get($1); $C2_tcnt++;		# string to recognise
	  $C2_line[$i+1]=$C2_tcnt;
          &P_line("sub C2_t${C2_tcnt}_$C2_innam {");
          $C2_ifd++;				# make a subroutine
          &P_line('local(*TMP);');
          &P_line('open(TMP,">txt$$.tmp");');
          while ($eod) {			# << seen
	    if ($_=<C2_IN>) {
	      $C2_cnt++; chop;
              if (/^$eod\s*$/) {$eod='';	# end seen
                &P_line('close(TMP);');
                &P_line('"txt$$.tmp";}');$C2_ifd--;} # show result
	      else {
		if ($eod=~/^'/) {
		  &P_line('print TMP ',&MS_word(&G_apo("'$_'")),
				'."\n"',$OUT_code);}
		elsif ($eod=~/^"/) {
		  &P_line('print TMP ',&MS_word($_,1),'."\n"',
				$OUT_code);}
		else {
		  &P_line('print TMP ',&MS_word(&G_non(&G_pipe($_)),1),
				'."\n"',$OUT_code);}
	      }
	    }
	    else { die "unclosed << data at end of file $C2_in";}
          }
          last;}
      }
    }
  }
  scalar(@C2_line=&G_paren(@C2_line));		# result
}
#
# @line= Compile a line (STDOUT,@line)
#
sub DO_line {

  local($DOP_file,@line)=@_;			# default STDOUT, input line
  local($ps,$pe,$i);
  local(@res);					# result

  for ($i=0;$i<=$#line;$i++) {			# remove empty words
    unless ($line[$i] ne '') {splice(@line,$i,1); $i--;}
  }
  for (@line) {
    if ($_ eq ';') {
      if ($pe > $ps) { push(@res,&DO_pipe($DOP_file,@line[$ps..$pe-1]));}
      $ps=$pe+1;}
    $pe++;
  }
  if ($pe > $ps) { push(@res,&DO_pipe($DOP_file,@line[$ps..$pe-1]));} # result
  @res;
}
#
# @line= Compile a pipe (STDOUT,@line)
#
sub DO_pipe {

  local($DOP_file,@line)=@_;			# default STDOUT, input pipe
  local($IP_file,$OP_file);			# in/out redirecting
  local($ps,$pe);
  local(@res);					# result

  $OP_file=$DOP_file;				# default STDOUT
  if ($C2_goto) { $C2_gotos=1; $C2_goto=0;}	# special goto/label handling
  else {$C2_gotos=0;}
  for (@line) {
    if ($_ eq '|' || $_ eq '|&') {
      $OP_file=&N_file($OP_file);		# next STDOUT
      if ($_ eq '|&') {$OP_File="&$OP_file";}
      push(@res,&DO_stmt($IP_file,$OP_file,@line[$ps..$pe-1]),
			$OUT_code);		# get result
      $OP_file=~s/^&//;				# remove &
      $IP_file=$OP_file;			# next STDIN
      $ps=$pe+1;}
    $pe++;
  }
  if ($pe > $ps) {
    push(@res,&DO_stmt($IP_file,$DOP_file,@line[$ps..$pe-1]),$OUT_code);}
  @res;
}
#
# @line= Compile a stmt (STDIN,STDOUT,@line)
#
sub DO_stmt {

  local($IP_file,$OP_file,@line)=@_;		# default STDIN, STDOUT, stmt
  local(@res,$line);				# result
  local($IN_alias);
  local(@t,@q,$i);

#
# Set correct input/output order
#
  if ($#line >= 3 && $line[$#line-1]=~/^</) {
    if ($line[$#line-3]=~/^>/) {
      local($l1,$l2,$l3,$l4)=(pop(@line),pop(@line),pop(@line),pop(@line));
      push(@line,$l2,$l1,$l4,$l3);}
  }
#
# Remove '' around command
#
  if ($line[0]=~/^\@\@(s\d\d\d\d)$/) {$line[0]=&G_obtain($1);}
#
# Check for dynamic terms (alias)
#
  $line=shift(@line);				# command
  for(@line) {
    if (/\![\*\:]/) { local($t);
      push(@res,'&doalias_x(');
      while( @line && $line[0]!~/^>/) {
        push(@res,$t,&MS_word(shift(@line),1)); $t=',';}
      for ($i=0; $i<=$#res; $i++) {$res[$i]=~s/"\$/"\\\$/g;}
      if ($IP_file) { push(@res,',',"'<'",',',"\"$IP_file\"");}
      if ($OP_file) { push(@res,',',"'>'",',',"\"$OP_file\"");}
      push(@res,')',$OUT_code); $IN_alias=1; last;}
  }
#
# %set
#
  if ($line eq 'set') { local(@t,$i,$val,$name,$t);
    for (@line) { push(@t,split(/(=)/));}	# isolate =
    @line=split(' ',join(' ',@t));
    if ($#line>2 && $line[2] eq '=') { splice(@line,2,2,"@line[2,3]");}
    if ($IN_alias) { push(@res,'&set(',&M_inw,',',&M_out,')');}
    elsif (! @line) {push(@res,"&set('',".&M_out.")");} # show only
    else {
      for ($i=0;$i<=$#line;$i++) {		# do all settings
        if ($line[$i]=~/^\$/) { push(@res,"&set($line[$i])");}
        else { $val=''; $name=$line[$i];
          unless ($i < $#line && $line[$i+1] eq '=') {} # empty set
          elsif ($i == $#line-1) { $i++;}		# no word given
          else { $val=$line[$i+2]; $i+=2;}
          $t='';
          unless ($val) {push(@res,"\$$name=''");}
          elsif ($val=~/^\@\@(e\d\d\d\d)$/) {	# list
            push(@res,"\$$name=");
            for (split(' ',&G_obtain($1))) {
              if ($t) { push(@res,$t,&MS_word($_));}
              else { push(@res,&MS_word($_)); $t=".' '.";}
            }
          }
          else {
            if ($name =~ /^(\w+)\[(.+)\]$/) { local($l1,$l2)=($1,$2); local(@l2);
              @l2=&MS_word($l2);
              push(@res,"\@$l1=split(' ',\$$l1);",
                  "splice(\@$l1,",@l2,'-1,1,',&MS_word($val),');',
                  "\$$l1=join(' ',\@$1);");}
            else {push(@res,"\$$name=",&MS_word($val));}
          }
        }
      }
    }
    @line=();
  }
#
# %setenv
#
  elsif ($line eq 'setenv') { local(@l1);
    push(@res,"\$$line[0]=",@l1=&MS_word($line[1]),"$OUT_code");
    push(@res,"&ENV_EXPORT(",shift(@line),',',@l1,')'); shift(@line);}
#
# %unset
#
  elsif ($line eq 'unset') {
    while ($i=shift(@line)) {
      if ($i=~/^\$/) {
        push(@res,"eval(\"undef \\\$$i\")",$OUT_code);}
      else { push(@res,"undef \$$i",$OUT_code);}
    }
  }
#
# %@
#
  elsif ($line eq '@') {
    $i=shift(@line); shift(@line);
    push(@res,"\$$i=",&M_exp(join(' ',splice(@line,0))));}
#
# %if
#
  elsif ($line eq 'if') {
    push(@res,'if (',&M_exp(&G_obtain(substr(shift(@line),2,5))),') {');
    if ($line[0] eq 'then') { shift(@line); $C2_incid++;}
    else {
      push(@res,&DO_stmt($IP_file,$OP_file,@line),$OUT_code,'}');
      @line=();}
  }
#
# %else
#
  elsif ($line eq 'else') {
    $C2_ifd--; &P_line('}'); $C2_incid++;
    if ($line[0] eq 'if') { shift(@line);
      push(@res,'elsif (',&M_exp(&G_obtain(substr(shift(@line),2,5))),') {');
      shift(@line);}
    else { push(@res,'else {');}
  }
#
# %endif
#
  elsif ($line eq 'endif') {
    $C2_ifd--; push(@res,'}');}
#
# %while
#
  elsif ($line eq 'while') { $C2_incfd++;
    push(@res,'while (',&M_exp(&G_obtain(substr(shift(@line),2,5))),') {');}
#
# %foreach
#
  elsif ($line eq 'foreach') { $C2_incfd++;
    $i=shift(@line);
    push(@res,"for \$${i}__x (split(' ',join(' '");
    @line=split(' ',&G_obtain(substr(shift(@line),2,5)));
    while (@line) { push(@res,',',&MS_word(shift(@line)));}
    push(@res,'))) {',"\$$i=\$${i}__x");}
#
# % break
#
  elsif ($line eq 'break') {
    push(@res,'last');}
#
# % continue
#
  elsif ($line eq 'continue') {
    push(@res,'next');}
#
# %end
#
  elsif ($line eq 'end') { $C2_ford--;
    push(@res,'}');}
#
# %label
#
  elsif ($line=~s/:$//) {
    unless ($C2_gotos) {
      push(@res,"&${line}_$C2_innam",$OUT_code); &P_line(@res); @res=();}
    push(@res,'sub ',"${line}_$C2_innam",'{'); &P_line(@res); @res=();
    $C2_labd++; $C2_labdp{$C2_labd}=$C2_ifd+$C2_ford;
    push(@res,&DO_stmt($IP_file,$OP_file,@line));
  }
#
# %goto
#
  elsif ($line eq 'goto') { $C2_goto++;		# set seen
    push(@res,'&'.shift(@line)."_$C2_innam");}
#
# %onintr
#
  elsif ($line eq 'onintr') {
    push(@res,"\$SIG{'INT'}=",shift(@line)."_$C2_innam");}
#
# %alias
#
  elsif ($line eq 'alias') {
    local($l1)=(shift(@line));			# name
    local(@l2)=&M_inw;				# value
    local($l2)=eval("@l2");
    local($l3)=(&M_out);			# output
    local(@C2_line);				# to compile
    $l2=~s/\\([\!])/\1/g;			# remove \ from !
    unless ($l2) { push(@res,"&alias(","'$l1','',","$l3)");}
    else { local(@t);
      push(@C2_line,&L_word(&G_apo(&G_pipe($l2)))); # make proper line
      @t=&P_strip(&DO_line($OP_file,@C2_line));	# compiled line
      push(@res,"&alias(","'$l1',");			# and save
      for (@t) {
        s/'/\\'/g; push(@res,"'$_'.");}
      push(@res,"'',","$l3)");}
    $C2_alias{$l1}=1;				# set seen
  }
#
# %unalias
#
  elsif ($line eq 'unalias') {
    local($l1)=(shift(@line));			# name
    push(@res,"&unalias('$l1')");}
#
# %shift
#
  elsif ($line eq 'shift') { local($l1)=(shift(@line));
    $l1='argv' unless $l1;
    push(@res,"\@$l1=split(' ',\$$l1)",$OUT_code);
    push(@res,"shift(\@$l1)",$OUT_code);
    push(@res,"\$$l1=join(' ',\@$l1)");}
#
# %source
#
  elsif ($line eq 'source') {
    push(@res,'&source(',&MS_word(shift(@line)),')');}
#
# %commands
#
  elsif ($C2_cmd{$line} || $line=~s/^\/usr\/bin\/tr$/tr/) {
    local(@t)=split(//,$C2_cmd{$line}); push(@res,"&$line(");
    for (@t) { push(@res,eval("&$C2_cmode{$_}"),',');}
    push(@res,')');}
#
# %alias seen
#
  elsif ($C2_alias{$line}) {
    push(@res,"&doalias('$line'");
    while(@line) {
      push(@res,',',&MS_word(shift(@line)));}
    push(@res,')');
  }
#
# %.csh
#
  elsif ($line=~/\.csh$/) {
    push(@res,'&docsh(',&MS_word($line),',',&M_in,',',&M_out,')');}
#
# %.exe
#
  elsif ($line=~/\.exe$/) {
    push(@res,'&doexe(',&MS_word($line),',',&M_in,',',&M_out,')');}
#
# %$
#
  elsif ($line=~s/^\$//) {
    push(@res,"&dollar(\"$line\"",',',&M_in,',',&M_out,')');}
#
# %()
#
  elsif ($line=~/\@\@(e\d\d\d\d)$/) { local($l1)=(&G_obtain($1));
    local(@l3)=(&G_paren(&L_word($l1)));
    push(@res,&DO_line($OP_file,@l3));}
#
# %alias assumed
#
  elsif ($line) {
    push(@res,"&doalias('$line'");
    while(@line) {
      push(@res,',',&MS_word(shift(@line)));}
    push(@res,')');
  }
#
# %Unknown
#
  if (@line) { push(@C2_com,"### @line");
    print "Error: line $C2_cnt containes uncompiled part:\n",
			"@line\n";}
  @res;
}
#
# Make proper parts of a statement
#
# Make output file name from @line
#
sub M_out {

  local(@res);

  if (&M_hist('out')) {}
  elsif ( @line && $line[0]=~/^>(.*)$/) { shift(@line);
    $1=~s/\!$//;
    push(@res,"'$1'.",&MS_word(shift(@line)));}
  if (@res) {@res;}
  else {@res="\"$OP_file\"";}
}
#
# Make input file names from @line
#
sub M_in {

  local(@res,$t);

  if (&M_hist('in')) { }
  else {
    while( @line && $line[0]!~/^>/) {
      if ($line[0] eq '<<') { shift(@line); shift(@line);
        push(@res,$t,"&C2_t${C2_tcnt}_$C2_innam");}
      elsif ($line[0] eq '<') { shift(@line);
        push(@res,$t,&MS_word(shift(@line)));}
      else { push(@res,$t,&MS_word(shift(@line)));}
      $t=".' '.";}
  }
  if (@res) {@res;}
  else {@res="\"$IP_file\"";}
}
#
# Make input term from @line
#
sub M_int {

  local(@res);

  if (&M_hist('int')) { }
  elsif( @line && $line[0]!~/^[<>]/) {
    push(@res,&MS_word(shift(@line)));}
  if (@res) {@res;}
  else {"''";}
}
#
# Make input word list from @line
#
sub M_inw {

  local(@res,$t);

  if (&M_hist('inw')) { }
  else {
    while( @line && $line[0]!~/^[<>]/) {
      push(@res,$t,&MS_word(shift(@line))); $t=".' '.";}
  }
  if (@res) {@res;}
  else {"''";}
}
#
# Make switch list from @line
#
sub M_sw {

  local(@res);

  if (&M_hist('sw')) { @res;}
  else {
    while ($line[0]=~/^[\-\+]/) {push(@res,shift(@line));}
    if (@res) { '"'.join(' ',@res).'"';}
    else {"''";}
  }
}
#
# Make field from @line
#
sub M_inf {

  local(@res);

  if (&M_hist('inf')) { @res;}
  else {
    if (@line && $line[0]!~/^[<>]/) { push(@res,shift(@line));}
    if (@res) { '"'.join(' ',@res).'"';}
    else {"''";}
  }
}
#
# Make a history reference
#
sub M_hist {

  local($n,$t)=@_;

  if ($IN_alias) { push(@res,"&D_input('$n')"); 1;}
  else {0;}
}
#
# Make a single term
#
sub MS_term {

  local($t,$nog)=@_;
  local($l1,$l2,@l3,@res,$lval);

  $t=~s/\${?([1-9])}?/\$argv\[\1\]/g;			# cater for $n
  $t=&G_sparen($t);					# isolate ${}, []
  if ($t =~ /^\$<$/) {					# $<
    push(@res,'($_=scalar(<STDIN>),','chop,','$_)');}
  elsif ($t =~ /^\$0$/) {				# $0
    push(@res,'$0');}
  elsif ($t =~ /^\$\$$/) {				# $$
    push(@res,'$$');}
  elsif ($t =~ /^\$\*$/) {				# $*
    push(@res,'join(" ",@argv)');}
  elsif ($t =~ /^\$#(\w+)$/) {				# $#
    $lval='d'; push(@res,"&vn(\$$1)");}
  elsif ($t =~ /^\$\?(\w+)$/) {				# $?
    push(@res,"defined(\$$1)");}
  elsif ($t =~ /^\$(\w+)$/) {				# $name
    push(@res,"\$$1");}
  elsif ($t =~ /^0\d+$/) {				# 0d..
    $lval='d'; push(@res,"\"$t\"");}
  elsif ($t =~ /^[\-\+]?\d+$/) {			# +-d...
    $lval='d'; push(@res,"$t");}
  elsif ($t =~ /^\$(\w+)(\@\@q\d\d\d\d)?:([a-z])$/) {	# $...:l
    push(@res,"&fp('$3',",&MS_term("\$$1$2",$nog),')');}
  elsif ($t =~ /^\@\@(c\d\d\d\d)$/) {			# ${name...}
    push(@res,&MS_term('$'.&G_obtain($1),$nog));}
  elsif ($t =~ /^\$(\w+)\@\@(q\d\d\d\d)$/) {		# $name[]
    ($l1,$l2)=($1,&G_obtain($2));
    @l3=split(/(\-)/,$l2);
    push(@res,"(split(' ',\$$l1))","[",&MS_term($l3[0],$nog),'-1');
    if ($l3[1] eq '-') { push(@res,'..');
      if ($l3[2]) { push(@res,&MS_term($l3[2],$nog).'-1');}
      else { push(@res,"&vn(\$$l1)-1");}
    }
    push(@res,']');}
  elsif ($t =~ /^\@\@(p\d\d\d\d)$/) {			# ``
    $OUT_code=',';					# indicate
    $l1=&G_obtain($1);					# get code
    @l3=&G_paren(&L_word($l1));				# break up in words, ()
    push(@res,'&Pipe("'.&N_file($OP_file).'",',
			&DO_line(&N_file($OP_file),@l3));
    pop(@res); push(@res,')');				# finish code
    $OUT_code=';';}					# reset
  else {						# other
    push(@res,&MS_word($t,$nog));}
  $dval='' unless $lval; @res;
}
#
# Make a single word
#
sub MS_word {

  local($t,$nog)=@_;
  local($l1,$l2,$q,@res);

  $t=&G_sparen($t);					# isolate ${}, []
  if ($t=~/\$</) { $nog=1;}				# no globbing
  $t=&G_vname($t);					# isolate variable names
  $t=~s/"/\\"/g;					# make sure no single "
  while ($t=~/\@\@(n\d\d\d\d)/) {			# restore \
    if ($nog || $C2_acnt{$1} eq '$') {
      $t=~s/\@\@(n\d\d\d\d)/\\$C2_acnt{\1}/;}		# restore \
    else {
      $t=~s/\@\@(n\d\d\d\d)/$C2_acnt{\1}/g;}		# restore \ char
  }
  if ($t=~/\@\@([sdp]\d\d\d\d)/) { $nog=1;}		# no globbing
  while ($t=~/\@\@([a-z]\d\d\d\d)/) {			# scan string
    ($l1,$l2,$t)=($1,$&,$');				# save remainder
    if ($`) { $`=~s/\\$/\\\\/; push(@res,"$q\"$`\""); $q='.';}
    if ($l1=~/^d/) { push(@res,$q,&MS_word(&G_obtain($l1),$nog));}
    elsif ($l1=~/^s/) { local($t)=&G_obtain($l1);
      while (length($t)>$C2_nstr) {
        push(@res,"$q'".substr($t,0,$C2_nstr)."'");
        $t=substr($t,$C2_nstr); $q='.';}
      push(@res,"$q'".substr($t,0,$C2_nstr)."'");
    }
    elsif ($l1=~/^c/) { push(@res,$q,&MS_term($l2,$nog));}
    elsif ($l1=~/^p/) { push(@res,$q,&MS_term($l2,$nog));}
    if (@res) {$q='.';}
  }
  if ($t) { $t=~s/\\$/\\\\/;
    while (length($t)>$C2_nstr) {
      push(@res,"$q\"".substr($t,0,$C2_nstr).'"');
      $t=substr($t,$C2_nstr); $q='.';}
    push(@res,"$q\"".substr($t,0,$C2_nstr).'"');
  }
  push(@res,"''") unless @res;
  if (@res && !$nog && "@res"=~/[^\\]?\$|[\*\?]/) {
    unshift(@res,'&fn('); push(@res,')');}
  @res;  
}
#
# Make an expression
#
sub M_exp {

  local($t)=@_;
  local($dval)='d';
  local($l1,$l2,@l3,@l4,@l5,@res);

  @l3=(&G_paren(&L_word($t)));			# make separate items
  for (@l3) {					# split | || & && > >> >=
						#     < <= << ! !~ != == =~ %
    push(@l4,split(/(\|\|?)|(\&\&?)|(<[<=]?)|(>[>=]?)/));}
  @l3=@l4; @l4=();
  for (@l3) {
    push(@l4,split(/(\![\~=]?)|(=[=\~])|([%])/));}
  @l4=split(' ',join(' ',@l4));
  while (@l4) {
    if ($l4[0] eq '{') { local(@l3,$i); shift(@l4);
      if (pop(@l4) ne '}') {
	print "Expression syntax error in $C2_cnt:\n\t$t\n";}
      else { push(@res,'$status=system(',&MS_term("@l4",1),')/256');}
      @l4=();}
    elsif ($l4[0] eq '!') { shift(@l4); push(@res,'!');} # !
    elsif ($l4[0] eq '~') { shift(@l4); push(@res,'~');} # ~
    elsif ($l4[0]=~/^\-([a-z])$/) { shift(@l4);	# -l
      push(@res,"&ft('$1',",&MS_term(shift(@l4),1),')');}
    else {
      if (($l1=shift(@l4)) eq '&&' || $l1 eq '||') { push(@res,$l1);}
      else {
        if ($l1=~/^\@\@(e\d\d\d\d)$/) {		# first term
	  @l5=('(',&M_exp(&G_obtain($1)),')');}
        else { @l5=(&MS_term($l1,1));}
        unless (@l4) { push(@res,@l5);}
        elsif (($l2=shift(@l4)) eq '&&' || $l2 eq '||') {
	  push(@res,@l5); unshift(@l4,$l2);}
	else {
          if (!@l4) {
            print "Expression syntax error in $C2_cnt:\n\t$t\n";}
	  else {
            if (($l1=shift(@l4))=~/^\@\@(e\d\d\d\d)$/) {
	      @l3=('(',&M_exp(&G_obtain($1)),')');}
            else { @l3=(&MS_term($l1,1));}
            if ($l2 eq '==' && !$dval) {
	      push(@res,'&eq(',@l5,',',@l3,')');}
            elsif ($l2 eq '!=' && !$dval) {
	      push(@res,'!&eq(',@l5,',',@l3,')');}
            elsif ($l2 eq '=~') {push(@res,'&peq(',@l5,',',@l3,')');}
            elsif ($l2 eq '!~') {push(@res,'!&peq(',@l5,',',@l3,')');}
            else { push(@res,@l5,$l2,@l3);}
	  }
        }
      }
    }
  }
  @res;
}
1;					# require correct code
