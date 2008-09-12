/*+
     
  genaid.c - general aid for program maintenance

  Revision:
	WNB 931115	Make useful for VAX (include changes)
	WNB 931115	Changed code=-1 anachroism
			VAX: CC/debug/list/opt/def="wn_vx__/name=as_is"
        CMV 940214      Filter unwanted *.x?? and *.a?? for compare
	CMV 940216	If local more recent than remote: no retrieve
	HjV 940516	Small changes for Convex use
	CMV 940516	No negative checksums, please
        CMV 940617	Moved extract and mosaic option from docaid to genaid
	CMV 940617	Add option size to display just the size of a file
 	WNB 940620	Change case of logicals for VAX
 	WNB 940621	Make sure fstat sees n_src for VAX; count bytes
	CMV 940719	Add KEYS, SIZE and HYPER commands
	CMV 940721	Correct \ref to keywords
	CMV 940812	Double underscores in names of keyword help
	CMV 941102	Single underscores again, allow -i option in import command
	CMV 941103	Add options to read volume label and init tape
	CMV 941111	Change text for keys option (was confusing in update)
	CMV 000929      Solved millenium bug

  Syntax:
  
    $n_exe/genaid.exe expand [-t:types] {name of groupfile}
    
        writes switches and full pathnames to the standard output;
        if the name starts with +, $n_src will be prefixed, otherwise 
        the path to the groupfile is taken (may be empty if working 
        in the current directory).
        
    $n_exe/genaid.exe files [-t:types] {name of groupfile}
    
        writes just the filenames with respect to $n_src, or with 
        respect to the groupfile directory if not rooted in $n_src.

    $n_exe/genaid.exe select file1 file2
    
        as files, but also writes the date

    $n_exe/genaid.exe fstat [-c] [-t:types] [+{prefix}] 
                            {filename}|@{name of groupfile} ...   or
                            ... @ {name of groupfile} ...    
                            
        calculates checksum, get size and date, write groupfile
        entry for the file; if file roots in $n_src, $n_src is
        replaced by a plus sign.
        files prefixed by @ (no space in between) will be expanded
        as groupfiles, if the @ has a space behind it, all remaining
        files will be treated as groupfiles.

    $n_exe/genaid.exe mstat [-c] [-t:types] filename

        stdin will be copied to stdout and any lines starting with
        the name of the file will be replaced by a like as produced
        by fstat; only useful for master database updates.

    $n_exe/genaid.exe check [-c] [-t:types] {name of groupfile} ...
    
        expands the groupfile(s), calculates checksums etc for 
        resulting files, compare with the values specified in the
        groupfile and write a groupfile entry if they differ.

    $n_exe/genaid.exe import [-c|i] [-t:types] {name of groupfile} ...

        idem, write get commands for ftp to stdout, report files
        that are already correct to stderr.

        with the -i, no checks are made and no subdirectory appears in 
        the ftp get command.

        NB: both check and import ignore the file sys/database.idx

    $n_exe/genaid.exe compare file1 file2
    
        compare two groupfiles (typically master indices) and copy
        the lines for files that appear in both with different 
        size/checksum/date or appear in file1 only.

    $n_exe/genaid.exe group file...
    
        read an old-style groupfile and output new style data
      
    $n_exe/genaid.exe split file
    
        split contents of a new-style groupfile over one or more old stylers

    $n_exe/genaid.exe psc psc_file >pin_file
   
        expand includes in psc files, write pin file to stdout 
   
    $n_exe/genaid.exe extract {name of file}...

        Extract documentation from one or more files (presumably source
        code etc.) into text-files. Such text-files can be converted to 
        html files with the html option.

    $n_exe/genaid.exe hyper {name of homepage}

        Start xmosaic such that DWARF can communicate with it

    $n_exe/genaid.exe size {name of file}

        Show the size of the file
       
    $n_exe/genaid.exe keys {name of psc/pin/pef file} ...
      
        Convert pin file to html version

    $n_exe/genaid.exe label Unit                 (not on VAXes)
    
        Return volume label of Unit, if any
    
    $n_exe/genaid.exe init  Unit Label           (not on VAXes)
    
        Put volume label on Unit
       
    Original program created early 1993 for SCASIS reduction programs,
    including parts from addwhat.c what.c and fstat.c (1991).
    
    This version created for Newstar maintenance (groupfiles, no SCCS-like
    version strings, checksums etc.) in June 1993.
    
    Marco de Vos, NFRA Dwingeloo.
        
*-*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#ifdef wn_vx__

void *malloc();
void free();
#include <types.h>
#include <stat.h>
int vfork();
void delete();
#define fork vfork
#define unlink delete

#else

#include <fcntl.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#endif

/* List of valid options and their indices               */

#define _EXPAND   1
#define _FILES    2
#define _SELECT   3
#define _FSTAT    4
#define _CHECK    5
#define _IMPORT   6
#define _COMPARE  7
#define _MSTAT    8
#define _GROUP    9
#define _PSC     10
#define _SPLIT   11
#define _EXTRACT 12
#define _HYPER   13
#define _SIZE    14
#define _KEYS    15
#define _LABEL   16
#define _INIT    17

char *options[]={"expand","files","select","fstat","check","import",
                 "compare","mstat","group","psc","split",
                 "extract","hyper","size","keys","label","init",NULL};

/* Define environment names and internal things          */
/* Note: the following should also be lc for VAX to bypass vax_style */
#define ROOT       "n_src"  /* Root of directory tree    */
#define ROOT_UC    "N_SRC"

#define MAX_STRING  512     /* Maximum length of strings */
#define MAX_BUF    5000     /* Length of help buffer      */

#ifdef wn_vx__
#define DOCDIR     "N_HLP"  /* Directory for hypertext   */
#else
#define DOCDIR     "n_hlp"  /* Directory for hypertext   */
#endif

/* Root directory of source tree, read from environment  */

char x_root[MAX_STRING];    /* Root of directory tree    */
int  x_root_len;            /* strlen(x_root)            */
int  vax_style;             /* True if VMS directories   */
char d_root[MAX_STRING];    /* Directory for hypertext   */
int  d_root_len;            /* strlen(d_root)            */

#ifdef wn_vx__
char vms_root[MAX_STRING];  /* Root of directory tree    */
int  vms_root_len;          /* strlen(vms_root)          */
#endif

/* General string variables               */

char line[MAX_STRING];       /* Character buffer          */
char buf[MAX_STRING];        /* Another one               */
unsigned char bbuf[MAX_BUF];     /* Buffer for reading files  */


/* Directory specification of groupfile   */

char grpdir[MAX_STRING];


/* Filepointer for groupfile              */

FILE *grp=NULL;              /* Initialised: none         */

/* Values from groupfile entry            */

char orgname[MAX_STRING];    /* Original groupfile entry  */
char fullname[MAX_STRING];   /* Full name of the file     */
char filename[MAX_STRING];   /* Name w.r.t. x_root        */
char switches[MAX_STRING];   /* Contents of switches      */
long grpdate,grpsize,grpchsum; /* Date, size and checksum */


/* String with types to be selected       */

char types[MAX_STRING]=".";  /* Initialised: select all   */

/* Prefix for groupfile output            */
 
char prefix[MAX_STRING]="";  /* Initialised: none         */

/* Flag for testing files in current dir  */

int  test_cwd=0;             /* Initialised: no           */
int  test_include=0;         /* Initialised: no           */

/* Values calculated from actual file     */

long date,size,chsum;        /* Date, size and checksum   */


/* File types within Newstar              */

static char *ftyp[]={"SCN", "WMP","MDL",  "NGF",   "FLF", NULL};
static char *fnam[]={"Scan","Map","Model","NGCALC","Flag",NULL};


/* Function declarations                  */

int   check_command();
int   get_root();
char *swap_path();
char *my_lower();
char *my_upper();
char *str_date();

int  open_group();
int  next_group();
int  get_types();
int  check_type();
int  stat_out();
int  check_out();
int  import_out();
int  do_compare();
int  doc_extract();
int  start_mosaic();
int  show_size();

int  convert_pin();
int  flush_latex();
int  flush_html();
int  flush_include();
char *decode_data_type();
char *to_latex();
char *to_html();
char *to_anchor();

void get_label();
void put_label();


main(argc,argv)

int  argc;
char **argv;

{
    int  code,iarg,is_group;
    char *file;

    get_root();

    if (argc<2) {
       fprintf(stderr,"Syntax: %s command ...\n",argv[0]);
       code = (-1);
    } else {
        code=check_command(argv[1],options);
    }

    for (iarg=2; argc>iarg && *argv[iarg]=='-'; iarg++) {
       if      (*(argv[iarg]+1)=='t' || *(argv[iarg]+1)=='T') 
            get_types(argv[iarg]);
       else if (*(argv[iarg]+1)=='c' || *(argv[iarg]+1)=='C')
            test_cwd=1;
       else if (*(argv[iarg]+1)=='i' || *(argv[iarg]+1)=='I') 
            test_include=1; 
       else fprintf(stderr,"Invalid switch %s, ignored\n",argv[iarg]);
    }
    
    if (code==_FSTAT) { 

       if (argc>iarg && *argv[iarg]=='+') strcpy(prefix,argv[iarg++]);
       if (iarg<argc) {           
          is_group=0;
          for (; iarg<argc; iarg++) {
            if (*argv[iarg]=='@') {
               if (*(argv[iarg]+1)=='\0') {
                  is_group=1;     /* @ files ... -> rest all groupfiles */
               } else if (open_group(argv[iarg]+1)) {
                  while (group_next()) 
                     if (check_type(fullname)) stat_out(fullname);
               }
            } else if (is_group) {
               if (open_group(argv[iarg])) {
                  while (group_next()) 
                     if (check_type(fullname)) stat_out(fullname);
               }
            } else {
               if (check_type(argv[iarg])) stat_out(argv[iarg]);
            }
          }
       } else {
          while (scanf(" %s",line)==1) 
             if (check_type(line)) stat_out(line);
       }


    } else if (code==_MSTAT) {
       mstat_out(argv[iarg]);

    } else if (code==_CHECK) {
       while (iarg<argc) {
       	  if (open_group(argv[iarg++])) {
             while (group_next()) 
                if (check_type(fullname)) check_out();
          }
       }

    } else if (code==_IMPORT) {
       while (iarg<argc) {
       	  if (open_group(argv[iarg++])) {
             while (group_next()) 
                if (check_type(fullname)) import_out();
          }
       }

    } else if (code==_EXPAND) {
       while (iarg<argc) {
       	  if (open_group(argv[iarg++])) {
             while (group_next()) 
                if (check_type(fullname)) printf("%s %s \n",switches,fullname);
          }
       }

    } else if (code==_FILES) {
       while (iarg<argc) {
       	  if (open_group(argv[iarg++])) {
             while (group_next()) 
                if (check_type(fullname)) printf("%s\n",filename);
          }
       }

    } else if (code==_SELECT) {
       while (iarg<argc) {
       	  if (open_group(argv[iarg++])) {
             while (group_next()) 
                if (check_type(fullname)) printf("%s %ld\n",filename,grpdate);
          }
       }

    } else if (code==_COMPARE) {
       do_compare(argv[iarg],argv[iarg+1]);

    } else if (code==_GROUP) {
       while (iarg<argc) old_to_new(argv[iarg++]);

    } else if (code==_PSC) {
       if (iarg<argc) expand_psc(argv[iarg]);
       else  fprintf(stderr,"Should give input psc-file\n");

    } else if (code==_SPLIT) {
       if (iarg+1<argc) new_to_old(argv[iarg],argv[iarg+1]);
       else  fprintf(stderr,"Should give input file and postfix\n");

    } else if (code==_EXTRACT) {
       while (iarg<argc) doc_extract(argv[iarg++]);

    } else if (code==_HYPER) {
       if (iarg<argc) start_xmosaic(argv[iarg++]);
       else           start_xmosaic("");

    } else if (code==_SIZE) {
       if (iarg<argc) show_size(argv[iarg]);
       else  fprintf(stderr,"Should give input filename\n");

    } else if (code==_KEYS) {
       while (iarg<argc) convert_pin(argv[iarg++]);

    } else if (code==_LABEL) {
#ifdef wn_vx__
       fprintf(stderr,"Use MOUNT/FOR instead\n");
#else
       if (iarg<argc) get_label(argv[iarg]);
       else fprintf(stderr,"Should give name of tapedevice\n");
#endif

    } else if (code==_INIT) {
#ifdef wn_vx__
       fprintf(stderr,"Use INITIALISE instead\n");
#else
       if (iarg+1<argc) put_label(argv[iarg],argv[iarg+1]);
       else fprintf(stderr,"Should give name of tapedevice and label\n");
#endif

    } else {
    	
        fprintf(stderr,"Invalid option; valid options are: \n");
        for (iarg=0; options[iarg]!=NULL; iarg++)
            fprintf(stderr,"%s ",options[iarg]);
        fprintf(stderr,"\n");
        exit(-1);

    }

    exit(0);
}


check_command(option,options)

char *option,**options;

{
    int ii;
    
    my_lower(option);
    for (ii=0; options[ii]!=NULL && strcmp(option,options[ii]); ii++);
    if (options[ii]==NULL) return(0); else return(ii+1);
}



/****** Directory stuff ******************************/

get_root()

{
   char *p;

   p=getenv(ROOT);
   if (p==NULL) {
      fprintf(stderr,"Getting upset: environment not setup...\n");
      exit(-1);
   }
   
   x_root_len=strlen(p);
   if (x_root_len>=MAX_STRING) {
      fprintf(stderr,"%s translates to %s.\n",ROOT,p);
      fprintf(stderr,"Too many characters in root directory...\n");
      exit(-1);
   }
   strcpy(x_root,p); 

#ifdef wn_vx__

   p=getenv(ROOT_UC);
   if (p==NULL) {
      fprintf(stderr,"Getting upset: environment not setup...\n");
      exit(-1);
   }
   
   vms_root_len=strlen(p) - 1;
   if (vms_root_len>=MAX_STRING) {
      fprintf(stderr,"%s translates to %s.\n","N_SRC",p);
      fprintf(stderr,"Too many characters in VMS root directory...\n");
      exit(-1);
   }
   strcpy(vms_root,p); my_lower(vms_root); 
#endif      

   vax_style=(x_root[x_root_len-1]==']' || x_root[x_root_len-1]==':');

   if (!vax_style && x_root[x_root_len-1]!='/') {
        x_root[x_root_len++]='/'; x_root[x_root_len]='\0';
   }

   p=getenv(DOCDIR);
   if (p==NULL) {
      fprintf(stderr,"Getting upset: no hypertext directory...\n");
      exit(-1);
   }
   
   d_root_len=strlen(p);
   if (d_root_len>=MAX_STRING) {
      fprintf(stderr,"%s translates to %s.\n",DOCDIR,p);
      fprintf(stderr,"Too many characters in doc. directory...\n");
      exit(-1);
   }
      
   strcpy(d_root,p); 

   if (!vax_style && d_root[d_root_len-1]!='/') {
        d_root[d_root_len++]='/'; d_root[d_root_len]='\0';
   }
         
}

/* 
   Translates pathspecifications from groupfiles to VMS style paths.
   Prefixes should end with ] or :, if any directory stuff follows
   the ] it is replaced by a ., any slashes are replaced by . :
*/

char *swap_path(string)

char *string;

{
   int dirspec=0,ii;

   if (vax_style) {    /* Make VAX-style path */
      if (*string=='/') *string='[';
      for (ii=strlen(string); ii>=0; ii--) {
        if (string[ii]=='/') {
           if (dirspec) { string[ii]='.'; } 
           else         { string[ii]=']'; dirspec=1; }
        } else if (string[ii]==']' && dirspec) {
           string[ii]='.';
        } else if (string[ii]==':' && dirspec) {  /* Need opening [ */
           strcpy(line,string+ii+1);              /* So make space  */
           strcpy(string+ii+2,line);
           string[ii+1]=='[';                     /* and fill in    */
        }
      }
   }
}


char *my_lower(string)

char *string;

{
  char *p;
  for (p=string; *p!='\0'; p++) if (*p>='A' && *p<='Z') *p=(*p)-'A'+'a';
  return(string);
}


char *my_upper(string)

char *string;

{
  char *p;
  for (p=string; *p!='\0'; p++) if (*p>='a' && *p<='z') *p=(*p)-'a'+'A';
  return(string);
}


char *str_date(sec_in)

long sec_in;

{
    static char date_str[10]="";

    long   secnds;
    struct tm *tmb;
    
    if (sec_in==0L) time(&secnds); else secnds=sec_in;
    tmb=localtime(&secnds);
    sprintf(date_str,"%02d/%02d/%02d",
            tmb->tm_mday,tmb->tm_mon+1,tmb->tm_year);
    return(date_str);
}


/*+*

   Expand groupfiles
   
*-*/

open_group(grpfile)

char *grpfile;

{
   int ii;
   if (grp!=NULL) fclose(grp);

   grp=fopen(grpfile,"r");
   if (grp==NULL) {
      fprintf(stderr,"Error: cannot open groupfile \"%s\".\n",grpfile);
      return(0);
   }

   strcpy(grpdir,grpfile);
   for (ii=strlen(grpdir); 
        ii>=0 && grpdir[ii]!='/' && grpdir[ii]!=']' && grpdir[ii]!=':';
        ii--);
   grpdir[ii+1]='\0';

   return(1);
}


group_next()

{
   int found=0,field;
   char *p,*q;
  
   if (grp!=NULL) {
      while (!found && fgets(line,MAX_STRING-1,grp)!=NULL) {
        for (p=line; *p==' ' || *p=='\t'; p++);
        for (q=p;    *q!='!' && *q!='\n' && *q!='\0'; q++);
        if (p!=q) found=1;  /* We've got an entry   */
      }
    }

    if (!found) {
       fclose(grp); grp=NULL;  /* Must be end of file */

    } else {
       if (*q=='\0') *(q+1)='\0';   /* Already at end of line */
       *q='\0';                     /* Strip comment and \n   */
       my_lower(p);                 /* Lower case             */
       
       for (q=p; *q!=' ' && *q!='\t' && *q!='\0'; q++); 
       if (*q=='\0') q=NULL; else *q='\0';  /* Mark end of filename */

       strcpy(orgname,p);
       if (*p=='+') {
          strcpy(fullname,x_root); strcat(fullname,p+1);
          strcpy(filename,p+1);
       } else {
          strcpy(fullname,grpdir); strcat(fullname,p);
          if ( strncmp(fullname,x_root,x_root_len) ) {
             strcpy(filename,fullname);
          } else {
             strcpy(filename,fullname+x_root_len);
          }
       }
       swap_path(filename); swap_path(fullname);
       
       switches[0]='\0';
       grpdate=grpsize=grpchsum=0L;    

       /* Scan all remaining fields */

       field=0;

       while (q!=NULL) {
         for (p=q+1; *p==' ' || *p=='\t'; p++);
         for (q=p; *q!=' ' && *q!='\t' && *q!='\0'; q++); 
         if (*q=='\0') q=NULL; else *q='\0';
         if (q!=p) {
            if (*p=='-' || *p=='+')   strcat(switches,p);
            else if (field==0)      { grpsize=atol(p);  field++; }
            else if (field==1)      { grpchsum=atol(p); field++; }
            else if (field==2)      { grpdate=atol(p);  field++; }
         }
       }
    }    
 
    return(found);
}


/*
   Return val if switch not present
   Return 1 if switch present and last occurence as -X
   Return 0 if switch present and last occurence as -NX
*/

check_switch(sw,val)

char sw;
int  val;

{
   char *p,sw2;

   if (sw>='a' && sw<='z') sw2=sw-'a'+'A'; else sw2='\n';

   for (p=switches; *p!='\0'; p++) {
     if (*p=='-' && (*(p+1)==sw || *(p+1)==sw2))        val=1;
     else if (*p=='-' && (*(p+1)=='n' || *(p+1)=='N') 
                      && (*(p+1)==sw  || *(p+1)==sw2))  val=0;
   }
   
   return(val);
}


/*+*

     Get string from switch (-t:....) which contains the types that 
     should be selected. The string has a list of valid types separated 
     by slashes, with wildcard . and optional "end mark" $.
     If an item starts with a ^ it has to be ignored.
     
     Examples:     f        matches .f, .fsc, .for
                   f$ f..   matches .fsc, .for, but not .f
                   ^exe     ignores .exe

*-*/

get_types(argv)

char *argv;

{
    char *p;
    for (p=argv; *p!='\0' && *p!=':'; p++);
    if (*p!=':') strcpy(types,"."); else strcpy(types,my_lower(p+1));
}

check_type(file)

char *file;

{
    int  ii,result,all_negate=1;
    char *p,*q;

    for (ii=strlen(file);               /* Find the extension      */
         ii>0 && file[ii]!='.' && file[ii]!=']' && 
                 file[ii]!='/'; ii--);
    if (file[ii]!='.') return(0);   /* Invalid or no extension */
       
    for (p=types; *p!='\0'; p++) {
       if (*p=='^') { result=0; p++; } else { result=1; all_negate=0; }
       if (*p!='/' && *p!='\0') {   /* Catch /^/ error */
          for (q=file+ii+1; 
               *p!='/' && *p!='\0' && *q!='\0' && ( (*p)==(*q) || *p=='.');
               q++,p++);
          if (*p=='\0' || *p=='/' || (*q=='\0' && *p=='$') ) return(result);
          while (*p!='\0' && *p!='/') p++;   /* No match, get to next type */
       }
    }

    return(all_negate);
}

/*+

    Utility program for Newstar: returns filename, checksum, size and date
    for files given on the commandline or read from standard input.

    If the first argument starts with a dash, it is used as a prefix
    for filenames. 

    Output format (size in kbyte, checksum simple bytesum with overflow):

filename____________  size____ chsum___ yymmdd


*-*/

static char *includes[128]={NULL};
static int  nname=0;


stat_out(file)

char *file;

{
   int  ii,jj,ext;
   FILE *fp;
   char *p,*q;


   get_stat(file);
   
   /* If the file roots in $n_src, replace n_src by a +      */

#ifdef wn_vx__
   if ( !strncmp(file,vms_root,vms_root_len) ) {
     for (ii=strlen(file);               /* Find the ]      */
         ii>0 && file[ii]!=']'; ii--);
     if (file[ii]==']') file[ii]='/';    /* replace ] with / */
     printf("+%-20s %s",file+vms_root_len,switches); }
#else
   if ( !strncmp(file,x_root,x_root_len) ) 
         printf("+%-20s %s",file+x_root_len,switches); 
#endif
   else  printf("%s%-20s %s",prefix,file,switches);
    
   printf("  %8ld %8ld %6.6ld",size,chsum,date);


   if (test_include) {

      nname=0;
      
      ii=0;
      if (test_cwd) {
         for (ii=strlen(file); 
              ii>=0 && file[ii]!='/' && file[ii]!=']' && file[ii]!=':'; ii--);
         ii++;
      }

      for (jj=strlen(file); jj>=0 && file[jj]!='.'; jj--);
      jj++;

      if      (file[jj]=='f')               ext=1;      /* Fortran source */
      else if (file[jj]=='c')               ext=2;      /* C source       */
      else if (file[jj]=='p')               ext=3;      /* pin/pef/psc    */
      else if (!strncmp(file+jj,"dsf",3) ||
               !strncmp(file+jj,"dsc",3))   ext=4;      /* WNB includes   */
      else                                  ext=0;

      if (ext) {
         
         printf(" ! ");
         fp=fopen(file+ii,"r");
         if (fp!=NULL) {
            while (fgets(line,MAX_STRING-1,fp)!=NULL) {

              if (ext==1) {
                 for (p=line; *p==' ' || *p=='\t'; p++);
                 if ( (*p=='i' || *p=='I') && 
                      (*(p+3)=='L' || *(p+3)=='l') ) {
                    my_lower(line);
                    if (!strncmp(p,"include",7)) {
                       for (p+=7; *p!='\'' && *p!='\0'; p++);
                       if (*p=='\'') {
                       	  for (q=p+1; *q!='\'' && *q!='\0'; q++);
                       	  if (*q=='\'') {
                       	     *q='\0';
                             while (q!=p && *q!='_') q--;
                             if (*q=='_') *q='.';
                             add_include(p+1);
                       	  } else {
                            fprintf(stderr,"Error: %s",line);
                          }
                       } else {
                       	  fprintf(stderr,"Error: %s",line);
                       }
                    }
                 }

              } else if (ext==2) {
                 for (p=line; *p==' ' || *p=='\t'; p++);
                 if (*p=='#') {
                    for (p++; *p==' ' || *p=='\t'; p++);
                    if (!strncmp(p,"include",7)) {
                       for (p+=7; *p!='\"' && *p!='<' && *p!='\0'; p++);
                       if (*p=='\"') {
                       	  for (q=p+1; *q!='\"' && *q!='\0'; q++);
                       	  if (*q=='\"') {
                       	     *q='\0';
                             while (q!=p && *q!='_') q--;
                             if (*q=='_') *q='.';
                             add_include(p+1);
                       	  } else {
                            fprintf(stderr,"Error: %s",line);
                          }
                       } else if (*p!='<') {
                       	  fprintf(stderr,"Error: %s",line);
                       }
                    }
                 }

              } else if (ext==3) {
                 for (p=line; *p==' ' || *p=='\t'; p++);
                 if ( (*p=='i' || *p=='I') && 
                      (*(p+3)=='L' || *(p+3)=='l') ) {
                    my_lower(line);
                    if (!strncmp(p,"include=",8)) {
                       for (p+=8; *p==' ' || *p=='\t'; p++);
                       if (*p!='\0') {
                       	  for (q=p; *q!='\n' && *q!='\0'; q++);
                          *q='\0';
                          while (q!=p && *q!='_') q--;
                          if (*q=='_') *q='.';
                          add_include(p);
                       } else {
                       	  fprintf(stderr,"Error: %s",line);
                       }
                    }
                 }
              } else if (ext==4) {
                 for (p=line; *p==' ' || *p=='\t'; p++);
                 if (*p=='%') {
                    my_lower(line);
                    if (!strncmp(p,"%include=",9)) {
                       for (p+=9; *p==' ' || *p=='\t'; p++);
                       if (*p!='\0') {
                       	  for (q=p; *q!='\n' && *q!='\0'; q++);
                          *q='\0';
                          while (q!=p && *q!='_') q--;
                          if (*q=='_') *q='.';
                          add_include(p);
                       } else {
                       	  fprintf(stderr,"Error: %s",line);
                       }
                    }
                 }
              }
           }
           fclose(fp);
           
           for (ii=0; ii<nname; ii++) {
             printf("@%s ",includes[ii]);
             free(includes[ii]);
           }
         }
      }
   }

   printf("\n");
}


add_include(name)

char *name;

{
   int ii;
   for (ii=0; ii<nname && strcmp(includes[ii],name); ii++);
   if (ii==nname) {
      if (nname==128) {
      	 fprintf(stderr,"Too many includes...\n"); exit(-1);
      }
      includes[nname]=(char *)malloc(strlen(name)+1);
      strcpy(includes[nname],name);
      nname++;
   }
}


mstat_out(file)

char *file;

{
   int ll;
   
   get_stat(file);

   /* If the file roots in $n_src, replace n_src by a +      */

   if ( !strncmp(file,x_root,x_root_len) ) {
      file[0]='+'; strcpy(file+1,file+x_root_len);
   }

   /* copy stdin to stdout and replace matching lines        */

   ll=strlen(file);
   
   while (fgets(line,MAX_STRING,stdin)!=NULL) {
     if (strncmp(line,file,ll)) printf("%s",line);
     else printf("%-20s  %8ld %8ld %6.6ld %s",file,size,chsum,date,switches);
   }
}


check_out()

{
  long adate,agrpdate;
                         
  if (strcmp(filename,"sys/database.idx") && 
      strcmp(filename,"sys/lock.idx") ) {

    get_stat(fullname);
    adate=date;       if (date<800000)    adate=1000000+date;
    agrpdate=grpdate; if (grpdate<800000) agrpdate=1000000+grpdate; 

    if ( (grpsize >0L  && size  != grpsize)  ||
         (grpchsum>0L  && chsum != grpchsum) ||
         (grpdate >0L  && adate <  agrpdate) ||
         (size == 0L && chsum == 0L && date == 0L) ) {

    /* If the file roots in $n_src, replace n_src by a +      */
       if ( !strncmp(fullname,x_root,x_root_len) ) 
             printf("+%-20s",fullname+x_root_len); 
       else  printf("%-20s",fullname);
    
       printf("  %8ld %8ld %6.6ld %s ! %8ld %8ld %6.6ld\n",
                 grpsize,grpchsum,grpdate,switches,size,chsum,date);
    }
  }
}


import_out()

{
   int  ii;

   if (strcmp(filename,"sys/database.idx")) {

     for (ii=strlen(filename);      /* Strip filename */
          ii>=0 && filename[ii]!='/' && filename[ii]!=']' && filename[ii]!=':';
          ii--);
     ii++;

     if (!check_switch('r',1)) {
        fprintf(stderr,"File %s needs not be retrieved.\n",filename+ii);
     } else {
        get_stat(fullname);

        if (test_include) {
           if (check_switch('b',0)) printf("binary\n"); else printf("ascii\n");
           printf("get %s\n",filename+ii);
           
        } else if ( (grpsize >0L  && size  != grpsize)  ||
                    (grpchsum>0L  && chsum != grpchsum) ||
                    (grpdate >0L  && date  <  grpdate)  || size==0L) {

           if (check_switch('b',0)) printf("binary\n"); else printf("ascii\n");
           printf("get %s %s\n",filename,filename+ii);

        } else {
           fprintf(stderr,"File %s seems to be correct already.\n",filename+ii);
        }
     }
   }
}



get_stat(file)

char *file;

{
    int  ii,rr;
    long ltime,vmscnt;
    FILE *fp;

    struct stat fst;
    struct tm *tmb;
       
    chsum=size=date=vmscnt=0L; 

    /* If file should be tested in cwd, strip the filename   */

    ii=0;
    if (test_cwd) {
       for (ii=strlen(file); 
            ii>=0 && file[ii]!='/' && file[ii]!=']' && file[ii]!=':'; ii--);
       ii++;
    }
     
    /* Get checksum from byte count                          */

    fp=fopen(file+ii,"r");
    if (fp!=NULL) {
       while ( (rr=fread(bbuf,sizeof(char),512,fp)) > 0 ) {
	 vmscnt+=rr;
       	 while (--rr>=0) chsum+=(long)bbuf[rr];
       }
       if (chsum<0L) chsum= -1*chsum;   /* In case of overflow */
       fclose(fp);
    }    

    /* Stat the file and get size and revision date           */
    
    if (!stat(file+ii,&fst)) { 
       size=fst.st_size; ltime=fst.st_mtime; 
       tmb=localtime(&ltime); 
       date=(long)(tmb->tm_mday+100*(tmb->tm_mon+1))+
            10000L*(long)(tmb->tm_year%100);
#ifdef wn_vx__
       size=vmscnt;
#endif
    }   
}

/*
   Return 0 if executable or library for architecture not in wanted
*/

static int requested(line,wanted)

char *line,*wanted;

{
   char *p,*q;

   if (wanted==NULL) return(1);
   
   for (p=line; *p!='.' && *p!='!' && *p!='\0'; p++);
   if (*p!='.' || (*(p+1)!='x' && *(p+1)!='a') ) {
      return(1);                                   /* Not exe, not lib */
   } else {
      q=wanted; 
      p+=2;
      while (*q!='\0') {
      	if (*q == *p && *(q+1) == *(p+1)) return(1);     /* We want it  */
      	while (*q!='\0' && *q!=' ' && *q!=',' && *q!='/') q++;
      	while (            *q==' ' || *q==',' || *q=='/') q++;
      }
   }          

   return(0);   /* We do not want it, or we would have returned earlier */
}



/*+*

     Compare two groupfiles (typically master indices)

*-*/

do_compare(file1,file2)

char *file1,*file2;

{
   FILE *fp;
   int found=0,field;
   char *p,*q,*wanted;
   
   if (!open_group(file1)) exit(-1);

#ifdef wn_vx__
   wanted=getenv("N_INSTALL");
#else
   wanted=getenv("n_install");
#endif
   fp=fopen(file2,"r");
   if (fp==NULL) {
      fprintf(stderr,"Cannot open second input groupfile (%s)...\n",file2);
      fprintf(stderr,"First input file copied to stdout.\n");
      printf("! Blind copy of %s\n",file1);
      while (fgets(line,MAX_STRING-1,grp)!=NULL)
      	 if (requested(line,wanted)) printf("%s",line);
      fclose(grp); grp=NULL;

   } else {
      printf("! Groupfile for update based on database comparison.\n");
      printf("! Local:  %s\n! Remote: %s\n! \n",file2,file1);
      while (group_next()) {

        if (check_type(orgname) && requested(orgname,wanted)) {
           rewind(fp); found=0; date=size=chsum=0L;    

           while (!found && fgets(line,MAX_STRING-1,fp)!=NULL) {
              for (p=line; *p==' ' || *p=='\t'; p++);
              for (q=p;    *q!='!' && *q!='\n' && *q!='\0' &&
                           *q!=' ' && *q!='\t'; q++);
              if (p!=q) {
                 *q='\0'; my_lower(p);          /* Mark end of filename       */
                 found=(!strcmp(p,orgname));    /* We've got found the entry  */
              }
           }

           if (found) {
             field=0;                         /* Scan all remaining fields */
             while (q!=NULL) {
               for (p=q+1; *p==' ' || *p=='\t'; p++);
               for (q=p; *q!=' ' && *q!='\t' && *q!='\0'; q++); 
               if (*q=='\0') q=NULL; else *q='\0';
               if (q!=p && *p!='-' && *p!='+') {
                  if      (field==0)      { size=atol(p);  field++; }
                  else if (field==1)      { chsum=atol(p); field++; }
                  else if (field==2)      { date=atol(p);  field++; }
               }
             }
           }

/*
    Select if  - not found (and existing on remote node: grpsize!=0)
               - older than file at remote node (or no date defined) and
                   uneqal size or unequal checksum
*/

           if ( (!found  && grpsize!=0L) ||
                ( (date<=0L || grpdate<=0L || date<=grpdate) &&
                  ( (grpsize  != 0L  && size  != grpsize) || 
                    (grpchsum != 0L  && chsum !=grpchsum) ) ) ) {
               printf("%-20s  %8ld %8ld %6.6ld %s ! %8ld %8ld %6.6ld\n",
                   orgname,grpsize,grpchsum,grpdate,switches,size,chsum,date);
           }
        }
      }
   }
}



/*
    Expand psc file
*/

expand_psc(file)

char *file;

{
   FILE *fp,*fp2;
   char *p,*q,list[MAX_STRING];
   int  ii,do_key;

   fp=fopen(file,"r");
   if (fp==NULL) {
      fprintf(stderr,"Cannot open input %s\n",file); return(0);
   }

/*
   Expect INCLUDE= and KEYWORD= lines at start of line
*/
   while (fgets(line,MAX_STRING,fp)!=NULL) {
      for (p=line; *p==' ' || *p=='\t'; p++);
      strncpy(buf,p,8); buf[8]='\0'; my_lower(buf);
/*
   Normal line, just copy
*/
      if (strncmp(buf,"include=",8)) {
      	 printf("%s",line);
/*
   Include keyword found, open included file and copy relevant keywords
*/
      } else {

         printf("!>>>>> %s",line);
/*
   Split off comments
*/
      	 for (p+=8; *p==' ' || *p=='\t'; p++);
      	 for (q=p; *q!=' ' && *q!='\t' && *q!='!' && 
      	           *q!='\n' && *q!='\0'; q++);
      	 *q='\0';
      	 my_lower(p);
/*
   Separate filename and keyword list (INCLUDE=xxx_PEF:Key,Key)
*/
         for (ii=strlen(p); ii>0 && p[ii]!=':'; ii--); 
         if (ii!=0) {
            p[ii]='\0';
            strcpy(list,p+ii+1);
         }

         if (strcmp(p+strlen(p)-4,"_pef")) {
            fprintf(stderr,"Invalid include: %s\n",p);
         } else {
            p[strlen(p)-4]='.';
            strcpy(buf,p); 
#ifdef wn_vx__
            sprintf(buf,"%s%s",getenv("N_UINC"),p);
#else
            sprintf(buf,"%s/%s",getenv("n_uinc"),p);
#endif
            fp2=fopen(buf,"r");
            if (fp2==NULL) {
#ifdef wn_vx__
              sprintf(buf,"%s%s",getenv("N_INC"),p);
#else
              sprintf(buf,"%s/%s",getenv("n_inc"),p);
#endif
              fp2=fopen(buf,"r");
            }
            if (fp2==NULL) {
               fprintf(stderr,"Cannot find include file: %s\n",p);
/*
   Read include file, copy all keywords
*/
            } else if (ii==0) {
                while (fgets(line,MAX_STRING,fp2)!=NULL) printf("%s",line);
                fclose(fp2);
/*
   Read include file, copy keywords in list only
*/
            } else { 
               do_key=0;
      	       while (fgets(line,MAX_STRING,fp2)!=NULL) {
                 for (p=line; *p==' ' || *p=='\t'; p++);
                 strncpy(buf,p,8); buf[8]='\0'; my_lower(buf);
/*
   Found keyword
*/
                 if (!strncmp(buf,"keyword=",8)) {
/*
   Strip comments
*/
                    for (p+=8; *p==' ' || *p=='\t'; p++);
                    strcpy(buf,p);
      	            for (q=buf; *q!=' ' && *q!='\t' && *q!='!' && 
      	                        *q!='\n' && *q!='\0'; q++);
       	            *q='\0';
                    my_lower(buf);
/*
   Check if in list                 
*/
                    q=list; 
                    do_key=0;
                    while (!do_key && *q!='\0') {
                      for (ii=0; q[ii]!='\0' && q[ii]!=','; ii++);
                      do_key=(ii==strlen(buf) && !strncmp(buf,q,ii));
                      q+=ii; if (*q==',') q++;
                    }
                 }
/*
   If keyword in list, copy all lines (until next keyword)
*/
                 if (do_key) printf("%s",line);
               }

               fclose(fp2);
      	    }
      	 }
      }
   }

   fclose(fp); 
}    

/* 
    Two options that will become obsolete soon
*/

old_to_new(file)

char *file;

{
   FILE *fp;
   char *p,*q,*r,*flag;
   int  ii,sp;

   fp=fopen(file,"r");
   if (fp==NULL) {
      fprintf(stderr,"Cannot open input %s\n",file); return(0);
   }

/*
   Split of prefix which determines the directory
*/
   my_lower(file);
   for (ii=strlen(file); ii>0 && file[ii]!='/'; ii--);
   if (file[ii]=='/') ii++;
   for (p=file+ii; *p>='a' && *p<='z'; p++); *p='\0';
   
   while (fgets(line,MAX_STRING-1,fp)!=NULL) {
     for (p=line; *p==' ' || *p=='\t'; p++);
     for (q=p; *q!='$' && *q!='#' && *q!='!' && *q!='\n' && *q!='\0'; q++);
     if (p!=q) {            /* We've got an entry   */
        for (r=p; *r!='.' && r<q; r++);
        if (*r=='.' && (*(r+1)=='a' || *(r+1)=='A' || 
                        *(r+1)=='X' || *(r+1)=='x') ) flag="-B"; else flag="";
        for (sp=0,q=p; *q!='!' && *q!='\n' && *q!='\0'; q++) {
          if (*q=='-')                                sp=1; 
          else if (*q==' ' || *q=='\t' || *q=='!')    sp=0;
          if (sp && *q!='!' && *q!='\n' && *q!='\0')  *q=' ';
        }
        if (*q=='!') {
           *q='\0'; printf("+%s/%s %s !%s\n",file+ii,p,flag,q+1);
        } else {
           *q='\0'; printf("+%s/%s %s\n",file+ii,p,flag);
        }
     } else if (*p=='!') {  /* Comment              */
        printf("%s",p);
     } else {               /* Command              */
        if (!strncmp(p,"$$nc$ ncomp ",12)) {
           while (*q!='\n' && *q!='\0') q++; *q='\0';
           printf("+%s/%s -NR \n",p+12);
        } else {
           printf("!!! %s",p);  
        }
     }
   }

   fclose(fp);
}


/* No advanced things! */

new_to_old(file,post)

char *file,*post;

{
   FILE *fp,*fp2;
   char *p,*q,*r;
   char cmt[MAX_STRING][25],savfile[MAX_STRING];
   int  ncmt=0,icmt;
   
   fp=fopen(file,"r");
   if (fp==NULL) {
      fprintf(stderr,"Cannot open input %s\n",file); return(0);
   }
   fp2=NULL;
   savfile[0]='\0';

   while (fgets(line,MAX_STRING-1,fp)!=NULL) {
     for (p=line; *p==' ' || *p=='\t'; p++);
     for (q=p; *q!='!' && *q!='\n' && *q!='\0'; q++);
     if (p!=q) {            /* We've got an entry   */
        for (r=p; *r!='.' && *r!='\0'; r++); /* Find extension */
        for (q=p; *q!='/' && *q!=' ' && *q!='\0' && *q!='\n'; q++);
        if (*p!='+' && !strncmp(r,".EXE",4)) {
           fprintf(fp2,"%s",p);
        } else if (*p!='+' || *q!='/') {
           fprintf(stderr,"Illegal entry: %s",p);
        } else {
           *q='\0';
           sprintf(filename,"%s%s.grp",p+1,post);
           if (strcmp(filename,savfile)) {  /* New file */
             strcpy(savfile,filename);
             if (fp2!=NULL) fclose(fp2);
             fp2=fopen(filename,"r");
             if (fp2==NULL) {
                fp2=fopen(filename,"w");
                fprintf(fp2,"!%s created from %s\n",filename,file);
                for (icmt=0; icmt<ncmt; icmt++) fprintf(fp2,"%s",cmt[icmt]);
             }
             if (fp2!=NULL) {
                fclose(fp2);
                fp2=fopen(filename,"a");
             }
             if (fp2==NULL) {
                fprintf(stderr,"Cannot open output file %s\n",filename);
                fclose(fp);
                return(0);
             }
           }
           for (p=q+1; *p!='\t' && *p!=' ' && *p!='\n' && *p!='\0'; p++);
           if (*p==' ' || *p=='\t') {
              *p='\0'; fprintf(fp2,"%s ! %s",q+1,p+1);
           } else {
              fprintf(fp2,"%s",q+1); 
           }
        }
     } else if (*q=='!') {
     	if (fp2!=NULL)    fprintf(fp2,"%s",q);
     	else if (ncmt<25) strcpy(cmt[ncmt++],q);
     }
   }

   fclose(fp);
   if (fp2!=NULL) fclose(fp2);

}

/*
   Extract documentation from source files.

Taken from pager.c, without all editing options apart from: 

  {+}                        Switch output on  (if -d on commandline) 
  {-}                        Switch output off (if -d on commandline)

The closing brace is optional in all commands. Commands are case
insensitive, and only the first character after the brace is necessary.
You may include commands in Fortran of C-type comments.
In ordinary text files, enclose a command in braces: {+} {-}
In Fortran source files, start a command with C*, eg C*+, C*-.
Also allowed are: C+, C-
In C source files, include the command between slash-star and star-slash.
The closing slash-star is optional for docaid, but not for the compiler!
In Unix C-shell scripts, start the command with a #* 
In DCL command files, start the command with a !*

If a line starts with minus-star-slash, this is handled as a {-} command.
If a line starts with #- or #+, this is handles as a {+} or {-} command.

Lines are detabbed with a tabstep of 8 spaces.

*/

#define TABSTEP 8

doc_extract(file)

char *file;

{
    FILE *fp;
    int  doc,nskip,iskip,ii;
    char precmd,*p;
    
    fp=fopen(file,"r");
    if (fp==NULL) {
       fprintf(stderr,"\nError: cannot open %s...\n",file);
       return(0);
    }

/*
   Prescan to see if there is any documentation at all
*/
    doc=0;
    while (!doc && fgets(line,130,fp)!=NULL) {

       if ( (line[0]=='{' && line[1]=='+' )                      ||
             ( (line[0]=='C' || line[0]=='c' || line[0]=='/' ||
                line[0]=='#' || line[0]=='!')
                    && line[1]=='*' && line[2]=='+')              ||
             (line[0]=='$' && line[1]=='!' &&
              line[2]=='*' && line[3]=='+')                       ||
             ( (line[0]=='C' || line[0]=='c' || 
                line[0]=='#' || line[0]=='!') && line[1]=='+')    ) doc=1;
    }

/*
   Rewind and do the real work
*/
    if (doc) {

      rewind(fp);

      printf(">>>>> %-50.50s >>>>>\n\n",file);
  
      doc=0; precmd=' '; nskip=999;
    
      while (fgets(line,130,fp)!=NULL) {

         p=NULL; 
         if (line[0]=='{') {
            precmd='{'; p=line+1;
         } else if ( (line[0]=='C' || line[0]=='c' || line[0]=='/' ||
                      line[0]=='#' || line[0]=='!') 
                      && line[1]=='*') {
            precmd=line[0]; p=line+2;
         } else if (line[0]=='$' && line[1]=='!' && line[2]=='*') {
            precmd='!'; p=line+3;
         } else if ((line[0]=='C' || line[0]=='c' || 
                     line[0]=='#' || line[0]=='!') && 
                    (line[1]=='+' || line[1]=='-')) {
            precmd=line[0]; p=line+1;
         } else if (line[0]=='*' && line[1]=='-' && 
                    line[2]=='*' && line[3]=='/') {
            precmd='/'; p=line+1;
         } else if (line[0]=='-' && line[1]=='*' && line[2]=='/') {
            precmd='/'; p=line;
         }
   
         if (p!=NULL && *p=='+') {
            doc=1;
         } else if (p!=NULL && *p=='-') {
            doc=0;
         } else if (doc) {
/*
    Expand all tabs in the line
*/
       			
            strcpy(buf,line);
            for (ii=0,p=buf; *p!='\0'; p++) {
               if (*p == '\t') {
               	  line[ii++]=' ';
               	  while ( (ii%TABSTEP)!=0 ) line[ii++]=' ';
               } else {
                  line[ii++]=(*p);
               }
            }
            line[ii]='\0';
            
/* 
    If we have a block starting with the command prefix, remove that prefix.
    Also remove the Fortran comment statements
*/
            if (line[0]==precmd ||
                ( (line[0]=='C' || line[0]=='c') &&
                  (line[1]==' ' || line[1]=='\t') ) ) {
               line[0]=' ';
               for (iskip=0; 
                    iskip<nskip && (line[iskip]==' ' || line[iskip]=='\t');
                    iskip++);
               if (nskip==999) nskip=iskip;
            } else {
               iskip=0;
            }
            printf("%s",line+iskip);
         }
      }

      printf("\n<<<<< %-50.50s <<<<<\n",file);
    }
    
    fclose(fp);
}


start_xmosaic(home_page)

char *home_page;

{
#ifdef wn_vx__
   printf("Cannot use xmosaic on the VAX\n");
   return(-1);
#else
   static char page[MAX_STRING],*args[]={"xmosaic",page,NULL};

   int  pid=0,ii,start_new,l1,l2;
   FILE *fp;

   strcpy(page,home_page);
   strcpy(buf,getenv("DISPLAY"));

/*
   Construct the name of the xmosaic interface file
*/
   strcpy(line,"/tmp/xm-"); 
   for (ii=0; ii<100 && buf[ii]!='\0'; ii++) 
     if (buf[ii]==':') line[8+ii]='.'; else line[8+ii]=buf[ii];
   line[8+ii]='\0';
   
   pid=fork();
   if (pid==0) {                 /* We are the child process now */
      freopen("/dev/null","w",stderr);
      sprintf(buf,"%s/xmosaic.exe",getenv("n_exe"));
      if (execv(buf,args)==(-1)) { 
         printf("Error starting xmosaic...\n");  
         unlink(line);
         exit(-1); 
      }
   } else {
/*
   Save pid in file for future calls
*/
      fp=fopen(line,"w");
      if (fp!=NULL) {
         fprintf(fp,"%d\n",pid);
         fclose(fp);
      }
   }
   
   return(pid);
#endif
}



show_size(file)

char *file;

{
    struct stat fst;

    /* Stat the file and get size */
    
    if (!stat(file,&fst)) printf("%ld\n",fst.st_size);
    else                  printf("-1\n");
}


/*
    This is the routine to convert a pin-file to something 
    readable and printable.
    
    It used to be in docaid, but I put it here when JPH revised
    the documentations-system. Only hypertext files are being
    written now.
    
*/


static char *valkey[]={"include",
                       "keyword","prompt","options","defaults","help",
                       "data_type","length","units","switches",
                       "nvalues","min_nvalues","max_nvalues",
                       "checks","minimum","maximum",
                       "io","max_nsets","attributes","search",NULL};

static char name[32]="",key[32]="";     /* Name of the keyword */
static char prompt[MAX_STRING+1]="";    /* Prompt string       */
static char deflt[MAX_STRING+1]="";     /* Default values      */
static char units[MAX_STRING+1]="";     /* Units for input     */
static char switcher[MAX_STRING+1]="";  /* Allowed switches    */
static char help[MAX_BUF]="";           /* Help text           */
static int  input_len=0;                /* Length of input     */
static char data_type=' ';              /* Expected input      */
static int  nval,min_nval,max_nval;     /* Number of values    */
static double val_min,val_max;          /* Range               */

static FILE *ppp,*kindex,*html;
static char pppname[MAX_STRING]="";
static char PPPname[MAX_STRING]="";
static int  do_pef=0;


convert_pin(file)

char *file;

{
   int  ii,jj,found;

   char *p,*q;

/*
   Open the input file, get the filename in ppdname and PPDname (uppercase)
*/
   ppp=fopen(file,"r");
   if (ppp==NULL) {
      fprintf(stderr,"Error: cannot open file \"%s\".\n",file);
      return(0);
   }   

   for (ii=strlen(file); ii>=0 && file[ii]!='.'; ii--);
   do_pef=(!strncmp(file+ii,".pef",4));

   for (jj=strlen(file); jj>=0 && file[jj]!='/' && file[jj]!=']' &&
		file[jj]!=':'; jj--);
   if (jj!=0) jj++;
   strncpy(pppname,file+jj,(ii-jj)); pppname[ii-jj]='\0';
   strcpy(PPPname,pppname); my_upper(PPPname);

   fprintf(stderr,"Translating %s (%s) to html document.\n",file,PPPname);

/*
  Open and initialise the html index file
*/
   sprintf(line,"%s%s",d_root,pppname);
   mkdir(line,0777);
   
   if (do_pef) {
      sprintf(line,"%s%s_comm",d_root,pppname);
      mkdir(line,0777);
      sprintf(line,"%s%s_comm/%s_comm.html",d_root,pppname,pppname);
   } else {
      sprintf(line,"%s%s_keys",d_root,pppname);
      mkdir(line,0777);
      sprintf(line,"%s%s_keys/%s_keys.html",d_root,pppname,pppname);
   }
   kindex=fopen(line,"w");
   if (kindex==NULL) {
      fprintf(stderr,"\nError: cannot open output file %s\n",line);
      fclose(ppp); return(-1);
   }

   if (do_pef) {
      fprintf(kindex,
         "<TITLE>Index of general keywords from %s</TITLE>\n",PPPname);
      fprintf(kindex,
         "<H1>Description of general keywords (%s)</H1>\n\n<UL>\n",PPPname);
   } else {
      fprintf(kindex,
         "<TITLE>Index of private keywords for %s </TITLE>\n",PPPname);
      fprintf(kindex,
         "<H1>Description of keywords for program %s</H1>\n\n<UL>\n",PPPname);
   }
   

/*
  Scan the input file to find the next keyword.
  If a KEYWORD= entry is found and we have something in our
  buffers, flush them.
*/  

   found=0;

   while (fgets(line,MAX_STRING,ppp)!=NULL) {
     for (p=line; *p==' ' || *p=='\t'; p++);            /* Start of line */
     for (q=p;   *q!='!' && *q!='\n' && *q!='\0'; q++); /* End of line   */
     if (q!=p) {
        while (*(q-1)==' ' || *(q-1)=='\t') q--; /* Skip trailing spaces */ 
        *q='\0';                                        /* Strip comment */
        for (q=p; *q!='=' && *q!='\0'; q++);            /* Find keyword  */
        if (*q!='=') {
           fprintf(stderr,"Isolated line: %s\n",p);
        } else {                                        /* Match keyword */
           *q='\0'; q++;
           my_lower(p); jj=strlen(p);
           for (ii=0; valkey[ii]!=NULL && strncmp(p,valkey[ii],jj); ii++);
           if (valkey[ii]==NULL) {
              fprintf(stderr,"Invalid keyword: %s\n",p);
           } else {
              ii--;
              if (ii==-1) {                             /* INCLUDE=      */
                 if (found>0) flush_html();             /* Pending output*/
              	 flush_include(q,found);
              	 found=(-1); 
              	
              } else if (ii==0) {                       /* KEYWORD=      */
                 if (found>0) {                         /* Pending output*/
                    flush_html();                    
                    found=0;
                 } else if (found<0) {            /* Space after include */
                    fprintf(kindex,"<P>");
                 }

                 found=1;                               /* Found key     */
                 strcpy(name,q);
                 prompt[0]=help[0]=units[0]=deflt[0]=switcher[0]='\0';
                 data_type='?'; input_len=0;
                 nval=min_nval=max_nval=(-1);
                 val_min=(-12345.00); val_max=(-12345.0); 

              } else if (ii==1) {                        /* PROMPT=       */
                 get_continuation(prompt,q,0);

              } else if (ii==2) {                        /* OPTIONS=      */
                 get_continuation(prompt,q,0);
                 
              } else if (ii==3) {                        /* DEFAULT=      */
                 get_continuation(deflt,q,0);

              } else if (ii==4) {                        /* HELP=         */
                 get_continuation(help,q,1);
                 
              } else if (ii==5) {                        /* DATA_TYPE=    */
                 data_type=(*q);
                 
              } else if (ii==6) {                        /* LENGTH=       */
                 input_len=atoi(q);
                 
              } else if (ii==7) {                        /* UNITS=        */
                 get_continuation(units,q,0);

              } else if (ii==8) {                        /* SWITCHES=     */
                 get_continuation(switcher,q,0);

              } else if (ii==9) {                        /* NVALUES=      */
                 nval=atoi(q);
                 
              } else if (ii==10) {                       /* MIN_NVALUE=   */
                 min_nval=atoi(q);
                 
              } else if (ii==11) {                       /* MAX_NVALUE=   */
                 max_nval=atoi(q);
                 
              } else if (ii==12) {                       /* CHECKS=       */
                 
              } else if (ii==13) {                       /* MINIMUM=      */
                 val_min=atof(q);

              } else if (ii==14) {                       /* MAXIMUM=      */
                 val_max=atof(q);

              }
           }
        }
     } 
   }

   if (found>0) flush_html(kindex);

   fclose(ppp);
 
/*
   Finish and close HTML file
*/
   fprintf(kindex,"</UL>\n\n");
   fprintf(kindex,"\n <H3> More information: </H3> <UL>\n");
   fprintf(kindex,
      "<LI><A HREF=\"../homepage.html\">NEWSTAR Documentation Home page</A>\n");
   fprintf(kindex,
      "<LI><A HREF=\"../hb_contents/hb_contents.html\">The NEWSTAR Cookbook</A>\n");
   if (!do_pef) {
     fprintf(kindex,
      "<LI>Description of <A HREF=\"../%s_descr/%s_descr.html\">program %s</A>\n",
             pppname,pppname,PPPname);
   }

   fprintf(kindex,"</UL>\n");
   fclose(kindex);
   
}

char *decode_data_type(data_type)

char data_type;

{
   static char tmp[2]="?";

   if (data_type>='a' && data_type<='z') data_type+=('A'-'a');

   if      (data_type=='C') return("Character");
   else if (data_type=='R') return("Real number");
   else if (data_type=='D') return("Double precision number");
   else if (data_type=='J') return("Integer number");
   else if (data_type=='L') return("Logical");

   tmp[0]=data_type;
   return(tmp);  
}


get_continuation(string,first,long_string)

char *string,*first;
int  long_string;

{
   char *p,*q,*nl="\n"; 
   int  max,cnt,done;
   
   if (long_string) max=MAX_BUF; else max=MAX_STRING; 

   if (*string!='\0') strcat(string," ");
   
   if (*first=='\0') { 
      fgets(line,MAX_STRING,ppp);
      first=line;
   }
   	
   if (*first=='"') {
      if (strlen(string)+strlen(first)<max) {
      	 strcat(string,first+1);  cnt=strlen(string);
      }

      done=0; 
      for (q=first+1; !done && *q!='\0'; q++) {
        if (*q=='"') {
           for (p=q+1; *p==' ' || *p=='\t'; p++);
           done=(*p=='\0' || *p=='\n');
        }
      }
      *q='\0';

      while (!done) {
        fgets(line,MAX_STRING,ppp);
        for (q=line; !done && *q!='\0' && *q!='\n'; q++) {
          if (*q=='"') {
             for (p=q+1; *p==' ' || *p=='\t'; p++);
             done=(*p=='\0' || *p=='\n');
          }
        }

        if (!done) while (*(q-1)==' ' || *(q-1)=='\t') q--;
        *q='\0';

        if (cnt+strlen(line)<max) {
           if (cnt>0) { strcpy(string+cnt,nl); cnt++; }
           strcpy(string+cnt,line); 
           cnt+=strlen(line);
        }
      }
      if (string[cnt-1]=='"') string[cnt-1]='\0';

   } else {
      if (strlen(string)+strlen(first)<max) {
      	 strcat(string,first);  cnt=strlen(string);
      }
      for (q=string; *q!='\0' && *q!='!'; q++); *q='\0';
      while (*(q-1)=='-') {
        *(q-1)='\0'; 
      	fgets(line,MAX_STRING,ppp);
        for (q=line; *q!='\0' && *q!='\n' && *q!='!'; q++); *q='\0';
      	if (cnt+strlen(line)<max) {
      	   strcat(string,line);  cnt=strlen(string);
        }
      }
   }
   string[cnt]='\0';
}


flush_html()

{
   int  first,ii,jj;
   char *p,*q,*r,file[MAX_STRING],tmp[4];
   
   strcpy(key,name); my_lower(key); 
   sprintf(file,"%s/%s/%s__%s.html",d_root,pppname,pppname,key);
   
   fprintf(kindex,"<LI> <A HREF=\"../%s/%s__%s.html\">\n  %s</A>\n",
                  pppname,pppname,key,to_html(name));
   fprintf(kindex,"  %s\n",to_html(prompt));

   html=fopen(file,"w");
   if (html==NULL) {
      fprintf(stderr,"Cannot open %s\n",file);
      return(0);
   }

   fprintf(html,"<TITLE>Description of %s (%s)</TITLE>\n",name,PPPname);
   if (do_pef) 
        fprintf(html,"<H1>Description of general keyword %s</H1>\n",name);
   else fprintf(html,"<H1>Program %s: private keyword %s</H1> \n",PPPname,name);

   fprintf(html,"\n<DT><EM>Prompt:</EM> %s\n",to_html(prompt));

   if (*deflt!='\0') {
      fprintf(html,"<DT><EM>Default:</EM> %s",to_html(deflt));
      if (*units!='\0') fprintf(html," %s",to_html(units));
      fprintf(html,".\n");
   }
   
   fprintf(html,"<DT><EM>Expected input:</EM> %s",decode_data_type(data_type));
   if (data_type=='C')    fprintf(html,"(%d)",input_len);
   if (*units!='\0')      fprintf(html," in %s",to_html(units));
   if (nval>0)            fprintf(html,", %d values",nval);
   else if (nval==1)      fprintf(html,", single value");
   if (min_nval>0)        fprintf(html,", min: %d",min_nval);
   if (max_nval>0)        fprintf(html,", max: %d",max_nval);
   if (val_min!=-12345.0) fprintf(html,"; min.value: %f",val_min);
   if (val_max!=-12345.0) fprintf(html,"; max.value: %f",val_max);
   fprintf(html,".<P>\n");
   
   first=0;
   strcpy(buf,help);
   for (p=q=buf; *q!='\0'; q++) {
       if (*q=='\n') {
          if (*(q+1) == '.' && *(q+2) == '\n') {
             *q='\0'; fprintf(html,"%s <P>\n",to_html(p));
             q+=2; 
       	  } else if (*(q+1) == '\n') {
             *q='\0'; fprintf(html,"%s <P>\n",to_html(p));
             q++;
       	  } else {
             *q='\0'; fprintf(html,"%s \n",to_html(p));
       	  }
       	  p=q+1; first=1;
       	  for (r=p; *r==' ' || *r=='\t'; r++);
       	  while (*r!='\0' && *r!='\n' && *r!='\t' && *r!=' ') r++;
       	  if (*r=='\t') {    /* We find  <whitespace>XXXX<tab>text */
             *r='\0';
       	     fprintf(html,"<DT><STRONG>%s</STRONG> ",to_html(p));
       	     p=r+1; while (*p=='\t') p++;
             q=p-1;
       	     first=0;
       	  }
       } else if (first && *q=='-') {
       	  fprintf(html,"\n<DT>"); first=0;
       } else if (!strncmp(q,"\\ref{",5)) {
       	  *q='\0'; fprintf(html,"%s \n",to_html(p));
          for (r=q+5; *r!='}' && *r!='\0'; r++);
          if (*r!='\0') {
             *r='\0';
             fprintf(html,"%s\n",to_anchor(q+5,1));
             q=r;
          } 
          p=q+1; first=0;
       } else if (*q!=' ' && *q!='\t') {
       	  first=0;
       }
   }
   
   if (*p!='\0') fprintf(html,"%s <P>\n",to_html(p));

   fprintf(html,"\n <H3> More information: </H3> <UL>\n");
   if (do_pef) {
      fprintf(html,
        "<LI><A HREF=\"../%s/%s_comm.html\">List of general keywords</A> for %s\n",
             pppname,pppname,PPPname);
   } else { 
      fprintf(html,
        "<LI><A HREF=\"../%s/%s_keys.html\">List of keywords</A> for %s\n",
             pppname,pppname,PPPname);
   }
   /* If keyword has to do with filetype, looks like ....WMP_....  */
   
   for (jj=strlen(name); jj>3 && name[jj]!='_'; jj--);
   if (name[jj]=='_') { 
     for (ii=0; ftyp[ii]!=NULL && strncmp(name+jj-3,ftyp[ii],3); ii++);
     if (ftyp[ii]!=NULL) {
       strncpy(tmp,ftyp[ii],3); tmp[3]='\0'; my_lower(tmp);
       fprintf(html,
         "<LI>Description of the <A HREF=\"../%s_descr/%s_descr.html\">%s file format</A>\n",
         tmp,tmp,ftyp[ii],fnam[ii]);
     }
   }

   fprintf(html,
      "<LI><A HREF=\"../homepage.html\">NEWSTAR Documentation Home page</A>\n");
   if (!do_pef) {
     fprintf(html,
      "<LI>Description of <A HREF=\"../%s_descr/%s_descr.html\">program %s</A>\n",
             pppname,pppname,PPPname);
   }
   fprintf(html,
      "<LI>The <A HREF=\"../common_descr/common_descr.html\">DWARF User Interface</A>\n");

   fprintf(html,"</UL>\n");

   fclose(html);
}


flush_include(name,found)

char *name;

{
   int ii,jj,done;
   char tmp[MAX_STRING];

/*
   Separate filename and keyword list (INCLUDE=xxx_PEF:Key,Key)
*/
   for (ii=strlen(name); ii>0 && name[ii]!=':'; ii--); 
   if (ii!=0) name[ii]='\0';

   if (strcmp(name+strlen(name)-4,"_PEF")) {
      fprintf(stderr,"Invalid include: %s\n",name); return(0);
   } else {
      name[strlen(name)-4]='\0';
   }
   strcpy(tmp,name); my_lower(tmp);
   
   if (found>0) fprintf(kindex,"<P>\n");  /* Space behind keyword only */
   if (ii!=0) {
     fprintf(kindex,
      "\n<UL><LI>General keywords from <A HREF=\"../%s_comm/%s_comm.html\">%s</A><UL>\n",
      tmp,tmp,name);

     for (jj=ii+1,done=0; !done; ii=jj++) {
       while (name[jj]!=',' && name[jj]!='\0') jj++;
       if (name[jj]==',') name[jj]='\0'; else done=1; 
       sprintf(tmp,"%s__%s",name,name+ii+1); my_lower(tmp);
       fprintf(kindex,"<LI><A HREF=\"../%s/%s.html\">%s</A>\n",
               tmp,tmp,name+ii+1);
     }

     fprintf(kindex,"<P></UL></UL>\n\n");
   } else {
     fprintf(kindex,
        "\n<UL><LI>See also <A HREF=\"../%s_comm/%s_comm.html\">%s</A> </UL>\n\n",
         tmp,tmp,name);   
   }
}


char *to_html(line)

char *line;

{
     static char h_buf[5*MAX_STRING];
     
     char *p,*q;
     
     for (p=line,q=h_buf; *p!='\0'; p++) {

        if (*p=='&') {
            strcpy(q,"&amp;"); q+=5;
        } else if (*p=='>') {
            strcpy(q,"&gt;");  q+=4;
        } else if (*p=='<') {
            strcpy(q,"&lt;");  q+=4;
        } else if (*p==',' && *(p+1)!=' ') {
            strcpy(q,", ");    q+=2;
        } else {
            *(q++)=(*p); 
        }

     }

     *q='\0';
     return(h_buf);
}

/*
    Tranform latex reference to a html anchor

    LaTeX references look like:
    
       entity.attribute[.internal]
   
    eg: nscan.descr.tapes, gen.problems, scn.descr.sectors, ch.files
       
    The first two words translate to the filename by replacing the 
    dot with an underscore and appending the extension .html.
    The last word, if present, translates to an internal anchor name.

    CMV 940812: If reference to keyword (n*.*) use double underscore
*/

char *to_anchor(label,do_close)

char *label;
int  do_close;

{
    int  ndot,l1;
    char *p,*q;
    
    static char a_buf[MAX_STRING]="";

    for (p=label; *p!=' ' && *p!='\t' && *p!='\0'; p++);
    
    for (ndot=l1=0,q=NULL,p=label; ndot<2 && *p!='\0'; p++) {
      if (*p=='.') {
      	 if (!ndot) *p='_'; else { *p='\0'; q=p+1; }
         ndot++;
      } else if (!ndot) {
      	 l1++;   /* Count up to first dot */
      }
    }

    if (ndot) {
       label[l1]='\0';
       if (q==NULL) {
          if (*label=='n') {
             sprintf(a_buf,"<A HREF=\"../%s/%s__%s.html\">",label,label,label+l1+1);
          } else {
             sprintf(a_buf,"<A HREF=\"../%s/%s_%s.html\">",label,label,label+l1+1);
          }
       } else {
          sprintf(a_buf,"<A HREF=\"../%s/%s_%s.html#%s\">",label,label,label+l1+1,q);
       }
    } else {
       if (q==NULL) {
          sprintf(a_buf,"<A HREF=\"%s.html\">",label);
       } else {
          sprintf(a_buf,"<A HREF=\"%s.html#%s\">",label,q);
       }
    }
    if (do_close) strcat(a_buf,"<STRONG>here</STRONG></A>");
    return(a_buf);
}



void get_label(unit)

char *unit;

{
   int ld;
   
   ld=open(unit,O_RDONLY);
   if (read(ld,buf,80)==80 && !strncmp(buf,"VOL1",4)) {
      printf("%-10.10s\n",buf+4);
   } else {
      printf("Unlabeled\n");
   }
   close(ld);
}


void put_label(unit,label)

char *unit,*label;

{
   int ld;
   
   ld=open(unit,O_WRONLY+O_CREAT,0666);
   sprintf(buf,"VOL1%-10.10s%80c",label,' ');
   if (write(ld,buf,80)==80) {
      printf("Init: %-10.10s\n",buf+4);
   } else {
      printf("Init: Unlabeled\n");
   }
   close(ld);
}

