/*
docScript.c - program to convert the output of a ndoc Script session to a
LaTeX script file that can be included in a LaTeX document

NOTE: The treatment of ^D and ^H characters is Sun-specific


History:
	JPH 940607	Created by extraction from CMV's docaid.c	
*/
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#define MAX_STRING 256     /* Length of general strings  */
#define MAX_BUF    5000    /* Length of help buffer      */

char line[MAX_STRING];       /* Character buffer          */
char buf[MAX_BUF];           /* Multi purpose buffer      */

static char *s_line[]={"line","long"};


/*****************************************************************************/

char* getline (ln, len, f)
	char *ln; int len; FILE *f;
{
	char *p; char* st;

	if ( (st=fgets (ln, len, f))!=NULL ){
/*
Clean up input line:
Remove \b (=^H) and the preceding character, terminate at \n or \r (=^M)
Do not remove trailing blanks because they are significant in determining where
the user reply is (see further down)!
*/
	  for (p=ln; *p!='\n' && *p!='\r' && *p!='\0'; p++) {
     	    if (*p=='\b'){
	      p--; strcpy(p,p+2); p--;
            } else if (*p=='\t') {
	      *p=' ';
	    }	
       	  }
	  *p='\0';
	}
	return st;
}


/*****************************************************************************/

/*
    Transform a line of characters to a string that can be printed by
    LaTeX. This is achieved by escaping all LaTeX special characters
    and by removing any Control key characters
*/

char *to_latex(line)
     	char *line;
{
     	static char la_buf[2*MAX_STRING];     
     	char *p,*q;
   
     	for (p=line, q=la_buf; *p!='\0'; p++) {
      	  if (*p=='%' || *p=='_' || *p=='$' || *p=='#' || 
      	      *p=='~' || *p=='&' || *p=='{' || *p=='}' || *p=='\\') {
       	    *(q++)='\\'; *(q++)=(*p); 
       	  } else if (*p==',' && *(p+1)!=' ') {
       	    *(q++)=','; *(q++)=' ';
       	  } else if (*p=='>' || *p=='<') {      /* Should be in math mode */
       	    *(q++)='$'; *(q++)=(*p); *(q++)='$'; 
       	  } else if (*p==0x07 || *p==0x0a) {
       	  } else if (*p=='^' || (*p>0 && *p<=0x12) ) {
      	    strcpy(q,"$\\wedge$"); q+=8; 
       	    if (*p!='^') {
	      *(q++)=('@'+(*p));
       	    } 
	  }else if (*p<0x20 || *p==0x7f) {
       	      *(q++)='?';
       	  } else {
       	      *(q++)=(*p); 
       	  }
	}

     	*q='\0';
   	return(la_buf);
}
/*****************************************************************************/
     
int main(argc,argv)
    	int  argc;
    	char **argv;
{
    	int  started=0, nline_per_par=0, scan_key=0, iline=0, forcereply=0, ll;
    	char *p,*q,*w2;
    	FILE *fp;
    	char *prompt,*deflt,*user,*cmt;
    	static char *s_line[]={"line","long"};
    	int type=0; enum{ UNIXPR=1, PROGPR, PROGSTART };
    	int verbatim=0, page=0;
  
    	fp=fopen(argv[1],"r");
    	if (fp==NULL) {
       	  fprintf(stderr,"\nError: cannot open %s...\n",argv[1]);
      	  return 1;
    	}
   
    	while (getline(line,MAX_STRING,fp) !=NULL) {

/* Find pointer to second word, if any  */        
     	  for (w2=line; *w2!=' ' && *w2!='\t' && *w2!='\0'; w2++);
     	  while (*w2==' ' || *w2=='\t') w2++;
	  if (*w2=='\0') continue;

/* Wait for first line with "scr> " prompt */
      	  if (!started) started=(!strncmp(line,"scr> ",5));

/*
Check the input: messages from "script" are ignored, 
                "program started message" on separate line
                 operating-system prompts all start with "> "
                 DWARF prompts split in components

A Newstar prompt has the following syntax:
    
    <bell>KEYWORD (<prompt>) = <default>: <user_response> \n
    
Prompts and defaults can extend over multiple lines.
 In the output, we restrict the length of the prompt to 5 options.

User_response may be continued on the next line, in which 
 case an underscore (_) is used as a continuation prompt.
 This is not handled yet.     
*/
      	  if (!started || 
             !strncmp(line,"Script started",14) ||  /* Script messages  */
             !strncmp(line,"script done",11)    ||
             !strcmp(line,"exit") ) {       
    	  } else if (*line==0x07 && line[1]>='A' && line[1]<='Z') {
	    type=PROGPR;
      	  } else if (!strncmp(line,"scr> ",5)) {
	    type=UNIXPR;
/*      	  } else if (!strncmp(w2,"is started at",13)) {
	    type=PROGSTART; */
	  } else {
/*
Program message output. It can not be formatted by means of a new LaTeX command like we do for all other components, because the expansion of such a command would have to contain a verbatim environment and this is not allowed. (See LaTeX Book, "Reference Manual" Appendix, section "Verbatim".)
*/
	    type= 0;
	    if (!verbatim){
	      if (*line !=0){
	        printf("\n\\svbegin\\begin{verbatim}"); verbatim=1;
	      };
	    }
	    if (verbatim){
	      printf ("\n%s",line);
	    }
	  }
/*
Terminate verbatim section if necessary
*/
	  switch (type){
	  case PROGPR:
	  case UNIXPR:
	    break;
	  }
	  switch (type){
	  case PROGPR:
	  case UNIXPR:
	    if (verbatim){
	      verbatim=0; printf("\n\\end{verbatim}\\svend");
	    }
	    if (page){
	      page=0; printf("\n\\spend %%.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.");
	    }
	    if (!page){
	      page=1; printf ("\n%%\n\\spbegin %%.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.");
	    }
	  }   
	  switch (type){
/*
Program prompt sequence
*/
	  case PROGPR:
            iline=0; /* Reset margin */

            strcpy(buf,line+1);
            prompt=deflt=user=cmt=NULL; forcereply=0;
	    for (p=buf; user==NULL; ) {
              if (*p=='\0') {
             	if (getline(line,MAX_STRING,fp)==NULL) { /* continuation line */
             	  if (prompt==NULL) prompt="(????)";
                  if (deflt==NULL)  deflt="= :";
             	  if (user==NULL)   user="??";
                } else {
                  for (q=line; *q==' ' || *q=='\t'; q++);
                  strcpy(p,q);
                }
              }
/* Locate prompt */
              if (*p!='\0' && prompt==NULL) {
                while (*p!=' ' && *p!='\0') p++;
                if (*p==' ') {
		  *p='\0'; prompt=p+1; ll=0; p++;
		}
              }
/* Locate default */
              if (*p!='\0' && deflt==NULL) {
             	while (*p!='=' && *p!='\0') {
             	  if (*p==',') {
             	    ll++; if (ll>4) *p='\0';
             	  }
             	  p++;
             	}
             	if (*p=='=') { *(p-1)='\0'; deflt=p; } 
              }
/* Locate user's reply. The simple logic of the previous sections is compounded
here by the possibility of the user reply starting on a new line (in which case
the previous line ends in a naked ':' */
	      if (forcereply) {
		user=p++; forcereply=0;
              } else if ( *p!='\0' && user==NULL) {
             	while (*p!=':' && *p!='\0') p++;
             	if (*p++==':') { 
             	  if (*p=='\0') {
             	    *++p='\0'; forcereply=1;
             	  } else {
                    *p++='\0'; 
                    user=p;
		  }
		}
	      }
	      if (user !=NULL) { 
               	while (*user==' ' || *user=='\t') user++;
             	for (cmt=user; *cmt!='!' && *cmt!='\0'; cmt++);
             	if (*cmt=='!') { *cmt='\0'; cmt++; }
              }

	    } /* end for */
/*
   In order not to overburden LaTeX memory, insert blank line if 
   paragraphs get too long. Just before a prompt seems a suitable place 
*/
      	    if (nline_per_par>50) { printf("\n"); nline_per_par=0; }
       	    printf("\n\\skeyword{%s}",to_latex(buf));
       	    if (ll>4) printf("\n\\sprompt{%s...)}",to_latex(prompt));
       	    else      printf("\n\\sprompt{%s}",to_latex(prompt));
       	    printf("\n\\sdefault{%s}",to_latex(deflt));
      	    if (*user=='\0'){
	      printf("\n\\suser{\\scr}");
            } else if (*user==0x04 || *user=='#'){
	      printf("\n\\suser{\\seof}");
            } else {
	      printf("\n\\suser{%s}",to_latex(user));
	    }
            if (*cmt!='\0'){
	      printf("\n\\sinline{%s}",to_latex(cmt));
	    }
	    break;           
/*
UNIX command
*/
	  case UNIXPR:
            iline=0; 					/* Reset margin */

/* separate comment          */

            for (p=w2; *p!='#' && *p!='\0'; p++);
            q=p; if (p!=w2) q--;
            while (q!=w2 && (*q==' ' || *q=='\t' || *q==';')) q--;
                                               		/* Only a comment */
            if (q==w2) {
              if (*p=='#') printf("\n\\scomment{%s}",to_latex(p+1,0));         
                                               /* "exit" and ^D are skipped */
            } else if (strncmp(w2,"exit",4) && *w2!=0x04) { 
              if (*p=='#') {
                 *q='\0';
                 printf("\n\\scmd{%s}",    to_latex(w2,1));
                 printf("\n\\sinline{%s}", to_latex(p+1,0));
              } else {
                 printf("\n\\scmd{%s}",    to_latex(w2,1));
              }           
              nline_per_par++;
            };
	    break;
/*
Program start message
*/
	  case PROGSTART:
            printf("\n\\s%s{%s}",s_line[iline],to_latex(line,1));
            ll=strlen(line);
            if (ll>80) {          	/* Long line, indicate and strip */
              line[72]='>'; line[73]='\0'; 
            } else if (ll>73) {  	 /* Long line: change margin      */
              iline=1;
            }
            if (ll==0) {
	      printf("\n\\sskip");
            } else {
	      printf("\n\\s%s{%s}",s_line[iline],line /*to_latex(line)*/);
              nline_per_par++;
	    }
       	  } /* end of switch */

	} /* end of input-line loop */
        
	if (verbatim) printf ("\n\\end{verbatim}}\\svend");
	if (page){
	  printf ("\n\\spend %%.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.\n");
	}
	printf ("\n");  
    	fclose(fp);
    	return 0;
};
