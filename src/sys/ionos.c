/*
     ionos.c  -  Simple program for entry of f0f2 values
     
     Syntax:   ionos.exe [input_file] | scissor.exe

     Input file can be either according to the Meudon standard
     or a simple list  with values per hour. The file has to start 
     with a header-line "f0f2 station_name dd/mm/yyyy hh:mm:ss", e.g.

      f0f2 havelte 27/02/1995 00:07:00

     and should further contain values in 0.1 MHz separated by commas 
     or whitespace. Two commas indicate a missing value for that hour.
     Additional header lines start a new series of values.

     For entry of ypf2 values, use simple list files with header lines
     containing ypf2 instead of f0f2.
    
     If no input file is given, standard input is read with appropriate 
     prompting to stderr.

     Output is in the form of Scissor commands to stdout.
     
    
     Syntax:   scissor.exe select=f0f2 date=dd/mm/yyyy | ionos.exe -w
     
     The program will create an output file f2MONyy.01 suitable for
     processing by program ionost.exe


     Revision:
       950821 CMV  Initial version
       960130 CMV  Changed averaging in make_ionos_file (first days, then hrs)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MEUDON_FLAG "UFOFH "
#define MEUDON_YR   "199"      /* Stupid notation... */

#define MAXSTRING  1024

static char line[MAXSTRING];

#define find_space(p)  { while (*p!='\0' && *p!='\n' && *p!=' ' && *p!='\t') p++; }
#define find_cspace(p) { while (*p!='\0' && *p!='\n' && *p!=' ' && *p!='\t' && *p!=',') p++; }
#define skip_space(p)  { while (*p==' ' || *p=='\t' || *p=='\n') p++; }


/*
   Some general routines for dates
*/
static int   dpm[]={   0,   31,   28,   31,   30,   31,   30,   31,   31,   30,   31,   30,   31};
static char *nom[]={"???","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"};


static char *mon(mm) 

int mm; 

{ 
  if (mm<0 || mm>12) mm=0; 
  return(nom[mm]); 
}


static int  days(mm,yy) 

int mm,yy;

{
  if (mm<0 || mm>12) {
     return(0); 
  } else if (mm==2 && yy%4 == 0 && yy%100 !=0) {
     return(dpm[mm]+1); 
  } else {
     return(dpm[mm]);
  }
}

static void next_day(p) char *p;

{ 
    int dy,mm,yy;

    dy=atoi(p);
    mm=atoi(p+3);
    yy=atoi(p+6);
    dy++; 
    if (dy>days(mm,yy)) mm++; 
    if (mm>12) yy++;
    sprintf(p,"%2.2d/%2.2d/%4.4d",dy,mm,yy);
}



main(argc,argv)

int argc;
char **argv;

{


  if (argc>1 && *argv[1]=='-' && argv[1][1]=='w') {
  	
     write_ionos_file(argv[2],argv[3]);
  
  } else if (argc>1 && is_meudon_type(argv[1])) {
  	
     read_meudon_type(argv[1]);

  } else if (argc>1 && is_list_file(argv[1])) {
  	
     read_list(argv[1]);

  } else {
  	
     read_list(NULL);

  }

}


int is_meudon_type(file)

char *file;

{
   int found=0;
   FILE *fp;
   
   fp=fopen(file,"r");
   if (fp!=NULL) {
   
      while (!found && fgets(line,MAXSTRING,fp)!=NULL) {
        char *p=line;
        skip_space(p);
        found=(!strncasecmp(p,MEUDON_FLAG,strlen(MEUDON_FLAG)));
      }
   
      fclose(fp);

   }
   
   return(found);
}


/*
   Definition of states
*/
#define _FIND_ID      0
#define _FIND_STATION 1
#define _FIND_DATE    2
#define _FIND_TIME    3
#define _GET_VALUES   4
#define _FOUND_END    5


int read_meudon_type(file)

char *file;

{
   char station[MAXSTRING];
   char date[12];
   char time[12];
  
   int state=_FIND_ID;
   int count=0; 
 
   FILE *fp;
   
   fp=fopen(file,"r");
   if (fp==NULL) {
      fprintf(stderr,"Cannot open Meudon-format file %s\n",file);
      return(count);
   }
   
   /*
      Read lines until done of end-of-file found
   */
   while (state!=_FOUND_END && fgets(line,MAXSTRING,fp)!=NULL) {
     char *p=line;
     
     skip_space(p);

     /*
       Handle the words on each line
     */
     while (state!=_FOUND_END && *p!='\0') {
     	
     
       if (state==_FIND_ID) {
       	  if (!strncasecmp(p,MEUDON_FLAG,strlen(MEUDON_FLAG)))
       	     state=_FIND_STATION;

       } else if (state==_FIND_STATION) {
          char *q=station;
       	  state=_FIND_DATE;
          strcpy(station,p);
          find_space(q);
          *q='\0';

       } else if (state==_FIND_DATE) {
          state=_FIND_TIME;
       	  sprintf(date,"%c%c/%c%c/%s%c",p[3],p[4],p[1],p[2],MEUDON_YR,*p);
       	  
       } else if (state==_FIND_TIME) {
       	  if (*p!='/') {
       	     state=_FOUND_END;
       	     fprintf(stderr,"Invalid Meudon-format file...");
          } else {
             state=_GET_VALUES;
             sprintf(time,"%c%c:%c%c:00",p[1],p[2],p[3],p[4]);
          }
          
       } else if (state==_GET_VALUES) {
          if (*p<'0' || *p>'9') {
             state=_FIND_ID;
          } else { 
             if (*p<time[1]) time[0]++;
             time[1]=(*p);
             if (time[0]>'2') { time[0]='0'; next_day(date); }
             if (p[1]>='0' && p[1]<='9' && 
                 p[2]>='0' && p[2]<='9' && 
                 p[3]>='0' && p[3]<='9') 
               printf("PUT=F0F2 STATION=%s DATE=%s TIME=%s F0F2=%c%c%c\n",
                       station,date,time,p[1],p[2],p[3]);
             count++;
          }       	
       }

       find_space(p); skip_space(p);

     }

   }

   fclose(fp);
   fprintf(stderr,"Total of %d values written for %s.\n",count,station);
   return(count);
}


int is_list_file(file)

char *file;

{
   int found=0;
   FILE *fp;
   
   fp=fopen(file,"r");
   if (fp!=NULL && fgets(line,MAXSTRING,fp)!=NULL) {
      found=(!strncasecmp(line,"f0f2 ",5) ||
             !strncasecmp(line,"ypf2 ",5));
      fclose(fp);
   }
   
   return(found);
}


/*
   Definition of states
*/
#define _READ_HEADER    0
#define _PARSE_HEADER   1
#define _READ_VALUES    2
#define _PARSE_VALUES   3
#define _DONE_FILE      4

int read_list(file)

char *file;

{
   char station[MAXSTRING];
   char date[12];
   char time[12];
   char *view=NULL;
  
   int state=_READ_HEADER;
   int count=0;
  
   char *word;
   FILE *fp;
   
   if (file!=NULL) {
     fp=fopen(file,"r");
     if (fp==NULL) {
        fprintf(stderr,"Cannot open f0f2 list %s.\n",file);
     	return(count);
     }
   } else {
     fp=stdin;
   }


   while (state!=_DONE_FILE) {

     if (state==_READ_HEADER) {

       if (fp==stdin) 
          fprintf(stderr,"Enter station, date (dd/mm/yyyy), time (hh:mm): ");
       if (fgets(line,MAXSTRING,fp)==NULL) {
          fprintf(stderr,"Cannot read header line\n");
          state=_DONE_FILE;
       } else if (fp!=stdin && strncasecmp(line,"f0f2 ",5) &&
                               strncasecmp(line,"ypf2 ",5)) {
          fprintf(stderr,"Invalid header line\n");
       } else {
       	  state=_PARSE_HEADER;
       }
  

     } else if (state==_READ_VALUES) {

       if (fp==stdin) 
          fprintf(stderr,"Enter f0f2 values (MHz, comma separated): ");
       if (fgets(line,MAXSTRING,fp)==NULL) {
          state=_DONE_FILE;
       } else if (!strncasecmp(line,"f0f2 ",5)) {
          fprintf(stderr,"New header line!\n");
          state=_PARSE_HEADER;
       } else {
          word=line;
       	  state=_PARSE_VALUES;
       }
  
     } else if (state==_PARSE_HEADER) {

       char *p=line;
       skip_space(p);

       if (!strncmp(p,"ypf2",4)) { 
          view="YPF2";
          find_space(p); skip_space(p);
       } else {
          view="F0F2";
          if (!strncmp(p,"f0f2",4)) { find_space(p); skip_space(p); };
       }
       
       if (*p=='\0') {
      	 fprintf(stderr,"Missing station and date on header line\n");
      	 state=_READ_HEADER;
       } else {
         char *q=station; 
     	 strcpy(station,p);
         find_cspace(q);
         *q='\0';
         find_cspace(p); 
         if (*p==',') p++;
         skip_space(p);
       }

       if (state!=_PARSE_HEADER || *p=='\0' || p[2]!='/' || p[5]!='/') {
          if (state==_PARSE_HEADER) 
               fprintf(stderr,"Missing or invalid date on header line\n");
          state=_READ_HEADER;
       } else {      
         strncpy(date,p,10);
         date[10]='\0';
         find_cspace(p); 
         if (*p==',') p++;
         skip_space(p);
       }

       if (state!=_PARSE_HEADER || *p=='\0' || p[2]!=':') {
          if (state==_PARSE_HEADER) 
               fprintf(stderr,"Missing or invalid time on header line\n");
          state=_READ_HEADER;
       } else {      
         strncpy(time,p,5);
         time[5]='\0';
         strcat(time,":00");
         state=_READ_VALUES;
       }

     } else if (state==_PARSE_VALUES) {
       skip_space(word);

       if (*word=='\0') {              /* End of current line */
      	 state=_READ_VALUES;
       } else {
         if (*word!=',') {             /* Not an empty field  */
           double val;
           val=atof(word);
           printf("PUT=%s STATION=%s DATE=%s TIME=%s F0F2=%f\n",
                   view,station,date,time,val*10.);
           count++;
           
           /*
              Words may be separated by a comma or by whitespace.
              The comma may be surrounded by whitespace.
           */
           find_cspace(word);
           if (*word==',') {
              word++;
           } else {
              skip_space(word);
              if (*word==',') word++;
           }
         } else {
           word++;                     /* Just skip the comma */
         }
         
         /*
            Prepare for next value
         */
         if (time[1]++ == '9') { time[1]='0'; time[0]++;      }
         if (time[0]>'2')      { time[0]='0'; next_day(date); }

       	
       }

     }
   }

   if (fp!=stdin) fclose(fp);   
   fprintf(stderr,"Total of %d values written.\n",count);
   return(count);

}

#define NDAY 33
#define NHR  24

int write_ionos_file(yy,mm)

char *yy,*mm;

{
   FILE *fp;
   short xday[NDAY];
   short f0f2[NHR][NDAY];
   short ypf2[NHR][NDAY];
   int   nf0f2[NHR][NDAY];
   int   nypf2[NHR][NDAY];

   int year,month,iday,nday,ihr;
  
   for (iday=0; iday<NDAY; iday++) {
     xday[iday]=0;
     for (ihr=0; ihr<NHR; ihr++) {
       f0f2[ihr][iday]=ypf2[ihr][iday]=nf0f2[ihr][iday]=nypf2[ihr][iday]=0;
     }
   }
  
   year=atoi(yy);
   month=atoi(mm);
   if (year<1900 || year>3000) {
      fprintf(stderr,"Invalid year %d.\n",year);
      return(0);
   } else if (month<1 || month>12) {
      fprintf(stderr,"Invalid month %d.\n",month);
      return(0);
   }

   sprintf(line,"f2%s%2.2d.01",mon(month),year%100);
   fp=fopen(line,"wb");
   if (fp==NULL) {
      fprintf(stderr,"Error: cannot open f0f2-file %s.\n",line);
      return(0);
   }

   nday=days(month,year);
   for (iday=0; iday<=nday; iday++) xday[iday]=iday;
   
   while (fgets(line,MAXSTRING,stdin)!=NULL) {
     char *p;

     p=strstr(line,"DATE=");
     if (p!=NULL && atoi(p+8)==month && atoi(p+11)==year) {
        iday=atoi(p+5);
        p=strstr(line,"TIME=");
        if (p!=NULL) {
          ihr=atoi(p+5);

          p=strstr(line,"F0F2=");
          if (p!=NULL) {
             f0f2[ihr][iday]=
               ( (f0f2[ihr][iday]*nf0f2[ihr][iday]) + atoi(p+5) ) /
                                 (nf0f2[ihr][iday]+1) ;
             nf0f2[ihr][iday]++;
          }

          p=strstr(line,"YPF2=");
          if (p!=NULL) {
             ypf2[ihr][iday]=
               ( (ypf2[ihr][iday]*nypf2[ihr][iday]) + atoi(p+5) ) /
                                 (nypf2[ihr][iday]+1) ;
             nypf2[ihr][iday]++;
          }
        }
     }
   }

   for (iday=1; iday<=nday; iday++) {
     for (ihr=0; ihr<NHR; ihr++) {
       if (f0f2[ihr][iday]==0) {
          if (iday>1 && f0f2[ihr][iday-1]!=0) {
             f0f2[ihr][iday]=f0f2[ihr][iday-1];
          } else if (iday<nday && f0f2[ihr][iday+1]!=0) {
             f0f2[ihr][iday]=f0f2[ihr][iday+1];
          } else if (ihr>0 && ihr<NHR-1 &&
              f0f2[ihr-1][iday]!=0 && f0f2[ihr+1][iday]!=0) {
             f0f2[ihr][iday]=(f0f2[ihr-1][iday]+f0f2[ihr-1][iday])/2;
          }
       }
       if (ypf2[ihr][iday]==0) ypf2[ihr][iday]=100;
     }
   }

   fwrite(xday,sizeof(short),NDAY,fp);
   fwrite(f0f2,sizeof(short),NDAY*NHR,fp);
   fwrite(ypf2,sizeof(short),NDAY*NHR,fp);   
   fwrite(xday,sizeof(short),1,fp);
   fwrite(xday,sizeof(short),1,fp);
   fclose(fp);
}
