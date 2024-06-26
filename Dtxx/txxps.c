/*****************************************************************************
*
*                              Physics Division
*                       Oak Ridge National Laboratory
*
*                           Copyright(C) 1994-2002
*
*
*
*                         IMPORTANT NOTICE TO USER
*
*     All statements, technical information and recommendations contained
*     herein are based on tests we believe to be reliable, but the accuracy
*     or completeness thereof is not guaranteed, and the following  is
*     made in lieu of all warranties expressed or implied:
*
*     Our only obligation shall be to correct or replace, at our convenience,
*     any part of the product proven to be defective.  We shall not be liable
*     for any loss or damage, direct or consequential, arrising out of the
*     use or the inability to use the product.  Before using, the USER shall
*     determine the suitability of the product for his or her intended use,
*     and the USER assumes all risk and liability whatsoever in connection
*     therewith.
*
******************************************************************************
*
*    Environment:  ORPHAS Acquisition System
*
*    File:         /usr/users/mcsq/Dlinux/Ddoc/txxps.c
*
*    Description:  Converts documents in Milner's txx format to a
*                  Postscript file.  In theory anyone with a Postscript
*                  printer should be able to print txx documents in
*                  all their glory.
*
******************************************************************************
*
*    Revision History:
*
*    Date       Programmer   Comments
*
*    2/12/94    MCSQ         Original
*
*   10/23/02    MCSQ         Added a return (0); statment for the normal
*                            return.  This makes "dodoc" and "viewdoc"
*                            happy.
*
*   10/24/02    MCSQ         This version is for Milner's TXX as compiled
*                            and linked with the Intel compiler.
*****************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#if defined(__APPLE__) || defined(__CYGWIN__)
#include <stdlib.h>
#endif

#define  XPOSITION  975
#define  YPOSITION  14975
#define  YDELTA     220

/*   Function Prototypes  */
int lenstr(char *);
void exit_error(int);

char in_line[257];
char outline[257];
char nline[257],bline[257],uline[257];
char hdrname[257] = "/usr/local/hhirf/doc/hhirf.pro";
int  Xposition,Yposition,Ydelta;

FILE *infile, *outfile, *hdrfile;

 int main(int argc, char *argv[])
{
   char *cptr,*ncptr,*bcptr,*ucptr,*optr;
    int ncount,bcount,ucount;
    int flgbold,flgunder;
    int xpos,ypos;
    int boldoff;
    int page = 0;

int i;

   if (argc < 2) {fprintf(stderr,"Need name of a .txx file!\n"); exit(1);}
   strcpy(nline,argv[1]);
   strcat(nline,".txx");
/*
*   Open the input file
*/
   if ((infile = fopen(nline,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open input file - %s\n",nline);
       exit(2);
     }
/*
*   Set the default parameters then check for changes on the command
*   line.
*/
   Yposition = YPOSITION;
   Xposition = XPOSITION;
   Ydelta = YDELTA;
   if (argc >= 3) strcpy(hdrname,argv[2]);
   if (argc >= 4) sscanf(argv[3],"%i",&Xposition);
   if (argc >= 5) sscanf(argv[4],"%i",&Yposition);
   if (argc >= 6) sscanf(argv[5],"%i",&Ydelta);
/*
*   Open the header file.  Has header stuff for the Postscript file.
*/
   if ((hdrfile = fopen(hdrname,"r")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open header file - %s\n",hdrname);
       exit(2);
     }
/*
*   Remove any path specification from the .txx file name.  We always
*   put the .ps file in our current working directory.
*/
   cptr = strrchr(argv[1],'/');
   if (cptr == NULL) cptr = argv[1];
   else  cptr++;
   strcpy(nline,cptr);
   strcat(nline,".ps");
/*
*   Open the output file
*/
   if ((outfile = fopen(nline,"w")) == (FILE *)NULL)
     {
       fprintf(stderr,"Cannot open output file - %s\n",nline);
       exit(3);
     }
/*
*   Copy the header file to the output file
*/
   while (fgets(in_line,sizeof(in_line),hdrfile) != NULL)
     {
       fwrite(in_line, sizeof(char), strlen(in_line), outfile);
     }
   fclose(hdrfile);

   xpos = Xposition;
   ypos = Yposition;
   ypos -= Ydelta;
   flgbold = flgunder = 0;
/*
*   Select normal font and start a page
*/
   page++;
   sprintf(outline,"%%%%Page: %d %d\n",page,page);
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   fwrite("0 F\n",sizeof(char),4,outfile); 
   boldoff = 1;
   strcpy(outline,"StartPage\n");
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   while (fgets(in_line,sizeof(in_line),infile) != NULL)
     {
       ncount = bcount = ucount = 0;
       ncptr = nline;
       bcptr = bline;
       ucptr = uline;
       *ncptr = *bcptr = *ucptr = '\0';
       cptr = in_line;
       while(*cptr != '\n') {if (*cptr == '\0') *cptr = 0x0d; cptr++;}
       cptr = in_line;
       if (*cptr == '\13')  /* Escape sequence should follow a vertical tab */
         {
/*
*    Start of an escape sequence.  Only the following escape sequences
*    are allowed:
*       <ESC>[1m   -  Turn on Bold print
*       <ESC>[4m   -  Start underline
*       <ESC>[22m  -  Turn off bold
*       <ESC>[24m  -  End underline
*/
              cptr++;
              if (*cptr++ != '\33') exit_error(1);
              if (*cptr++ != '[') exit_error(1);
              if (*cptr == '1') flgbold = 1;
              else if (*cptr == '4') flgunder = 1;
              else if (*cptr == '2')
                {
                  cptr++;
                  if (*cptr == '2') flgbold = 0;
                  else if (*cptr == '4') flgunder = 0;
                  else  exit_error(2);
                }
              else  exit_error(3);
              cptr++;
              if (*cptr != 'm') exit_error(4);
              continue;
         }
       cptr = in_line;
       while (*cptr != '\0')
        {
          if (*cptr == '\33')   /*  0x1b  */
/*
*    Start of an escape sequence.  Only the following escape sequences
*    are allowed:
*       <ESC>[1m   -  Turn on Bold print
*       <ESC>[4m   -  Start underline
*       <ESC>[22m  -  Turn off bold
*       <ESC>[24m  -  End underline
*/
            {
              cptr++;
              if (*cptr++ != '[') exit_error(1);
              if (*cptr == '1') flgbold = 1;
              else if (*cptr == '4') flgunder = 1;
              else if (*cptr == '2')
                {
                  cptr++;
                  if (*cptr == '2') flgbold = 0;
                  else if (*cptr == '4') flgunder = 0;
                  else  exit_error(2);
                }
              else  exit_error(3);
              cptr++;
              if (*cptr != 'm') exit_error(4);
              cptr++;
              continue;
            }
          if (*cptr == '\14')    /*  0x0c  */
            {
/*
*   Top of form.  End current page,  start new page, and reset all
*   counters and pointers.
*/
              strcpy(outline,"EndPage\n");
              fwrite(outline, sizeof(char), strlen(outline), outfile);
              page++;
              sprintf(outline,"%%%%Page: %d %d\n",page,page);
              fwrite(outline, sizeof(char), strlen(outline), outfile);
              strcpy(outline,"StartPage\n");
              fwrite(outline, sizeof(char), strlen(outline), outfile);
              ypos = YPOSITION;
              ncptr = nline;
              bcptr = bline;
              ucptr = uline;
              ncount = bcount = ucount = 0;
              *ncptr = *bcptr = *ucptr = '\0';
              boldoff = 1;
            }
          else if (*cptr == '\15')   /*  0x0d  */
            {
/*
     Do nothing for a carriage return
*/
            }
          else if (*cptr == '\n')    /*  0x0a  */
            {
/*
*   Line feed.  Print all data we have and prepare for next line.
*/
              sprintf(outline,"%i %i(\0",xpos,ypos);
              optr = outline + strlen(outline);
              if (ncount != 0 && lenstr(nline) != 0)
                {
/*
*   Normal font output
*/
                  if (!boldoff)
                    {
                      fwrite("0 F\n",sizeof(char),4,outfile); 
                      boldoff = 1;
                    }
                  strcat(optr,nline);
                  strcat(optr,")B\n");
                  fwrite(outline, sizeof(char), strlen(outline), outfile);
                  ncount = 0;
                }
              if (bcount != 0 && lenstr(bline) != 0)
                {
/*
*   Bold font output
*/
                  if (boldoff)
                    {
                      fwrite("1 F\n",sizeof(char),4,outfile); 
                      boldoff = 0;
                    }
                  *optr = '\0';
                  strcat(optr,bline);
                  strcat(optr,")B\n");
                  fwrite(outline, sizeof(char), strlen(outline), outfile);
                  bcount = 0;
                }
              if (ucount != 0 && lenstr(uline) != 0)
                {
/*
*   Underlining
*/
                  *optr = '\0';
                  strcat(optr,uline);
                  strcat(optr,")B\n");
                  fwrite(outline, sizeof(char), strlen(outline), outfile);
                  ucount = 0;
                }
              ypos -= Ydelta;
              if (ypos < 675)
                {
                  *cptr = 0x0c;
                  continue;
                }
            }
          else
            {
/*
*   Some other character.
*/
              if (*cptr == '(' || *cptr == ')' || *cptr == '\\')
                {
/*
*   Prefix for special characters
*/
                  if (flgbold) *bcptr++ = '\\', bcount++;
                  else  *ncptr++ = '\\', ncount++;
                }
              if (flgbold)
                {
/*
*   Bold font data
*/
                  *bcptr++ = *cptr;
                  *ncptr++ = ' ';
                   if (*cptr != ' ') bcount++;
                }
              else
                {
/*
*   Normal font data
*/
                  *ncptr++ = *cptr;
                  *bcptr++ = ' ';
                  if (*cptr != ' ') ncount++;
                }
              if (flgunder)
                {
/*
*   Underline
*/
                  *ucptr++ = '_';
                  ucount++;
                }
              else
                {
                  *ucptr++ = ' ';
                }
              *ncptr = '\0';
              *bcptr = '\0';
              *ucptr = '\0';
            }
          cptr++;
         }
     }
/*
*   End last page.
*/
   strcpy(outline,"EndPage\n");
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   strcpy(outline,"%%Trailer\n");
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   sprintf(outline,"%%%%Pages: %d\n",page);
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   strcpy(outline,"%%EOF\n");
   fwrite(outline, sizeof(char), strlen(outline), outfile);
   return (0);
}
/******************************************************************************
*
*   Null terminate a string pointed to by line after the right most nonblank
*   character on the line.  Returns the number of characters in the string.
******************************************************************************/
int lenstr(char *line)
{
   int  len;
   char *s1;

   len = strlen(line);
   if (len == 0) return 0;
   s1 = line + len - 1;
   while(s1 > nline)
     {
       if (*s1 == ' ') *s1-- = '\0';
       else  break;
     }
   return strlen(line);
}
/******************************************************************************
*
*   Illegal escape sequence
******************************************************************************/
void exit_error(int code)
{
   printf("Illegal escape sequence in input file\n");
   exit(code);
}
