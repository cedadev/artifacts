C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C*                    D 1 R E A D   M O D U L E                       *C
C*--------------------------------------------------------------------*C
C* THIS PROGRAM READS D1 HDF FORMAT FILES PRODUCED BY ISCCP           *C
C*--------------------------------------------------------------------*C
C* CONTAINS:                                                          *C
C*    PROGRAM SAMPLE    :  EXAMPLE OF HOW TO USE THESE SUBROUTINES    *C
C*    SUBROUTINE D1OPEN :  POSITION FILE AT REQUESTED SDS             *C
C*    SUBROUTINE D1READ :  UNPACK D1 DATA FOR ONE LATITUDE BAND       *C
C*    SUBROUTINE D1PHYS :  CONVERT DATA IN LAT BAND TO PHYSICAL VALUES*C
C*    SUBROUTINE MIDPRS :  CALCULATE MID-LAYER PRESSURES FOR GRID BOX *C
C*    SUBROUTINE RDANC  :  READ ANCILLARY DATA FILE                   *C
C*    SUBROUTINE PRINTI :  PRINT COUNT VALUES FOR ONE GRID BOX        *C
C*    SUBROUTINE PRINTR :  PRINT PHYSICAL VALUES FOR ONE GRID BOX     *C
C*    SUBROUTINE CENTER :  CALCULATE CENTER LON/LAT OF GRID BOX       *C
C*    SUBROUTINE TOTIR  :  CALCULATE TOTAL IR RADIANCE                *C
C*    SUBROUTINE TOTVIS :  CALCULATE TOTAL VS RADIANCE                *C
C*    SUBROUTINE CLDHGT :  CALCULATE CLOUD TOP HEIGHT IN METERS       *C
C*    SUBROUTINE EQ2SQ  :  CONVERT EQUAL AREA MAP TO SQUARE MAP       *C
C*    BLOCK DATA        :  CONVERSION TABLES AND EQUAL-AREA GRID INFO *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* THIS SAMPLE PROGRAM READS AN ENTIRE D1 FILE BY LOOPING OVER THE 72   
C* LATITUDE BANDS ONE AT A TIME.  FOR EACH PASS THROUGH THE LOOP,       
C* D1READ IS CALLED TO UNPACK THE DATA FOR THAT LAT BAND, AND THEN      
C* D1PHYS IS CALLED TO CONVERT THAT DATA TO PHYSICAL VALUES.  CLOUD     
C* AMOUNT IS SAVED IN AN EQUAL-AREA ARRAY, THEN CONVERTED TO A          
C* SQUARE LAT/LON ARRAY AT THE END OF THE PROGRAM, AND WRITTEN OUT.     
C* USE OF THE OTHER SUBROUTINES IS ILLUSTRATED FOR A FEW GRID BOXES.    
C*--------------------------------------------------------------------*C
C*  Modifications                                                     *C
C*    April 2002 - The format of the ISCCP D1 hdf files was modified  *C
C*    for all data after Jan. 1994.  This software will read both file*C
C*    formats.
C*--------------------------------------------------------------------*C
      PROGRAM SAMPLE                                                    
C*--------------------------------------------------------------------*C
C* D1 DATA                                                            *C
C*--------------------------------------------------------------------*C
C* NUMBER OF VARIABLES REPORTED IN EACH D1 GRID BOX                     
      PARAMETER     ( MAXVAR = 202 )                                    
C* GRIDS                                                                
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( MAXLAT = 72  )                                    
      PARAMETER     ( MAXBOX = 6596 )                                   
C* UNDEFINED VALUE FOR INTEGER VALUES                                   
      PARAMETER     ( IUNDEF = 255 )                                    
C* UNDEFINED VALUE FOR FLOATING POINT VALUES                            
      PARAMETER     ( RUNDEF = -1000.0 )                                
C* D1 DATA FOR ONE LATITUDE ZONE                                        
C* D1READ WILL SET NLON TO THE NUMBER OF EQUAL-AREA BOXES IN THE        
C* LATITUDE ZONE, AND SET IVAR TO THE INTEGER DATA.  TO CONVERT         
C* TO PHYSICAL VALUES, CALL D1PHYS WHICH WILL TAKE THE DATA IN          
C* IVAR, CONVERT IT TO PHYSICAL VALUES, AND STORE IT IN RVAR.           
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
C* EQUAL AREA GRID INFO                                                 
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
C* COUNT TO PHYSICAL VALUE CONVERSION TABLES                            
      PARAMETER     ( MAXCNT = 255 )                                    
      COMMON/CNTTAB/TMPTAB(0:MAXCNT),TMPVAR(0:MAXCNT),PRETAB(0:MAXCNT), 
     1              RFLTAB(0:MAXCNT),TAUTAB(0:MAXCNT),PRWTAB(0:MAXCNT), 
     2              OZNTAB(0:MAXCNT)                                    
C* ARRAYS TO HOLD ONE DECODED VARIABLE AS EQUAL-AREA MAP, AND SQUARE MAP
      REAL EQMAP(MAXBOX)                                              
      REAL SQMAP(MAXLON,MAXLAT)                                       
      CHARACTER*100 STARS/'*********************************************
     $*******************************************************'/         
C* NAME OF HDF FILE
      CHARACTER*33 HDFFILE/'HDF_LINK'/
C*--------------------------------------------------------------------*C
C* READ ANCILARY DATA FILE IF PLANNING TO USE CLDHGT FUNCTION         *C
C* OR TO CONVERT EQUAL AREA GRID TO SQUARE GRID                       *C
C*--------------------------------------------------------------------*C
      CALL RDANC(HDFFILE,IRC)                                            
      IF ( IRC .NE. 0 ) GOTO 900                                        
C*--------------------------------------------------------------------*C
C* LOOP OVER UTC IN FILE FOR POSITIONING                              *C
C*--------------------------------------------------------------------*C
      DO 600 IUTC=1,1
      CALL D1OPEN(HDFFILE,IUTC,IRC)
C*--------------------------------------------------------------------*C
C* LOOP OVER LATITUDES, CALL D1READ SUBROUTINE FOR EACH LAT BAND      *C
C*--------------------------------------------------------------------*C
      IBOX = 0                                                          
      IFULL = 0                                                         
      DO 500 LAT=1,MAXLAT                                               
         CALL D1READ(HDFFILE,IRC) 
C* CHECK FOR END OF FILE                                                
         IF ( IRC .LT. 0 ) THEN                                         
            GOTO 800                                                    
C* CHECK FOR READ ERROR                                                 
         ELSE IF ( IRC .GT. 0 ) THEN                                    
            GOTO 920                                                    
         END IF                                                         
C*--------------------------------------------------------------------*C
C* CONVERT TO PHYSICAL QUANTITIES IF DESIRED                          *C
C* D1PHYS WILL CONVERT INTEGER COUNTS IN IVAR INTO PHYSICALS IN RVAR  *C
C*--------------------------------------------------------------------*C
         CALL D1PHYS                                                    
C*--------------------------------------------------------------------*C
C* LOOP OVER LONGITUDES TO PROCESS BOXES WITHIN THIS LAT BAND         *C
C*--------------------------------------------------------------------*C
         DO 400 LON=1,NLON                                              
         IBOX = IBOX + 1                                                
C* CHECK FOR EMPTY BOX                                                  
         IF ( IVAR(6,LON) .EQ. 255 ) GOTO 400                           
         IFULL = IFULL + 1                                              
C*--------------------------------------------------------------------*C
C* DO WHATEVER YOU WANT TO DO WITH THE BOX HERE                       *C
C*--------------------------------------------------------------------*C

C* FOR THIS SAMPLE PROGRAM, JUST SELECT A FEW BOXES                     
         IF ( LAT .lt. 72 .AND. LON .LT. 144 ) THEN                       
            PRINT 310,STARS,LON,LAT                                     
  310       FORMAT(//,A100,//,1X,'PROCESSING EQUAL-AREA LON/LAT',2I10)  
C* PRINT CONTENTS OF IVAR - COUNTS                                      
            CALL PRINTI(LON)                                            
C* PRINT CONTENTS OF RVAR - PHYSICAL VALUES                             
            CALL PRINTR(LON)                                            
C* FIND CENTER LON/LAT OF EQUAL AREA BOX                                
            CALL CENTER(LON)                                            
C* FIND TOTAL IR RADIANCE                                               
            CALL TOTIR(LON)                                             
C* FIND TOTAL VIS RADIANCE                                              
            CALL TOTVIS(LON)                                            
C* FIND ACTUAL PRESSURE LAYER MID-POINTS                                
            CALL MIDPRS(LON)                                            
C* FIND CLOUD HEIGHT IN METERS                                          
            CALL CLDHGT(LON)                                            
         END IF                                                         
C* FOR THIS SAMPLE PROGRAM, SAVE CLOUD AMOUNT IN A GLOBAL EQUAL AREA MAP
         EQMAP(IBOX) = RVAR(12,LON)                                     
C*--------------------------------------------------------------------*C
C* END OF LON,LAT LOOPS                                               *C
C*--------------------------------------------------------------------*C
  400    CONTINUE                                                       
  500 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* END OF UTC LOOP                                                    *C
C*--------------------------------------------------------------------*C
  600 CONTINUE
C*--------------------------------------------------------------------*C
C* NORMAL END                                                         *C
C*--------------------------------------------------------------------*C
  800 CONTINUE                                                          
      PRINT 810,IFULL                                                   
  810 FORMAT(/1X,'NUMBER OF FULL BOXES:',I6)                            
C* CONVERT EQUAL AREA MAP TO SQUARE MAP FOR DISPLAY PURPOSES            
      CALL EQ2SQ(1,EQMAP,SQMAP)                                         
C* WRITE OUT THE SQUARE MAP                                             
      LUNOUT = 90                                                       
      OPEN(LUNOUT,ACCESS='DIRECT',RECL=576,FORM='UNFORMATTED')          
      DO 850 J=1,MAXLAT                                                 
         WRITE(LUNOUT,REC=J) (SQMAP(I,J),I=1,MAXLON)                    
  850 CONTINUE                                                          
      PRINT 860                                                         
  860 FORMAT(/1X,'NORMAL END OF PROGRAM')                               
      STOP 0                                                            
C*--------------------------------------------------------------------*C
C* ERROR ENDS                                                         *C
C*--------------------------------------------------------------------*C
  900 CONTINUE                                                          
      PRINT *,'ERROR:  RDANC  RC=',IRC                                  
      STOP 999                                                          
  910 CONTINUE                                                          
      PRINT *,'ERROR:  D1OPEN RC=',IRC                                  
      STOP 999                                                          
  920 CONTINUE                                                          
      PRINT *,'ERROR:  D1READ RC=',IRC                                  
      STOP 999                                                          
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:         D1OPEN                                               *C
C* DESCRIPTION:  OPEN THE HDF FILE AND FIND REQUESTED SDS             *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE D1OPEN(HDFFILE,IUTC,IRC) 
      CHARACTER*(*)   HDFFILE
C*      INTEGER         IREFS(8)/11,13,15,17,19,21,23,25/
      INTEGER         IREF, ITAG/720/
      INTEGER         ID_HDF
      INTEGER         HOPEN,DAGFIDL,DAGFID,DSRREF,DAGLAB,HCLOSE
      INTEGER         DALLIST
      INTEGER*4       DFTAG_SD, LISTSIZE, DFS_MAXLEN
      CHARACTER*255   FILEID, LABEL

      PARAMETER       (DFTAG_SD = 720)
      PARAMETER       (LISTSIZE = 55)
      PARAMETER       (DFS_MAXLEN = 132)

      CHARACTER*60    LABEL_LIST(DFS_MAXLEN*LISTSIZE)
      INTEGER         NUM_OF_LABELS, ST_POS, REF_LIST(DFS_MAXLEN)

      ST_POS = 1
      ID_HDF = HOPEN(HDFFILE,1,0)
      LENGTH = DAGFIDL(ID_HDF, 1)
      IRC = DAGFID(ID_HDF,FILEID,255,1)
      IRC = HCLOSE(ID_HDF)
      PRINT *,'FILE ID: ',FILEID(1:LENGTH)
      
      NUM_OF_LABELS = DALLIST(HDFFILE, DFTAG_SD, REF_LIST,
     $                        LABEL_LIST, 10, DFS_MAXLEN,
     $                        ST_POS)

C*      IREF = IREFS(IUTC)
      IREF = REF_LIST(IUTC)
      LENGTH = DAGLLEN(HDFFILE, ITAG, IREF)
      IRC = DAGLAB(HDFFILE, ITAG, IREF, LABEL, 255)
      IRC = DSRREF(HDFFILE, IREF)
      PRINT *,'FILE POSITIONED AT DATA LABEL: ',LABEL(1:10)
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:         D1READ                                               *C
C* DESCRIPTION:  READ AND UNPACK D1 DATA FOR A SINGLE LATITUDE BAND   *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE D1READ(HDFFILE,IRC)
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( NUMBOX = 100 )                                    
      PARAMETER     ( MAXLAT =  72 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( IUNDEF = 255 )                                    
      PARAMETER     ( RUNDEF = -1000.0 )                                
      COMMON /D1BUFS/ CHRBUF(MAXVAR)                             
      CHARACTER*1     CHRBUF                                            
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
      CHARACTER*(*)   HDFFILE
      INTEGER         ISTART(2)/1,1/
      INTEGER         ISIZE(2)/MAXVAR,1/
      INTEGER         ISTRIDE(2)
      INTEGER         DSRSLAB
      INTEGER         IDIM(2)
C*--------------------------------------------------------------------*C
C*  INITIALIZE THE OUTPUT ARRAYS (IVAR AND RVAR)                      *C
C*--------------------------------------------------------------------*C
      DO 100 LON=1,MAXLON                                               
      DO 100 I=1,MAXVAR                                                 
         IVAR(I,LON) = IUNDEF                                           
         RVAR(I,LON) = RUNDEF                                           
  100 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C*  LOOP OVER ALL BOXES FOR THIS LAT                                  *C
C*--------------------------------------------------------------------*C
      NLON = ICELLS(LAT)                                                
      NPREV = NCELLS(LAT)                                               
      IDIM(1) = MAXVAR
      IDIM(2) = 1
      DO 500 LON=1,NLON                                                 
         IBOX = NPREV + LON
         ISTART(2) = IBOX
         IRC = DSRSLAB(HDFFILE,ISTART,ISIZE,ISTRIDE,CHRBUF,IDIM)
         DO 490 I=1,MAXVAR
            IVAR(I,LON) = ICHAR(CHRBUF(I))
 490    CONTINUE
  500 CONTINUE
C*--------------------------------------------------------------------*C
C* END                                                                *C
C*--------------------------------------------------------------------*C
  900 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  EQ2SQ                                                       *C
C* DESCRIPTION:  CONVERT EQUAL AREA MAP TO SQUARE LAT/LON MAP FOR     *C
C*               DISPLAY PURPOSES                                     *C
C* ISHIFT = 1  =>  SHIFT LONGITUDES TO BE IN RANGE -180 TO +180       *C
C* ISHIFT ANY OTHER VALE =>  KEEP LONGITUDES IN RANGE 0 TO 360        *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE EQ2SQ(ISHIFT,EQMAP,SQMAP)                              
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( MAXLAT = 72 )                                     
      PARAMETER     ( MAXBOX = 6596 )                                   
      REAL EQMAP(MAXBOX)                                                
      REAL SQMAP(MAXLON,MAXLAT)                                         
C* EQUAL AREA GRID INFO                                                 
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
      COMMON /SQUARE/ LONLIM(2,MAXBOX)                                  
      IBOX = 0                                                          
      DO 200 LAT=1,MAXLAT                                               
      DO 200 LON=1,ICELLS(LAT)                                          
         IBOX = IBOX + 1                                                
         LONSQ1 = LONLIM(1,IBOX)                                        
         LONSQ2 = LONLIM(2,IBOX)                                        
         DO 100 ILON=LONSQ1,LONSQ2                                      
            LONSQ = ILON                                                
            IF ( ISHIFT .EQ. 1 ) THEN                                   
               LONSQ = LONSQ + MAXLON/2                                 
               IF ( LONSQ .GT. MAXLON ) LONSQ = LONSQ - MAXLON          
            END IF                                                      
            SQMAP(LONSQ,LAT) = EQMAP(IBOX)                              
  100    CONTINUE                                                       
  200 CONTINUE                                                          
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  D1PHYS                                                      *C
C* DESCRIPTION:  CONVERT DATA FOR ALL GRID BOXES WITHIN A SINGLE LAT  *C
C*               BAND FROM INTEGER COUNTS TO FLOATING POINT           *C
C*               PHYSICAL VALUES                                      *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE D1PHYS                                                 
      PARAMETER     ( PATHW = 6.292 )                                   
      PARAMETER     ( PATHI = 10.5 )                                  
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( NUMBOX = 100 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( IUNDEF = 255 )                                    
      PARAMETER     ( RUNDEF = -1000.0 )                                
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PARAMETER     ( MAXCNT = 255 )                                    
      COMMON/CNTTAB/TMPTAB(0:MAXCNT),TMPVAR(0:MAXCNT),PRETAB(0:MAXCNT), 
     1              RFLTAB(0:MAXCNT),TAUTAB(0:MAXCNT),PRWTAB(0:MAXCNT), 
     2              OZNTAB(0:MAXCNT)                                    
C*--------------------------------------------------------------------*C
C* BOX ID                                                             *C
C*--------------------------------------------------------------------*C
      DO 500 LON=1,NLON                                                 
         DO 90 I=1,7                                                    
            IF ( IVAR(I,LON) .EQ. IUNDEF ) THEN                         
               RVAR(I,LON) = RUNDEF                                     
            ELSE                                                        
               RVAR(I,LON) = IVAR(I,LON)                                
            ENDIF                                                       
   90    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* VIEWING GEOMETRY                                                   *C
C*--------------------------------------------------------------------*C
         DO 100 I=8,9                                                   
            IF ( IVAR(I,LON) .EQ. IUNDEF ) THEN                         
               RVAR(I,LON) = RUNDEF                                     
            ELSE                                                        
               RVAR(I,LON) = FLOAT(IVAR(I,LON)) / 100.0                 
            ENDIF                                                       
  100    CONTINUE                                                       
         IF ( IVAR(10,LON) .EQ. IUNDEF ) THEN                           
            RVAR(10,LON) = RUNDEF                                       
         ELSE                                                           
            RVAR(10,LON) = IVAR(10,LON)                                 
         ENDIF                                                          
C*--------------------------------------------------------------------*C
C* CLOUD AMOUNTS                                                      *C
C*--------------------------------------------------------------------*C
         IF ( IVAR(11,LON) .EQ. IUNDEF ) THEN                           
            RVAR(11,LON) = RUNDEF                                       
         ELSE                                                           
            RVAR(11,LON) = IVAR(11,LON)                                 
         ENDIF                                                          
         DO 101 I=12,19                                                 
            IF ( IVAR(I,LON) .EQ. IUNDEF ) THEN                         
               RVAR(I,LON) = RUNDEF                                     
            ELSE                                                        
               RVAR(I,LON) = FLOAT(IVAR(I,LON)) / RVAR(11,LON) * 100.0  
            ENDIF                                                       
  101    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* ALGORITHM STATS                                                    *C
C*--------------------------------------------------------------------*C
         IF ( IVAR(20,LON) .EQ. IUNDEF ) THEN                           
            RVAR(20,LON) = RUNDEF                                       
         ELSE                                                           
            RVAR(20,LON) = FLOAT(IVAR(20,LON)) / RVAR(11,LON) * 100.0   
         ENDIF                                                          
         DO 102 I=21,22                                                 
            IF ( IVAR(I,LON) .EQ. IUNDEF ) THEN                         
               RVAR(I,LON) = RUNDEF                                     
            ELSE                                                        
               RVAR(I,LON) = FLOAT(IVAR(I,LON)) / 100.0                 
            ENDIF                                                       
  102    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* CLOUD TYPE DISTRIBUTIONS                                           *C
C*--------------------------------------------------------------------*C
         DO 103 I=23,77                                                 
            IF ( IVAR(I,LON) .EQ. IUNDEF ) THEN                         
               RVAR(I,LON) = RUNDEF                                     
            ELSE                                                        
               RVAR(I,LON) = FLOAT(IVAR(I,LON)) / RVAR(11,LON) * 100.0  
            ENDIF                                                       
  103    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE AND VARIANCE OF THE CLOUD TOP PRESSURE             *C
C*--------------------------------------------------------------------*C
         DO 110 I=78,84                                                 
            RVAR(I,LON) = PRETAB(IVAR(I,LON))                           
  110    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE AND VARIANCE OF THE CLOUD TOP TEMPERATURE          *C
C*--------------------------------------------------------------------*C
         DO 120 I=85,90                                                 
            RVAR(I,LON) = TMPTAB(IVAR(I,LON))                           
  120    CONTINUE                                                       
         RVAR(91,LON) = TMPVAR(IVAR(91,LON))                            
C*--------------------------------------------------------------------*C
C* CONVERT TAU AND PATH                                               *C
C*--------------------------------------------------------------------*C
         DO 130 I=92,105                                                
            RVAR(I,LON) = TAUTAB(IVAR(I,LON))                           
  130    CONTINUE                                                       
         DO 135 I=99,105                                                
            IF ( RVAR(I,LON).GE.0.0 ) RVAR(I,LON) = RVAR(I,LON) * PATHW 
  135    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* CONVERT TC DISTRIBUTION                                            *C
C*--------------------------------------------------------------------*C
         DO 140 I=106,112                                               
            RVAR(I,LON) = TMPTAB(IVAR(I,LON))                           
  140    CONTINUE                                                       
C*--------------------------------------------------------------------*C
C* CONVERT CLOUD TYPES                                                *C
C*--------------------------------------------------------------------*C
      I = 113                                                           
      DO 150 ITYP=1,15                                                  
         RVAR(I,LON) = TMPTAB(IVAR(I,LON))                              
         RVAR(I+1,LON) = TAUTAB(IVAR(I+1,LON))                          
         I = I + 3                                                      
  150 CONTINUE                                                          
      RVAR(115,LON) = TAUTAB(IVAR(115,LON))                             
      IF ( RVAR(115,LON).GE.0.0 ) RVAR(115,LON) = RVAR(115,LON) * PATHW 
      RVAR(118,LON) = TAUTAB(IVAR(118,LON))                             
      IF ( RVAR(118,LON).GE.0.0 ) RVAR(118,LON) = RVAR(118,LON) * PATHW 
      RVAR(121,LON) = TAUTAB(IVAR(121,LON))                             
      IF ( RVAR(121,LON).GE.0.0 ) RVAR(121,LON) = RVAR(121,LON) * PATHW 
      RVAR(124,LON) = TAUTAB(IVAR(124,LON))                             
      IF ( RVAR(124,LON).GE.0.0 ) RVAR(124,LON) = RVAR(124,LON) * PATHI 
      RVAR(127,LON) = TAUTAB(IVAR(127,LON))                             
      IF ( RVAR(127,LON).GE.0.0 ) RVAR(127,LON) = RVAR(127,LON) * PATHI 
      RVAR(130,LON) = TAUTAB(IVAR(130,LON))                             
      IF ( RVAR(130,LON).GE.0.0 ) RVAR(130,LON) = RVAR(130,LON) * PATHI 
      RVAR(133,LON) = TAUTAB(IVAR(133,LON))                             
      IF ( RVAR(133,LON).GE.0.0 ) RVAR(133,LON) = RVAR(133,LON) * PATHW 
      RVAR(136,LON) = TAUTAB(IVAR(136,LON))                             
      IF ( RVAR(136,LON).GE.0.0 ) RVAR(136,LON) = RVAR(136,LON) * PATHW 
      RVAR(139,LON) = TAUTAB(IVAR(139,LON))                             
      IF ( RVAR(139,LON).GE.0.0 ) RVAR(139,LON) = RVAR(139,LON) * PATHW 
      RVAR(142,LON) = TAUTAB(IVAR(142,LON))                             
      IF ( RVAR(142,LON).GE.0.0 ) RVAR(142,LON) = RVAR(142,LON) * PATHI 
      RVAR(145,LON) = TAUTAB(IVAR(145,LON))                             
      IF ( RVAR(145,LON).GE.0.0 ) RVAR(145,LON) = RVAR(145,LON) * PATHI 
      RVAR(148,LON) = TAUTAB(IVAR(148,LON))                             
      IF ( RVAR(148,LON).GE.0.0 ) RVAR(148,LON) = RVAR(148,LON) * PATHI 
      RVAR(151,LON) = TAUTAB(IVAR(151,LON))                             
      IF ( RVAR(151,LON).GE.0.0 ) RVAR(151,LON) = RVAR(151,LON) * PATHI 
      RVAR(154,LON) = TAUTAB(IVAR(154,LON))                             
      IF ( RVAR(154,LON).GE.0.0 ) RVAR(154,LON) = RVAR(154,LON) * PATHI 
      RVAR(157,LON) = TAUTAB(IVAR(157,LON))                             
      IF ( RVAR(157,LON).GE.0.0 ) RVAR(157,LON) = RVAR(157,LON) * PATHI 
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE  OF THE SURFACE TEMPERATURE                        *C
C*--------------------------------------------------------------------*C
      DO 160 I=158,161                                                  
         RVAR(I,LON) = TMPTAB(IVAR(I,LON))                              
  160 CONTINUE                                                          
      RVAR(162,LON) = TMPVAR(IVAR(162,LON))                             
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE OF THE REFLECTANCE                                 *C
C*--------------------------------------------------------------------*C
      DO 170 I=163,168                                                  
         RVAR(I,LON) = RFLTAB(IVAR(I,LON))                              
  170 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE AND VARIANCE OF THE IR RADIANCE                    *C
C*--------------------------------------------------------------------*C
      RVAR(169,LON)  = TMPTAB(IVAR(169,LON))                            
      RVAR(170,LON)  = TMPVAR(IVAR(170,LON))                            
      RVAR(171,LON)  = TMPTAB(IVAR(171,LON))                            
      RVAR(172,LON)  = TMPTAB(IVAR(172,LON))                            
      RVAR(173,LON)  = TMPVAR(IVAR(173,LON))                            
      RVAR(174,LON)  = TMPTAB(IVAR(174,LON))                            
      RVAR(175,LON)  = TMPTAB(IVAR(175,LON))                            
C*--------------------------------------------------------------------*C
C* CONVERT AVERAGE AND VARIANCE OF THE VIS RADIANCE                   *C
C*--------------------------------------------------------------------*C
      DO 180 I=176,182                                                  
         RVAR(I,LON) = RFLTAB(IVAR(I,LON))                              
  180 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* FETCH THE ATMOSPHERE QC ( = 255 IF NO DATA )                       *C
C*--------------------------------------------------------------------*C
      RVAR(183,LON) = IVAR(183,LON)                                     
C*--------------------------------------------------------------------*C
C* CONVERT SURFACE PRESSURE AND TEMPERATURE                           *C
C*--------------------------------------------------------------------*C
      RVAR(184,LON) = PRETAB(IVAR(184,LON))                             
      RVAR(185,LON) = TMPTAB(IVAR(185,LON))                             
C*--------------------------------------------------------------------*C
C* CONVERT TEMPERATURE AT 7  STANDARD PRESSURE LEVELS                 *C
C* NOMINALLY = 900, 740, 620, 500, 375, 245, 115 MILLIBARS            *C
C* TOP AND BOTTOM LAYERS ARE REALLY DEFINED BY SURFACE AND TROPOPAUSE *C
C* PRESSURE.  TO CALCULATE THE REAL LAYER MID-POINTS FOR A PARTICULAR *C
C* GRID BOX, CALL SUBROUTINE MIDPRS                                   *C
C*--------------------------------------------------------------------*C
      DO 190 I = 186,192                                                
         RVAR(I,LON) = TMPTAB(IVAR(I,LON))                              
  190 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* CONVERT TROPOPAUSE PRESSURE AND TEMPERATURE                        *C
C*--------------------------------------------------------------------*C
      RVAR(193,LON) = PRETAB(IVAR(193,LON))                             
      RVAR(194,LON) = TMPTAB(IVAR(194,LON))                             
C*--------------------------------------------------------------------*C
C* CONVERT STRATOSPHERE TEMPERATURES AT 70 AND 30 MB                  *C
C*--------------------------------------------------------------------*C
      RVAR(195,LON) = TMPTAB(IVAR(195,LON))                             
      RVAR(196,LON) = TMPTAB(IVAR(196,LON))                             
C*--------------------------------------------------------------------*C
C* CONVERT PRECIPITABLE WATER FOR 5 LOWEST PRESSURE LAYERS            *C
C*--------------------------------------------------------------------*C
      DO 200 I=197,201                                                  
         RVAR(I,LON) = PRWTAB(IVAR(I,LON))                              
  200 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* CONVERT O3-OZONE ABUNDANCE                                         *C
C*--------------------------------------------------------------------*C
      RVAR(202,LON) = OZNTAB(IVAR(202,LON))                             
C*--------------------------------------------------------------------*C
C* END OF LON LOOP                                                    *C
C*--------------------------------------------------------------------*C
  500 CONTINUE                                                          
C*--------------------------------------------------------------------*C
C* NORMAL RETURN                                                      *C
C*--------------------------------------------------------------------*C
      PRINT 510,LAT                                                     
  510 FORMAT(1X,'D1PHYS:  LAT BAND',I4,' CONVERTED TO PHYSICAL VALUES') 
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  MIDPRS                                                      *C
C* DESCRIPTION:  CALCULATE ACTUAL MID-POINTS FOR THE 7 STANDARD       *C
C*               PRESSURE LAYERS FOR A PARTICULAR GRID BOX            *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE MIDPRS(LON)                                            
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( RUNDEF = -1000.0 )                                
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PARAMETER     ( NLAYER = 7 )                                      
      PARAMETER     ( NBOUND = NLAYER + 1 )                             
      REAL*4          PRSMID(NLAYER)                                    
      REAL*4          PBOUND(NBOUND)                                    
     $                   /1000.,800.,680.,560.,440.,310.,180.,30./      
      PSURF = RVAR(184,LON)                                             
      PTROP = RVAR(193,LON)                                             
      LYRSRF = 1                                                        
      LYRTRP = NLAYER                                                   
      DO 10 IBOUND=2,NBOUND                                             
         IF ( PSURF .LE. PBOUND(IBOUND) ) LYRSRF = IBOUND               
         IF ( PTROP .GT. PBOUND(IBOUND) ) LYRTRP = IBOUND-1             
   10 CONTINUE                                                          
      DO 20 ILAYER=1,NLAYER                                             
         IF ( ILAYER .LT. LYRSRF .OR. ILAYER .GT. LYRTRP ) THEN         
            PRSMID(ILAYER) = RUNDEF                                     
         ELSE IF ( ILAYER .EQ. LYRSRF ) THEN                            
            PRSMID(ILAYER) = ( PSURF + PBOUND(ILAYER+1) ) * 0.5         
         ELSE IF ( ILAYER .EQ. LYRTRP ) THEN                            
            PRSMID(ILAYER) = ( PTROP + PBOUND(ILAYER) ) * 0.5           
         ELSE                                                           
            PRSMID(ILAYER) = ( PBOUND(ILAYER) + PBOUND(ILAYER+1) ) * 0.5
         END IF                                                         
   20 CONTINUE                                                          
      PRINT 90,PRSMID                                                   
   90 FORMAT(/1X,'MIDPRS:  ACTUAL PRESSURE LAYER MID-POINTS (MB)',7F8.2)
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  RDANC                                                       *C
C* DESCRIPTION:  READ D1 ANCILARY DATA FILE (FILE DESCRIPTION 4)      *C
C*               AND SAVE TOPOGRAPHIC HEIGHT FOR CLOUD HEIGHT         *C
C*               CALCULATION                                          *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE RDANC(HDFFILE,IRC)                                      
      PARAMETER     ( MAXBOX = 6596 )
      PARAMETER     ( MAXLINE = 6598 )
      COMMON /HEIGHT/ ITOPOG(MAXBOX)                                    
      COMMON /SQUARE/ LONLIM(2,MAXBOX)                                  
      CHARACTER*80    ANCIL(MAXLINE)
      CHARACTER*(*)   HDFFILE
      INTEGER         HOPEN,HCLOSE,DAGFDSL,DAGFDS
      ID_HDF = HOPEN(HDFFILE,1,0)
      ISFIRST = 1
      DO 10 I=1,3
         LENGTH = DAGFDSL(ID_HDF,ISFIRST)
         IRC = DAGFDS(ID_HDF,ANCIL,1,ISFIRST)
         ISFIRST = 0
   10 CONTINUE
      LENGTH = DAGFDSL(ID_HDF,ISFIRST)
C FIND OUT IF THE ANCILLARY FILE IS PRESENT AND DECODE IT
C OR READ THE ANCILLARY VALUES FROM THE VDATA RECORD AND 
C STORE THE VALUES INTO THE ANCILLARY ARRAY.
      if(LENGTH .LT. 740) THEN
	CALL VDATASUB(ID_HDF)
      ELSE
        IRC = DAGFDS(ID_HDF,ANCIL,LENGTH,ISFIRST)
        DO 100 ILINE=3,MAXBOX+2         
           READ(UNIT=ANCIL(ILINE),FMT=110) IBOX,J,I,LONBEG,LONEND,        
     $         CENLAT,CENLON,IAREA,LANDFR,ITOPOG(IBOX),IVEG,
     $         ISAT1,ISAT2,ISAT3,ISAT4
           LONLIM(1,IBOX) = LONBEG                                        
           LONLIM(2,IBOX) = LONEND                                        
  100   CONTINUE                                                          
  110   FORMAT(5I4,2F9.2,I8,I6,I7,5I4)
      ENDIF
        IRC = HCLOSE(ID_HDF)
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  VDATASUB                                                    *C
C* DESCRIPTION:  READ D1 ANCILLARY DATA FILE IN THE VDATA FIELD       *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE VDATASUB(ID_HDF)
      PARAMETER     ( MAXBOX = 6596 )
      COMMON /HEIGHT/ ITOPOG(MAXBOX)                                    
      COMMON /SQUARE/ LONLIM(2,MAXBOX)                                  
      CHARACTER*7     STR
      CHARACTER*1     ACCESS
      INTEGER         VFSTART
      INTEGER         VSFGID,VSFATCH,VDATA_REF
      INTEGER         VFSTART
      INTEGER         VDATA_ID,STATUS
      INTEGER         IBOX(MAXBOX)
      REAL            DATA(6596)

	VDATA_REF=-1
	ACCESS = "w"
	STATUS = VFSTART(ID_HDF)
	VDATA_ID = VSFATCH(ID_HDF, VDATA_REF, ACCESS)
	VDATA_REF = VSFGID(ID_HDF,VDATA_REF)
	ACCESS = "r"
C THIS ROUTINE DECODES 15 FIELDS FROM THE HDF VDATA RECORD.
C THESE ARE THE ANCILLARY FILE FIELD NAMES.
C
C  GRD BOX
	STR="IBOX"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
        DO 10 I = 1 ,MAXBOX
          IBOX(I) = DATA(I)
10      CONTINUE
C  LAT IND 
	STR="J"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  LON IND
	STR="I"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  SQ1 IND
	STR="LONBEG"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
        DO 20 I = 1 ,MAXBOX
          LONLIM(1,IBOX(I)) = DATA(I)
20      CONTINUE
C  SQ2 IND
	STR="LONEND"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
        DO 30 I = 1 ,MAXBOX
          LONLIM(2,IBOX(I)) = DATA(I)
30      CONTINUE
C  LAT CENTER
	STR="CENLAT"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  LON CENTER
	STR="CENLON"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  BOX AREA
	STR="IAREA"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  PER LAND
	STR="LANDFR"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  TOPOG HEIGHT
	STR="ITOPOG"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
        DO 40 I = 1 ,MAXBOX
          ITOPOG(I) = DATA(I)
40      CONTINUE
C  VEG TYPE
	STR="IVEG"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  SAT 1ST
	STR="ISAT1"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  PREF 2ND
	STR="ISAT2"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  PREF 3RD
	STR="ISAT3"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
C  PREF 4TH
	STR="ISAT4"
	CALL NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF,DATA,
     &      STR,NRECS)
      END
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  NEXT_VD_FIELD                                               *C
C* DESCRIPTION:  OBTAIN THE NEXT FIELD IN THE VDATA                   *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE NEXT_VD_FIELD(ID_HDF,STATUS,VDATA_ID,ACCESS,VDATA_REF
     $                        ,DATA,STR,NRECS)
      CHARACTER*(*)     ACCESS
      CHARACTER*7     VCLASS,VDATA_NAME,STR
      CHARACTER*80    VFIELDS,FIELDS
      INTEGER VDATA_ID,VSFATCH,ID_HDF,VDATA_REF,VSFINQ,NRECS
      INTEGER INTERLACE,VSIZE,STATUS,NRECS
      REAL DATA(*)
      VDATA_ID = VSFATCH(ID_HDF, VDATA_REF, ACCESS)

      STATUS=VSFINQ(VDATA_ID,NRECS,INTERLACE,VFIELDS,VSIZE,VDATA_NAME)
      STATUS = VSFGCLS(VDATA_ID,VCLASS)

      FIELDS = STR

      STATUS = VSFSFLD(VDATA_ID,FIELDS)
      STATUS = VSFREAD(VDATA_ID,DATA,NRECS,INTERLACE)
      STATUS = VSFDTCH(VDATA_ID)
      END
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  PRINTI                                                      *C
C* DESCRIPTION:  PRINT COUNT VALUES FOR THE BOX                       *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE PRINTI(LON)                                            
      PARAMETER ( MAXVAR = 202 )                                        
      PARAMETER ( MAXLON = 144 )                                        
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PRINT 140                                                         
  140 FORMAT(/1X,'PRINTI:  COUNT VALUES FOR ALL VARIABLES')             
      PRINT 145,(K,K=1,10)                                              
  145 FORMAT(1X,18X,10I8)                                               
      DO 150 I=1,MAXVAR,10                                              
         IEND = I + 9                                                   
         IF ( IEND .GT. MAXVAR ) IEND = MAXVAR                          
         PRINT 155,I,IEND,(IVAR(K,LON),K=I,IEND)                        
  150 CONTINUE                                                          
  155 FORMAT(1X,'VARIABLE (',I3.3,'-',I3.3,')',10I8)                    
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  PRINTR                                                      *C
C* DESCRIPTION:  PRINT PHYSICAL VALUES FOR THE BOX                    *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE PRINTR(LON)                                            
      PARAMETER ( MAXVAR = 202 )                                        
      PARAMETER ( MAXLON = 144 )                                        
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PRINT 140                                                         
  140 FORMAT(/1X,'PRINTR:  PHYSICAL VALUES FOR ALL VARIABLES')          
      PRINT 145,(K,K=1,10)                                              
  145 FORMAT(1X,18X,10I8)                                               
      DO 150 I=1,MAXVAR,10                                              
         IEND = I + 9                                                   
         IF ( IEND .GT. MAXVAR ) IEND = MAXVAR                          
         PRINT 155,I,IEND,(RVAR(K,LON),K=I,IEND)                        
  150 CONTINUE                                                          
  155 FORMAT(1X,'VARIABLE (',I3.3,'-',I3.3,')',10F8.2)                  
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  CENTER                                                      *C
C* DESCRIPTION:  CALCULATE CENTER LON/LAT OF BOX (EQUAL-AREA GRID)    *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE CENTER(LON)                                            
      PARAMETER     ( DLAT = 2.5 )                                      
      PARAMETER     ( MAXLAT = 72 )                                     
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      DLON = 360.0 / NLON                                               
      CENLAT = ( LAT - 1 ) * DLAT + DLAT/2.0 - 90.0                     
      CENLON = ( LON - 1 ) * DLON + DLON/2.0                            
      PRINT 300,CENLON,CENLAT                                           
  300 FORMAT(/1X,'CENTER:  CENTER LON/LAT',2F8.2)                       
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  TOTIR                                                       *C
C* DESCRIPTION:  CALCULATE TOTAL IR RADIANCE FROM CLOUD/CLEAR RADIANCE*C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE TOTIR(LON)                                             
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PARAMETER     ( IUNDEF = 255 )                                    
      PARAMETER     ( MAXCNT = 255 )                                    
      COMMON/CNTTAB/TMPTAB(0:MAXCNT),TMPVAR(0:MAXCNT),PRETAB(0:MAXCNT), 
     1              RFLTAB(0:MAXCNT),TAUTAB(0:MAXCNT),PRWTAB(0:MAXCNT), 
     2              OZNTAB(0:MAXCNT)                                    
                                                                        
      PRINT 100                                                         
  100 FORMAT(/1X,'TOTIR:   IR RADIANCES')                               
      NTOT = IVAR(11,LON)                                               
                                                                        
      NIRCLD = IVAR(13,LON)                                             
      NIRCLR = NTOT - NIRCLD                                            
      ICLD = IVAR(169,LON)                                              
      ICLR = IVAR(172,LON)                                              
      ITOT = NINT(FLOAT(NIRCLD*ICLD+NIRCLR*ICLR)/FLOAT(NTOT))           
      PRINT 210,NIRCLD,NIRCLR,NTOT,                                     
     $          TMPTAB(ICLD),TMPTAB(ICLR),TMPTAB(ITOT)                  
  210 FORMAT(10X,'USING IR CLOUD/CLEAR CATEGORIES',                     
     $      /16X,'NUMBER OF PIXELS CLOUD/CLEAR/TOTAL      ',3I10,       
     $      /16X,'RADIANCE (K) CLOUD/CLEAR/TOTAL          ',3F10.2)     
                                                                        
      NVSCLD = IVAR(12,LON) - IVAR(16,LON)                              
      NVSCLR = NTOT - NVSCLD                                            
      ICLD = IVAR(171,LON)                                              
      ICLR = IVAR(174,LON)                                              
      IF ( IVAR(6,LON) .GT. 100 ) THEN                                  
         ITOT = IUNDEF                                                  
      ELSE                                                              
         ITOT = NINT(FLOAT(NVSCLD*ICLD+NVSCLR*ICLR)/FLOAT(NTOT))        
      END IF                                                            
      PRINT 220,NVSCLD,NVSCLR,NTOT,                                     
     $          TMPTAB(ICLD),TMPTAB(ICLR),TMPTAB(ITOT)                  
  220 FORMAT(10X,'USING VIS/IR CLOUD/CLEAR CATEGORIES',                 
     $      /16X,'NUMBER OF PIXELS CLOUD/CLEAR/TOTAL      ',3I10,       
     $      /16X,'RADIANCE (K) CLOUD/CLEAR/TOTAL          ',3F10.2)     
                                                                        
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  TOTVIS                                                      *C
C* DESCRIPTION:  CALCULATE TOTAL VS RADIANCE FROM CLOUD/CLEAR RADIANCE*C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE TOTVIS(LON)                                            
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
      PARAMETER     ( IUNDEF = 255 )                                    
      PARAMETER     ( MAXCNT = 255 )                                    
      COMMON/CNTTAB/TMPTAB(0:MAXCNT),TMPVAR(0:MAXCNT),PRETAB(0:MAXCNT), 
     1              RFLTAB(0:MAXCNT),TAUTAB(0:MAXCNT),PRWTAB(0:MAXCNT), 
     2              OZNTAB(0:MAXCNT)                                    
                                                                        
C*    PRINT 100,LON                                                     
  100 FORMAT(/1X,'TOTVIS:  VIS RADIANCES')                              
      NTOT = IVAR(11,LON)                                               
                                                                        
      NIRCLD = IVAR(13,LON)                                             
      NIRCLR = NTOT - NIRCLD                                            
      ICLD = IVAR(178,LON)                                              
      ICLR = IVAR(181,LON)                                              
      IF ( IVAR(6,LON) .GT. 100 ) THEN                                  
         ITOT = IUNDEF                                                  
      ELSE                                                              
         ITOT = NINT(FLOAT(NIRCLD*ICLD+NIRCLR*ICLR)/FLOAT(NTOT))        
      END IF                                                            
      PRINT 210,NIRCLD,NIRCLR,NTOT,                                     
     $          RFLTAB(ICLD),RFLTAB(ICLR),RFLTAB(ITOT)                  
  210 FORMAT(10X,'USING IR CLOUD/CLEAR CATEGORIES',                     
     $      /16X,'NUMBER OF PIXELS CLOUD/CLEAR/TOTAL      ',3I10,       
     $      /16X,'RADIANCE (REFLECTANCE) CLOUD/CLEAR/TOTAL',3F10.2)     
                                                                        
      NVSCLD = IVAR(12,LON) - IVAR(16,LON)                              
      NVSCLR = NTOT - NVSCLD                                            
      ICLD = IVAR(176,LON)                                              
      ICLR = IVAR(179,LON)                                              
      IF ( IVAR(6,LON) .GT. 100 ) THEN                                  
         ITOT = IUNDEF                                                  
      ELSE                                                              
         ITOT = NINT(FLOAT(NVSCLD*ICLD+NVSCLR*ICLR)/FLOAT(NTOT))        
      END IF                                                            
      PRINT 220,NVSCLD,NVSCLR,NTOT,                                     
     $          RFLTAB(ICLD),RFLTAB(ICLR),RFLTAB(ITOT)                  
  220 FORMAT(10X,'USING VIS/IR CLOUD/CLEAR CATEGORIES',                 
     $      /16X,'NUMBER OF PIXELS CLOUD/CLEAR/TOTAL      ',3I10,       
     $      /16X,'RADIANCE (REFLECTANCE) CLOUD/CLEAR/TOTAL',3F10.2)     
                                                                        
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:  CLDHGT                                                      *C
C* DESCRIPTION:  CALCULATE CLOUD TOP HEIGHT IN METERS, BY USING       *C
C*               CLOUD TOP PRESSURE AND OBSERVED ATMOSPHERIC          *C
C*               TEMPERATURE AND HUMIDITY PROFILES                    *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      SUBROUTINE CLDHGT(LON)                                            
      PARAMETER     ( MAXVAR = 202 )                                    
      PARAMETER     ( NUMBOX = 100 )                                    
      PARAMETER     ( MAXLON = 144 )                                    
      PARAMETER     ( MAXLAT = 72 )                                     
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
C* D1 DATA ARRAYS                                                       
      COMMON /D1DATA/ LAT,NLON,IVAR(MAXVAR,MAXLON),RVAR(MAXVAR,MAXLON)  
C* TOPOGRAPHIC HEIGHT (READ FROM ANCILARY DATA FILE)                    
      PARAMETER     ( MAXBOX = 6596 )                                   
      COMMON /HEIGHT/ ITOPOG(MAXBOX)                                    
C* CONSTANTS                                                            
      PARAMETER     ( CONSTG = 9.806 )                                  
      PARAMETER     ( CONSTR = 8.3144 )                                 
      PARAMETER     ( CONSTM = 0.028966 )                               
C* PRESSURE LEVELS                                                      
      PARAMETER     ( NLEVEL = 7 )                                      
      PARAMETER     ( NBOUND = NLEVEL + 1 )                             
      REAL PLIM(NBOUND)/1000.,800.,680.,560.,440.,310.,180.,30./        
      REAL P(NBOUND)                                                    
C*                                                                      
      REAL LNP(NBOUND)                                                  
      REAL Z(NBOUND)                                                    
      REAL T(NLEVEL)                                                    
      REAL PW(NLEVEL)                                                   
                                                                        
C* FIND EQUAL AREA BOX NUMBER                                           
      IBOX = NCELLS(LAT) + LON                                          
      PC = RVAR(78,LON)                                                 
      IF ( PC .LT. 0.0 ) THEN                                           
         HGT = -1000.0                                                  
      ELSE                                                              
                                                                        
C* INIT TO STANDARD BOUNDARIES                                          
      DO 2 I=1,NBOUND                                                   
         P(I) = PLIM(I)                                                 
    2 CONTINUE                                                          
                                                                        
C* FIND LOWER AND UPPER BOUNDARIES FOR SURFACE AND TROPOPAUSE           
      PSURF = RVAR(184,LON)                                             
      PTROP = RVAR(193,LON)                                             
      IUPBND = NBOUND                                                   
      ILWBND = 1                                                        
      DO 5 I=NBOUND,1,-1                                                
         IF ( PTROP .GT. P(I) ) IUPBND  = I - 1                         
         IF ( PSURF .GT. P(I) ) ILWBND  = I                             
    5 CONTINUE                                                          
                                                                        
C* GET D1 TEMPERATURE AND PRECIPITABLE WATER PROFILES                   
      TSURF = RVAR(185,LON)                                             
      TTROP = RVAR(194,LON)                                             
      DO 10 I=1,NLEVEL                                                  
         T(I) = RVAR(185+I,LON)                                         
   10 CONTINUE                                                          
      DO 20 I=1,5                                                       
         PW(I) = RVAR(196+I,LON)                                        
   20 CONTINUE                                                          
      PW(6) = 0.0                                                       
      PW(7) = 0.0                                                       
                                                                        
C* SET LN(P)                                                            
      DO 30 I=ILWBND,IUPBND                                             
         LNP(I) = ALOG(P(I))                                            
   30 CONTINUE                                                          
      IF ( ILWBND.GT.1 .AND. PSURF-P(ILWBND).GT.0.0 ) THEN              
         ILWBND = ILWBND - 1                                            
         LNP(ILWBND) = ALOG(PSURF)                                      
         P(ILWBND) = PSURF                                              
      END IF                                                            
      IF ( IUPBND.LT.NBOUND .AND. P(IUPBND)-PTROP.GT.0.0 ) THEN         
         IUPBND = IUPBND + 1                                            
         LNP(IUPBND) = ALOG(PTROP)                                      
         P(IUPBND) = PTROP                                              
      END IF                                                            
                                                                        
C* Z(1) IS TOPOGRAPHIC HEIGHT IN METERS FROM ANCILLARY DATA             
      Z(ILWBND) = ITOPOG(IBOX)                                          
                                                                        
C* LOOP THROUGH BOUNDARIES 2 THROUGH TOP TO CALCULATE Z FOR EACH        
      DO 100 I=ILWBND+1,IUPBND                                          
         Q = CONSTG / ( P(I-1) - P(I) ) * PW(I-1)                       
         TSTAR = (1.0 + 0.6077 * Q ) * T(I-1)                           
         Z(I) = Z(I-1)+(CONSTR*TSTAR)/(CONSTM*CONSTG)*ALOG(P(I-1)/P(I)) 
  100 CONTINUE                                                          
                                                                        
C* LOOK UP CLOUD TOP PRESSURE ON LNP(), FIND CORRESPONDING Z()          
      ALNPC = ALOG(PC)                                                  
      IBOT = IUPBND - 1                                                 
      ITOP = IUPBND                                                     
      DO 200 I=ILWBND+1,IUPBND                                          
         IF ( ALNPC .GT. LNP(I) ) THEN                                  
            IBOT = I-1                                                  
            ITOP = I                                                    
            GOTO 210                                                    
         END IF                                                         
  200 CONTINUE                                                          
  210 CONTINUE                                                          
      RATIO = (ALNPC-LNP(ITOP)) / (LNP(IBOT)-LNP(ITOP))                 
      HGT = Z(ITOP) + RATIO * (Z(IBOT)-Z(ITOP))                         
                                                                        
      END IF                                                            
                                                                        
      PRINT 340, PC,HGT                                                 
  340 FORMAT(/1X,'CLDHGT:  CLOUD TOP PRESSURE (MB)',F10.0,              
     $       10X,'CLOUD TOP HEIGHT (M)',F10.0)                          
                                                                        
      RETURN                                                            
      END                                                               
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C* NAME:         BLOCK DATA                                           *C
C* DESCRIPTION:  INITIALIZE CONVERSION TABLES AND EQUAL-AREA GRID     *C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
      BLOCK DATA                                                        
C*--------------------------------------------------------------------*C
      PARAMETER     ( MAXCNT = 255 )                                    
      COMMON/CNTTAB/TMPTAB(0:MAXCNT),TMPVAR(0:MAXCNT),PRETAB(0:MAXCNT), 
     1              RFLTAB(0:MAXCNT),TAUTAB(0:MAXCNT),PRWTAB(0:MAXCNT), 
     2              OZNTAB(0:MAXCNT)                                    
C*--------------------------------------------------------------------*C
      PARAMETER     ( MAXLAT = 72 )                                     
      COMMON /D1GRID/ NCELLS(MAXLAT),ICELLS(MAXLAT)                     
C*--------------------------------------------------------------------*C
      DATA (TMPTAB(I),I=0,127) /                                        
     &  -100.000,165.000,169.000,172.000,175.000,177.800,180.500,       
     &   183.000,185.500,187.800,190.000,192.000,194.000,195.700,       
     &   197.500,199.200,201.000,202.700,204.500,206.200,208.000,       
     &   209.700,211.500,212.800,214.100,215.400,216.700,217.900,       
     &   219.200,220.500,221.800,223.100,224.400,225.400,226.500,       
     &   227.500,228.600,229.600,230.600,231.700,232.700,233.800,       
     &   234.800,235.700,236.600,237.500,238.400,239.200,240.100,       
     &   241.000,241.900,242.800,243.700,244.500,245.300,246.100,       
     &   246.900,247.700,248.500,249.300,250.100,250.900,251.700,       
     &   252.400,253.100,253.900,254.600,255.300,256.000,256.700,       
     &   257.500,258.200,258.900,259.500,260.200,260.800,261.500,       
     &   262.100,262.800,263.400,264.100,264.700,265.400,266.000,       
     &   266.600,267.200,267.800,268.400,269.100,269.700,270.300,       
     &   270.900,271.500,272.100,272.700,273.200,273.800,274.400,       
     &   275.000,275.600,276.100,276.700,277.300,277.800,278.400,       
     &   278.900,279.500,280.000,280.500,281.100,281.600,282.200,       
     &   282.700,283.200,283.700,284.200,284.700,285.200,285.800,       
     &   286.300,286.800,287.300,287.800,288.300,288.800,289.300,       
     &   289.800,290.200/                                               
      DATA (TMPTAB(I),I=128,255) /                                      
     &                   290.700,291.200,291.700,292.200,292.700,       
     &   293.200,293.600,294.100,294.600,295.000,295.500,296.000,       
     &   296.500,296.900,297.400,297.800,298.300,298.700,299.200,       
     &   299.600,300.100,300.500,301.000,301.400,301.900,302.300,       
     &   302.800,303.200,303.600,304.000,304.500,304.900,305.300,       
     &   305.800,306.200,306.600,307.000,307.500,307.900,308.300,       
     &   308.700,309.100,309.600,310.000,310.400,310.800,311.200,       
     &   311.600,312.000,312.400,312.900,313.300,313.700,314.100,       
     &   314.500,314.900,315.300,315.700,316.100,316.400,316.800,       
     &   317.200,317.600,318.000,318.400,318.800,319.200,319.500,       
     &   319.900,320.300,320.700,321.100,321.400,321.800,322.200,       
     &   322.600,323.000,323.300,323.700,324.100,324.500,324.900,       
     &   325.200,325.600,326.000,326.400,326.700,327.100,327.400,       
     &   327.800,328.200,328.500,328.900,329.200,329.600,329.900,       
     &   330.300,330.600,331.000,331.300,331.700,332.000,332.400,       
     &   332.700,333.100,333.400,333.800,334.100,334.500,334.800,       
     &   335.200,335.500,335.900,336.200,336.600,336.900,337.300,       
     &   337.600,338.000,338.300,338.600,339.000,339.300,339.700,       
     &   340.000,345.000,-200.000,-1000.000/                            
      DATA (TMPVAR(I),I=0,127) /                                        
     & -100.000, 0.075, 0.300,0.600,0.900,1.200,1.500,1.800,2.100,2.400,
     &    2.700, 3.000, 3.300,3.600,3.900,4.200,4.500,4.800,5.100,5.400,
     &    5.700, 6.000, 6.300,6.600,6.900,7.200,7.500,7.800,8.100,8.400,
     &    8.700, 9.000, 9.300, 9.600, 9.900,10.200,10.500,10.800,11.100,
     &   11.400,11.700,12.000,12.300,12.600,12.900,13.200,13.500,13.800,
     &   14.100,14.400,14.700,15.000,15.300,15.600,15.900,16.200,16.500,
     &   16.800,17.100,17.400,17.700,18.000,18.300,18.600,18.900,19.200,
     &   19.500,19.800,20.100,20.400,20.700,21.000,21.300,21.600,21.900,
     &   22.200,22.500,22.800,23.100,23.400,23.700,24.000,24.300,24.600,
     &   24.900,25.200,25.500,25.800,26.100,26.400,26.700,27.000,27.300,
     &   27.600,27.900,28.200,28.500,28.800,29.100,29.400,29.700,30.000,
     &   30.300,30.600,30.900,31.200,31.500,31.800,32.100,32.400,32.700,
     &   33.000,33.300,33.600,33.900,34.200,34.500,34.800,35.100,35.400,
     &   35.700,36.000,36.300,36.600,36.900,37.200,37.500,37.800/       
      DATA (TMPVAR(I),I=128,255) /                                      
     &                                                           38.100,
     &   38.400,38.700,39.000,39.300,39.600,39.900,40.200,40.500,40.800,
     &   41.100,41.400,41.700,42.000,42.300,42.600,42.900,43.200,43.500,
     &   43.800,44.100,44.400,44.700,45.000,45.300,45.600,45.900,46.200,
     &   46.500,46.800,47.100,47.400,47.700,48.000,48.300,48.600,48.900,
     &   49.200,49.500,49.800,50.100,50.400,50.700,51.000,51.300,51.600,
     &   51.900,52.200,52.500,52.800,53.100,53.400,53.700,54.000,54.300,
     &   54.600,54.900,55.200,55.500,55.800,56.100,56.400,56.700,57.000,
     &   57.300,57.600,57.900,58.200,58.500,58.800,59.100,59.400,59.700,
     &   60.000,60.300,60.600,60.900,61.200,61.500,61.800,62.100,62.400,
     &   62.700,63.000,63.300,63.600,63.900,64.200,64.500,64.800,65.100,
     &   65.400,65.700,66.000,66.300,66.600,66.900,67.200,67.500,67.800,
     &   68.100,68.400,68.700,69.000,69.300,69.600,69.900,70.200,70.500,
     &   70.800,71.100,71.400,71.700,72.000,72.300,72.600,72.900,73.200,
     &   73.500,73.800,74.100,74.400,74.700,75.400,78.000,85.000,       
     &   -200.000,-1000.000/                                            
      DATA (PRETAB(I),I=0,127) /                                        
     &  -100.00,  1.00, 5.00, 10.00,15.00,20.00,25.00,30.00,35.00,40.00,
     &    45.00, 50.00, 55.00,60.00,65.00,70.00,75.00,80.00,85.00,90.00,
     &    95.00,100.00,105.00,110.00,115.00,120.00,125.00,130.00,135.00,
     &   140.00,145.00,150.00,155.00,160.00,165.00,170.00,175.00,180.00,
     &   185.00,190.00,195.00,200.00,205.00,210.00,215.00,220.00,225.00,
     &   230.00,235.00,240.00,245.00,250.00,255.00,260.00,265.00,270.00,
     &   275.00,280.00,285.00,290.00,295.00,300.00,305.00,310.00,315.00,
     &   320.00,325.00,330.00,335.00,340.00,345.00,350.00,355.00,360.00,
     &   365.00,370.00,375.00,380.00,385.00,390.00,395.00,400.00,405.00,
     &   410.00,415.00,420.00,425.00,430.00,435.00,440.00,445.00,450.00,
     &   455.00,460.00,465.00,470.00,475.00,480.00,485.00,490.00,495.00,
     &   500.00,505.00,510.00,515.00,520.00,525.00,530.00,535.00,540.00,
     &   545.00,550.00,555.00,560.00,565.00,570.00,575.00,580.00,585.00,
     &   590.00,595.00,600.00,605.00,610.00,615.00,620.00,625.00,630.00/
      DATA (PRETAB(I),I=128,255) /                                      
     &   635.00,640.00,645.00,650.00,655.00,660.00,665.00,670.00,675.00,
     &   680.00,685.00,690.00,695.00,700.00,705.00,710.00,715.00,720.00,
     &   725.00,730.00,735.00,740.00,745.00,750.00,755.00,760.00,765.00,
     &   770.00,775.00,780.00,785.00,790.00,795.00,800.00,805.00,810.00,
     &   815.00,820.00,825.00,830.00,835.00,840.00,845.00,850.00,855.00,
     &   860.00,865.00,870.00,875.00,880.00,885.00,890.00,895.00,900.00,
     &   905.00,910.00,915.00,920.00,925.00,930.00,935.00,940.00,945.00,
     &   950.00,955.00,960.00,965.00,970.00,975.00,980.00,985.00,990.00,
     &   995.00,1000.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-200.00,-200.00,       
     &   -200.00,-200.00,-200.00,-200.00,-200.00,-1000.00/              
      DATA (RFLTAB(I),I=0,127) /                                        
     &  -100.000,0.000,0.008,0.012,0.016,0.020,0.024,0.028,0.032,0.036, 
     &     0.040,0.044,0.048,0.052,0.056,0.060,0.064,0.068,0.072,0.076, 
     &     0.080,0.084,0.088,0.092,0.096,0.100,0.104,0.108,0.112,0.116, 
     &     0.120,0.124,0.128,0.132,0.136,0.140,0.144,0.148,0.152,0.156, 
     &     0.160,0.164,0.168,0.172,0.176,0.180,0.184,0.188,0.192,0.196, 
     &     0.200,0.204,0.208,0.212,0.216,0.220,0.224,0.228,0.232,0.236, 
     &     0.240,0.244,0.248,0.252,0.256,0.260,0.264,0.268,0.272,0.276, 
     &     0.280,0.284,0.288,0.292,0.296,0.300,0.304,0.308,0.312,0.316, 
     &     0.320,0.324,0.328,0.332,0.336,0.340,0.344,0.348,0.352,0.356, 
     &     0.360,0.364,0.368,0.372,0.376,0.380,0.384,0.388,0.392,0.396, 
     &     0.400,0.404,0.408,0.412,0.416,0.420,0.424,0.428,0.432,0.436, 
     &     0.440,0.444,0.448,0.452,0.456,0.460,0.464,0.468,0.472,0.476, 
     &     0.480,0.484,0.488,0.492,0.496,0.500,0.504,0.508/             
      DATA (RFLTAB(I),I=128,255) /                                      
     &                                                     0.512,0.516, 
     &     0.520,0.524,0.528,0.532,0.536,0.540,0.544,0.548,0.552,0.556, 
     &     0.560,0.564,0.568,0.572,0.576,0.580,0.584,0.588,0.592,0.596, 
     &     0.600,0.604,0.608,0.612,0.616,0.620,0.624,0.628,0.632,0.636, 
     &     0.640,0.644,0.648,0.652,0.656,0.660,0.664,0.668,0.672,0.676, 
     &     0.680,0.684,0.688,0.692,0.696,0.700,0.704,0.708,0.712,0.716, 
     &     0.720,0.724,0.728,0.732,0.736,0.740,0.744,0.748,0.752,0.756, 
     &     0.760,0.764,0.768,0.772,0.776,0.780,0.784,0.788,0.792,0.796, 
     &     0.800,0.804,0.808,0.812,0.816,0.820,0.824,0.828,0.832,0.836, 
     &     0.840,0.844,0.848,0.852,0.856,0.860,0.864,0.868,0.872,0.876, 
     &     0.880,0.884,0.888,0.892,0.896,0.900,0.904,0.908,0.912,0.916, 
     &     0.920,0.924,0.928,0.932,0.936,0.940,0.944,0.948,0.952,0.956, 
     &     0.960,0.964,0.968,0.972,0.976,0.980,0.984,0.988,0.992,1.000, 
     &     1.016,1.040,1.072,1.108,-200.000,-1000.000/                  
      DATA (TAUTAB(I),I=0,127) /                                        
     &  -100.000,0.020,0.040,0.060,0.090,0.110,0.140,0.160,0.190,0.220, 
     &     0.240,0.270,0.300,0.330,0.370,0.400,0.430,0.460,0.500,0.530, 
     &     0.570,0.600,0.640,0.680,0.720,0.750,0.790,0.830,0.870,0.920, 
     &     0.960,1.000,1.040,1.090,1.130,1.180,1.220,1.270,1.320,1.370, 
     &     1.420,1.470,1.520,1.570,1.620,1.670,1.730,1.780,1.830,1.890, 
     &     1.950,2.000,2.060,2.120,2.180,2.240,2.300,2.360,2.430,2.490, 
     &     2.550,2.620,2.690,2.750,2.820,2.890,2.960,3.030,3.100,3.180, 
     &     3.250,3.320,3.400,3.480,3.550,3.630,3.710,3.790,3.880,3.960, 
     &     4.040,4.130,4.220,4.300,4.390,4.480,4.570,4.670,4.760,4.850, 
     &     4.950,5.050,5.150,5.250,5.350,5.450,5.560,5.660,5.770,5.880, 
     &     5.990,6.110,6.220,6.340,6.450,6.570,6.690,6.820,6.940,7.070, 
     &     7.190,7.330,7.460,7.590,7.730,7.870,8.010,8.150,8.300,8.440, 
     &     8.590,8.740,8.900,9.060,9.220,9.380,9.540,9.710/             
      DATA (TAUTAB(I),I=128,255) /                                      
     &                                                     9.880,10.050,
     &   10.230,10.410,10.590,10.780,10.970,11.160,11.350,11.550,11.760,
     &   11.960,12.170,12.390,12.600,12.830,13.050,13.280,13.520,13.760,
     &   14.000,14.250,14.510,14.770,15.030,15.300,15.580,15.860,16.150,
     &   16.440,16.740,17.050,17.360,17.690,18.020,18.350,18.700,19.050,
     &   19.410,19.780,20.160,20.540,20.940,21.350,21.770,22.200,22.630,
     &   23.080,23.550,24.030,24.520,25.020,25.540,26.070,26.620,27.190,
     &   27.770,28.370,28.990,29.630,30.290,30.970,31.670,32.400,33.160,
     &   33.940,34.740,35.580,36.450,37.350,38.290,39.260,40.260,41.320,
     &   42.420,43.570,44.760,46.000,47.310,48.680,50.110,51.600,53.170,
     &   54.840,56.590,58.430,60.360,62.400,64.590,66.900,69.360,71.960,
     &   74.720,77.730,80.940,84.380,88.060,92.020,96.400,101.010,      
     &   105.510,109.870,114.330,119.590,125.920,133.660,143.120,       
     &   154.650, 169.560, 187.490, 207.200, 228.130, 250.440, 282.780, 
     &   323.920, 378.650,-200.000,-200.000,-200.000,-200.000,-200.000, 
     &  -200.000,-200.000,-200.000,-200.000,-200.000,-200.000,          
     &  -1000.000/                                                      
      DATA (PRWTAB(I),I=0,127) /                                        
     &  -100.000,0.000,0.030,0.060,0.090,0.120,0.150,0.180,0.210,0.240, 
     &   0.270,0.300,0.330,0.360,0.390,0.420,0.450,0.480,0.510,0.540,   
     &   0.570,0.600,0.630,0.660,0.690,0.720,0.750,0.780,0.810,0.840,   
     &   0.870,0.900,0.930,0.960,0.990,1.020,1.050,1.080,1.110,1.140,   
     &   1.170,1.200,1.230,1.260,1.290,1.320,1.350,1.380,1.410,1.440,   
     &   1.470,1.500,1.530,1.560,1.590,1.620,1.650,1.680,1.710,1.740,   
     &   1.770,1.800,1.830,1.860,1.890,1.920,1.950,1.980,2.010,2.040,   
     &   2.070,2.100,2.130,2.160,2.190,2.220,2.250,2.280,2.310,2.340,   
     &   2.370,2.400,2.430,2.460,2.490,2.520,2.550,2.580,2.610,2.640,   
     &   2.670,2.700,2.730,2.760,2.790,2.820,2.850,2.880,2.910,2.940,   
     &   2.970,3.000,3.030,3.060,3.090,3.120,3.150,3.180,3.210,3.240,   
     &   3.270,3.300,3.330,3.360,3.390,3.420,3.450,3.480,3.510,3.540,   
     &   3.570,3.600,3.630,3.660,3.690,3.720,3.750,3.780/               
      DATA (PRWTAB(I),I=128,255) /                                      
     &                                                   3.810,3.840,   
     &   3.870,3.900,3.930,3.960,3.990,4.020,4.050,4.080,4.110,4.140,   
     &   4.170,4.200,4.230,4.260,4.290,4.320,4.350,4.380,4.410,4.440,   
     &   4.470,4.500,4.530,4.560,4.590,4.620,4.650,4.680,4.710,4.740,   
     &   4.770,4.800,4.830,4.860,4.890,4.920,4.950,4.980,5.010,5.040,   
     &   5.070,5.100,5.130,5.160,5.190,5.220,5.250,5.280,5.310,5.340,   
     &   5.370,5.400,5.430,5.460,5.490,5.520,5.550,5.580,5.610,5.640,   
     &   5.670,5.700,5.730,5.760,5.790,5.820,5.850,5.880,5.910,5.940,   
     &   5.970,6.000,6.030,6.060,6.090,6.120,6.150,6.180,6.210,6.240,   
     &   6.270,6.300,6.330,6.360,6.390,6.420,6.450,6.480,6.510,6.540,   
     &   6.570,6.600,6.630,6.660,6.690,6.720,6.750,6.780,6.810,6.840,   
     &   6.870,6.900,6.930,6.960,6.990,7.020,7.050,7.080,7.110,7.140,   
     &   7.170,7.200,7.230,7.260,7.290,7.320,7.350,7.380,7.410,7.440,   
     &   7.470,7.500,7.650,8.000,-200.000,-1000.000/                    
      DATA (OZNTAB(I),I=0,127) /                                        
     &  -100.0,0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.0,18.0,20.0,22.0,  
     &   24.0,26.0,28.0,30.0,32.0,34.0,36.0,38.0,40.0,42.0,44.0,46.0,   
     &   48.0,50.0,52.0,54.0,56.0,58.0,60.0,62.0,64.0,66.0,68.0,70.0,   
     &   72.0,74.0,76.0,78.0,80.0,82.0,84.0,86.0,88.0,90.0,92.0,94.0,   
     &   96.0,98.0,100.0,102.0,104.0,106.0,108.0,110.0,112.0,114.0,     
     &   116.0,118.0,120.0,122.0,124.0,126.0,128.0,130.0,132.0,134.0,   
     &   136.0,138.0,140.0,142.0,144.0,146.0,148.0,150.0,152.0,154.0,   
     &   156.0,158.0,160.0,162.0,164.0,166.0,168.0,170.0,172.0,174.0,   
     &   176.0,178.0,180.0,182.0,184.0,186.0,188.0,190.0,192.0,194.0,   
     &   196.0,198.0,200.0,202.0,204.0,206.0,208.0,210.0,212.0,214.0,   
     &   216.0,218.0,220.0,222.0,224.0,226.0,228.0,230.0,232.0,234.0,   
     &   236.0,238.0,240.0,242.0,244.0,246.0,248.0,250.0,252.0/         
      DATA (OZNTAB(I),I=128,255) /                                      
     &                                                         254.0,   
     &   256.0,258.0,260.0,262.0,264.0,266.0,268.0,270.0,272.0,274.0,   
     &   276.0,278.0,280.0,282.0,284.0,286.0,288.0,290.0,292.0,294.0,   
     &   296.0,298.0,300.0,302.0,304.0,306.0,308.0,310.0,312.0,314.0,   
     &   316.0,318.0,320.0,322.0,324.0,326.0,328.0,330.0,332.0,334.0,   
     &   336.0,338.0,340.0,342.0,344.0,346.0,348.0,350.0,352.0,354.0,   
     &   356.0,358.0,360.0,362.0,364.0,366.0,368.0,370.0,372.0,374.0,   
     &   376.0,378.0,380.0,382.0,384.0,386.0,388.0,390.0,392.0,394.0,   
     &   396.0,398.0,400.0,402.0,404.0,406.0,408.0,410.0,412.0,414.0,   
     &   416.0,418.0,420.0,422.0,424.0,426.0,428.0,430.0,432.0,434.0,   
     &   436.0,438.0,440.0,442.0,444.0,446.0,448.0,450.0,452.0,454.0,   
     &   456.0,458.0,460.0,462.0,464.0,466.0,468.0,470.0,472.0,474.0,   
     &   476.0,478.0,480.0,482.0,484.0,486.0,488.0,490.0,492.0,494.0,   
     &   496.0,498.0,500.0,505.0,515.0,-200.0,-1000.0/                  
      DATA NCELLS /                                                     
     &      0,   3,  12,  28,  50,  78, 112, 152, 198, 250,             
     &    308, 372, 441, 516, 596, 681, 771, 866, 966,1070,             
     &   1178,1290,1406,1526,1649,1775,1904,2036,2170,2306,             
     &   2444,2584,2725,2867,3010,3154,3298,3442,3586,3729,             
     &   3871,4012,4152,4290,4426,4560,4692,4821,4947,5070,             
     &   5190,5306,5418,5526,5630,5730,5825,5915,6000,6080,             
     &   6155,6224,6288,6346,6398,6444,6484,6518,6546,6568,             
     &   6584,6593 /                                                    
      DATA ICELLS /                                                     
     &      3,   9,  16,  22,  28,  34,  40,  46,  52,  58,             
     &     64,  69,  75,  80,  85,  90,  95, 100, 104, 108,             
     &    112, 116, 120, 123, 126, 129, 132, 134, 136, 138,             
     &    140, 141, 142, 143, 144, 144, 144, 144, 143, 142,             
     &    141, 140, 138, 136, 134, 132, 129, 126, 123, 120,             
     &    116, 112, 108, 104, 100,  95,  90,  85,  80,  75,             
     &     69,  64,  58,  52,  46,  40,  34,  28,  22,  16,             
     &      9,   3 /                                                    
      END                                 
