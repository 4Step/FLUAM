
dir = 'M:\Models\StateWide\TSM_Legacy\FLUAM\Output'

LOOP YEAR = 1, 7
IF (YEAR = 1) 
   yyyy = '2020'
ELSEIF (YEAR = 2) 
   yyyy = '2025'
ELSEIF (YEAR = 3) 
   yyyy = '2030'
ELSEIF (YEAR = 4) 
   yyyy = '2035'
ELSEIF (YEAR = 5) 
   yyyy = '2040'
ELSEIF (YEAR = 6) 
   yyyy = '2045'
ELSEIF (YEAR = 7) 
   yyyy = '2050'
ENDIF


    ; Export to CUBE format
    RUN PGM=MATRIX

    FILEI MATI[1] = @dir@\@yyyy@_mat.csv,
        PATTERN=IJ:V FIELDS=#1,2,3

    FILEO MATO[1] = @dir@\@yyyy@_mat.mat, MO=1 

    ZONES=6461

    MW[1]=MI.1.1
    ENDRUN
 
   
    ; Convert to TRANPLAN Format
    RUN PGM=MATRIX
    FILEI MATI[1] = @dir@\@yyyy@_mat.mat
    FILEO MATO[1] = @dir@\@yyyy@_mat.trp,
          MO=1-1 DEC=2 format=TRANPLAN

    FILLMW MW[1]=mi.1.1(1)

    ; VOYAGER to LEGACY
    IF ('TRANPLAN'='Tranplan' & 'T'='T')
       loop k=1,1
       MW[k]=MW[k]
       endloop
    elseif ('TRANPLAN'='Tranplan' & 'T'='F')
       loop k=1,1
       VAR=rowfix(k)
       endloop
    endif

    ENDRUN

    ; delete first set of files
    * del @dir@\@yyyy@_mat.mat
    
ENDLOOP


