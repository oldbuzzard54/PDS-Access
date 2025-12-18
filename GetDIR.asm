         TITLE 'GETDIR - READ PDS DIRECTORY'
**********************************************************************
*
*    THE "GETDIR" SUBROUTINE PROVIDES FOR ACCESS TO A PDS DIRECTORY
*    SEQUENTIALLY.
*
*   THE ORIGINAL SOURCE WAS OBTAINED FROM CBT THAT COME WITH TURNKEY
*           PDS : CBT429.FILE183(GETDIR)
*
*   THE ORIGINAL BASE FOR THIS SUBROUTINE IS A MACRO CALLED GETDIR.
*   THE OLD VERSION OF GETDIR GENERATED A CSECT INLINE WHERE IT WAS
*   CODED.
*
*   THE GETDIR MACRO WAS SIGNIFICANTLY MODIFIED TO GENERATE STANDARD
*   MVS CALLS TO OPEN, CLOSE AND GET MEMBER NAME AND OPTIONAL STATS.
*   GET.
*
**********************************************************************
*                       .
*
*   USAGE:
*      A   GETDIR FUNC=OPEN,DDN=DDNAME
*
*      DDNAME DC CL8' '    THIS IS THE DDNAME IN THE JCL FOR THE PDS.
*                          THE DEFAULT IS PDS IF BLANK.
*
*      A   GETDIR FUNC=CLOSE  NO OPTIONS TO CLOSE THE PDS.
*
*      A   GETDIR FUNC=RESET  START/RESTART DIRECTORY AT BEGINNING.
*
*      A   GETDIR FUNC=GET,MEM=MEMNAME,STATS=MEMSTAT
*
*      MEMNAME DS CL8      THIS IS WHERE THE MEMBER NAME AND STATS
*                          (IF ANY) ARE RETURNED.  THERE ARE COPY
*                          ITEMS GETPDSPA FOR COBOL AND PL/I,
*                          PDSTATS FOR ASM.
*                          MEMBER NAME IS SET TO X'FF' FOR END OF DIR.
*
*      MEMSTAT  AREA DEFINE BY COPY/INCLUDE PDSSTATS
*
**********************************************************************
*
*   V1   11/28/25    ORIGINAL VERSION
*
**********************************************************************
GETDIR   ESTART TYPE=CSECT,DESC='GETDIR - ACCESS PDS DIRECTORY',       X
               VER='1.0.0',BASE=R12,REGS=YES        ,PLIF=GETDIRP
*
         USING IHADCB,R11
         USING PDSSTATS,R10
         USING MEMTBL,R9
         MVC   @GETRC,=F'0'
         LA    R11,@GETDCB
         LR    R3,R1                   ADDRESS OF PARM LIST
         CLC   0(4,R3),=X'00010002'    V1 FUNC 2 GET
         BE    @GET                    = 2
         CLC   0(4,R3),=X'00010001'    V1 FUNC 1 OPEN
         BE    @OPEN                   = 1
         CLC   0(4,R3),=X'80010003'    V1 FUNC 3 CLOSE
         BE    @CLOSE                  = 3
         CLC   0(4,R3),=X'80010004'    V1 FUNC 3 RESET
         BE    @RESET                  = 4
@BADFUNC EQU   *
         MVC   @GETRC,=F'16'
         B     @GETEXIT
*********************************************************************
*
*        OPEN DATASET
*
*********************************************************************
@OPEN    EQU   *
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BO    @DCBOPN                  Y BOMB OUT
         L     R4,4(,R3)                GET ADDR OF DDNAME
         CLC   0(8,R4),=CL8' '          BLANK?
         BE    *+10
         MVC   DCBDDNAM,0(R4)           MOVE DDNAME
         OPEN  (@GETDCB,INPUT)
         LA    R5,256
         STH   R5,DCBLRECL
         STH   R5,DCBBLKSI
*
*   ALLOCATE THE DIR TABLE AND CLEAR IT.
*    SET THE MAX USED TO 1 ENTRY BEFORE TABLE
*    THE TABLE LOAD RTN WILL INCREMENT 1 ENTRY BEFORE STORING INFO
*
         L     R0,TABSIZE
         GETMAIN R,LV=(0)
         ST    R1,DIRTABAD               SAVE THE AREA ADDR
         ST    R1,DIRTABCU               SAVE THE CURR ITEM
         S     R1,=F'42'
         ST    R1,DIRTABMX               SAVE THE LAST ITEM ADDR
         L     R1,DIRTABAD
         A     R1,TABSIZE
         ST    R1,DIRTABEN               SAVE THE END OF THE ADDR
*
         STM   R14,R12,12(R13)           SAVE REGS FOR MVCL
         L     R2,DIRTABAD
         L     R3,TABSIZE
         LA    R4,=X'00'
         L     R5,MVCLEN
         MVCL  R2,R4
         L     R2,DIRTABAD
         MVI   0(R2),X'FF'               MAKE CURRENT ITEM
         MVC   1(41,R2),0(R2)              AN EOD ENTRY
         LM    R14,R12,12(R13)           RESTORE REGS
         B     RESET
MVCLEN   DC    A(0)
*
*   SET UP TO FORCE FIRST READ OF DIRECTORY
*       MAKE THE NEXT NAME ADDR BEYOND THE END OF BLOCK ADDR.
*       THIS WILL FORCE A READ ON FIRST GET.
*
RESET    LA    R1,@GETDBUF+256         GET END OF BLOCK ADDR
         ST    R1,@GETDR1              SAVE THE BLOCK ADDR
         ST    R1,@GETEOB                 TO FORCE READ
*
*   READ THE DIRECTORY AND LOAD THE DIRTAB WITH ALL MEMBERS
*       AND THE STATS, IF PRESENT.
*
@GETREAD L     R5,@GETDR1
         CLC   @GETDR1,@GETEOB         END OF BUFFER?
         BL    @BYPASS                 NO?
         READ  @GETDECB,SF,@GETDCB,@GETDBUF,'S'
         CHECK @GETDECB
         LA    R5,@GETDBUF+2           GET BLK ADDR
         ST    R5,@GETDR1              SAVE THE BLOCK ADDR
         LH    R2,@GETDBUF
         LA    R2,0(R2,R5)             CALC END IF BLOCK ADDR
         S     R2,=F'4'
         ST    R2,@GETEOB
@BYPASS  EQU   *
         L     R9,DIRTABMX             GET LAST TABLE ADDR
         A     R9,=F'42'
         C     R9,DIRTABEN             COMPARE TO END OF AREA
         BL    TABADD
         WTO   'GETDIR-MEMBER TABLE OVERFLOW',ROUTCDE=(2,11)
         ABEND 1234
TABADD   ST    R9,DIRTABMX             SAVE LAST USED ADDR
         LA    R10,MEMSTATS
         MVC   MEMNAME,0(R5)           MOVE THE MEMBER NAME TO MEM
         MVC   MEMTTRN,8(R5)           MOVE THE TTRN
         CLI   11(R5),X'00'            CHECK "C" BYTE
         BNE   MOVSTATS
         MVI   PDSC,X'00'              SET "C" TO ZEROS
         MVI   PDSC+1,C' '               SPACE IT OUT
         MVC   PDSC+2(PDSSTATL-2),PDSC+1
         B     EODTST
MOVSTATS MVC   PDSC(PDSSTATL),11(R5)
EODTST   CLI   0(R5),X'FF'             END OF DIRECTORY?
         BE    @EOF                      YES,BRANCH
         MVC   @GETDRLN+1(1),11(R5)    CONVERT "C" TO HALF WORD
         LH    R14,@GETDRLN            GET "C" BYTE
         LA    R14,12(R14,R14)          DOUBLE IT AND ADD 12
         LA    R5,0(R14,R5)            ADVANCE BUFFER ADDR
         ST    R5,@GETDR1
         B     @GETREAD
*
*    END OF DIRECTORY
*
@EOF     CLOSE @GETDCB
         MVC   @GETRC,=F'0'
******   ABEND 1234,DUMP,,USER
         B     @GETEXIT
@DCBOPN  EQU  *
         MVC   @GETRC,=F'4'
         B     @GETEXIT
*********************************************************************
*
*       RESET DIRECTORY - NEXT MEMBER IS THE FIRST
*
*********************************************************************
@RESET   EQU   *
         L     R2,DIRTABAD               GET TABLE
         ST    R2,DIRTABCU               SAVE THE CURR ITEM
         B     @GETEXIT
*********************************************************************
*
*       CLOSE DIRECTORY - ACTUALLY FREE THE DIR TABLE
*
*********************************************************************
@CLOSE   EQU   *
         L     R0,TABSIZE
         L     R5,DIRTABAD
         FREEMAIN R,LV=(0),A=(5)
         MVI   DIRTABAD,X'00'                CLEAR OUT CONTROL
         MVC   DIRTABAD+1(DIRTABL-1),DIRTABAD
         MVC   @GETRC,=F'0'
         B     @GETEXIT
*********************************************************************
*
*        GET NEXT MEMBER FROM DIRECTORY
*
*********************************************************************
@GET     EQU   *
         L     R9,DIRTABCU             GET ADDR OF CURRENT/NEXT ENTRY
         LA    R10,MEMSTATS
         L     R5,4(,R3)               GET MEMBER NAME PARM
         MVC   0(L'MEMNAME,R5),MEMNAME MOVE MEMNAME TO PARM
         CLI   MEMNAME,X'FF'           END OF TABLE?
         BE    @EOD
         TM    4(R3),X'80'             IS NAME LAST PARM?
         BO    @NOSTAT                 YES, BRANCH NO STATS
         L     R6,8(,R3)               GET ADDR OF STATS AREA
         MVC   0(PDSSTATL,R6),11(R9)   COPY STATS
@NOSTAT  EQU   *
         LA    R9,42(,R9)
         ST    R9,DIRTABCU             SAVE ADDR OF NEXT ENTRY
         B     @GETEXIT
@EOD     EQU   *
         MVC   @GETRC,=F'12'
@GETEXIT EQU   *
         ERETURN IN=@GETRC
@GETDCB  DCB   DDNAME=PDS,DSORG=PO,MACRF=(R)
@GETRC   DC    F'0'                    RETURN CODE
         DC    CL16'****DIR BLK****'
@GETDR1  DS    A(@GETDBUF+2)           CURRENT POINTER IN THE BUFFER
@GETDBUF DS    H,254X
         DC    CL16'****END BLK****'
@GETDRLN DC    H'0'                    DIR ENTRY LEN WORK AREA
@GETEOB  DC    A(@GETDBUF+256)         END OF BLOCK ADDR
         DC    CL16'***TABLE BLK****'
TABSIZE  DC    F'40960'                FOR GETMAIN
DIRTABAD DC    A(0)                    TABLE ADDRESS
DIRTABEN DC    A(0)                    TABLE END ADDRESS
DIRTABCU DC    A(0)                    TABLE CURRENT ITEM
DIRTABMX DC    A(0)                    TABLE MAX (LAST) ITEM IN TABLE
DIRTABL  EQU   *-DIRTABAD              CONTROL INFO LENGTH
         LTORG
         DROP  R12
MEMTBL   DSECT
MEMNAME  DS    CL8
MEMTTRN  DS    CL3
MEMSTATS DS    CL29                    STATS AREA
         COPY  PDSTATS
         IHADCB DSORG=BS,DEVD=DA
         END
**********************************************************************
*
*       END OF GETDIR SUB-ROUTINE
*
**********************************************************************
