GETPDS   TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM DOCUMENTATION'
         PUNCH ' ALIAS NCZ93205'
         PUNCH ' ALIAS GETPDSP'
***********************************************************************
*                                                                     *
* GETPDS - SUBROUTINE TO READ PDS DIRECTORIES AND MEMBERS             *
*          FORMERLY KNOWN AS NCZ93205                                 *
*                                                                     *
* V1.0.0 - MODULE RENAMED FROM NCZ93205.  A ALIAS WAS ADDED FOR       *
*          BACKWARD COMPATABILITY WITH OLDER VERSIONS.                *
*        = TWO NEW FUNCTIONS WERE ADDED - START SEQUENTIAL DIRECTORY  *
*          RETRIEVE AND GET NEXT MEMBER NAME.                         *
*        - SUPPORT FOR PL/1(F) ADDED.  SINCE PL/1 DOES NOT SUPPORT    *
*          RETURN CODE, A 4TH PARM FOR RETURN_CODE MUST BE INCLUDED.  *
*                                                                     *
* V1.1.0 - BUG FIX.  IF THE PDS HAD A LARGE BLKSIZE (>3200), THIS     *
*          ROUTINE WOULD ABORT WITH A S0C4. FOUND THE EX INSTRUCTION  *
*          WAS CONSISTANTLY MOVE 1 EXTRA BYTE OF DATA.                *
*                                                                     *
* V1.2.0 - BAND AID PATCH FOR BUG IN PDSDIR MACRO.  CONTROL INFO      *
*          IS NOT INITIALIZED ON AN OPEN.  THIS RESULTS IN AN SOC4.   *
*          APPLIED PATCH TO ZERO OUT CONTROL FIELDS.                  *
*        - REVISED DOCUMENTATION FOR COBOL CALLERS AND ADDED          *
*          DOCUMENTATION FOR PL/I CALLERS.                            *
*                                                                     *
* V1.3.0 - ADDED CODE TO RETURN THE STATS, IF PRESENT, IN THE         *
*          FIRST 30 BYTES OF THE INPUT AREA.                      .   *
*                                                                     *
***********************************************************************
***********************************************************************
*                                                                     *
* ID:          GETPDS  -  SUBROUTINE TO READ PDS MEMBERS              *
*                                                                     *
* NCZ93205.1 PROGRAM DESCRIPTION                                      *
*                                                                     *
* THIS PROGRAM CAN BE CALLED AS A SUBROUTINE FROM ASSEMBLER, COBOL OR *
* COBOL OR PL/I TO PROVIDE READ ACCESS TO MEMBER(S) OF A PDS. THE     *
* MEMBER NAME(S) MAY BE SPECIFIED DYNAMICALLY VIA THE PARAMETERS.     *
* THUS THIS PROGRAM ENABLES A CALLING PROGRAM TO ACCESS MANY MEMBERS  *
* OF A PDS WHEN THE NAMES OF THE MEMBERS ARE NOT KNOWN UNTIL EXECUTION*
* TIME.                                                               *
*                                                                     *
* ONLY COMBINATIONS OF RECFM = F/B/A ARE SUPPORTED.                   *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.2 USER INSTRUCTIONS FOR COBOL                              *
*                                                                     *
* PARAMETERS:                                                         *
* ----------                                                          *
* THE PROGRAM OBSERVES A STANDARD OS LINKAGE. IT SHOULD ALWAYS BE     *
* CALLED WITH 3 PARAMETERS WHICH ARE:                                 *
*                                                                     *
* P1 - REQUEST CODE      FULLWORD BINARY  COBOL PIC S9(8) COMP.       *
*      VALUES:                                                        *
*      0   -  OPEN DDNAME 'PDS'                                       *
*      1   -  START SEQUENTIAL PROCESSING OF DIRECTORY                *
*      4   -  LOCATE MEMBER (DOES NOT READ MEMBER)                    *
*      5   -  GET NEXT MEMBER NAME (DOES NOT READ MEMBER)             *
*      8   -  READ NEXT RECORD IN CURRENT MEMBER                      *
*      12  -  CLOSE DDNAME 'PDS'                                      *
*                                                                     *
* P2 - MEMBER NAME      CHARACTER STRING COBOL PIC X(8).              *
*      CONTAINS NAME OF MEMBER WHEN P1 = 4                            *
*                                                                     *
* P3 - INPUT AREA       CHARACTER STRING. IT IS THE RESPONSIBLITY OF  *
*      WHEN P1 = 8                        THE CALLING PROGRAM TO      *
*                                         ENSURE THAT THIS AREA WILL  *
*                                         ACCOMMODATE THE LONGEST     *
*                                         RECORD TO BE READ.          *
*                                                                     *
* RETURN CODE -                                                       *
*      VALUES:                                                        *
*      0   - REQUESTED FUNCTION COMPLETED WITHOUT ERROR               *
*      4   - REQUESTED FUNCTION DID NOT COMPLETE FOR REASON:          *
*            WHEN P1 = 0  FILE COULD NOT BE OPENED                    *
*                      1  FILE COULD NOT BE OPENED                    *
*                      4  MEMBER WAS NOT FOUND                        *
*                      5  END OF DIRECTORY                            *
*                      8  END OF FILE ON CURRENT MEMBER               *
*      8   - SERIOUS ERROR. PROCESSING SHOULD BE TERMINATED.          *
*                                                                     *
* EXAMPLE CALL FROM COBOL:                                            *
*                                                                     *
*      CALL 'GETPDS' USING FUNC, MEMBER, INPUT-AREA.                  *
*      IF RETURN-CODE = ZERO GO TO .....                              *
*                                                                     *
* THE SPECIAL REGISTER 'RETURN-CODE' SHOULD BE CLEARED TO ZERO BY     *
* THE APPLICATION PROGRAM BEFORE PROGRAM TERMINATION OTHERWISE        *
* IT MAY BE PROPAGATED UPWARDS TO OS WITH A NON ZERO VALUE.           *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.2 USER INSTRUCTIONS FOR PL/I                               *
*                                                                     *
* PARAMETERS:                                                         *
* ----------                                                          *
* THE PROGRAM OBSERVES A STANDARD OS LINKAGE. IT SHOULD ALWAYS BE     *
* CALLED WITH 4 PARAMETERS WHICH ARE DECLARED STATIC.                 *
*                                                                     *
* P1 - REQUEST CODE      FULLWORD BINARY  FIXED BINARY(31).           *
*      VALUES:                                                        *
*      0   -  OPEN DDNAME 'PDS'                                       *
*      1   -  START SEQUENTIAL PROCESSING OF DIRECTORY                *
*      4   -  LOCATE MEMBER (DOES NOT READ MEMBER)                    *
*      5   -  GET NEXT MEMBER NAME (DOES NOT READ MEMBER)             *
*      8   -  READ NEXT RECORD IN CURRENT MEMBER                      *
*      12  -  CLOSE DDNAME 'PDS'                                      *
*                                                                     *
* P2 - MEMBER NAME      CHARACTER STRING CHAR(8).                     *
*      CONTAINS NAME OF MEMBER WHEN P1 = 4                            *
*                                                                     *
* P3 - INPUT AREA       CHARACTER STRING. IT IS THE RESPONSIBLITY OF  *
*      WHEN P1 = 8                        THE CALLING PROGRAM TO      *
*                                         ENSURE THAT THIS AREA WILL  *
*                                         ACCOMMODATE THE LONGEST     *
*                                         RECORD TO BE READ.          *
*                                                                     *
* P4 - RETURN CODE      FULLWORD FIXED BINARY(31).                    *
*      WHEN P1 = 8                        PL/I (F) DOES NOT HAVE      *
*                                         A RETURN-CODE REGISTER      *
*                                         LIKE COBOL.                 *
*                                                                     *
* RETURN CODE -                                                       *
*      VALUES:                                                        *
*      0   - REQUESTED FUNCTION COMPLETED WITHOUT ERROR               *
*      4   - REQUESTED FUNCTION DID NOT COMPLETE FOR REASON:          *
*            WHEN P1 = 0  FILE COULD NOT BE OPENED                    *
*                      1  FILE COULD NOT BE OPENED                    *
*                      4  MEMBER WAS NOT FOUND                        *
*                      5  END OF DIRECTORY                            *
*                      8  END OF FILE ON CURRENT MEMBER               *
*      8   - SERIOUS ERROR. PROCESSING SHOULD BE TERMINATED.          *
*                                                                     *
* EXAMPLE CALL FROM COBOL:                                            *
*                                                                     *
*      CALL PDSGETP(FUNC, MEMBER, INPUT-AREA, RETURN_CODE);           *
*      IF RETURN_CODE = ZERO          ..                              *
*                                                                     *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.3 MESSAGES                                                 *
*                                                                     *
* NONE.                                                               *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.4 ABEND CODES                                              *
*                                                                     *
* NONE. IT IS THE RESPOSIBILITY OF THE CALLING PROGRAM TO TERMINATE   *
* AFTER A SERIOUS ERROR CONDITION.                                    *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.5 IMPLEMENTATION METHOD                                    *
*                                                                     *
* MOVE TO MAC1.ASM AND PASMAL.                                        *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.6 AMENDMENT HISTORY                                        *
*                                                                     *
* MAINTAIN THE RECORD BELOW.                                          *
*                                                                     *
* CHANGE HISTORY:                                                     *
* DATE      INITS VERSION COMMENTS                                    *
*                                                                     *
* 17 NOV 82 RH    V01     CREATED                                     *
*                                                                     *
* SUGGESTED ENHANCEMENTS:                                             *
*                                                                     *
* NONE YET.                                                           *
*                                                                     *
* DD MMM YY WHO : COMMENTS                                            *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.7 MODULARISATION DETAILS                                   *
*                                                                     *
* SIMPLE LINEAR STRUCTURE.                                            *
* USES OPEN, FIND, CLOSE, READ, CHECK (BSAM).                         *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.8 PROGRAM LOGIC OVERVIEW                                   *
*                                                                     *
* OMITTED. IT SHOULD BE NOTED THAT THIS PROGRAM HAS TO BE PRETTY      *
* RUGGED. DO NOT ASSUME THAT ALL CALLS ARE IN LOGICAL SEQUENCE.       *
*                                                                     *
*****                                                             *****
         TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM PROLOGUE'
*****                                                             *****
*                                                                     *
* ID:          GETPDS   - SUBROUTINE TO READ PDS MEMBERS              *
*                         FORMERLY KNOWN AS NCZ93205                  *
*                                                                     *
* ENTRY INTFCE: STANDARD OS - SEE ABOVE.                              *
*                                                                     *
* EXIT  INTFCE: STANDARD OS - SEE ABOVE.                              *
*                                                                     *
* RETURN CODES:RC = 0       - OK                                      *
*                                                                     *
*              RC = 4       - REQUESTED FUNCTION DID NOT COMPLETE     *
*                                                                     *
*              RC = 8       - SERIOUS ERROR - TERMINATION ADVISED     *
*                                                                     *
* REG USAGE:   R0 -                                                   *
*              R1 -                                                   *
*              R2 -      -> P1  (FUNCTION)                            *
*              R3 -      -> P2  (MEMBER)                              *
*              R4 -      -> P3  (INPUT AREA)                          *
*              R5 -      WORK: -> BUFFER                              *
*              R6 -      WORK: -> RECORD                              *
*              R7 -      -> P4 (RETURN CODE PL/1 ONLY)                *
*              R8 -                                                   *
*              R9                                                     *
*              R10 -                                                  *
*              R11 -     BASE REGISTER                                *
*              R12 -     -> DCB                                       *
*              R13 -     SAVE AREA                                    *
*              R14 -                                                  *
*              R15 -                                                  *
*                                                                     *
*****                                                             *****
         TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM CODE'
GETPDS   ESTART TYPE=START,DESC='GETPDS - ACCESS PDS MEMBERS',         X
               VER='1.3.0',BASE=R11,REGS=YES,PLIF=GETPDSP
         ENTRY NCZ93205
NCZ93205 EQU   GETPDS
         BNE   NOTPLI
         LM    R2,R5,0(R1)         GET PLI PARMLIST OF DDB
         L     R2,0(0,R2)          GET DATA ADDRS
         L     R3,0(0,R3)
         L     R4,0(0,R4)
         L     R7,0(0,R5)              PL1 ONLY
         B     COMM
NOTPLI   EQU   *
         LM    R2,R4,0(R1)
         LA    R7,PLIRC                PL1 ONLY
COMM     EQU   *
         LA    R12,PDS
         USING IHADCB,R12
******** LM    R2,R4,0(R1)
         SPACE
         L     R15,0(R2)               GET FUNCTION CODE
         CH    R15,=H'0'               Q - OPEN ?
         BE    P01                     Y
         CH    R15,=H'4'               Q - FIND ?
         BE    P02                     Y
         CH    R15,=H'8'               Q - READ ?
         BE    P03                     Y
         CH    R15,=H'12'              Q - CLOSE ?
         BE    P04                     Y
         CH    R15,=H'1'               Q - START SEQ DIR ?
         BE    P05                     Y
         CH    R15,=H'5'               Q - NEXT SEQ DIR ?
         BE    P06                     Y
         B     P99                     BAD FUNCTION CODE
*********************************************************************
*        OPEN DATASET
P01      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BO    P99                      Y BOMB OUT
*********************************** START FIX V1.2.0 ****************
         L     R8,=A(@GETDR1)
         MVI   0(R8),X'00'
         MVC   1(256,R8),0(R8)          CLEAR OUT WORKING STORAGE
**************************************END FIX V1.2.0 ****************
         CLC   0(8,R3),=CL8' '          DDNAME SPECIFIED?
         BE    P01GO                    NO, DEFAULT=PDS
         MVC   PDSDDNAM(8),0(R3)        MOVE DDNAME TO DCB
P01GO    EQU   *
         OPEN  (PDS,INPUT)              OPEN IT
         TM    DCBOFLGS,DCBOFOPN        Q - OK ?
         BZ    P98                      N EXIT RC=4
         CLI   DCBDSORG,DCBDSGPO        Q - IS THIS A PDS ?
         BNE   P0105                    N GO CLOSE
         TM    DCBRECFM,X'FF'-(DCBRECF+DCBRECBR+DCBRECCA) Q - RECFM ?
         BZ    P0110                    OK
         SPACE
P0105    EQU   *
         CLOSE PDS
         B     P99
P0110    LH    R0,DCBBLKSI
         GETMAIN R,LV=(0)               GET A BUFFER
         ST    R1,BUFA
         B     P97                      EXIT OK
**********************************************************************
         SPACE
*        FIND MEMBER
P02      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         FIND  (12),(3),D               LOCATE MEMBER
         LTR   R15,R15                  Q - OK ?
         BNZ   P98                      N EXIT RC=4
         NI    FLAGS,X'FF'-EOM          Y CLEAR END OF MEMBER FLAG
         OI    FLAGS,DOREAD             SET FLAG TO DO READ
         B     P97                      EXIT RC=0
         EJECT
**********************************************************************
*        GET NEXT RECORD FOR THIS MEMBER
P03      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N - BOMB OUT
         SPACE
         TM    FLAGS,EOM                Q - EOM ALREADY ?
         BO    P99                      Y BOMB OUT
         SPACE
P0305    L     R5,BUFA                  R5 -> INPUT BUFFER
         L     R6,RECA                  R6 -> NEXT RECORD IN BUFFER
         TM    FLAGS,DOREAD             Q - SHOULD WE READ NEXT BLOCK ?
         BZ    P0310                    N
         SPACE
         READ  DECB1,                   READ A BLOCK OF RECORDS        *
               SF,                                                     *
               (12),                                                   *
               (5),                                                    *
               'S'
         CHECK DECB1                    WAIT FOR IO TO COMPLETE
         SPACE
         LH    R10,DCBBLKSI             # OF BYTES WE COULD HAVE READ
         L     R1,DCBIOBA
         SH    R10,22(R1)               LESS # OF BYTES WE DID'NT READ
         BZ    P03EOM                   NOTHING READ - MUST BE EOM
         AR    R10,R5
         ST    R10,BLOCKEND             SAVE ADDRESS END OF THIS BLOCK
         NI    FLAGS,X'FF'-DOREAD       SET FLAG OFF - WE DID A READ
         LR    R6,R5                    R6 -> FIRST RECORD IN BLOCK
         SPACE
P0310    LH    R15,DCBLRECL             GET RECORD LENGTH
*******  EX    R15,EXMVC                MOVE RECORD TO INPUT AREA
*******  AR    R6,R15                   R6 -> NEXT RECORD IN BUFFER
********************************* START FIX V1.1.0     ***************
         BCTR  R15,0
         EX    R15,EXMVC                MOVE RECORD TO INPUT AREA
         LA    R6,1(R15,R6)             R6 -> NEXT RECORD IN BUFFER
*********************************  END  FIX V1.1.0     ***************
         C     R6,BLOCKEND              Q - END OF THIS BLOCK ?
         BNL   P0320                    Y GO SET FLAG TO DO READ
         SPACE
         ST    R6,RECA                  SAVE CURRENT RECORD POINTER
         B     P97                      EXIT RC=0
         SPACE
P0320    OI    FLAGS,DOREAD             DO READ FOR NEXT RECORD
         B     P97                      EXIT RC=0
         SPACE
P03EOM   DS    0H
         OI    FLAGS,EOM                INDICATE END OF MEMBER
         B     P98                      EXIT RC = 4
         EJECT
**********************************************************************
*        CLOSE PDS
P04      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         LH    R0,DCBBLKSI              FREE MEMBER BLOCK
         L     R5,BUFA
         FREEMAIN R,LV=(0),A=(5)
         CLOSE PDS
         B     P97
**********************************************************************
*        START SEQUENTIAL DIR PROCESS
P05      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         MVC   PDSRELAD(4),=4X'00'      RESET DIRECTORY TO START
         B     P97
         SPACE
**********************************************************************
*        GET NEXT MEMBER NAME
P06      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
GETD     GETDIR PDS,EODAD=P06EOF
         MVC   0(8,R3),0(R1)            SAVE MEMBER NAME AND TTR
         USING PDSSTATS,R10
         LA    R10,11(,R1)              SKIP MEM NAME AND TTR
         MVI   0(R4),C' '               CLEAR 30 BYTES OF
         MVC   1(PDSSTATL-1,R4),0(R4)      DATA AREA
         CLI   PDSC,X'0F'               15 HALFWORDS?
         BNE   P97                      NO STATS
*
*   COULD BE STATS.  CONTINUE TESTING
*
         ST    R2,R2SAVE                TRT COULD CLOBER R2
         TRT   PDSDATEC(3),NUMTBL
         BNZ   NOTSTAT
         TRT   PDSDATEC+3(1),SIGNTBL
         BNZ   NOTSTAT
         TRT   PDSDATEU(3),NUMTBL
         BNZ   NOTSTAT
         TRT   PDSDATEU+3(1),SIGNTBL
         BNZ   NOTSTAT
         MVC   0(PDSSTATL,R4),PDSSTATS
         LR    R10,R4                    STATS IN INPUT AREA
         CLI   PDSDATEC,X'01'                                                                                                                                                                                                          M
         BE    CENT20C
         CLI   PDSDATEC,X'00'
         BNE   NOTSTAT
         MVI   PDSDATEC,X'19'
         B     CHECKU
CENT20C  MVI   PDSDATEC,X'20'
CHECKU   EQU   *
         CLI   PDSDATEU,X'01'                                                                                                                                                                                                          M
         BE    CENT20U
         CLI   PDSDATEU,X'00'
         BNE   NOTSTAT
         MVI   PDSDATEU,X'19'
         B     NOTSTAT
CENT20U  MVI   PDSDATEU,X'20'
NOTSTAT  L     R2,R2SAVE                 RESTORE R2
         B     P97
P06EOF   EQU   *
         B     P98
*
         SPACE
P97      MVC   0(4,R7),=F'0'
         ERETURN RC=0
P98      MVC   0(4,R7),=F'4'
         ERETURN RC=4
P99      MVC   0(4,R7),=F'8'
         ERETURN RC=8
         SPACE
PDS      DCB   DDNAME=PDS,DSORG=PO,MACRF=(R),EODAD=P03EOM
PDSRELAD EQU   PDS                      CURRENT TTRN
PDSDDNAM EQU   PDS+40                                            *JLM*
FLAGS    DC    X'00'
EOM      EQU   X'80'                    REACHED END OF CURRENT MEMBER
DOREAD   EQU   X'40'                    INDICATES CURRENT BLOCK IS     *
                                        EXHAUSTED
EXMVC    MVC   0(0,R4),0(R6)            MOVE RECORD TO INPUT AREA
PLIRC    DS    F                        USED FOR PLI INTERFACE ONLY
BUFA     DS    F                        -> BUFFER
RECA     DS    F                        -> CURRENT RECORD IN BUFFER
BLOCKEND DS    F                        -> END OF CURRENT BLOCK
*
*   NUMERIC VALIDATION TABLES
*                      0 1 2 3 4 5 6 7 8 9 A B C D E F
R2SAVE   DS    F
NUMTBL   DC    10XL16'00000000000000000000FFFFFFFFFFFF' X'0?'-X'9?'
         DC     6XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' X'A?'-X'F?'
SIGNTBL  DC    10XL16'FFFFFFFFFFFFFFFFFFFFFFFF00FFFF00' X'0?'-X'9?'
         DC     6XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' X'A?'-X'F?'
         LTORG
         DCBD  DSORG=BS,DEVD=DA
***#INCLUDE PDSTATS.ASM
         COPY PDSTATS
         END
