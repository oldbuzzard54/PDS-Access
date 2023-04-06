DATECNV  TITLE 'SUBROUTINE FOR DATE CONVERSIONS'
         PUNCH ' ALIAS DATCNVP'
***********************************************************************
*                                                                     *
* DATECNV - SUBROUTINE FOR DATE CONVERSIONS                           *
*                                                                     *
*    THIS ROUTINE HANDLES DATE CONVERSIONS FROM ONE FORMAT TO         *
*    ANOTHER.                                                         *
*                                                                     *
*    DATA IS PASSED IN A BLOCK OF 4 FIELDS                            *
*      RETURN CODE           1 WORD                                   *
*                                =0 CONVERSION OK                     *
*                                =1 INVALID INPUT FORMAT              *
*                                =2 INVALID DATE                      *
*                                =3 INVALID OUTPUT FORMAT             *
*                                                                     *
*      INPUT FORMAT          8 CHARACTERS                             *
*      INPUT DATE            10 DIGITS                                *
*      OUTPUT FORMAT         8 CHARACTERS                             *
*      OUTPUT DATE           10 DIGITS                                *
*                                                                     *
***********************************************************************
DATECNV  ESTART TYPE=START,DESC='DATECNV - DATE CONVERSIONS',          X
               VER='1.0.0',BASE=R12,REGS=YES,PLIF=DATCNVP
         BNE   NOTPL1
         L     R10,0(,R1)
         L     R10,0(,R10)
         B     PL1DONE
NOTPL1   EQU   *
         USING PARMS,R10
         L     R10,0(,R1)                                                                                                                                                                                           I
PL1DONE  EQU   *
         MVC   RC,=F'0'
**
**   DETERMINE FORMAT OF THE IN DATE AND VALIDATE IT.
**
         CLC   INPFMT,=C'YYYYDDD '
         BE    INPJ7                INPUT IS JULIAN 7 DIGITS
         MVC   RC,=F'1'             INVALID INPUT FORMAT
         B     EXIT                 RETURN TO CALLER
INPJ7    EQU   *                    INPUT IS JULIAN 7 DIGITS
         TRT   INPDATE(7),NUMTBL    TEST FOR 7 DIGITS
         BNZ   INVDATE              INVALID DATE
**
**   DETERMINE FORMAT OF THE OUT DATE.
**
         CLC   OUTFMT,=C'YYYYMMDD'
         BE    OUTG8                OUTPUT IS GREG 8 DIGITS
         MVC   RC,=F'3'             INVALID OUTPUT FORMAT
         B     EXIT
OUTG8    EQU   *                    OUTPUT IS GREG 8
*
*   CONVERSION ROUTINES
*
J7G8     EQU   *           YYYYDDD -> YYYYMMDD
         PACK  JYEAR,INPDATE(4)       PACK THE YEAR
         ZAP   DWORD,JYEAR
         CVB   R2,DWORD                AND CONVERT TO BINARY
*
*  SPECIAL LEAP YEAR FOR CENTURY YEARS
*
         CLC   INPDATE+2(2),=C'00'    CENTURY YEAR?
         BNE   J7NOTCEN                NO, BRANCH
         SR    R4,R4                  DIVIDE BY 400
         LR    R5,R2                   IF THERE IS A REMAINDED
         D     R4,=F'400'                IT IS NOT A CENTURY YEAR
         C     R5,=F'0'
         BE    J7LEAP                 CENTURY YEAR (IE 2000)
J7NOTCEN EQU   *
         LR    R3,R2                  DETERMINE LEAP YEAR
         SRA   R3,2                   DIVIDE BY 4
         SLA   R3,2                   MULTIPLY BY 4
         CR    R3,R2                  EQUAL?
         BE    J7LEAP                  YES LEAP YEAR
         MVC   JFEB+2(2),=PL2'28'
         B     J7NEXT
J7LEAP   EQU   *
         MVC   JFEB+2(2),=PL2'29'
J7NEXT   EQU   *
         PACK  JDAY,INPDATE+4(3)
*
*    SEARCH JDAYS TO DETERMINE THE MONTH
*
         LA    R2,JDAYS
         LA    R3,12
JSEARCH  CP    JDAY,2(2,R2)
         BNH   JFND
         SP    JDAY,2(2,R2)
         LA    R2,4(,R2)
         BCT   R3,JSEARCH
         B     INVDATE
JFND     EQU   *
         UNPK  OUTDATE+5(3),JDAY
         OI    OUTDATE+7,X'F0'
         MVC   OUTDATE(4),INPDATE
         MVC   OUTDATE+4(2),0(R2)
         B     EXIT
INVDATE  EQU   *
         MVC   RC,=F'2'             INVALID DATE
EXIT     ERETURN RC=0
*                      0 1 2 3 4 5 6 7 8 9 A B C D E F
NUMTBL   DC    15XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
         DC      XL16'00000000000000000000FFFFFFFFFFFF' X'F0'-X'F9'
DWORD    DS    D
JYEAR    DS    PL3
JDAY     DS    PL2
JDAYS    DS    0CL48
         DC    CL2'01',PL2'31'
JFEB     DC    CL2'02',PL2'28'
         DC    CL2'03',PL2'31'
         DC    CL2'04',PL2'30'
         DC    CL2'05',PL2'31'
         DC    CL2'06',PL2'30'
         DC    CL2'07',PL2'31'
         DC    CL2'08',PL2'31'
         DC    CL2'09',PL2'30'
         DC    CL2'10',PL2'31'
         DC    CL2'11',PL2'30'
         DC    CL2'12',PL2'31'
         LTORG
PARMS    DSECT
RC       DS    F
INPFMT   DS    CL8
INPDATE  DS    CL10
OUTFMT   DS    CL8
OUTDATE  DS    CL10
         END
