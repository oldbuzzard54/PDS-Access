PDSSTATS DSECT  ,                      MEMBER WITH ISPF USER DATA
*                                      OR THE RPF USER DATA
PDSC     DS    C                       CONTROL DATA
PDSVERS  DS    X                       ISPF/RPF VERSION
PDSMOD   DS    X                           MOFIFICATION NUMBER
PDSNULL  DS    2X                          NULLS (RECOGNIZE ISPF)
PDSDATEC DS    PL4                         DATE CREATED
PDSDATEU DS    PL4                         DATE UPDATED
PDSTIME  DS    XL2                         TIME LAST UPDATE
PDSRECU  DS    XL2                         CURRENT NUMBER OF LINES
PDSRECI  DS    XL2                         INITIAL NUMBER OF LINES
PDSRECM  DS    XL2                         RECORDS MODIFIED
PDSUSER  DS    XL10                        USERID + 2 BLANKS
         DS    CL2
PDSSTATL EQU   *-PDSSTATS
