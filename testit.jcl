//TESTIT1 JOB CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),NOTIFY=HERC01,
//    USER=HERC01,PASSWORD=CUL8TR
//*
//*
//*   TEST/DEMO PDSUTIL
//*
//*
//S1   EXEC PGM=PDSUNLDC
//STEPLIB DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//PRINTER  DD SYSOUT=*
//PDSIN    DD DSN=HERC01.PDSUTIL.COB,DISP=SHR
//CARDOUT  DD SYSOUT=*
//*
//TESTIT2 JOB CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),NOTIFY=HERC01,
//    USER=HERC01,PASSWORD=CUL8TR
//*
//S2   EXEC PGM=PDSUNLDP
//STEPLIB DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//        DD DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//PDSIN    DD DSN=HERC01.PDSUTIL.PLI,DISP=SHR
//CARDOUT  DD SYSOUT=*,DCB=BLKSIZE=80
//*
//TESTIT3 JOB CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),NOTIFY=HERC01,
//    USER=HERC01,PASSWORD=CUL8TR
//*
//S3   EXEC PGM=PDSCANR
//STEPLIB DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//        DD DSN=SYS1.PL1LIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//PDSIN    DD DSN=HERC01.PDSUTIL.PLI,DISP=SHR
//SYSIN    DD *
'PUT','PROC','GET'
