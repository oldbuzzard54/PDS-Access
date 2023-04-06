//INSTALL2 JOB 00,'PDSUTIL',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),
//    USER=HERCEL,PASSWORD=CUL8TR
//*********************************************************************
//*                                                                   *
//*   THIS JCL RELINKS THE PDSUTIL.LOADLIB FOR A DIFFERENT BLKSIZE    *
//*                                                                   *
//*   YOU MAY CHANGE THE HIGH LEVEL QUALIFIER AND/OR THE VOLUME       *
//*   THE LOADLIB WILL BE CREATED ON.                                 *
//*                                                                   *
//*********************************************************************
//*
//S1     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE HERCEL.PDSUTIL2.LOADLIB NONVSAM PURGE
 SET MAXCC = 0
// EXEC PGM=IEWL,PARM='LIST,MAP,XREF'
//SYSPRINT DD SYSOUT=*
//SYSLMOD  DD DSN=HERCEL.PDSUTIL2.LOADLIB,DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,VOL=SER=EVOL03,SPACE=(TRK,(100,100,2)),
//         DCB=(RECFM=U,BLKSIZE=19069)
//SYSLIB   DD DSN=SYS1.PL1LIB,DISP=SHR
//         DD DSN=HERCEL.PDSUTIL.LOADLIB,DISP=SHR
//SYSUT1 DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLIN DD *
 INCLUDE SYSLIB(PDSCANR)
 NAME PDSCANR(R)
 INCLUDE SYSLIB(PDSUNLDC)
 NAME PDSUNLDC(R)
 INCLUDE SYSLIB(PDSUNLDP)
 NAME PDSUNLDP(R)
//*
