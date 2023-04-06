//INSTALL JOB 00,'PDSUTIL',CLASS=S,MSGCLASS=A,MSGLEVEL=(1,1),
//    USER=HERC01,PASSWORD=CUL8TR
//*********************************************************************
//*                                                                   *
//*   THIS JCL CREATES, COMPILES THE PDS MEMBER ACCESS ROUTINES       *
//*                                                                   *
//*   THE DATASETS CREATE BY RECEIVE.JCL WILL BE USED TO GENERATE     *
//*   EXECUTABLES INTO DATASET HERC01.PDSUTIL.LOADLIB ON PUB000.      *
//*                                                                   *
//*   YOU MAY CHANGE THE HIGH LEVEL QUALIFIER AND/OR THE VOLUME       *
//*   THE LOADLIB WILL BE CREATED ON.  IF YOU PLAN TO MOVE/COPY       *
//*   THE LOAD MODULES TO A DIFFERENT LOADLIB, YOU MIGHT WANT TO      *
//*   ADJUST THE BLKSIZE IN STEP S2.                                  *
//*                                                                   *
//*********************************************************************
//*
//S1     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE HERC01.PDSUTIL.LOADLIB NONVSAM PURGE
 SET MAXCC = 0
//S2     EXEC PGM=IEFBR14
//DD       DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=(NEW,CATLG,DELETE),
//         UNIT=SYSDA,VOL=SER=PUB000,SPACE=(TRK,(10,10,10)),
//         DCB=(RECFM=U,BLKSIZE=4096,DSORG=PO)
//*
//S3     EXEC ASMFCL,
//            MAC1='HERC01.PDSUTIL.ASM'
//ASM.SYSIN DD DSN=HERC01.PDSUTIL.ASM(DATECNV),DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PDSUTIL.LOADLIB(DATECNV),DISP=SHR
//*
//S4     EXEC ASMFCL,
//            MAC1='HERC01.PDSUTIL.ASM'
//ASM.SYSIN DD DSN=HERC01.PDSUTIL.ASM(GETPDS),DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PDSUTIL.LOADLIB(GETPDS),DISP=SHR
//*
//S5     EXEC COBUCL,
//           PARM.COB='LOAD,SUPMAP,SIZE=2048K,BUF=1024K,LIB',
//           PARM.LKED='MAP'
//COB.SYSIN DD DSN=HERC01.PDSUTIL.COB(PDSUNLDC),DISP=SHR
//COB.SYSLIB DD DSN=HERC01.PDSUTIL.COB,DISP=SHR
//COB.SYSPUNCH DD DUMMY
//LKED.SYSLIB DD
//          DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PDSUTIL.LOADLIB(PDSUNLDC),DISP=SHR
//*
//S6     EXEC PL1LFCL,
//            PARM.PL1L='MACRO'
//PL1L.SYSIN DD DSN=HERC01.PDSUTIL.PLI(PDSUNLDP),DISP=SHR
//PL1L.SYSLIB DD DSN=HERC01.PDSUTIL.PLI,DISP=SHR
//LKED.SYSLIB  DD
//             DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PDSUTIL.LOADLIB(PDSUNLDP),DISP=SHR
//*
//S7     EXEC PL1LFCL,
//            PARM.PL1L='MACRO'
//PL1L.SYSIN DD DSN=HERC01.PDSUTIL.PLI(PDSCANR),DISP=SHR
//PL1L.SYSLIB DD DSN=HERC01.PDSUTIL.PLI,DISP=SHR
//LKED.SYSLIB  DD
//             DD DSN=HERC01.PDSUTIL.LOADLIB,DISP=SHR
//LKED.SYSLMOD DD DSN=HERC01.PDSUTIL.LOADLIB(PDSCANR),DISP=SHR
//*