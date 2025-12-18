//RESTORE  JOB 'ME',MSGCLASS=?,MSGLEVEL=(1,1),CLASS=A,COND=(0,NE),
//        USER=user,PASSWORD=password
//********************************************************************
//*
//*  THIS JOB INSTALLS THE ZIP FOR PDSACES V2 WHICH WILL BE A COMBO
//*     OF PDSUNLD AND PDSCANR.  PDSUNLD IS A SUPERSET OF PDSACES.
//*     THREE DEMO PROGRAMS ARE INCLUDED.
//*
//********************************************************************
//*
//*  BEFORE SUBMITTING THIS JCL, CHANGE
//*  - CHANGE USER and password
//*  - CHANGE VOLSER
//*  - CHANGE MSGCLASS
//*
//********************************************************************
//*
//*    DELETE DATASET THAT WILL BE CREATED IN THIS JOB
//*
//********************************************************************
//STEP01  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 DELETE 'user.PDSACES.ASM'     NONVSAM PURGE
 DELETE 'user.PDSACES.COB'     NONVSAM PURGE
 DELETE 'user.PDSACES.INCLLIB' NONVSAM PURGE
 DELETE 'user.PDSACES.LOADLIB' NONVSAM PURGE
 DELETE 'user.PDSACES.MACLIB'  NONVSAM PURGE
 DELETE 'user.PDSACES.NCALIB'  NONVSAM PURGE
 DELETE 'user.PDSACES.PLI'     NONVSAM PURGE
 DELETE 'user.PDSACES.PDS'     NONVSAM PURGE
 DELETE 'user.PDSACES.PDSXMI'  NONVSAM PURGE
 SET MAXCC = 0
//********************************************************************
//*
//*    LIST ZIP CONTENTS
//*
//********************************************************************
//STEP02  EXEC PGM=MINIUNZ,REGION=7M,
//             PARM='-L ZIPFILE PDSOUT'
//STDOUT    DD SYSOUT=*
//ZIPFILE   DD DISP=SHR,DSN=user.PDSACES.ZIP
//PDSOUT    DD DUMMY
//********************************************************************
//*
//*    UNZIP CONTENTS PDS
//*
//********************************************************************
//STEP03  EXEC PGM=MINIUNZ,REGION=7M,
//             PARM='ZIPFILE SEQOUT PDS'
//STDOUT    DD SYSOUT=*
//ZIPFILE   DD DISP=SHR,DSN=user.PDSACES.ZIP
//SEQOUT    DD DSN=user.PDSACES.PDSXMI,DISP=(NEW,CATLG,DELETE),
//             UNIT=DISK,VOL=SER=volser,SPACE=(TRK,(10,10),RLSE),
//             DCB=(DSORG=PS,LRECL=80,BLKSIZE=3120,RECFM=FB)
//*
//********************************************************************
//*
//*  RECEIVE ALL THE XMI FILES TO PDS
//*
//********************************************************************
//TRANS    EXEC PGM=IKJEFT01,REGION=4096K
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 PROFILE PREFIX(HERC01)
 RECEIVE  INDATASET(PDSACES.PDSXMI)
 RECEIVE  INDATASET(PDSACES.PDS(ASM))
 RECEIVE  INDATASET(PDSACES.PDS(COB))
 RECEIVE  INDATASET(PDSACES.PDS(INCLLIB))
 RECEIVE  INDATASET(PDSACES.PDS(LOADLIB))
 RECEIVE  INDATASET(PDSACES.PDS(MACLIB))
 RECEIVE  INDATASET(PDSACES.PDS(NCALIB))
 RECEIVE  INDATASET(PDSACES.PDS(PLI))
//*
