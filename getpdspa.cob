      ******************* GETPDSPA V1.2.0 *****************************
      *****************************************************************
      *                                                               *
      * NAME: GETPDSPA - PARM LIST DEFINITIONS FOR GETPDS V1.3.0      *
      *                                                               *
      * NOTE: THERE IS A PL/1 VERSION OF THIS.  THEY SHOULD BE KEPT   *
      *       IN SYNC.                                                *
      *                                                               *
      *****************************************************************
      *                                                               *
      *  (C) COPYRIGHT 2017 EDWARD G LISS   ALL RIGHTS RESERVED       *
      *  (C) COPYRIGHT 2023 EDWARD G LISS   ALL RIGHTS RESERVED       *
      *                                                               *
      *  THIS SOURCE CODE AS WELL AS ANY OBJECT CODE RESULTING FROM   *
      *  THIS SOURCE CODE MAY BE DISTRIBUTED FREELY PROVIDED NO FEE   *
      *  IS CHARGED AND FOR NON-COMERCIAL PURPOSES.                   *
      *                                                               *
      *  FOR COMMERCIAL DISTRIBUTION RIGHTS, CONTACT THE COPYRIGHT    *
      *  OWNER.                                                       *
      *                                                               *
      *****************************************************************
      *                                                               *
      * REVISION HISTORY                                              *
      * ------  ----------------------------------------------------- *
      * V1.1.0  INITIAL VERSION.                                      *
      * V1.2.0  ADDED THE DEFINITION OF MEMBERS STATS AND ADDED       *
      *         WORK AREAS FOR STATISTICS CODE CONVERSION.            *
      *                                                               *
      *****************************************************************
       01  PDSGET-PARAMETERS.
           02  PDSGET-REQUEST          PIC S9(08)  COMP.
           02  PDSGET-MEMBER           PIC X(08).
           02  PDSGET-RECORD           PIC X(255).
           02  FILLER                  REDEFINES PDSGET-RECORD.
               03  PDSGET-RECORD80     PIC X(80).
               03  FILLER              PIC X(75).
           02  FILLER                  REDEFINES PDSGET-RECORD.
               03  PDS-C               PIC X.
               03  PDS-VERSION         PIC X.
               03  PDS-MOD             PIC X.
               03  PDS-NULL            PIC XX.
               03  PDS-DATE-CREATED    PIC S9(7) COMP-3.
               03  PDS-DATE-UPDATED    PIC S9(7) COMP-3.
               03  PDS-TIME-CHANGED-H  PIC X.
               03  PDS-TIME-CHANGED-M  PIC X.
               03  PDS-CURRENT-LINES   PIC S9(4) COMP.
               03  PDS-INITIAL-LINES   PIC S9(4) COMP.
               03  PDS-CHANGED-LINES   PIC S9(4) COMP.
               03  PDS-USER-ID         PIC X(8).

       01  PDSGET-REQUEST-CODES.
           02  PDSGET-REQUEST-OPEN     PIC S9(08)  COMP    VALUE +0.
           02  PDSGET-REQUEST-START    PIC S9(08)  COMP    VALUE +1.
           02  PDSGET-REQUEST-LOCATE   PIC S9(08)  COMP    VALUE +4.
           02  PDSGET-REQUEST-NEXT     PIC S9(08)  COMP    VALUE +5.
           02  PDSGET-REQUEST-READ     PIC S9(08)  COMP    VALUE +8.
           02  PDSGET-REQUEST-CLOSE    PIC S9(08)  COMP    VALUE +12.

      *
      * PDSGET_RETURN_CODE DEFINITIONS
      *       RETURN_CODE 0 = SUCCESSFUL COMPLETION OF REQUEST
      *       RETURN_CODE 8 = SERIOUS ERROR.
      *       RETURN_CODE 4 = DEPENDS ON REQUEST CODE
      *            REQUEST_CODE    MEANING
      *                  0         PDS COULD NOT BE OPENED.
      *                  1         PDS COULD NOT BE OPENED.
      *                  4         MEMBER NOT FOUND.
      *                  5         END OF DIRECTORY.
      *                  8         END OF CURRENT MEMBER.
      *

       01  PDSGET-WORK-AREAS.
      *
      *   PDS-TIME-CHANGED -H AND -M ARE UNSIGNED PACKED DECIMALS
      *   THEY MUST TO CONVERTED TO SIGNED PACK DECIMAL TO USE THEM.
      *   THE FOLLOWING AREAS CAN BE USED FOR THAT.
      *
      *   MOVE PDS-TIME-CHANGED-? TO PDS-TIME-CHAR.
      *   MOVE PDS-TIME-WORK      TO PDS-4 DIGIT-TIME.
      *
      *   PDS-TIME WILL HAVE A USABLE FORM OF THE HOUR OR MIN.
      *
      *   THIS CONVERSION MUST TAKE PLACE FOR BOTH THE -H AND =M
      *   PDS-TIME-CHANGED- FIELDS.
      *
           05  PDS-TIME-CONVERSION.
               10 PDS-TIME-WORK        PIC S9(5) COMP-3 VALUE ZERO.
           05  FILLER                  REDEFINES PDS-TIME-CONVERSION.
               10  FILLER              PIC X.
               10  PDS-TIME-CHAR       PIC X.
               10  FILLER              PIC X.

           05  PDS-4-DIGIT-TIME        PIC 9(4).
           05  FILLER                  REDEFINES PDS-4-DIGIT-TIME.
               10  FILLER              PIC X.
               10  PDS-TIME            PIC 99.
               10  FILLER              PIC X.

      ******************* GETPDSPA V1.2.0 END *************************
