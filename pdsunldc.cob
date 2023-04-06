       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDSUNLDC.
       AUTHOR. ED LISS.
       DATE-WRITTEN. MARCH, 2010.
       DATE-COMPILED.
       REMARKS. THIS PROGRAM READS A PDS AND WRITES EACH MEMBER WITH
            AN ./ ADD SUITABLE FOR RELOADING USING IEBUPDTX OR
            IEBUPDTE.  THE './ ADD NAME=XXXXXXXX' IS GENERATED FOR
            EACH MEMBER.  IF A EXEC_CARD PARM IS PASSED, THE PARM
            IS APPENDED I.E.  './ ADD NAME=XXXXXXXX,PARM'.  USING
            THE PARM ENABLES UTILITY CONTROL STATEMENTS FOR
            IEBUPDTE OR IEBUPDTX TO BE GENERATED.

            VERSION     DATE     COMMENTS
            -------   ---------  ---------------------------------
             1.0.0      03/2010  ORIGINAL VERSION
             1.1.0    06/17/2010 CHANGED SO ONLY NAME PARM
                                 IS GENERATED.  IF AN EXEC PARM
                                 IS PASSED, IT WILL BE ADDED TO
                                 THE ./ ADD NAME=,PARM
             1.2.0    03/21/2021 CLEANED UP CODE AND ADDED
                                 ISPF/RPF STATS TO REPORT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILE   ASSIGN TO UT-S-CARDOUT.
           SELECT PRINT-FILE  ASSIGN TO UT-S-PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  WS-CARD-OUT                 PIC X(80).

       FD  PRINT-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PF-PRINT-LINE               PIC X(133).

       WORKING-STORAGE SECTION.

       77  WS-MEMBER-COUNT             PIC 9(9)            VALUE +0.
       77  WS-CHAR-INDEX               PIC S9(4) COMP SYNC.
       77  WS-PARM-INDEX               PIC S9(4) COMP SYNC.
       77  WS-LINE-CNT                 PIC S9(4) COMP SYNC VALUE +57.
       77  WS-LINE-MAX                 PIC S9(4) COMP SYNC VALUE +56.

       01  WS-ADD-REC.
           05  FILLER                  PIC X(8)    VALUE './  ADD '.
           05  FILLER                  PIC X(5)    VALUE 'NAME='.
           05  WS-PARM-AREA            PIC X(67)   VALUE SPACES.
           05  FILLER            REDEFINES WS-PARM-AREA.
              10  WS-MEMBER-CHAR       PIC X OCCURS 67 TIMES.

       01  PH-PAGE-HEADING.
           05  FILLER                  PIC X(100) VALUE
               '1PDSUNLDC V1.2.0'.
       01  PH-LINE-2.
           05  FILLER                   PIC X(49)  VALUE
               '0MEMBER    USERID    CREATED    UPDATED      TIME'.
           05  FILLER                   PIC X(11)  VALUE
               '   COMMENTS'.
       01  DL-DETAIL-LINE.
           05  CC                       PIC X.
           05  DL-MEMBER                PIC X(8).
           05  FILLER                   PIC XX.
           05  DL-USERID                PIC X(8).
           05  FILLER                   PIC XX.
           05  DL-CREATED               PIC 9999B99B99.
           05  FILLER                   PIC X.
           05  DL-UPDATED               PIC 9999B99B99.
           05  FILLER                   PIC XX.
           05  DL-TIME-H                PIC 99.
           05  DL-TIME-SEP              PIC X.
           05  DL-TIME-M                PIC 99.
           05  FILLER                   PIC XX.
           05  DL-COMMENTS              PIC X(50).

       01  WS-DATE-EDIT-01.
           05  WS-DATE-EDIT-X           PIC X(8).
           05  FILLER         REDEFINES WS-DATE-EDIT-X.
               10  WS-DATE-EDIT         PIC 9(8).
           EJECT
       01  DATECNV-PARM COPY DATECNVC.
      ***#INCLUDE DATECNVC.COB
           EJECT
       01  PDSGET-PARAMETERS COPY GETPDSPA.
      ***#INCLUDE GETPDSPA.COB
       EJECT
       LINKAGE SECTION.
       01  EXEC-PARM.
           05  EXEC-LEN               PIC S9(4)  COMP.
           05  EXEC-CHAR              OCCURS 100 TIMES
                                      PIC X.
       PROCEDURE DIVISION USING EXEC-PARM.

       000-INITIATE.

           OPEN OUTPUT CARD-FILE, PRINT-FILE.
           IF EXEC-LEN > 55
               DISPLAY 'EXEC PARM EXCEEDS 55 CHARACTERS'
               MOVE 16 TO RETURN-CODE
               STOP RUN.
           IF EXEC-LEN > ZERO
              PERFORM 000-MOVE-PARM
                 VARYING WS-CHAR-INDEX FROM 1 BY 1
                   UNTIL WS-CHAR-INDEX > EXEC-LEN
              DISPLAY 'WITH EXEC PARM ' WS-PARM-AREA.
           PERFORM 100-OPEN-PDS.
           PERFORM 600-CLOSE-PDS.

           DISPLAY 'MEMBER PROCESSED=' WS-MEMBER-COUNT.
           DISPLAY 'PDSUNLOD CONCLUDED'.
           CLOSE CARD-FILE, PRINT-FILE.

           GOBACK.

       000-MOVE-PARM.
           MOVE EXEC-CHAR(WS-CHAR-INDEX) TO
                WS-MEMBER-CHAR(WS-CHAR-INDEX).

       100-OPEN-PDS.
           MOVE PDSGET-REQUEST-OPEN TO PDSGET-REQUEST.
           MOVE 'PDSIN  ' TO PDSGET-MEMBER.
           PERFORM 900-CALL-PDSGET.
           IF RETURN-CODE EQUAL 0
               PERFORM 200-START-DIR
           ELSE
           IF RETURN-CODE EQUAL 4
               DISPLAY 'OPEN FAILED (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'OPEN FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       200-START-DIR.
           MOVE PDSGET-REQUEST-START TO PDSGET-REQUEST.
           PERFORM 900-CALL-PDSGET.
           IF RETURN-CODE EQUAL 0
               PERFORM 300-NEXT-DIR
           ELSE
           IF RETURN-CODE EQUAL 4
               DISPLAY 'START FAILED (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'START FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       300-NEXT-DIR.
           MOVE ZERO TO RETURN-CODE.
           PERFORM 310-NEXT-DIR
               UNTIL RETURN-CODE NOT = ZERO.
       310-NEXT-DIR.
           MOVE PDSGET-REQUEST-NEXT  TO PDSGET-REQUEST.
           PERFORM 900-CALL-PDSGET.
           IF RETURN-CODE EQUAL 0
               DISPLAY 'MEMBER NAME=' PDSGET-MEMBER
               PERFORM 400-LOCATE-MEMBER
               MOVE ZERO TO RETURN-CODE
           ELSE
           IF RETURN-CODE EQUAL 4
               DISPLAY 'END OF DIRECTORY (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'NEXT FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       400-LOCATE-MEMBER.
           MOVE PDSGET-REQUEST-LOCATE TO PDSGET-REQUEST.
           PERFORM 900-CALL-PDSGET.
           IF RETURN-CODE EQUAL 0
               PERFORM 700-PROCESS-STATS THRU 700-EXIT
               PERFORM 500-READ-MEMBER
           ELSE
           IF RETURN-CODE EQUAL 4
               DISPLAY 'MEMBER NOT FOUND (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'LOCATE FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       500-READ-MEMBER.
           MOVE PDSGET-REQUEST-READ TO PDSGET-REQUEST.
           IF EXEC-LEN = ZERO
               MOVE PDSGET-MEMBER   TO WS-PARM-AREA
           ELSE
               PERFORM 501-MOVE-TEXT.
           WRITE WS-CARD-OUT FROM WS-ADD-REC.
           ADD 1 TO WS-MEMBER-COUNT.
           PERFORM 900-CALL-PDSGET.
           PERFORM 510-READ-MEMBER
               UNTIL RETURN-CODE NOT EQUAL 0.
           IF RETURN-CODE EQUAL 4
               DISPLAY 'END OF FILE ON MEMBER (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'READ FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       501-MOVE-TEXT.
           MOVE PDSGET-MEMBER       TO WS-PARM-AREA.
           MOVE 1                   TO WS-CHAR-INDEX.
           PERFORM 502-FIND-BLANK
              UNTIL WS-MEMBER-CHAR(WS-CHAR-INDEX) = ' '.
           MOVE ','                 TO WS-MEMBER-CHAR(WS-CHAR-INDEX).
           ADD 1                    TO WS-CHAR-INDEX.
           PERFORM 503-MOVE-PARM
              VARYING WS-PARM-INDEX FROM 1 BY 1
                UNTIL WS-PARM-INDEX > EXEC-LEN.

       502-FIND-BLANK.
           ADD 1                    TO WS-CHAR-INDEX.

       503-MOVE-PARM.
           MOVE EXEC-CHAR (WS-PARM-INDEX) TO
                WS-MEMBER-CHAR (WS-CHAR-INDEX).
           ADD 1                    TO WS-CHAR-INDEX.

       510-READ-MEMBER.
           WRITE WS-CARD-OUT FROM PDSGET-RECORD80.
           PERFORM 900-CALL-PDSGET.

       600-CLOSE-PDS.
           MOVE PDSGET-REQUEST-CLOSE TO PDSGET-REQUEST.
           PERFORM 900-CALL-PDSGET.
           IF RETURN-CODE EQUAL 4
               DISPLAY 'CLOSE FAILED (RC=4)'
           ELSE
           IF RETURN-CODE EQUAL 8
               DISPLAY 'CLOSE FAILED (RC=8); EXECUTION TERMINATED'
               STOP RUN.

       700-PROCESS-STATS.
           MOVE SPACES             TO DL-DETAIL-LINE.
           MOVE PDSGET-MEMBER      TO DL-MEMBER.
           MOVE PDS-USER-ID        TO DL-USERID.
           IF PDS-USER-ID = SPACES
               GO TO 700-PRINT.
           MOVE PDS-DATE-CREATED   TO DC-INPUT-DATE.
           MOVE 'YYYYDDD '         TO DC-INPUT-FORMAT.
           MOVE 'YYYYMMDD'         TO DC-OUTPUT-FORMAT.
           CALL 'DATECNV'  USING   DATECNV-PARM.
           MOVE DC-OUTPUT-DATE     TO WS-DATE-EDIT-X.
           MOVE WS-DATE-EDIT       TO DL-CREATED.
           EXAMINE DL-CREATED   REPLACING ALL  ' ' BY '/'.
           MOVE PDS-DATE-UPDATED   TO DC-INPUT-DATE.
           MOVE 'YYYYDDD '         TO DC-INPUT-FORMAT.
           MOVE 'YYYYMMDD'         TO DC-OUTPUT-FORMAT.
           CALL 'DATECNV'  USING   DATECNV-PARM.
           MOVE DC-OUTPUT-DATE     TO WS-DATE-EDIT-X.
           MOVE WS-DATE-EDIT       TO DL-UPDATED.
           EXAMINE DL-UPDATED   REPLACING ALL  ' ' BY '/'.

           MOVE ZERO               TO PDS-TIME-WORK.
           MOVE PDS-TIME-CHANGED-H TO PDS-TIME-CHAR.
           MOVE PDS-TIME-WORK      TO PDS-4-DIGIT-TIME.
           MOVE PDS-TIME           TO DL-TIME-H.
           MOVE ZERO               TO PDS-TIME-WORK.
           MOVE PDS-TIME-CHANGED-M TO PDS-TIME-CHAR.
           MOVE PDS-TIME-WORK      TO PDS-4-DIGIT-TIME.
           MOVE PDS-TIME           TO DL-TIME-M.
           MOVE ':'                TO DL-TIME-SEP.

       700-PRINT.
           PERFORM 800-PRINT-DETAIL.
       700-EXIT.
           EXIT.

       800-PRINT-DETAIL.
           IF WS-LINE-CNT > WS-LINE-MAX
               WRITE PF-PRINT-LINE FROM PH-PAGE-HEADING
               WRITE PF-PRINT-LINE FROM PH-LINE-2
               MOVE 3     TO WS-LINE-CNT.
           WRITE PF-PRINT-LINE FROM DL-DETAIL-LINE.
           ADD 1 TO WS-LINE-CNT.

       900-CALL-PDSGET.
           CALL 'GETPDS'   USING PDSGET-REQUEST,
                                 PDSGET-MEMBER,
                                 PDSGET-RECORD.
