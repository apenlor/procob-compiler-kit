      ******************************************************************
      *
      * Loan Calculator Batch Example
      * =============================
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bcuota-with-copybook.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN ASSIGN TO "bcuota-input"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT CUOTA ASSIGN TO "bcuota-output"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD LOAN.
           01 LOAN-FILE PIC X(56).

       FD CUOTA.
           01 CUOTA-FILE.
               05 CUOTA-ACC  PIC X(29).
               05 CUOTA-DEL PIC X(1).
               05 CUOTA-PAY PIC 9(7)V9(2).

       WORKING-STORAGE SECTION.
           01 WS-LOAN.
               05 WS-DATE PIC X(10).
               05 FILER PIC X(1).
               05 WS-ACC  PIC X(29).
               05 FILER PIC X(1).
               05 WS-AMT PIC 9(7).
               05 FILER PIC X(1).
               05 WS-INT PIC 9(2)V9(2).
               05 FILER PIC X(1).
               05 WS-YEAR PIC 9(2).

           01 WS-SDATE PIC 9(8) DISPLAY.
           01 WS-TDATE REDEFINES WS-SDATE.
               05 WS-TYEAR PIC X(4).
               05 WS-TMONTH PIC X(2).
               05 WS-TDAY PIC X(2).
           01 WS-CDATE.
               05 WS-CDAY PIC X(2).
               05 FILLER VALUE "-".
               05 WS-CMONTH PIC X(2).
                05 FILLER VALUE "-".
               05 WS-CYEAR PIC X(4).

           01 WS-EOF PIC X(1) VALUE "N".

           01 WS-DATE-LOGMSG.
               05 WS-MSG PIC X(14) VALUE "CURRENT DATE: ".
               05 WS-DATELOG PIC X(10).
           01 WS-END-LOGMSG.
               05 WS-MSG PIC X(25) VALUE "TOTAL RECORDS PROCESSED: ".
               05 WS-COUNTER PIC 9(9).
      ****************************************************************
       COPY LOAN-COPYBOOK.

       PROCEDURE DIVISION.

           PERFORM 100-INIT.

           PERFORM UNTIL WS-EOF='Y'
               READ LOAN INTO WS-LOAN
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF WS-DATE = WS-CDATE
                      PERFORM 200-PROCESS
                      WRITE CUOTA-FILE
                      END-WRITE
                   END-IF
               END-READ
           END-PERFORM.

           PERFORM 300-WRAPUP.

       100-INIT.

           OPEN INPUT LOAN.
           OPEN OUTPUT CUOTA.

           MOVE ZEROES TO WS-COUNTER.

           ACCEPT WS-SDATE FROM DATE YYYYMMDD.
           MOVE WS-TYEAR TO WS-CYEAR.
           MOVE WS-TMONTH TO WS-CMONTH.
           MOVE WS-TDAY TO WS-CDAY.
           MOVE WS-CDATE TO WS-DATELOG
           DISPLAY WS-DATE-LOGMSG.

       200-PROCESS.

           MOVE WS-AMT TO PRIN-AMT.
           MOVE WS-INT TO INT-RATE.
           MOVE WS-YEAR TO TIMEYR.

           CALL "loancalc-with-copybook" USING LOAN-PARAMS.

           IF RETURN-CODE > 0
              MOVE 'Y' TO WS-EOF
           END-IF.

           ADD 1 TO WS-COUNTER.
           MOVE WS-ACC TO CUOTA-ACC.
           MOVE "," TO CUOTA-DEL.
           MOVE PAYMENT TO CUOTA-PAY.

       300-WRAPUP.

           CLOSE LOAN.
           CLOSE CUOTA.
           DISPLAY WS-END-LOGMSG.

           MOVE 0 TO RETURN-CODE.

           GOBACK.
      ******************************************************************





