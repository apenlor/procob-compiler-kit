      ******************************************************************
      *
      * Loan Calculator Subroutine
      * ==========================
      *
      * A sample program to demonstrate how to create a gRPC COBOL
      * microservice.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loancalc-with-copybook.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       01  WS-MSG.
           05 WS-ERROR      PIC X(01) VALUE 'N'.
           05 WS-MSG00      PIC X(20) VALUE 'OK'.
           05 WS-MSG10      PIC X(20) VALUE 'INVALID INT. RATE'.
           05 WS-MSG12      PIC X(20) VALUE 'INVALID NUMBER YEARS'.
       01  AUX-VARS.
           05 MONTHLY-RATE  USAGE IS COMP-2.
           05 AUX-X         USAGE IS COMP-2.
           05 AUX-Y         USAGE IS COMP-2.
           05 AUX-Z         USAGE IS COMP-2.

       LINKAGE SECTION.
      * Data to share with COBOL subroutines
       COPY LOAN-COPYBOOK.

       PROCEDURE DIVISION USING BY REFERENCE LOAN-PARAMS.
      * code goes here!

           PERFORM 100-INIT.
           IF WS-ERROR = 'N'
               PERFORM 200-PROCESS
           END-IF.
           PERFORM 300-WRAPUP.

       100-INIT.
           IF INT-RATE <= 0
               MOVE WS-MSG10 TO ERROR-MSG
               MOVE 10 TO RETURN-CODE
               MOVE 'Y' TO WS-ERROR
           ELSE
               IF TIMEYR <= 0
                   MOVE WS-MSG12 TO ERROR-MSG
                   MOVE 12 TO RETURN-CODE
                   MOVE 'Y' TO WS-ERROR
               END-IF
           END-IF.
       200-PROCESS.
           INITIALIZE AUX-VARS.
           COMPUTE MONTHLY-RATE = (INT-RATE / 12 / 100).
           COMPUTE AUX-X = ((1 + MONTHLY-RATE) ** (TIMEYR*12)).
           COMPUTE AUX-Y = AUX-X * MONTHLY-RATE.
           COMPUTE AUX-Z = (AUX-X - 1) / AUX-Y.
           COMPUTE PAYMENT = PRIN-AMT / AUX-Z.
           MOVE WS-MSG00 TO ERROR-MSG.
           MOVE 0 TO RETURN-CODE.

       300-WRAPUP.
           GOBACK.


