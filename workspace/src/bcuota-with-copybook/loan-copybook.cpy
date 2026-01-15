      ******************************************************************
      * COPYBOOK: LOAN-PARAMS
      * PURPOSE : Define input and output fields for loan payment
      *           computation.
      ******************************************************************

       01  LOAN-PARAMS.
           05  INPUT-MSG.
               10  PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
               10  INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
               10  TIMEYR        PIC S9(2)      USAGE IS DISPLAY.

           05  OUTPUT-MSG.
               10  PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
               10  ERROR-MSG     PIC X(20).
