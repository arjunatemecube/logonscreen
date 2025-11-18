      ******************************************************************
      * COPYBOOK: SECURITY.CBL                                         *
      * DESCRIPTION: Defines security data structures for GnuCOBOL app.*
      ******************************************************************
       01  AUTH-FILE-RECORD.
           05  AUTH-USER-ID        PIC X(8).
           05  FILLER              PIC X(1) VALUE ':'.
           05  AUTH-PASSWORD       PIC X(8).
           05  FILLER              PIC X(1) VALUE ':'.
           05  AUTH-PERMISSIONS    PIC X(8).
               88 AUTH-IS-ADMIN-USER      VALUE 'ADMIN'.
               88 AUTH-IS-STANDARD-USER   VALUE 'STANDARD'.

       01  WS-SECURITY-FLAGS.
           05  WS-CURRENT-USER-ID  PIC X(8).
           05  WS-LOGIN-SUCCESS    PIC X(1) VALUE 'N'.
           05  WS-IS-ADMIN         PIC X(1) VALUE 'N'.
               88 IS-ADMIN-USER           VALUE 'Y'.
               88 IS-STANDARD-USER        VALUE 'N'.
           05  WS-CURRENT-LEVEL    PIC 9(1) VALUE 0.
               88 LEVEL-LOGIN               VALUE 0.
               88 LEVEL-MAIN-MENU           VALUE 1.
               88 LEVEL-SUB-MENU            VALUE 2.
           05  WS-MESSAGE          PIC X(60) VALUE SPACES.
           