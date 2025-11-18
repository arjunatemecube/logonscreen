LOG-ON IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGONAPP.
       AUTHOR. ARJUN KALANI.
      ******************************************************************
      * PROGRAM: LOGONAPP.CBL                                           *
      * COMPILER: GnuCOBOL 3.x+ (using SCREEN SECTION)                 *
      * DESCRIPTION: Interactive menu with 2 levels and permissioning. *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTH-FILE ASSIGN TO 'AUTHFILE.dat' 
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  AUTH-FILE.
       01  AUTH-REC                PIC X(18).

       WORKING-STORAGE SECTION.
           COPY SECURITY.


       SCREEN SECTION.
      ******************************************************************
      * 1. LOGIN SCREEN DEFINITION                                     *
      ******************************************************************
       01  WS-USER-INPUT.
           05  WS-INPUT-USER-ID    PIC X(8).
           05  WS-INPUT-PASSWORD   PIC X(8).
           05  WS-MENU-CHOICE      PIC X(1).
       01  SUB-MENU-SCREEN BLANK SCREEN.
           05  LINE 2 COLUMN 25 VALUE "CICA SUB MENU - LEVEL 2".
           05  LINE 4 COLUMN 5 VALUE "Current User:".
           05  LINE 4 COLUMN 20 PIC X(8) FROM WS-CURRENT-USER-ID.
           05  LINE 7 COLUMN 5 VALUE "Enter Choice:".
           05  LINE 7 COLUMN 20 PIC X(1) USING WS-MENU-CHOICE AUTO.
           05  LINE 10 COLUMN 10 VALUE "1 - Data Entry A".
           05  LINE 11 COLUMN 10 VALUE "2 - Data Query B".
           05  LINE 12 COL 10 PIC X(60) FROM                             
      -    "WS-MESSAGE  FOREGROUND-COLOR 4".
           05  LINE 23 COLUMN 5 VALUE "Enter to Logon. Q to Quit.".
      ******************************************************************
      * 2. MAIN MENU (LEVEL 1) DEFINITION                              *
      ******************************************************************
       01  MAIN-MENU-SCREEN BLANK SCREEN.
           05  LINE 2 COLUMN 25 VALUE "CICA MAIN MENU - LEVEL 1".
           05  LINE 4 COLUMN 5 VALUE "Welcome, User:".
           05  LINE 4 COLUMN 20 PIC X(8) FROM WS-CURRENT-USER-ID.
           05  LINE 7 COLUMN 5 VALUE "Enter Choice:".
           05  LINE 7 COLUMN 20 PIC X(1) USING WS-MENU-CHOICE AUTO.
           05  LINE 10 COLUMN 10 VALUE "1 - Sub Menu 1".
           05  LINE 11 COLUMN 10 VALUE "2 - Run Report X".
      * Dynamic option based on permission flag
           05  ADMIN-OPTION.
               10  LINE 12 COLUMN 10 VALUE "3 - System Admin 
      -         "(Restricted)" FOREGROUND-COLOR 2.
           05  LINE 15 COLUMN 5 PIC X(60) FROM 
      -     "WS-MESSAGE FOREGROUND-COLOR 4".
           05  LINE 23 COLUMN 5 VALUE "Enter to Select. Q to Quit.".
      ******************************************************************
      * 3. SUB MENU (LEVEL 2) DEFINITION                               *
      ******************************************************************
       01  SUB-MENU-SCREEN BLANK SCREEN.
           05  LINE 2 COLUMN 25 VALUE "CICA SUB MENU - LEVEL 2".
           05  LINE 4 COLUMN 5 VALUE "Current User:".
           05  LINE 4 COLUMN 20 PIC X(8) FROM WS-CURRENT-USER-ID.
           05  LINE 7 COLUMN 5 VALUE "Enter Choice:".
           05  LINE 7 COLUMN 20 PIC X(1) USING WS-MENU-CHOICE AUTO.
           05  LINE 10 COLUMN 10 VALUE "1 - Data Entry A".
           05  LINE 11 COLUMN 10 VALUE "2 - Data Query B".
           05  LINE 15 COLUMN 5 PIC X(60) FROM 
      -     "WS-MESSAGE FOREGROUND-COLOR 4".
           05  LINE 23 COLUMN 5 VALUE 
      -     "9 to return to Main Menu. Q to Quit.".

       PROCEDURE DIVISION.
           PERFORM 1000-MAIN-LOGIC-LOOP.
           STOP RUN.

       1000-MAIN-LOGIC-LOOP.
      ******************************************************************
      * Main application loop managed by WS-CURRENT-LEVEL.             *
      ******************************************************************
           MOVE 0 TO WS-CURRENT-LEVEL
           PERFORM UNTIL WS-MENU-CHOICE = 'Q' OR WS-MENU-CHOICE = 'q'
      -        EVALUATE TRUE
      -            WHEN LEVEL-LOGIN
      -                 PERFORM 2000-PROCESS-LOGIN
      -             WHEN LEVEL-MAIN-MENU
      -                 PERFORM 3000-PROCESS-MAIN-MENU
      -             WHEN LEVEL-SUB-MENU
      -                 PERFORM 4000-PROCESS-SUB-MENU
      -         END-EVALUATE
      -    END-PERFORM.
           DISPLAY LOGIN-SCREEN.
           DISPLAY " " LINE 12 COLUMN 10 WITH BLANK LINE.
           DISPLAY "Application Terminated Normally." LINE 12 COLUMN 10.
           ACCEPT LOGIN-SCREEN.

       2000-PROCESS-LOGIN.
      ******************************************************************
      * Display login, get input, and validate credentials.            *
      ******************************************************************
           MOVE SPACES TO WS-INPUT-USER-ID WS-INPUT-PASSWORD WS-MESSAGE.
           MOVE 0 TO WS-CURRENT-LEVEL.
           DISPLAY LOGIN-SCREEN.
           ACCEPT LOGIN-SCREEN.

           IF FUNCTION TRIM(WS-INPUT-USER-ID) = SPACES OR 
      -       FUNCTION TRIM(WS-INPUT-PASSWORD) = SPACES
      -       MOVE "User ID and Password are required." TO WS-MESSAGE
      -    END-IF.

           PERFORM 2100-AUTHENTICATE-USER.

           IF WS-LOGIN-SUCCESS = 'Y'
      -        MOVE 1 TO WS-CURRENT-LEVEL
      -        MOVE SPACES TO WS-MESSAGE
      -    ELSE
               MOVE "Invalid User ID or Password." TO WS-MESSAGE
      -    END-IF.
           STOP RUN.

       2100-AUTHENTICATE-USER.
      ******************************************************************
      * Opens AUTHFILE.dat and searches for the user.                  *
      ******************************************************************
           MOVE 'N' TO WS-LOGIN-SUCCESS WS-IS-ADMIN.

           OPEN INPUT AUTH-FILE.
           IF AUTH-FILE-STATUS <>  '00'
      -        MOVE "Error opening auth file. Contact administrator."  
      -        TO WS-MESSAGE CLOSE AUTH-FILE 
           END-IF.
      
           PERFORM UNTIL AUTH-FILE-STATUS = '10'  
      -    " *> 10 = EOF
      -        READ AUTH-FILE INTO AUTH-FILE-RECORD
      -            AT END MOVE '10' TO AUTH-FILE-STATUS
      -            NOT AT END
      -                IF FUNCTION TRIM(WS-INPUT-USER-ID) =                
      -                FUNCTION TRIM(AUTH-USER-ID)
                          AND FUNCTION TRIM(WS-INPUT-PASSWORD) =        
      -                    FUNCTION TRIM(AUTH-PASSWORD)
      -                    MOVE 'Y' TO WS-LOGIN-SUCCESS
      -                    MOVE FUNCTION TRIM(AUTH-USER-ID) TO         
      -                    WS-CURRENT-USER-ID
      -                    IF AUTH-IS-ADMIN-USER
      -                       MOVE 'Y' TO WS-IS-ADMIN
      -                    ELSE
      -                       MOVE 'N' TO WS-IS-ADMIN
      -                    END-IF
      -                    MOVE '10' TO AUTH-FILE-STATUS                
      -                    "*> Found, exit loop"
      -                END-IF
      -        END-READ
           END-PERFORM.

           CLOSE AUTH-FILE.
           STOP RUN.

       3000-PROCESS-MAIN-MENU.
      ******************************************************************
      * Display Main Menu, handle option selection and security check. *
      ******************************************************************
           MOVE SPACES TO WS-MENU-CHOICE.
           DISPLAY MAIN-MENU-SCREEN.

      * Dynamically manage Option 3 display based on security
           IF IS-ADMIN-USER
      -       DISPLAY ADMIN-OPTION
      -    ELSE
      -       DISPLAY SPACES LINE 12 COLUMN 10 SIZE 35
      -    END-IF.

           ACCEPT MAIN-MENU-SCREEN.

           EVALUATE WS-MENU-CHOICE
      -        WHEN '1'
      -            MOVE 2 TO WS-CURRENT-LEVEL
      -            MOVE "Entering Sub Menu 1." TO WS-MESSAGE
      -        WHEN '2'
      -            MOVE "Running Report X..." TO WS-MESSAGE
      -            PERFORM 3100-SIMULATE-TASK
      -        WHEN '3'
      -            IF IS-ADMIN-USER
      -               MOVE "Admin Task Executed Successfully." 
      -               TO WS-MESSAGE
      -               PERFORM 3100-SIMULATE-TASK
      -            ELSE
      -               MOVE "ERROR: Permission Denied for Option 3." 
      -               TO WS-MESSAGE
      -             END-IF
      -         WHEN 'Q'
      -         WHEN 'q'
      -             EXIT PARAGRAPH
      -         WHEN OTHER
      -             MOVE "Invalid option. Select 1, 2, 3, or Q." 
      -             TO WS-MESSAGE
      -    END-EVALUATE.
           STOP RUN.

       3100-SIMULATE-TASK.
           DISPLAY WS-MESSAGE LINE 15 COLUMN 5.
           DISPLAY "Press Enter to continue..." LINE 16 COLUMN 5.
           ACCEPT OMITTED LINE 16 COLUMN 28.
           MOVE SPACES TO WS-MESSAGE.
           STOP RUN.

       4000-PROCESS-SUB-MENU.
      ******************************************************************
      * Display Sub Menu, handle option selection and return.          *
      ******************************************************************
           MOVE SPACES TO WS-MENU-CHOICE.
           DISPLAY SUB-MENU-SCREEN.
           ACCEPT SUB-MENU-SCREEN.

	       EVALUATE WS-MENU-CHOICE
	  -    		WHEN '1'
	  -            	MOVE 2 TO WS-CURRENT-LEVEL
	  -		       	MOVE "Entering Sub Menu 1." TO WS-MESSAGE
	  -    		WHEN '2'
	  -        		MOVE "Running Report X..." TO WS-MESSAGE
      -             PERFORM 3100-SIMULATE-TASK
	  -        	WHEN '3'
	  -    		IF IS-ADMIN-USER
	  -    		   MOVE "Admin Task Executed Successfully." 
      -            TO WS-MESSAGE
	  -    		   PERFORM 3100-SIMULATE-TASK
	  -    		ELSE
	  -    		   MOVE "ERROR: Permission Denied for Option 3." 
      -            TO WS-MESSAGE
	  -		    END-IF
	  -    		WHEN 'Q'
	  -    		WHEN 'q'
	  -    		EXIT PARAGRAPH
	  -    		WHEN OTHER
	  -    		MOVE "Invalid option. Select 1, 2, 3, or Q." 
      -            TO WS-MESSAGE
	  -    END-EVALUATE.
	       STOP RUN.
