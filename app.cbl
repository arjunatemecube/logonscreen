      ******************************************************************
      * PROGRAM: MENUAPP.CBL                                           *
      * COMPILER: GnuCOBOL 3.x+ (using SCREEN SECTION)                 *
      * DESCRIPTION: Interactive menu with 2 levels and permissioning. *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUAPP.
       AUTHOR. ARJUN KALANI.

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
       01  SUB-MENU-SCREEN BLANK SCREEN.
           05  LINE 2 COLUMN 25 VALUE "CICA SUB MENU - LEVEL 2".
           05  LINE 4 COLUMN 5 VALUE "Current User:".
           05  LINE 4 COLUMN 20 PIC X(8) FROM WS-CURRENT-USER-ID.

           05  LINE 7 COLUMN 5 VALUE "Enter Choice:".
           05  LINE 7 COLUMN 20 PIC X(1) USING WS-MENU-CHOICE AUTO.

           05  LINE 10 COLUMN 10 VALUE "1 - Data Entry A".
           05  LINE 11 COLUMN 10 VALUE "2 - Data Query B".

           05  LINE 15 COLUMN 5 VALUE WS-MESSAGE FOREGROUND-COLOR 4.
           05  LINE 23 COLUMN 5 VALUE SCREEN-LINE-23.   
       78  WS-MESSAGE  PIC X(60) VALUE SPACES. 
       78  SCREEN-LINE-23   PIC X(78) VALUE '9 to return to Main Menu.'.