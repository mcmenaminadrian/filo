\
\ Variables
\
VARIABLE ROWS
VARIABLE COLUMNS
VARIABLE CX
VARIABLE CY
HEX 5413 CONSTANT TIOCGWINSZ
DECIMAL 0 CONSTANT STDIN
DECIMAL 1 CONSTANT STDOUT
DECIMAL 2 CONSTANT STDERR

: welcomemsg
C" Filo editor -- (c) Adrian McMenamin, 2024"
;

\
\ Utility words					      *
\

\ check for error
: DROPERR
0<> IF ABORT" Halting on error" THEN ;

\ is character on stack CTRL Key for that letter
: CTRL-KEY ( n c -- f )
  [ HEX 1F DECIMAL ] LITERAL AND
  = IF TRUE ELSE FALSE THEN ;


: ISCNTRL ( c -- f)
  DUP
  32 <
  IF
    DROP TRUE
  ELSE
    126 >
    IF
      TRUE
    ELSE
      FALSE
    THEN
  THEN ;


\
\ terminal section
\

: GET-WINDOW-SIZE
( -- )
[ DECIMAL 16 ] LITERAL ALLOCATE DROPERR
>R
STDOUT TIOCGWINSZ R@ IOCTL
0=
IF
  DROP
  R@ C@ R@ 1+ C@ [ HEX FF ] LITERAL * + ROWS !
  R@ 2+ C@ R@ 3 + C@ [ HEX FF ] LITERAL * + COLUMNS !
  R> FREE DROPERR
ELSE
  ." ERRNO: " . CR
  R> FREE DROPERR
  ABORT" IOCTL failed"
THEN
;

: EDITOR-READ-KEY
  ( -- c )
  KEYRAW
;

: CRLF
  ( -- )
  [ DECIMAL 13 ] LITERAL EMIT [ DECIMAL 10 ] LITERAL EMIT
;

\
\ append buffer
\

VARIABLE BUFFER_PTR
VARIABLE BUFFER_LEN

: ABFREE
( -- )
BUFFER_PTR @ FREE DROPERR
0 BUFFER_LEN !
;

: dupx
DUP . ;

: ABAPPEND
( char* len -- )
>R                         \ store length to be added on return stack
BUFFER_LEN @               \ get existing length
0<> IF                     \ if ... not 0
  BUFFER_PTR @ 
  BUFFER_LEN @ R@ + RESIZE \ get a buffer of new size
  DROPERR                  \ get a buffer of new size
  R@ BUFFER_LEN @ + 
  BUFFER_LEN !
THEN
BUFFER_PTR !               \ store address
BUFFER_LEN @ R@ -          \ calculate offset for copying
BUFFER_PTR @ +             \ add to get start point
SWAP                       \ save that further back on the stack
R> 0 DO                    \ start loop
  DUP                      \ duplicate string base address
  I + C@                   \ character in string
  2 PICK                   \ get start from stack
  I + C!                   \ copy character
LOOP 
2DROP
;

\
\ output section
\

: CLEARSCREEN
( -- )
7 ALLOCATE DROPERR
>R
\ ESC [2J - clear entire screen
[ decimal 27 ] literal R@ C!
CHAR [ R@ 1+ C!
CHAR 2 R@ 2+ C!
CHAR J R@ 3 + C!
\ ESC [H - Cursor to top of screen (1 ,1)
[ decimal 27 ] literal R@ 4 + C!
CHAR [ R@ 5 + C!
CHAR H R@ 6 + C!
R@ 7 TYPE
R> FREE DROPERR
;

: EXTEND_BUFFER_NO_ADD_LEN
( n --  )
BUFFER_LEN @ +                                       \ get new length we seek
BUFFER_PTR @ SWAP RESIZE DROPERR                     \ get resized buffer
BUFFER_PTR !                                         \ store new buffer address
;

: EXTEND_BUFFER
( n -- )
DUP >R                                               \ duplicate and store extension length
BUFFER_LEN @ +                                       \ get new length we seek
BUFFER_PTR @ SWAP RESIZE DROPERR                     \ get resized buffer
BUFFER_PTR !                                         \ store new buffer address
R> BUFFER_LEN @ + BUFFER_LEN !                       \ store new buffer length
;

: EDITOR-DRAW-ROWS
( -- )
ROWS @ 1+ 1 DO
  ROWS @ 3 / I =
  IF  \ welcome message
    welcomemsg @ EXTEND_BUFFER_NO_ADD_LEN
    welcomemsg 8 + BUFFER_PTR @ BUFFER_LEN @ + welcomemsg @ MOVE
    BUFFER_LEN @ welcomemsg @ + BUFFER_LEN !
    5 EXTEND_BUFFER
  ELSE \ tilde
    6 EXTEND_BUFFER
    CHAR ~ BUFFER_PTR @ BUFFER_LEN @ + 6 - C!
  THEN
  \ ESC[K - redraw line
  [ decimal 27 ] literal BUFFER_PTR @ BUFFER_LEN @ 5 - + C!
  CHAR [ BUFFER_PTR @ BUFFER_LEN @ 4 - + C!
  CHAR K BUFFER_PTR @ BUFFER_LEN @ 3 - + C!
  I ROWS @ <
  IF
    [ DECIMAL 13 ] literal BUFFER_PTR @ BUFFER_LEN @ + 2- C! [ DECIMAL 10 ] literal BUFFER_PTR @ BUFFER_LEN @ + 1- C!
  ELSE
    BUFFER_LEN @ 2- BUFFER_LEN !
  THEN
LOOP
;
  
: PRINT_BUFFER
( -- )
BUFFER_PTR @ BUFFER_LEN @ TYPE
;


: MOVE_CURSOR
( -- )
S\" \e[" ABAPPEND                                     \ first part of escape sequence
CY @ 1+ >STRING DUP @ SWAP 8 + SWAP ABAPPEND               \ add y
S" ;" ABAPPEND
CX @ 1+ >STRING DUP @ SWAP 8 + SWAP ABAPPEND               \ add x
S" H" ABAPPEND
;

: EDITOR-RESET-SCREEN
  ( -- )
[ decimal 64 ] literal  ALLOCATE DROPERR BUFFER_PTR !
\ ESC [?25l - make cursor disappear
[ decimal 27 ] literal BUFFER_PTR @ C!
CHAR [ BUFFER_PTR @ 1+ C!
CHAR ? BUFFER_PTR @ 2+ C!
CHAR 2 BUFFER_PTR @ 3 + C!
CHAR 5 BUFFER_PTR @ 4 + C!
CHAR l BUFFER_PTR @ 5 + C!
[ decimal 6 ] literal BUFFER_LEN !
EDITOR-DRAW-ROWS
MOVE_CURSOR
6 EXTEND_BUFFER
\ ESC [?25h - cursor reappear
[ decimal 27 ] literal BUFFER_PTR @ BUFFER_LEN @ 6 - + C!
CHAR [ BUFFER_PTR @ BUFFER_LEN @ 5 - + C!
CHAR ? BUFFER_PTR @  BUFFER_LEN @ 4 -  + C!
CHAR 2 BUFFER_PTR @  BUFFER_LEN @ 3 - + C!
CHAR 5 BUFFER_PTR @  BUFFER_LEN @ 2-  + C!
CHAR h BUFFER_PTR @  BUFFER_LEN @ 1- + C!
PRINT_BUFFER
BUFFER_PTR @ FREE DROPERR
0 BUFFER_LEN !
;


: EDITOR-REFRESH-SCREEN
( -- )
EDITOR-RESET-SCREEN
  
;

\
\ input section	
\

: EDITOR-PROCESS-KEYPRESS
  ( --  )
  EDITOR-READ-KEY
  CASE
    CHAR Q [ HEX 1F ] LITERAL AND  OF
      EDITOR-RESET-SCREEN
      CLEARSCREEN
      ABORT" Leaving FILO " CRLF  
    ENDOF
    CHAR a OF
      CX @ 1- CX !
    ENDOF
    CHAR d OF
      CX @ 1+ CX !
    ENDOF
    CHAR w OF
      CY @ 1- CY !
    ENDOF
    CHAR s OF
      CY @ 1+ CY !
    ENDOF
  ENDCASE

;


\
\ main code section                                    \
\
: fori ( -- n )
  0 CX !
  0 CY !
  GET-WINDOW-SIZE
  BEGIN
    EDITOR-REFRESH-SCREEN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
  0
;

