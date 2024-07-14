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

VARIABLE TEXTROW
VARIABLE TEXTROWLEN
VARIABLE NUMROWS

VARIABLE TEMP

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
  ( -- c *scratchpad)
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
ELSE
  1 DROPERR
THEN
BUFFER_PTR !               \ store address
BUFFER_LEN @ R@ -          \ calculate offset for copying
BUFFER_PTR @ +             \ add to get start point
R> MOVE                    \ copy
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
  I NUMROWS @ >
  IF
    ROWS @ 3 / I =
    IF  \ welcome message
      welcomemsg @ EXTEND_BUFFER_NO_ADD_LEN
      welcomemsg 8 + BUFFER_PTR @ BUFFER_LEN @ + welcomemsg @ MOVE
      BUFFER_LEN @ welcomemsg @ + BUFFER_LEN !
    ELSE \ tilde
      1 EXTEND_BUFFER
      CHAR ~ BUFFER_PTR @ BUFFER_LEN @ + 1- C!
    THEN
  ELSE
    TEXTROWLEN @ TEMP !
    TEMP @ COLUMNS @ >
    IF
      COLUMNS @ TEMP !
    THEN
    TEXTROW @ TEMP @ ABAPPEND
  THEN
  \ ESC[K - redraw line
  S\" \e[K" ABAPPEND
  I ROWS @ <
  IF
    S\" \r\n" ABAPPEND
  THEN
LOOP
;
  
: PRINT_BUFFER
( -- )
BUFFER_PTR @ BUFFER_LEN @ TYPE
;


: MOVE_CURSOR
( -- )
S\" \e[" ABAPPEND                                          \ first part of escape sequence
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
S\" \e[1;1H" ABAPPEND
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
ABFREE
;


: EDITOR-REFRESH-SCREEN
( -- )
EDITOR-RESET-SCREEN
  
;

\
\ input section	
\

: HOMEKEY
  ( -- )
  0 CX !
  0 CY !
;

: ENDKEY
  ( -- )
  0 CX !
  ROWS @ CY !
;

: CHECKTILDE
  ( *char -- bool)
  3 + C@
  CHAR ~ =
  IF
    TRUE
  ELSE
    FALSE
  THEN
;
    

: PROCESS-ESCAPED-KEY
  ( *addr -- )
  >R                                         \ save address to return stack
  R@ 1+ C@                                   \ get next character
  CHAR [ =                                   \ is it [?
  IF
    R@ 2+ C@                                 \ yes - so process
    CASE
      CHAR H OF
        HOMEKEY
      ENDOF
      CHAR F OF
        ENDKEY
      ENDOF
      CHAR A OF                              \ up arrow
        CY @ DUP
        0<> 
        IF
          1- CY !                            \ decrease if not at top of screen
        ELSE
          DROP
        THEN
      ENDOF
      CHAR B OF                              \ arrow down
        CY @ DUP
        ROWS @ 1- <>
        IF
          1+ CY !                           \ increase if not at bottom of screen
        ELSE
          DROP
        THEN
      ENDOF
      CHAR C OF                             \ arrow right
        CX @ DUP
        COLUMNS @ 1- <>                     \ not at right hand edge
        IF
          1+ CX !
        ELSE
          DROP
        THEN
      ENDOF
      CHAR D OF                            \ arrow left
        CX @ DUP
        0<>
        IF
          1- CX !                          \ move left if not already at edge
        ELSE
          DROP
        THEN
      ENDOF
     CHAR 5 OF
       R@ CHECKTILDE
       IF
         0 CY !                             \ page up
       THEN
     ENDOF
     CHAR 6 OF
       R@ CHECKTILDE
       IF
         ROWS @ CY !                        \ page down
       THEN
     ENDOF
     CHAR 1 OF
       R@ CHECKTILDE
       IF
         HOMEKEY
       THEN
     ENDOF
     CHAR 7 OF
       R@ CHECKTILDE
       IF
         HOMEKEY
       THEN
     ENDOF
     CHAR 4 OF
       R@ CHECKTILDE
       IF
         ENDKEY
       THEN
     ENDOF
     CHAR 8 OF
       R@ CHECKTILDE
       IF
         ENDKEY
       THEN
     ENDOF
     CHAR 3 OF
       R@ CHECKTILDE
       IF
         NOP                                 \ delete key
       THEN
     ENDOF
      DUP OF ENDOF                          \ default
    ENDCASE
  ELSE
    R@ 1+ C@
    CHAR O =                                \ handle ^[OH and ^[OF cases
    IF
      R@ 2+ C@ >R
      R@ CHAR H =
      IF
        HOMEKEY
      ELSE
        R@ CHAR F =
        IF
          ENDKEY
        THEN
      THEN
      RDROP
    THEN
  THEN
  RDROP
;
      
: EDITOR-PROCESS-KEYPRESS
  ( --  )
  EDITOR-READ-KEY
  CASE
    CHAR Q [ HEX 1F ] LITERAL AND  OF
      DROP
      EDITOR-RESET-SCREEN
      CLEARSCREEN
      ABORT" Leaving FILO " CRLF  
    ENDOF
    CASE
      [ decimal 27 ] literal  OF
        SWAP
        PROCESS-ESCAPED-KEY
    ENDOF
    DUP OF ENDOF                    \ default
  ENDCASE

;

\
\ file io
\
: EDITOROPEN
  ( -- )
  S\" Hello, world!\n" 1- TEXTROWLEN ! TEXTROW !
  1 NUMROWS !
;


\
\ main code section                                    \
\
: fori ( -- n )
  0 CX !
  0 CY !
  GET-WINDOW-SIZE
  EDITOROPEN
  BEGIN
    EDITOR-REFRESH-SCREEN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
  0
;

