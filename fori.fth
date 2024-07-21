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

VARIABLE TEMP

: welcomemsg
S" Filo editor -- (c) Adrian McMenamin, 2024"
;

VARIABLE ROW-RECORDS
VARIABLE ROW-COUNT



\ free everything
: CLEANROWS
( -- )
  ROW-COUNT @ 0<>
  IF
    ROW-COUNT @ 1 DO
      I 1- 16 * ROW-RECORDS @ + 8 + @ FREE
      0<>
      IF
        ABORT" Memory failure in CLEANROWS word"
      THEN
    LOOP
    0 ROW-COUNT !
  THEN
  ROW-RECORDS @ FREE DROP
;


\ check for error
: DROPERR
0<>
IF
  CLEANROWS
  ABORT" Halting on error"
THEN
;

: CLEANROWS-ERR
  ( u -- )
  0<> IF
    CLEANROWS
    ABORT" Could not allocate additional row"
  THEN
;

: SETROW-ERR
  ( f -- )
  FALSE =
  IF
    CLEANROWS
    ABORT" Could not add row"
  THEN
;

\ set the data at a given index
: SET-ROW
  ( ptr* len index -- f )
  >R R@ ROW-COUNT @ >
  IF
    2DROP
    FALSE
  ELSE
    R@ 1- 16 * ROW-RECORDS @ + 48 DROP !                  \ length
    R@ 1- 16 * ROW-RECORDS @ 8 + + 49 DROP !              \ ptr
    TRUE
  THEN
  RDROP
;
 


\ for each row hold a counter for row length and a pointer to a line
: ADD-ROW
  (  -- )
  ROW-COUNT @ 0=
  IF
    [ decimal 256 ] literal ALLOCATE DROPERR
    ROW-RECORDS !
    1 ROW-COUNT !
    0 0 1 SET-ROW SETROW-ERR
  ELSE
    ROW-RECORDS @
    ROW-COUNT @ 1+ 16 *
    RESIZE CLEANROWS-ERR
    ROW-RECORDS !
    ROW-COUNT @ 1+ ROW-COUNT !
    0 0 ROW-COUNT @ SET-ROW SETROW-ERR
  THEN
;

\ get the data at a given index
: GET-ROW
  ( u -- ptr* len )
  >R R@ ROW-COUNT @ 1- >
  IF
    0 0         \ return nothing
  ELSE
    R@ 1- 16 * ROW-RECORDS @ +
    DUP
    @ SWAP 8 + @ SWAP
  THEN
  RDROP
;



\
\ Utility words					      *
\


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

: ABAPPEND
( char* len -- )
>R                                                  \ store old length
BUFFER_LEN @ TEMP !                                 \ get existing length
BUFFER_PTR @  BUFFER_LEN @ R@ + RESIZE DROPERR      \ get a buffer of new size
R@ BUFFER_LEN @ + BUFFER_LEN !                      \ store new size
BUFFER_PTR !                                        \ store address
TEMP @                                              \ calculate offset for copying
BUFFER_PTR @ +                                      \ add to get start point
R> MOVE                                             \ copy
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
  I ROW-COUNT @ >
  IF
    ROW-COUNT @ 0= ROWS @ 3 / I = AND
    IF  \ welcome message
      welcomemsg ABAPPEND
    ELSE \ tilde
      S" ~" ABAPPEND
    THEN
  ELSE
    I GET-ROW BASE DROP ABAPPEND
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

: EDITOR-REFRESH-SCREEN
  ( -- )
[ decimal 64 ] literal  ALLOCATE DROPERR BUFFER_PTR !
0 BUFFER_LEN !
\ ESC [?25l - make cursor disappear
S\" \e[?25l" ABAPPEND
\ ^[[1;1H - position at top of screen
S\" \e[1;1H" ABAPPEND
EDITOR-DRAW-ROWS
MOVE_CURSOR
\ ESC [?25h - cursor reappear
S\" \e[?25h" ABAPPEND
PRINT_BUFFER
ABFREE
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
         NOP                                 \ delete key - to be implemented
       THEN
     ENDOF
      DUP OF ENDOF                          \ default
    ENDCASE
  ELSE
    R@ 1+ C@
    CHAR O =                                \ handle ^[OH and ^[OF cases
    IF
      R@ 2+ C@
      CASE
        CHAR H OF
          HOMEKEY
        ENDOF
        CHAR F OF
          ENDKEY
        ENDOF
          DUP OF ENDOF
      ENDCASE
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
      CLEANROWS
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
  ( c-addr u  -- )
  S\" r\0" DROP OPEN-FILE
  0<>
  IF
    ABORT" Failed to open file"
  ELSE
    >R
    BEGIN
      [ decimal 512 ] literal ALLOCATE DROPERR DUP
      [ decimal 512 ] literal R@ READ-LINE
      0= AND
    WHILE
      ADD-ROW
      ROW-COUNT @ SET-ROW SETROW-ERR
    REPEAT
  THEN
  R> CLOSE-FILE
;


\
\ main code section                                    \
\
: fori ( -- n )
  0 CX !
  0 CY !
  0 ROW-RECORDS !
  0 ROW-COUNT !
  GET-WINDOW-SIZE
  PARSE-NAME
  DUP 0<>
  IF
    EDITOROPEN
  ELSE
    2DROP
  THEN
  BEGIN
    EDITOR-REFRESH-SCREEN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
  0
;

