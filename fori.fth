\
\ Variables
\
VARIABLE ROWS
VARIABLE COLUMNS
VARIABLE CX
VARIABLE RX
VARIABLE CY
VARIABLE ROWOFF
VARIABLE FILEROW
HEX 5413 CONSTANT TIOCGWINSZ
DECIMAL 0 CONSTANT STDIN
DECIMAL 1 CONSTANT STDOUT
DECIMAL 2 CONSTANT STDERR
DECIMAL 16 CONSTANT INTRAGAP
DECIMAL 8 CONSTANT INTRASPACE
DECIMAL 32 CONSTANT RECORDGAP
DECIMAL 512 CONSTANT LINESIZE
DECIMAL 8 CONSTANT TAB-EXPANSION
DECIMAL 9 CONSTANT TAB-CHAR
VARIABLE LINEBUFFER
VARIABLE TEMP
VARIABLE SIZE-OF-ROWALLOC

: welcomemsg
S" Filo editor -- (c) Adrian McMenamin, 2024"
;

VARIABLE ROW-RECORDS
VARIABLE ROW-COUNT



\ free everything
: CLEANROWS
( -- )
  ." Clean up" CR
  ROW-COUNT @ 0<>
  IF
    ROW-COUNT @ 1 DO
      I 1- RECORDGAP * ROW-RECORDS @ + INTRASPACE + @ 
      FREE
      0<>
        ABORT" Failure to clear buffer"
      I 1- RECORDGAP * ROW-RECORDS @ + INTRASPACE + INTRAGAP + @
      FREE
      0<>
        ABORT" Failure to clear render buffer"
    LOOP
    0 ROW-COUNT !
  THEN
  ROW-RECORDS @ FREE DROP
;


\ check for error
: DROPERR
0<>
IF
  ." Halting on Error..." CR
  CLEANROWS
  ABORT
THEN 
;

: CLEANROWS-ERR
  ( u -- )
  0<> IF
    ." Could not allocate additional row..." CR
    CLEANROWS
    ABORT
  THEN
;

: SETROW-ERR
  ( f -- )
  FALSE =
  IF
    ." Could not add row" CR
    CLEANROWS
    ABORT
  THEN
;

\ set the data at a given index
: SET-ROW
  ( ptr* len index -- f )
  >R R@ ROW-COUNT @ >                                      \ index > row-count ?
  IF
    2DROP                                                  \ yes - bad call
    FALSE
  ELSE
    R@ 1- RECORDGAP * ROW-RECORDS @ + >R                   \ r-stack: index addr
    R@ !                                                   \ write out length at addr
    R@ INTRASPACE + !                                      \ write out ptr at addr+8
    R> INTRAGAP + >R                                       \ now write out 0s for render buufer too
    0 R@ !
    0 R> INTRASPACE + !
    TRUE
  THEN                 
  RDROP
;
 

\ for each row hold a counter for row length and a pointer to a line
: ADD-ROW
  (  -- )
  ROW-COUNT @ 0=
  IF
    SIZE-OF-ROWALLOC @ ZALLOCATE DROPERR
    ROW-RECORDS !
    1 ROW-COUNT !
    0 0 1 SET-ROW SETROW-ERR
  ELSE
    ROW-COUNT @ 1+ RECORDGAP *
    SIZE-OF-ROWALLOC @ >
    IF
      SIZE-OF-ROWALLOC @ 4096 + ZALLOCATE DROPERR >R
      ROW-RECORDS @ R@ SIZE-OF-ROWALLOC @ MOVE
      ROW-RECORDS @ FREE DROPERR
      R> ROW-RECORDS !
      SIZE-OF-ROWALLOC @ 4096 + SIZE-OF-ROWALLOC !
    THEN
    ROW-COUNT @ 1+ ROW-COUNT !
  THEN
  0 0 ROW-COUNT @ SET-ROW SETROW-ERR
;

\ get the data at a given index
: GET-ROW
  ( u -- ptr* len )
  >R R@ ROW-COUNT @ 1- >
  IF
    0 0         \ return nothing
  ELSE
    R@ 1- RECORDGAP * ROW-RECORDS @ +
    DUP
    @ SWAP INTRASPACE + @ SWAP
  THEN
  RDROP
;

\ get the render row at a given index
: GET-RROW
  ( u -- ptr* len )
  >R R@ ROW-COUNT @ 1- >
  IF
    0 0         \ return nothing
  ELSE
    R@ 1- RECORDGAP * ROW-RECORDS @ +
    INTRAGAP +
    DUP
    @ SWAP INTRASPACE + @ SWAP
  THEN
  RDROP
;

\ count the number of tab characters in a row
: COUNT-TABS
  ( index -- count)
  0 >R                                                              \ set tab counter to zero
  GET-ROW                                                           \ get the *ptr len
  DUP 0<>
  IF
    0 SWAP                                                          \ stack now *ptr len 0
    DO
      DUP                                                           \ stack now *ptr *ptr
      I + C@                                                        \ stack now *ptr char
      TAB-CHAR =                                                    \ test for \t
      IF
        R> 1+ >R
      THEN
    LOOP
  ELSE
    DROP
  THEN
  DROP
  R>
;

: ZRBUFF   \ zero out the rbuffer
  ( rbuff size exp -- rbuff)
  +
  1 PICK                                                            \ stack: rbuff size+exp rbuff
  +
  0 SWAP C!
;

: TRANSFER-RBUFF
  \ copy buffer with expansions
  ( buff rbuff size -- )
  DUP >R                                                            \ save size
  DUP 0<>
  IF
    0 >R                                                            \ how many expansions added
    0 DO
      1 PICK                                                        \ stack: buff rbuff buff
      I + C@                                                        \ stack: buff rbuff char
      DUP                                                           \ stack: buff rbuff char char
      TAB-CHAR =                                                    \ stack: buff rbuff char bool
      IF
        R@ I + TAB-EXPANSION MOD                                    \ how close to tab stop?
        TAB-EXPANSION SWAP - 0
        DO
          [ decimal 32 ] literal                                    \ stack: buff rbuff char spc
          2 PICK J + R@ + I + C!
        LOOP
        TAB-EXPANSION
        R@ I + TAB-EXPANSION MOD
        - 1-
        R> + >R
        DROP
      ELSE                                                          \ stack: buff rbuff char
        1 PICK R@ + I + C!
      THEN
    LOOP
    2R>                                                             \ stack: buff rbuff size exp
    ZRBUFF
  ELSE
    RDROP
    DROP
  THEN
  2DROP
;

\ create a render row
: EDITOR-UPDATE-ROW
  ( index -- )
  >R                                                                \ save index
  R@ 1- RECORDGAP * INTRASPACE + INTRAGAP + ROW-RECORDS @ + @       \ fetch render pointer
  DUP                                                               \ duplicate
  0<>                                                               \ test not zero
  IF
    FREE                                                            \ if not zero, free
  ELSE
    DROP
  THEN
  R@ COUNT-TABS                                                     \ stack: tab-count
  TAB-EXPANSION 1- *                                                \ stack: tab-expansion-total
  R@ 1- RECORDGAP * ROW-RECORDS @ + @ DUP                           \ stack: tab-expnsion-total size
  + 1+ DUP                                                          \ stack: rsize rsize
  ZALLOCATE DROPERR                                                  \ stack: rsize rbuff
  R@ 1- RECORDGAP * INTRASPACE + ROW-RECORDS @ + @                  \ stack: rsize rbuff buff
  SWAP                                                              \ stack: rsize buff rbuff
  2DUP                                                              \ stack: rsize buff rbuff buff rbuff
  4 PICK 1-                                                         \ stack: rsize buff rbuff buff rbuff size
  TRANSFER-RBUFF                                                    \ stack: rsize buff rbuff
  R> 1- RECORDGAP * INTRAGAP + ROW-RECORDS @ +                      \ stack: rsize buff rbuff addr
  3 PICK 1 PICK !                                                   \ store length
  1 PICK 1 PICK INTRASPACE + !                                      \ store rbuff
  2DROP 2DROP                                                       \ clear stack
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
  16 ALLOCATE DROPERR
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
    ." IOCTL failed"
    ABORT
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
  DUP 0<>
  IF
    >R                                                  \ store old length
    BUFFER_LEN @ TEMP !                                 \ get existing length
    BUFFER_PTR @  BUFFER_LEN @ R@ + RESIZE DROPERR      \ get a buffer of new size
    R@ BUFFER_LEN @ + BUFFER_LEN !                      \ store new size
    BUFFER_PTR !                                        \ store address
    TEMP @                                              \ calculate offset for copying
    BUFFER_PTR @ +                                      \ add to get start point
    R> MOVE                                             \ copy
  ELSE
    2DROP
  THEN
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

: SET-RX
  ( index -- )        \ set RX for this line
  0 RX !
  ROWOFF @ + 1+
  GET-ROW             \ get underlying row - stack *ptr len
  0<> IF
    CX @ 0 DO
      DUP I + C@      \ get character
      TAB-CHAR =
      IF
        RX @ TAB-EXPANSION 1- + RX !
      THEN
    LOOP
    RX @ CX @ + RX !
  THEN 
  DROP
;
 
: EDITORSCROLL
  ( -- )
  CY @ ROWS @ >
  IF
    ROWOFF @ ROW-COUNT @ <
    IF
      ROWOFF @ 1+ ROWOFF !
    THEN
    ROWS @ CY !
  ELSE
    CY @ 0<
    IF
      ROWOFF @ 0<>
      IF
        ROWOFF @ 1- ROWOFF !
      THEN
      0 CY !
    THEN
  THEN
  SET-RX
; 

: EDITOR-DRAW-ROWS
  ( -- )
  ROWS @ 1+ 1 DO
    I ROWOFF @ + FILEROW !
    FILEROW @ ROW-COUNT @ >
    IF
      ROW-COUNT @ 0= ROWS @ 3 / I = AND
      IF  \ welcome message
        welcomemsg ABAPPEND
      ELSE \ tilde
        S" ~" ABAPPEND
      THEN
    ELSE
      FILEROW @ GET-RROW ABAPPEND
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

: LINE-LENGTH
  ( index -- len )
  ROWOFF @ + 1+       \ get the line length at an index
  GET-ROW
  SWAP
  DROP
;

: ADJUST-FOR-LENGTH
  ( -- )
  CY @ LINE-LENGTH    \ move the cursor to the end of a line if needed
  DUP
  CX @ <
  IF
    CX !
  ELSE
    DROP
  THEN
;


: MOVE_CURSOR
  ( -- )
  S\" \e[" ABAPPEND                                          \ first part of escape sequence
  CY @ 1+ >STRING COUNT ABAPPEND                             \ add y
  S" ;" ABAPPEND
  RX @ 1+ >STRING COUNT ABAPPEND                             \ add x
  S" H" ABAPPEND
;

: EDITOR-REFRESH-SCREEN
  ( -- )
  EDITORSCROLL
  [ decimal 64 ] literal  ZALLOCATE DROPERR BUFFER_PTR !
  0 BUFFER_LEN !
  \ make cursor disappear
  S\" \e[?25l" ABAPPEND
  \ position at top of screen
  S\" \e[1;1H" ABAPPEND
  EDITOR-DRAW-ROWS
  MOVE_CURSOR
  \ cursor reappear
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
;

: ENDKEY
  ( -- )
  CY @ LINE-LENGTH
  CX !
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
    
: HANDLE-O-CASES
  ( *addr -- )
  >R
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
  RDROP
;  

: HANDLE-BRACKET-CASES
  ( *addr -- )
  >R
  R@ 2+ C@                                 \ get next + 1 character
  CASE
    CHAR H OF
      HOMEKEY
    ENDOF
    CHAR F OF
      ENDKEY
    ENDOF
    CHAR A OF                             \ up arrow
      CY @ 
      1- CY !
      ADJUST-FOR-LENGTH
    ENDOF
    CHAR B OF                             \ arrow down
      CY @
      1+ CY !                             \ attempt to increase
      ADJUST-FOR-LENGTH
    ENDOF
    CHAR C OF                             \ arrow right
      CX @ DUP
      CY @ LINE-LENGTH <>                 \ not at right hand edge
      IF
        1+ CX !
      ELSE
        DROP
        0 CX !
        CY @ 1+ CY !
      THEN
    ENDOF
    CHAR D OF                            \ arrow left
      CX @ DUP
      0<>
      IF
        1- CX !                          \ move left if not already at edge
      ELSE
        DROP
        CY @ 1- CY !
        COLUMNS @ CX !                   \ force to end of line
        ADJUST-FOR-LENGTH
      THEN
    ENDOF
    CHAR 5 OF
      R@ CHECKTILDE                       \ page up
      IF 
        ROWOFF @ ROWS @ - 0<
        IF
          0 ROWOFF !
        ELSE
          ROWOFF @ ROWS @ - ROWOFF !
        THEN 
      THEN
    ENDOF
    CHAR 6 OF
      R@ CHECKTILDE                      \ page down
      IF
        ROWOFF @ ROWS @ + ROW-COUNT @ >
        IF
          ROW-COUNT @ ROWS @ - ROWOFF !
        ELSE
          ROWOFF @ ROWS @ + ROWOFF !
        THEN
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
  RDROP
;

: PROCESS-ESCAPED-KEY
  ( *addr -- )
  >R                                         \ save address to return stack
  R@ 1+ C@                                   \ get next character
  CHAR [ =                                   \ is it [?
  IF
    R> HANDLE-BRACKET-CASES
  ELSE
    R> HANDLE-O-CASES
  THEN
;
      
: EDITOR-PROCESS-KEYPRESS
  ( --  )
  EDITOR-READ-KEY
  CASE
    CHAR Q [ HEX 1F ] LITERAL AND  OF
      2DROP
      CLEANROWS
      CLEARSCREEN
      ." Leaving FILO " CR
      ABORT
    ENDOF
    CASE
      [ decimal 27 ] literal  OF
        SWAP
        PROCESS-ESCAPED-KEY
    ENDOF
    DUP OF DROP ENDOF                    \ default
  ENDCASE
;

\
\ file io
\
: BUFFERNULL
  ( addr len -- addr len)
  2DUP
  +
  10 SWAP C!
;


: EDITOROPEN
  ( c-addr u  -- )
  S\" r\0" DROP OPEN-FILE                               \ stack: fileid ior
  0<> IF
    DROP
    ." Halting: Failed to open file" CR
    ABORT
  THEN
  >R                                                     \ fileid on to R stack
  LINESIZE ZALLOCATE DROPERR LINEBUFFER !
  BEGIN
    LINEBUFFER @ LINESIZE R@ READ-LINE
    0= AND
  WHILE
    ADD-ROW
    DUP 0<>
    IF
      DUP DUP >R 1+ ZALLOCATE DROPERR >R                \ allocate a buffer same size as the line  r-stack: handle, len, addr
      R@ LINEBUFFER @                                   \ stack:  len, addr, linebuffer
      SWAP ROT                                          \ stack: linebuffer, addr, len
      BUFFERNULL
      MOVE                                              \ copy the line into the buffer
      R> R> ROW-COUNT @
      SET-ROW SETROW-ERR
    ELSE
      DROP
    THEN
    ROW-COUNT @ EDITOR-UPDATE-ROW                       \ insert a render buffer - even a blank one if we have to
  REPEAT
  DROP
  LINEBUFFER @ FREE DROPERR
  R> CLOSE-FILE DROP
;

\
\ main code section                                    \
\
: fori ( -- n )
  0 CX !
  0 RX !
  0 CY !
  0 ROW-RECORDS !
  0 ROW-COUNT !
  0 ROWOFF !
  8192 SIZE-OF-ROWALLOC !
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
    FALSE
  UNTIL
  0
;

