\
\ Variables
\
VARIABLE ROWS
VARIABLE COLUMNS
VARIABLE CX
VARIABLE RX
VARIABLE CY
VARIABLE ROWOFF
VARIABLE COLOFF
VARIABLE FILEROW
VARIABLE EDITFILE
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
VARIABLE ROW-RECORDS
VARIABLE ROW-COUNT

: welcomemsg
S" Filo editor -- (c) Adrian McMenamin, 2024"
;




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
  >R R@ ROW-COUNT @ >
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
  DUP 0<>                                                           \ stack: *ptr len bool
  IF
    0                                                               \ stack now *ptr len 0
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


: TRANSFER-RBUFF
  \ copy buffer with expansions
  ( buff rbuff size -- )
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
        TAB-EXPANSION SWAP -  DUP                                   \ stack: buff rbuff char exp exp
        0 DO                                                        \ stack: buff rbuff char exp
          [ decimal 32 ] literal                                    \ stack: buff rbuff char exp spc
          3 PICK J + R@ + I + C!                                    \ stack: buff rbuff char exp
        LOOP
        R> + 1- >R
        DROP
      ELSE                                                          \ stack: buff rbuff char
        1 PICK R@ + I + C!
      THEN
    LOOP
    RDROP
  ELSE
    DROP
  THEN
  2DROP
;

\ get the size of an unexpanded row
: GET-ROW-SIZE
  ( index -- size)
  1- RECORDGAP *                                                    \ stack: offset
  ROW-RECORDS @                                                     \ stack: offset *rows
  +                                                                 \ stack: *indexed-row
  @                                                                 \ stack: size
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
  R@ GET-ROW-SIZE                                                   \ stack: tab-expnsion-total size
  + 1+ DUP                                                          \ stack: rsize rsize
  ZALLOCATE DROPERR                                                 \ stack: rsize rbuff
  R@ 1- RECORDGAP * INTRASPACE + ROW-RECORDS @ + @                  \ stack: rsize rbuff buff
  SWAP                                                              \ stack: rsize buff rbuff
  2DUP                                                              \ stack: rsize buff rbuff buff rbuff
  R@ GET-ROW-SIZE                                                   \ stack: rsize buff rbuff buff rbuff size
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
  =
;


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
  ( -- *scratchpad c )
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
  ( index -- )                          \ set RX for this line
  0 RX !                                \ stack: index
  ROWOFF @ + 1+                         \ stack: row
  GET-ROW                               \ stack: buff len
  0<>                                   \ stack: buff bool
  IF
    CX @ 0<>
    IF
      CX @ 0 DO                           \ stack: buff
        DUP I + C@                        \ stack: buff char
        TAB-CHAR =                        \ stack: buff bool
        IF
          RX @ TAB-EXPANSION MOD          \ stack: buff modulo
          TAB-EXPANSION SWAP -            \ stack: buff to-next-tabstop
          RX @ + RX !                     \ stack: buff
        ELSE
          RX @ 1+ RX !                    \ stack: buff
        THEN
      LOOP
    THEN
  THEN
  DROP
;

          

: EDITORSCROLL
  ( -- )
  CY @ ROWS @ 2- >
  IF
    ROWOFF @ ROW-COUNT @ <
    IF
      ROWOFF @ 1+ ROWOFF !
    THEN
    ROWS @ 2- CY !
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
  CY @ SET-RX
; 

: EDITOR-DRAW-ROWS
  ( -- )
  ROWS @ 1 DO
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
      FILEROW @ GET-RROW                      \ stack: *ptr len
      COLOFF @ -                              \ shorten len: len - coloff = slen
      DUP                                     \ stack: *ptr slen slen
      0<                                      \ stack: *ptr slen bool
      IF
        DROP
        0
      THEN
      SWAP
      COLOFF @ +                              \ stack: slen *(ptr + coloff) = *sptr
      SWAP                                    \ stack: *sptr slen
      DUP                                     \ stack: *sptr slen slen
      COLUMNS @                               \ stack: *sptr slen slen cols
      >                                       \ stack: *sptr slen bool
      IF
        DROP
        COLUMNS @
      THEN
      ABAPPEND
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


: HOMEKEY
  ( -- )
  0 COLOFF !
  0 CX !
;

: ENDKEY
  ( -- )
  CY @ LINE-LENGTH                       \ stack: llen
  DUP                                    \ stack: llen llen
  COLUMNS @ -                            \ stack: llen excess
  DUP 0>                                 \ stack: llen excess bool
  IF
    COLOFF !                             \ stack: llen
    COLUMNS @ CX !
    DROP
  ELSE
    DROP
    CX !
  THEN
;

: OVER-LINE
  ( -- bool )
  CX @ COLOFF @ +
  CY @ LINE-LENGTH
  >
;

: RIGHT-LEN
  ( -- )
  OVER-LINE                                               \ stack: bool
  IF                                                      \ apos > llen
    CY @ 1+ CY !
    HOMEKEY
  ELSE                                                    \ apos <= llen
    CX @ COLUMNS @
    >
    IF
      CX @ 1- CX !
      COLOFF @ 1+ COLOFF !
    THEN
  THEN
;

: LEFT-LEN
  ( -- )
  CX @ 0<                                                 \ stack: bool
  IF                                                      \ cx < 0
    COLOFF @ 0<>                                          \ stack: bool
    IF                                                    \ coloffset != 0
      0 CX !                                              \ stack:
      COLOFF @ 1- COLOFF !                                \ stack:
    ELSE
      CY @ 1- CY !
      CY @ LINE-LENGTH CX !
    THEN
  THEN
;

: SNAP-TO-LENGTH
  ( -- )
  BEGIN
    OVER-LINE
    COLOFF @ 0>
    AND
  WHILE
    COLOFF @ 1- COLOFF !
  REPEAT
  BEGIN
    OVER-LINE
  WHILE
    CX @ 1- CX !
  REPEAT
;


: ADJUST-FOR-LENGTH
  ( -- )
  LEFT-LEN
  RIGHT-LEN
;


: MOVE_CURSOR
  ( -- )
  S\" \e[" ABAPPEND                                          \ first part of escape sequence
  CY @ 1+ <# #S #> ABAPPEND
  S" ;" ABAPPEND
  RX @ 1+ <# #S #> ABAPPEND
  S" H" ABAPPEND
;

: DRAW-STATUS-BAR
  ( -- )
  S\" \e[1mcol: \e[7G" ABAPPEND
  RX @ COLOFF @ + <# #S #> ABAPPEND
  S\"   \e[12G" ABAPPEND
  S\" row:\e[17G" ABAPPEND
  CY @ ROWOFF @ + <# #S #> ABAPPEND
  S"   " ABAPPEND
  EDITFILE @
  0<> IF
    S\" \e[25G" ABAPPEND
    EDITFILE @ COUNT ABAPPEND
  THEN
  S\" \e[m" ABAPPEND
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
  DRAW-STATUS-BAR
  MOVE_CURSOR
  \ cursor reappear
  S\" \e[?25h" ABAPPEND
  PRINT_BUFFER
  ABFREE
;


\
\ input section	
\


: CHECKTILDE
  ( *char -- bool)
  3 + C@
  CHAR ~ =
;
    
: HANDLE-O-CASES
  ( *addr --   )
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
      DUP OF DROP ENDOF
    ENDCASE
  THEN
  RDROP
;  

: HANDLE-BRACKET-CASES
  ( *addr --   )
  >R
  R@ 2+ C@                                 \ get next + 1 character
  CASE
    CHAR H OF
      HOMEKEY
      ADJUST-FOR-LENGTH
    ENDOF
    CHAR F OF
      ENDKEY
    ENDOF
    CHAR A OF                             \ up arrow
      CY @ 
      1- CY !
      SNAP-TO-LENGTH
    ENDOF
    CHAR B OF                             \ arrow down
      CY @
      1+ CY !
      SNAP-TO-LENGTH
    ENDOF
    CHAR C OF                             \ arrow right
      CX @ 1+ CX !
      ADJUST-FOR-LENGTH
    ENDOF
    CHAR D OF                             \ arrow left
      CX @ 1- CX !
      ADJUST-FOR-LENGTH
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
    DUP OF DROP ENDOF                          \ default
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
  EDITOR-READ-KEY                                        \ stack: *ptr, char
  CASE
    CHAR Q [ HEX 1F ] LITERAL AND  OF
      2DROP
      CLEANROWS
      CLEARSCREEN
      ." Leaving FILO " CR
      ABORT
    ENDOF
    [ decimal 27 ] literal  OF
      SWAP
      PROCESS-ESCAPED-KEY
    ENDOF
    DUP OF 2DROP ENDOF                    \ default
  ENDCASE
;

\
\ Save the name of the incoming file
\
: SAVE-FILE-NAME
  ( c-addr u -- c-addr u )
  2>R
  2R@
  HERE EDITFILE !
  DUP CELL+ ALLOT
  DUP EDITFILE @ !
  EDITFILE @ CELL+ SWAP
  MOVE
  2R>
;


\
\ file io
\
: EDITOROPEN
  ( c-addr u  -- )
  SAVE-FILE-NAME
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
  0 COLOFF !
  0 EDITFILE !
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

