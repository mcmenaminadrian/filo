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
VARIABLE EDITFILE
VARIABLE DIRTY
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
      4096 SIZE-OF-ROWALLOC +!
    THEN
    1 ROW-COUNT +!
  THEN
  0 0 ROW-COUNT @ SET-ROW SETROW-ERR
;

: INSERT-EMPTY-ROW
  ( addr addr -- )
  0 SWAP !
  0 SWAP CELL+ !
;


: INSERT-ROW-ALLOC
  ( row -- )
  ROW-COUNT @ 1+                                       \ stack: row count+
  RECORDGAP *                                          \ stack: row newsize
  SIZE-OF-ROWALLOC @ >
  \ create or identify dest
  IF
    SIZE-OF-ROWALLOC @ 4096 + ZALLOCATE DROPERR >R
    \ copy everything over
    ROW-RECORDS @ R@ SIZE-OF-ROWALLOC @ MOVE
    4096 SIZE-OF-ROWALLOC +!
    ROW-RECORDS @ FREE DROPERR
    R@ ROW-RECORDS !
  ELSE
    ROW-RECORDS @ >R
  THEN
  >R
  2R@ 1+ RECORDGAP * +                                 \ stack: src'
  2R@                                                  \ stack: src' dest row
  2+ RECORDGAP * +                                     \ stack: src' dest'
  ROW-COUNT @ RECORDGAP *                              \ stack: src' dest' oldsz
  R@ 1+ RECORDGAP * -                                  \ stack: src' dest' remsz
  MOVE                                                 \ stack: <empty>
  R> 1+ RECORDGAP * R> + DUP DUP                       \ stack: addr addr addr
  INSERT-EMPTY-ROW                                     \ stack: addr
  INTRAGAP + DUP                                       \ stack: addr' addr'
  INSERT-EMPTY-ROW                                     \ stack: <empty>
  1 ROW-COUNT +!
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
  >R R@ ROW-COUNT @ >
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
    0                                                               \ stack: *ptr len 0
    DO
      DUP                                                           \ stack: *ptr *ptr
      I + C@                                                        \ stack: *ptr char
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
      OVER                                                          \ stack: buff rbuff buff
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
        R> + 1- >R                                                  \ stack: buff rbuff char
        DROP                                                        \ stack: buff rbuff
      ELSE                                                          \ stack: buff rbuff char
        OVER R@ + I + C!                                            \ stack: buff rbuff
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
    FREE DROPERR                                                    \ if not zero, free
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
  3 PICK OVER !                                                     \ stack: rsize buff rbuff addr
  INTRASPACE + !                                                    \ stack: rsize buff
  2DROP                                                             \ clear stack
;

: REPLACE-ROW
  ( len index *ptr -- )
  SWAP                                                              \ stack: len *ptr index
  DUP GET-ROW                                                       \ stack: len *ptr index *optr len
  DROP FREE DROPERR                                                 \ stack: len *ptr index
  ROWOFF @ + 1-                                                     \ stack: len *ptr row
  RECORDGAP *                                                       \ stack: len *ptr offset
  ROW-RECORDS @ + >R                                                \ stack: len *ptr
  SWAP                                                              \ stack: *ptr len
  R@ !
  R> INTRASPACE + !
;


: SPLIT-LINES
  ( y x -- ) 
  >R                                                   \ stack: y                               R: x
  DUP                                                  \ stack: y y
  >R                                                   \ stack: y                               R: x y
  1+ GET-ROW                                           \ stack: *ptr len
  SWAP 2R@ DROP                                        \ stack: len *ptr x
  DUP                                                  \ stack: len *ptr x x
  3 PICK SWAP                                          \ stack: len *ptr x len x
  -                                                    \ stack: len *ptr x len-r
  DUP                                                  \ stack: len *ptr x len-r len-r
  0<>                                                  \ stack: len *ptr x len-r bool
  IF 
    DUP                                                \ stack: len *ptr x len-r len-r
    ZALLOCATE DROPERR                                  \ stack: len *ptr x len-r *dest
    3 PICK                                             \ stack: len *ptr x len-r *dest *ptr
    3 PICK                                             \ stack: len *ptr x len-r *dest *ptr x
    +                                                  \ stack: len *ptr x len-r *dest *src
    SWAP DUP >R                                        \ stack: len *ptr x len-r *src *dest     R: x y *dest
    ROT DUP >R MOVE                                    \ stack: len *ptr x                      R: x y *dest len-r
    2R> 2R>                                            \ stack: len *ptr x *dest len-r x y      R: <empty>
    DUP >R                                             \ stack: len *ptr x *dest len-r x y      R: y
    NIP                                                \ stacK: len *ptr x *dest len-r y
    1+ RECORDGAP * ROW-RECORDS @ +                     \ stack: len *ptr x *dest len-r addr
    2DUP !                                             \ stack: len *ptr x *dest len-r addr
    CELL+                                              \ stack: len *ptr x *dest len-r addr'
    NIP                                                \ stack: len *ptr x *dest addr'
    !                                                  \ stack: len *ptr x
    R@ 2+ EDITOR-UPDATE-ROW                            \ stack: len *ptr x
    DUP >R                                             \ stack: len *ptr x                      R: y x
    ROT                                                \ stack: *ptr x len
    SWAP                                               \ stack: *ptr len x
    DO                                                 \ stack: *ptr
      DUP I +                                          \ stack: *ptr *ptr'
      0 SWAP                                           \ stack: *ptr 0 *ptr'
      C!                                               \ stack: *ptr
    LOOP 
    DROP                                               \ stack: <empty>
    2R>                                                \ stack: y x                             R: <empty>
    SWAP                                               \ stack: x y
    DUP >R                                             \ stack: x y                             R: y
    RECORDGAP * ROW-RECORDS @ +                        \ stack: x addr
    !                                                  \ stack: <empty> 
    R> 1+ EDITOR-UPDATE-ROW                            \                                      R: <empty>
  ELSE
    2DROP 2DROP
    2RDROP
  THEN
;


: EDITOR-ROW-INSERT-CHAR
  ( index pos char -- )                                             \ insert char in row index and point pos
  2 PICK ROWOFF @ +                                                 \ stack: index pos char row
  GET-ROW                                                           \ stack: index pos char *ptr len
  3 PICK DUP                                                        \ stack: index pos char *ptr len pos pos
  0< SWAP                                                           \ stack: index pos char *ptr len bool pos
  2 PICK >                                                          \ stack: index pos char *ptr len bool bool
  OR IF
    2DROP 2DROP DROP                                                \ empty stack
  ELSE
    DUP 1+                                                          \ stack: index pos char *ptr len len+1
    ZALLOCATE DROPERR                                               \ stack: index pos char *ptr len *newptr
    >R                                                              \ stack: index pos char *ptr len
    OVER                                                            \ stack: index pos char *ptr len *ptr
    R@                                                              \ stack: index pos char *ptr len *ptr *newptr
    5 PICK                                                          \ stack: index pos char *ptr len *ptr *newptr pos
    MOVE                                                            \ stack: index pos char *ptr len
    R@                                                              \ stack: index pos char *ptr len *newptr
    4 PICK +                                                        \ stack: index pos char *ptr len *newptr+pos
    3 PICK                                                          \ stack: index pos char *ptr len *newptr+pos char
    SWAP C!                                                         \ stack: index pos char *ptr len
    SWAP                                                            \ stack: index pos char len *ptr
    3 PICK +                                                        \ stack: index pos char len *ptr'
    R@                                                              \ stack: index pos char len *ptr' *newptr
    4 PICK + 1+                                                     \ stack: index pos char len *ptr' *newptr'
    2 PICK 5 PICK -                                                 \ stack: index pos char len *ptr' *newptr' len'
    MOVE                                                            \ stack: index pos char len
    1+ 3 PICK R>                                                    \ stack: index pos char len+1 index *newptr
    REPLACE-ROW                                                     \ stack: index pos char
    2DROP 
    EDITOR-UPDATE-ROW
  THEN
;


\
\ Utility words
\

\ is character on stack CTRL Key for that letter
: CTRL-KEY ( n c -- f )
  [ HEX 1F ] LITERAL AND
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
    R@ BUFFER_LEN +!                                    \ store new size
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
          RX +!                           \ stack: buff
        ELSE
          1 RX +!                         \ stack: buff
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
      1 ROWOFF +!
    THEN
    ROWS @ 2- CY !
  ELSE
    CY @ 0<
    IF
      ROWOFF @ 0<>
      IF
        -1 ROWOFF +!
      THEN
      0 CY !
    THEN
  THEN
  CY @ SET-RX
; 

: EDITOR-DRAW-ROWS
  ( -- )
  ROWS @ 1 DO
    I ROWOFF @ + >R
    R@ ROW-COUNT @ >
    IF
      ROW-COUNT @ 0= ROWS @ 3 / I = AND
      IF  \ welcome message
        welcomemsg ABAPPEND
      ELSE \ tilde
        S" ~" ABAPPEND
      THEN
    ELSE
      R@ GET-RROW                             \ stack: *ptr len
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
    RDROP
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
    1 CY +!
    HOMEKEY
  ELSE                                                    \ apos <= llen
    CX @ COLUMNS @
    >
    IF
      -1 CX +!
      1 COLOFF +!
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
      -1 COLOFF +!                                        \ stack:
    ELSE
      -1 CY +!
      CY @ LINE-LENGTH CX !
    THEN
  THEN
;

: SNAP-TO-LENGTH
  ( -- )
  CY @ LINE-LENGTH
  >R
  CX @ COLOFF @ + R@ >
  IF
    R@ COLUMNS @ >
    IF
      COLUMNS @ CX !
      R@ COLUMNS @ -
      COLOFF !
    ELSE
      0 COLOFF !
      R@ CX !
    THEN
  THEN
  RDROP
;
 

: ADJUST-FOR-LENGTH
  ( -- )
  LEFT-LEN
  RIGHT-LEN
;


: MOVE_CURSOR
  ( -- )
  DECIMAL
  S\" \e[" ABAPPEND                                          \ first part of escape sequence
  CY @ 1+ <# #S #> ABAPPEND
  S" ;" ABAPPEND
  RX @ 1+ <# #S #> ABAPPEND
  S" H" ABAPPEND
;

: DRAW-STATUS-BAR
  ( -- )
  DECIMAL
  S\" \e[1mcol: \e[7G" ABAPPEND
  RX @ COLOFF @ + <# #S #> ABAPPEND
  S\"   \e[12G" ABAPPEND
  S\" row:\e[17G" ABAPPEND
  CY @ ROWOFF @ + <# #S #> ABAPPEND
  S"   " ABAPPEND
  EDITFILE @
  0<> IF
    S\" \e[25G" ABAPPEND
    DIRTY @ 0=
    IF
      S\" \e[39;49m" ABAPPEND
    ELSE
      S\" \e[31m" ABAPPEND
    THEN
    EDITFILE @ COUNT ABAPPEND
  THEN
  S\" \e[m" ABAPPEND
;

: DRAW-STATUS-MESSAGE
  ( c-addr u -- )
  EDITORSCROLL
  [ decimal 64 ] literal  ZALLOCATE DROPERR BUFFER_PTR !
  0 BUFFER_LEN !
  \ make cursor disappear
  S\" \e[?25l" ABAPPEND
  \ position at top of screen
  S\" \e[1;1H" ABAPPEND
  EDITOR-DRAW-ROWS
  DRAW-STATUS-BAR
  S\" \e[50G  " ABAPPEND 
  ABAPPEND
  MOVE_CURSOR
  \ cursor reappear
  S\" \e[?25h" ABAPPEND
  PRINT_BUFFER
  ABFREE
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

\ Convert everything to a counted string
: EDITOR-ROWS-TO-STRING
( -- c-str* )
  \ first of all count how big an allocation we will need
  0 >R
  ROW-COUNT @ 0 DO
    I RECORDGAP *                                         \ stack: offset
    ROW-RECORDS @ +                                       \ stack: address
    @                                                     \ stack: length
    R> + 1+ >R                                            \ stack:
  LOOP
  \ now allocate and store length
  R@ CELL+                                                \ stack: count+8
  ZALLOCATE DROPERR
  DUP                                                     \ stack: addr addr
  R> SWAP                                                 \ stack: addr count addr
  !                                                       \ stack: addr
  \ now copy the lines
  >R R@ CELL+ >R                                          \ r-stack base-addr target-addr
  ROW-COUNT @ 0 DO
    I RECORDGAP *
    ROW-RECORDS @ + INTRASPACE + @                        \ stack: addr1
    R@                                                    \ stack: addr1 addr2
    I RECORDGAP *
    ROW-RECORDS @ + @ DUP                                 \ stack: addr1 addr2 u u
    \ update r-stack
    R> + >R                                               \ stack: addr1 addr2 u
    MOVE
    [ decimal 10 ] literal R@ C!
    R> 1+ >R
  LOOP
  RDROP
  R>
;

\
\ input section	
\

: SHORTEN-LINE
  ( -- )
  CY @ ROWOFF @ +
  RECORDGAP *
  ROW-RECORDS @ +
  -1 SWAP +!
;

: REGENERATE-RROW
  ( -- )
  CY @ ROWOFF @ + 1+ 
  EDITOR-UPDATE-ROW
; 


: EDITOR-FREE-ROW
  ( u -- )
  \ free row at u
  DUP
  ROW-COUNT @ <=
  IF
    RECORDGAP * ROW-RECORDS @ + CELL+ >R
    R@ @  FREE DROPERR
    R> INTRAGAP + @ FREE DROPERR 
  ELSE
    DROP
  THEN
;

: EDITOR-REORDER-ROWS-UP
  ( u -- )
  \ reorder all rows from u onwards
  >R
  R@ ROW-COUNT @ <=
  IF
    ROW-COUNT @ 1+ R> 1+
    DO
      I RECORDGAP * ROW-RECORDS @ + >R
      4 0 DO
        R@ @ R@ RECORDGAP - !
        R> CELL+ >R
      LOOP
      RDROP
    LOOP
    ROW-COUNT @ RECORDGAP * ROW-RECORDS @ + >R
    4 0 DO
      0 R@ RECORDGAP - !
      R> CELL+ >R
    LOOP
    RDROP
    -1 ROW-COUNT +!
  ELSE
    RDROP
  THEN
;

: INSERT-NEW-SIZE
  ( newsize index -- )
  RECORDGAP * ROW-RECORDS @ + !
;

: INSERT-NEW-STR
  ( newstr index -- )
  RECORDGAP * ROW-RECORDS @ + CELL+ !
;

: EDITOR-JOIN-ROW
  ( u -- )
  DUP DUP 1+ GET-ROW SWAP >R                              \ stack: index index size
  SWAP GET-ROW                                            \ stack: index size *ptr len
  SWAP                                                    \ stack: index size len *ptr
  OVER                                                  \ stack: index size len *ptr len
  3 PICK                                                  \ stack: index size len *ptr len size
  +                                                       \ stack: index size len *ptr newsize
  DUP                                                     \ stack: index size len *ptr newsize newsize
  5 PICK 1-                                               \ stack: index size len *ptr newsize newsize index-
  INSERT-NEW-SIZE                                         \ stack: index size len *ptr newsize
  OVER 0<>                                                \ stack: index size len *ptr newsize bool
  IF
    RESIZE DROPERR                                        \ stack: index size len *newptr
  ELSE
    NIP                                                   \ stack: index size len newsize
    ZALLOCATE DROPERR                                      \ stack: index size len *newptr
  THEN
  DUP                                                     \ stack: index size len *newptr *newptr
  4 PICK  1-                                              \ stack: index size len *newptr *newptr index-
  INSERT-NEW-STR                                          \ stack: index size len *newptr
  +                                                       \ stack: index size *dest
  R> SWAP                                                 \ stack: index size *src *dest
  ROT                                                     \ stack: index *src *dest size
  MOVE                                                    \ stack: index
  DROP
;

: MARK-DIRTY
  ( -- )
  1 DIRTY !   
  S" Unsaved changes                      " DRAW-STATUS-MESSAGE
;

: PROCESS-ENTER
  ( -- )
  CY @ ROWOFF @ + DUP
  \ add a blank line
  INSERT-ROW-ALLOC
  \ fill the blank line
  CX @ COLOFF @ +
  SPLIT-LINES
  MARK-DIRTY
;

: PROCESS-BACKSPACE
  (  -- )
  CY @ ROWOFF @ + >R
  CX @ -1 =
  IF
    R@ 0<>
    IF
      R@ EDITOR-JOIN-ROW
      R@ EDITOR-FREE-ROW
      R@ EDITOR-REORDER-ROWS-UP
      -1 CY +! ENDKEY
      REGENERATE-RROW
      MARK-DIRTY
    ELSE
      0 CX !
    THEN
  ELSE
    \ get the row
    CY @ LINE-LENGTH                                        \ stack: llen
    0<>
    IF
      CX @ COLOFF @ +                                       \ stack: pos
      R@ 1+
      GET-ROW                                               \ stack: pos buff len
      2 PICK                                                \ stack: pos buff len pos
      -                                                     \ stack: pos buff diff
      OVER                                                \ stack: pos buff diff buff
      3 PICK                                                \ stack: pos buff diff buff pos
      +                                                     \ stack: pos buff diff rbuff
      DUP 1+                                                \ stack: pos buff diff rbuff saddr
      SWAP                                                  \ stack: pos buff diff saddr rbuff
      ROT                                                   \ stack: pos buff addr rbuff diff
      MOVE                                                  \ stack: pos buff
      SHORTEN-LINE                                          \ stack: pos buff
      REGENERATE-RROW                                       \ stack: pos buff
      MARK-DIRTY                                            \ stack: pos buff
      2DROP
    ELSE
      DROP
    THEN
  THEN
  RDROP
;


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
      \ no default here
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
      -1 CY +!
      SNAP-TO-LENGTH
    ENDOF
    CHAR B OF                             \ arrow down
      1 CY +!
      SNAP-TO-LENGTH
    ENDOF
    CHAR C OF                             \ arrow right
      1 CX +!
      ADJUST-FOR-LENGTH
    ENDOF
    CHAR D OF                             \ arrow left
      -1 CX +!
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
          ROW-COUNT @ CY @ - ROWOFF !
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
        PROCESS-BACKSPACE 
      THEN
    ENDOF
    \ no default
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

: EDITOR-INSERT-CHAR
  ( char -- )
  CY @ 1+ ROWOFF @ + ROW-COUNT @
  =
  IF
    ADD-ROW
  THEN
  CY @ 1+ ROWOFF @ + CX @ COLOFF @ +          \ stack: char index pos
  ROT                                         \ stack: index pos char
  EDITOR-ROW-INSERT-CHAR
  1 CX +!
  ADJUST-FOR-LENGTH
  1 DIRTY !
  S" Unsaved changes                      " DRAW-STATUS-MESSAGE
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
: ADD-RROWS
  ( -- )
  ROW-COUNT @ 0 DO
    I 1+ EDITOR-UPDATE-ROW
  LOOP
;

: EDITOROPEN
  ( c-addr u  -- )
  EDITFILE @ 0=
  IF
    SAVE-FILE-NAME
  THEN
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
  REPEAT
  ADD-RROWS
  DROP
  LINEBUFFER @ FREE DROPERR
  R> CLOSE-FILE DROP
;

: EDITOR-SAVE
  S"                                      " DRAW-STATUS-MESSAGE
  DECIMAL
  EDITFILE @ 0<>
  DIRTY @ 0<> AND
  IF
    EDITOR-ROWS-TO-STRING                             \ stack: *c-str
    EDITFILE @ @                                      \ stack: *c-str len
    EDITFILE @ CELL+                                  \ stack: *c-str len *str
    SWAP
    R/W CREATE-FILE                                   \ stack: *c-str fileid ior
    0= IF
      DUP
      >R
      SWAP
      DUP                                             \ stack: fileid *c-str *c-str
      @                                               \ stack: fileid *c-str u
      SWAP                                            \ stack: fileid u *c-str
      CELL+                                           \ stack: fileid u c-addr
      SWAP                                            \ stack: fileid c-addr u
      ROT                                             \ stack: c-addr u fileid
      WRITE-FILE                                      \ stack: len
      <# #S #>
      S\"  :BYTES SAVED"
      S+ 2>R
      2R@ DRAW-STATUS-MESSAGE
      2R> DROP FREE DROPERR
      R> CLOSE-FILE DROPERR
      0 DIRTY !
    ELSE
      S\" SAVING FAILED" DRAW-STATUS-MESSAGE
    THEN
  ELSE
    S\" NOTHING TO SAVE" DRAW-STATUS-MESSAGE
  THEN
  500 MS
  S"                       " DRAW-STATUS-MESSAGE
;   


: EDITOR-PROCESS-KEYPRESS
  ( --  )
  EDITOR-READ-KEY                                       \ stack: *ptr, char
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
    [ decimal 127 ] literal OF                           \ TODO : Handle backspace
      SWAP
      DROP
      -1 CX +!
      PROCESS-BACKSPACE
    ENDOF
    CHAR L [ hex 1F ] literal and OF
      SWAP
      DROP
    ENDOF
    CHAR S [ HEX 1F ] LITERAL AND OF
      SWAP
      DROP
      EDITOR-SAVE
    ENDOF
    CHAR Z [ HEX 1F ] LITERAL AND OF
      SWAP
      DROP
      DIRTY @ 0<>
      EDITFILE @ 0<>
      AND
      IF
        CLEANROWS
        EDITFILE @ CELL+
        EDITFILE @ @
        EDITOROPEN
        S"                                        " DRAW-STATUS-MESSAGE
        0 DIRTY !
      THEN
    ENDOF
    [ DECIMAL 13 ] LITERAL OF
      SWAP DROP
      PROCESS-ENTER
    ENDOF
    SWAP DROP DUP
    EDITOR-INSERT-CHAR                            \ default
  ENDCASE
;

\
\ main code section                                    \
\
: fori ( -- n )
  DECIMAL
  0 CX !
  0 RX !
  0 CY !
  0 ROW-RECORDS !
  0 ROW-COUNT !
  0 ROWOFF !
  0 COLOFF !
  0 EDITFILE !
  0 DIRTY !
  [ decimal 8192 ] literal SIZE-OF-ROWALLOC !
  GET-WINDOW-SIZE
  PARSE-NAME
  DUP 0<>
  IF
    EDITOROPEN
  ELSE
    2DROP
  THEN
  S" HELP: Ctrl-S - save, Ctrl-Q - quit" DRAW-STATUS-MESSAGE
  BEGIN
    EDITOR-REFRESH-SCREEN
    EDITOR-PROCESS-KEYPRESS
    FALSE
  UNTIL
  0
;

