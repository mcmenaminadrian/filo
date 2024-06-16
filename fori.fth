\
\ Variables
\
VARIABLE ROWS
VARIABLE COLUMNS
HEX 5413 CONSTANT TIOCGWINSZ
DECIMAL 0 CONSTANT STDIN
DECIMAL 1 CONSTANT STDOUT
DECIMAL 2 CONSTANT STDERR


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
  R> FREE
ELSE
  ." ERRNO: " . CR
  R> FREE
  ABORT" IOCTL failed"
THEN
;

: EDITOR-READ-KEY
  ( -- c )
  KEYRAW NOP
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
>R                         \ store length to be added on returstack
BUFFER_LEN @               \ get existing length
0<> IF                     \ if ... not 0
  BUFFER_PTR @ 
  BUFFER_LEN @ R@ + RESIZE \ get a buffer of new size
  DROPERR
  R@ BUFFER_LEN @ +
  BUFFER_LEN !
ELSE                       \ ... zero
  R@ ALLOCATE DROPERR      \ get a buffer of new size
  R@ BUFFER_LEN !
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

: EDITOR-DRAW-ROWS
(  -- )
ROWS @ 1+ 1 DO
  1 ALLOCATE DROPERR
  DUP
  CHAR ~ SWAP C!
  DUP 1 ABAPPEND
  I ROWS @ <
  IF
    2 ALLOCATE DROPERR 
    DUP DUP
    [ DECIMAL 13 ] LITERAL SWAP C! [ DECIMAL 10 ] LITERAL SWAP 1+ C!
    DUP 2 ABAPPEND 
  THEN
LOOP
;

: EDITOR-RESET-SCREEN
  ( -- )
7 ALLOCATE DROPERR
>R
[ decimal 27 ] literal R@ C!
CHAR [ R@ 1+ C!
CHAR 2 R@ 2+ C!
CHAR J R@ 3 + C!
[ decimal 27 ] literal R@ 4 + C!
CHAR [ R@ 5 + C!
CHAR H R@ 6 + C!
R@ 7 TYPE
R> FREE DROPERR
;

: EDITOR-REFRESH-SCREEN
( -- )
EDITOR-RESET-SCREEN 
EDITOR-DRAW-ROWS
BUFFER_PTR @ BUFFER_LEN @ TYPE
ABFREE
  
0 0  AT-XY
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
      ABORT" Leaving FILO " CRLF  
    ENDOF
  ENDCASE
;


\
\ main code section                                    \
\
: fori ( -- n )
  GET-WINDOW-SIZE
  EDITOR-REFRESH-SCREEN
  BEGIN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
  0
;

