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

\ is character on stack CTRL Key for that letter
: CTRL-KEY ( n c -- f )
  [ HEX 1F DECIMAL ] LITERAL .s AND
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
[ DECIMAL 16 ] LITERAL ALLOCATE
0=
  IF
    >R
    STDOUT TIOCGWINSZ R@ IOCTL
    0=
      IF
        DROP
        R@ @ [ HEX FF ] LITERAL AND  ROWS !
        R@ 2 + @ [ HEX FF ] LITERAL AND COLUMNS !
        R> FREE
      ELSE
        ." ERRNO: " .
        R> FREE
        ABORT" IOCTL failed"
      THEN
  ELSE
    ABORT" ALLOCATE failed"
  THEN ;

: EDITOR-READ-KEY
  ( -- c )
  KEYRAW
;

: CRLF
  ( -- )
  [ DECIMAL 13 ] LITERAL EMIT [ DECIMAL 10 ] LITERAL EMIT
;

\
\ output section
\

: EDITOR-DRAW-ROWS
  ( -- )
  ROWS @ 1+ 1 DO 1 I AT-XY
  CHAR ~ EMIT
  I ROWS @ < IF CRLF THEN
  LOOP ;

: EDITOR-RESET-SCREEN
  ( -- )
  TERMIOSSTRING "2J"
  TERMIOSSTRING "H"
;

: EDITOR-REFRESH-SCREEN
( -- )
  EDITOR-RESET-SCREEN
  EDITOR-DRAW-ROWS
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
     EDITOR-RESET-SCREEN ABORT" Leaving FILO " CRLF  ENDOF
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
