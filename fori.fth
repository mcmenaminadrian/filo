\
\ Utility words					      *
\

\ is character on stack CTRL Key for that letter
: CTRL-KEY ( n c -- f )
  [ HEX 1F DECIMAL ] LITERAL .s AND
  = IF TRUE ELSE FALSE THEN ;


: iscntrl ( c -- f)
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

: EDITOR-READ-KEY
  ( -- c )
  KEYRAW
;

: CRLF
  ( -- )
  13 emit 10 emit
;

\
\ output section
\

: EDITOR-DRAW-ROWS
  ( -- )
  24 0 DO 0 I AT-XY
  CHAR ~ EMIT CRLF LOOP ;

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
   CHAR Q [ hex 1F decimal ] literal AND  OF
     EDITOR-RESET-SCREEN ABORT" Leaving Forth Interactive " CRLF  ENDOF
  ENDCASE
;


\
\ main code section                                    \
\
: fori ( -- n )
  EDITOR-REFRESH-SCREEN
  BEGIN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
  0
;
