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
\ terminal section                                     \
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
\ input section					      \
\

: EDITOR-PROCESS-KEYPRESS
( --  )
EDITOR-READ-KEY
CASE
 CHAR Q [ hex 1F decimal ] literal AND  OF ABORT" Quitting..." CRLF  ENDOF
ENDCASE
;


\
\ main code section                                    \
\
: fori ( -- n )
  BEGIN
    EDITOR-PROCESS-KEYPRESS
    0
  UNTIL
0 ;
