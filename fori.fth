: CRLF
  ( -- )
  13 emit 10 emit
;

\ is character on stack CTRL Key for that letter
: CTRL-KEY ( n c -- f )
[ HEX 1F ] LITERAL AND
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

: fori ( -- n )
  BEGIN
    KEYRAW
    DUP CHAR Q CTRL-KEY IF EXIT THEN
    DUP >STRING DUP CELL+ SWAP @  TYPE
    DUP
    ISCNTRL
    IF
      DROP CRLF
    ELSE
      DUP ." (" EMIT ." )" CRLF
    THEN
    0
  UNTIL
0 ;
