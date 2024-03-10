: fori ( -- n )
  BEGIN
    RAWON
    KEYRAW .s emit cr
    RAWOFF
    0
  UNTIL
0 ;
