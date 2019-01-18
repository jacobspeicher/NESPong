    ; sprite attributes:
    ;       7 6 5 4 3 2 1 0
    ;       | | |       | |
    ;       | | |       + + - color palette of sprite
    ;       | | + - - - - - - Priority (0: in front of background, 1 : behind background)
    ;       | + - - - - - - - Flip sprite horizontally
    ;       + - - - - - - - - Flip sprite vertically
    ; vert tile attr horiz

ball:
  .db $80, $0B, $00, $78

paddle1:
  .db $80, $0C, $01, $60
  .db $88, $0D, $01, $60
  .db $90, $0C, %10000001, $60

paddle2:
  .db $80, $0C, $02, $90
  .db $88, $0D, $02, $90
  .db $90, $0C, %10000010, $90
