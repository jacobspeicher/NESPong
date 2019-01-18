  .inesprg 1              ; 1x 16KB PRG
  .ineschr 1              ; 1x 8KB CHR
  .inesmap 0              ; no bank swapping
  .inesmir 1              ; background mirroring

;; VARIABLES ;;
  .rsset $0000            ; set variables to start at address $0000

nmistate            .rs 1 ; 0 - disable NMIs, 1 - enable NMIs
gamestate           .rs 1 ; 0 - Title, 1 - Playing, 2 - Game Over
loadstate           .rs 1 ; 1 - background data already loaded
ballx               .rs 1 ; current ball horiztonal position
bally               .rs 1 ; current ball vertical position
ballup              .rs 1 ; 1 = ball moving up
balldown            .rs 1 ; 1 = ball moving down
ballleft            .rs 1 ; 1 = ball moving left
ballright           .rs 1 ; 1 = ball moving right
ballspeedx          .rs 1 ; current horiztonal movement speed
ballspeedy          .rs 1 ; current vertical movement speed
paddle1y            .rs 1 ; current vertical position for player 1 paddle
paddle1x            .rs 1 ; current horizontal position for player 1 paddle
paddle2y            .rs 1 ; current vertical position for player 2 paddle
paddle2x            .rs 1 ; current horizontal position for player 2 paddle
buttons1            .rs 1 ; player 1 gamepad input, one bit per button
buttons2            .rs 1 ; player 2 gamepad input, one bit per button
scoreones1          .rs 1 ; current ones place for player 1 score
scoretens1          .rs 1 ; current tens place for player 1 score
scoreones2          .rs 1 ; current ones place for player 2 score
scoretens2          .rs 1 ; current tens place for player 2 score
backgroundLowByte   .rs 1 ; low byte for the background data pointer (16b)
backgroundHighByte  .rs 1 ; high byte for background data pointer (16b)
paddlemoving1       .rs 1 ; 0 - not moving, 1 - moving up, 2 - moving down
paddlemoving2       .rs 1 ; 0 - not moving, 1 - moving up, 2 - moving down
gamewinner          .rs 1 ; 1 - Player 1 won, 2 - Player 2 won

;; CONSTANTS ;;
STATE_TITLE           = $00 ; display title screen
STATE_PLAYING         = $01 ; game is in session
STATE_END             = $02 ; display game over screen

RIGHT_WALL            = $F4 ; far right of the screen
LEFT_WALL             = $04 ; far left of the screen
TOP_WALL              = $18 ; top of the screen
BOTTOM_WALL           = $D8 ; bottom of the screen
PADDLE_BOTTOM_WALL    = $C8 ; bottom of the screen + 2 tiles so that the paddles stop in the right spot

PADDLE1_X             = $08 ; player 1 paddle horizontal position, doesn't change
PADDLE2_X             = $F0 ; player 2 paddle horizontal position, doesn't change

PADDLE_BOTTOM_OFFSET  = $18 ; for correct collission detection for bottoms of paddles
PADDLE_TOP_OFFSET     = $08 ; for correct collision detection for top of paddles
PADDLE_SIDE_OFFSET    = $06 ; for correct collision detection for sides of paddles

WINNER_OFFSET         = $24 ; offset for finding the correct tile in tileset

  
  .bank 0
  .org $C000

vblankwait:               ; wait for VBLANK to make sure the PPU is ready
  BIT $2002
  BPL vblankwait
  RTS

RESET:
  SEI                     ; disable IRQs
  CLD                     ; disable decimal mode
  LDX #$40
  STX $4017               ; disable APU frame IRQ
  LDX #$FF
  TXS                     ; set up stack
  INX                     ; X = 0
  STX $2000               ; disable NMI
  STX $2001               ; disable rendering
  STX $4010               ; disable DMC IRQs

  JSR vblankwait

clrmem:
  LDA #$00                ; fill $0100-$0700 with x00, except $0200
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE                ; fill $0200 with xFE
  STA $0200, x
  INX 
  BNE clrmem
  JSR vblankwait          ; wait for VBLANK again to make sure PPU is ready

LoadPalettes:
  LDA $2002               ; read PPU status to reset the high/low latch (resets PPU)
  LDA #$3F
  STA $2006               ; write high byte ($3F)
  LDA #$00
  STA $2006               ; write low byte ($00)
  
  LDX #$00                ; start loop at 0
.Loop:
  LDA palettes, x         ; load from palette address, offset by x
  STA $2007
  INX
  CPX #$20                ; compare to x20 - d32, so it will load 32B of palette data
  BNE .Loop               ; while X is not x20, continue loading

LoadAttributes:
  LDA $2002               ; reset PPU
  LDA #$23
  STA $2006
  LDA #$C0
  STA $2006

  LDX #$00
.Loop:
  LDA titleattributes, x  ; load in the title screen attributes, offset by x
  STA $2007               ; send to the PPU
  LDA gameattributes, x   ; load in the game screen attributes, offset by x
  STA $2007
  LDA endattributes, x    ; load in the end screen attributes, offset by x
  STA $2007
  INX
  CPX #$40                ; run this loop 64 times
  BNE .Loop

LoadSprites:
  LDX #$00
.Loop:
  LDA gamesprites, x
  STA $0200, x
  INX 
  CPX #$1C
  BNE .Loop

EndLoadSprites:

  LDA #STATE_TITLE
  STA gamestate           ; load the initial gamestate to be the title screen

  LDA #$01
  STA nmistate            ; initialize NMIs to be enabled

  LDA #$02
  STA ballspeedx
  STA ballspeedy          ; initialize speeds to x02

  LDA #%10000000          ; enable NMI, sprites from Pattern 0, background from Pattern 1
  STA $2000
  LDA #%00011110          ; enable sprites, enable background, no clipping from left side
  STA $2001

infinite:
  JMP infinite            ; infinite loop to wait for NMI

;;; GRAPHICS ;;;

LoadTitle:
  LDA #$01
  STA loadstate           ; title screen has been loaded

  LDA #LOW(titlescreen)
  STA backgroundLowByte   ; store low byte of titlescreen background
  LDA #HIGH(titlescreen)
  STA backgroundHighByte  ; store high byte of titlescreen background

  LDX #$00                ; need to initialize both X and Y
  LDY #$00                ; for a double for loop
.Loop:
  LDA [backgroundLowByte], y  ; load background tile offset by y
  STA $2007                   ; send bit to the PPU to be drawn

  INY                         ; increment the inner for loop
  CPY #$00                    ; if Y == x00, then it has run 256 times
  BNE .Loop                   ; rerun the loop if Y != 0

  INC backgroundHighByte  ; increment they high byte
                          ; this will move to the next $0i00 address
                          ; since the inner loop has run through
                          ; all $0ijj addresses
  INX
  CPX #$04                ; outer loop runs 4 times (4 * 256 = 1024)
  BNE .Loop

  LDA #$00
  STA nmistate            ; clear NMI flag
  LDA #%10000000
  STA $2000               ; enable NMI
  RTS

LoadPlaying:
  LDA #$01
  STA loadstate           ; game screen has been loaded

  LDA #LOW(gamescreen)
  STA backgroundLowByte   ; store low byte of gamescreen background
  LDA #HIGH(gamescreen)
  STA backgroundHighByte  ; store high byte of gamescreen background

  LDX #$00                ; need to initialize both X and Y
  LDY #$00
.Loop:
  LDA [backgroundLowByte], y  ; load low byte offset by Y
  STA $2007

  INY
  CPY #$00                    ; run through all 256 addresses of current page
  BNE .Loop

  INC backgroundHighByte      ; move to the next "page" of background data
  INX
  CPX #$04
  BNE .Loop

  LDA #$00
  STA nmistate            ; clear NMI flag
  LDA #%10000000
  STA $2000               ; enable NMI

  RTS

LoadEnd:
  LDA #$01
  STA loadstate           ; end screen has been loaded

  LDA #LOW(endscreen)     ; store low byte of endscreen background
  STA backgroundLowByte
  LDA #HIGH(endscreen)    ; store high byte of endscreen background
  STA backgroundHighByte

  LDX #$00
  LDY #$00                ; initialize both x and y to x00
.Loop:
  LDA [backgroundLowByte], y  ; low byte offset by y
  STA $2007               ; send if off to the PPU

  INY
  CPY #$00                ; run through 256 addresses of current page
  BNE .Loop

  INC backgroundHighByte  ; move to the next "page" of background data
  INX
  CPX #$04                ; run this whole loop 4 times
  BNE .Loop

  LDA #$00
  STA nmistate            ; restart the NMIs
  LDA #%10000000
  STA $2000               ; enable NMI again

  LDA $2002             ; reset the PPU
  LDA #$20
  STA $2006
  LDA #$F2
  STA $2006             ; start at $2041 for player 1 score

  LDA gamewinner        ; load the winner (1 or 2)
  CLC
  ADC WINNER_OFFSET     ; add the winner offset to get the right tile
  STA $2007             ; send to the PPU to update the winner tile

  RTS

CenterBall:
  LDA #$80
  STA bally
  LDA #$78
  STA ballx             ; center the ball

  RTS

ServeBall:
  LDA #$01
  STA ballspeedy
  STA ballspeedx        ; set the ball x and y speed so that it's easier to hit

  RTS

UpdateSprites:
  LDA bally               ; update ball x and y
  STA $0200
  LDA ballx
  STA $0203

  LDA paddle1y            ; update paddle 1 y positions
  STA $0204
  LDA $0204
  CLC
  ADC #$08
  STA $0208
  LDA $0208
  CLC
  ADC #$08
  STA $020C

  LDA paddle2y            ; update paddle 2 y positions
  STA $0210
  LDA $0210
  CLC
  ADC #$08
  STA $0214
  LDA $0214
  CLC
  ADC #$08
  STA $0218

  LDA paddle1x
  STA $0207
  STA $020B
  STA $020F

  LDA paddle2x
  STA $0213
  STA $0217
  STA $021B
  
  RTS

;;; INPUT ;;;

CheckInput:
  LDA #$00
  STA paddlemoving1     ; reset paddle 1 movement
  STA paddlemoving2     ; reset paddle 2 movement

CheckButtons1Up:
  LDA buttons1          ; check Player 1 movement
  CMP #%00001000        ; A B Select Start Up Down Left Right
  BNE CheckButtons1Down
  JSR CheckPaddle1Up

CheckButtons1Down:
  LDA buttons1
  CMP #%00000100
  BNE CheckButtons2Up
  JSR CheckPaddle1Down

CheckButtons2Up:
  LDA buttons2          ; check Player 2 movement
  CMP #%00001000
  BNE CheckButtons2Down
  JSR CheckPaddle2Up

CheckButtons2Down:
  LDA buttons2
  CMP #%00000100
  BNE CheckInputDone
  JSR CheckPaddle2Down

CheckInputDone:
  LDA paddlemoving1
  LDA paddlemoving2

  JSR CheckPaddleCollision

  RTS

CheckPaddle1Up:
  LDA paddle1y
  CMP #TOP_WALL
  BNE MovePaddle1Up     ; if paddle = TOP_WALL stop moving
  RTS

CheckPaddle1Down:
  LDA paddle1y
  CMP #PADDLE_BOTTOM_WALL
  BNE MovePaddle1Down   ; if paddle = BOTTOM_WALL stop moving
  RTS

CheckPaddle2Up:
  LDA paddle2y
  CMP #TOP_WALL
  BNE MovePaddle2Up
  RTS

CheckPaddle2Down:
  LDA paddle2y
  CMP #PADDLE_BOTTOM_WALL
  BNE MovePaddle2Down
  RTS

MovePaddle1Up:
  LDA paddle1y
  SEC
  SBC #$02
  STA paddle1y

  LDA #$01
  STA paddlemoving1       ; x01 means moving up

  RTS

MovePaddle1Down:
  LDA paddle1y
  CLC
  ADC #$02
  STA paddle1y

  LDA #$02
  STA paddlemoving1       ; x02 means moving down
  RTS

MovePaddle2Up:
  LDA paddle2y
  SEC
  SBC #$02
  STA paddle2y

  LDA #$01
  STA paddlemoving2       ; x01 means moving up
  RTS
  
MovePaddle2Down:
  LDA paddle2y
  CLC
  ADC #$02
  STA paddle2y

  LDA #$02
  STA paddlemoving2       ; x02 means moving down
  RTS

;;; Check Paddle Collision ;;;

CheckPaddleCollision:
  LDA ballleft
  CMP #$01
  BEQ CheckPaddleLeftCollision

CheckPaddleRightCollision:
  LDA paddle2x
  SEC
  SBC #PADDLE_SIDE_OFFSET         ; subtract the side offset to make the ball
  CMP ballx                       ; collide at the right spot
  BCS CheckPaddleCollisionDone    ; ballx < paddle2x - PADDLE_SIDE_OFFSET

  LDA paddle2y
  SEC
  SBC #PADDLE_TOP_OFFSET          ; subtract the top offset so that the ball will
  CMP bally                       ; bounce off the top corner
  BCS CheckPaddleCollisionDone    ; bally > paddle2y - PADDLE_TOP_OFFSET

  LDA paddle2y
  CLC
  ADC #PADDLE_BOTTOM_OFFSET       ; add the bottom offset so that the paddle
  CMP bally                       ; collision box will extend the right amount
  BCC CheckPaddleCollisionDone    ; bally < paddle2y + PADDLE_BOTTOM_OFFSET

  JSR ApplyEnglishRight

  LDA #$00
  STA ballright
  LDA #$01
  STA ballleft

  LDA ballspeedy
  CMP #$01
  BNE CheckPaddleCollisionDone

  LDA #$02
  STA ballspeedy                  ; reset to the original ball x and y speeds
  STA ballspeedx

  JMP CheckPaddleCollisionDone

CheckPaddleLeftCollision:
  LDA paddle1x
  CLC
  ADC #PADDLE_SIDE_OFFSET         ; add the side offset to make the ball
  CMP ballx                       ; collide at the right spot
  BCC CheckPaddleCollisionDone    ; ballx > paddle1x + PADDLE_SIDE_OFFSET

  LDA paddle1y
  SEC
  SBC #PADDLE_TOP_OFFSET          ; subtract the top offset so that the ball will
  CMP bally                       ; bounce off the top corner
  BCS CheckPaddleCollisionDone    ; bally > paddle1y - PADDLE_TOP_OFFSET

  LDA paddle1y
  CLC
  ADC #PADDLE_BOTTOM_OFFSET       ; add the bottom offset so that the paddle
  CMP bally                       ; collision box will extend the right amount
  BCC CheckPaddleCollisionDone    ; bally < paddle1y + PADDLE_BOTTOM_OFFSET

  JSR ApplyEnglishLeft

  LDA #$00
  STA ballleft
  LDA #$01
  STA ballright

  LDA ballspeedy
  CMP #$01
  BNE CheckPaddleCollisionDone

  LDA #$02
  STA ballspeedy                    ; reset to the original ball x and y speed
  STA ballspeedx

  JMP CheckPaddleCollisionDone

CheckPaddleCollisionDone:
  RTS

ApplyEnglishRight:
  LDA ballup
  CMP #$01
  BNE ApplyEnglishRightDown

  LDA paddlemoving2
  CMP #$01
  BNE ResetBallSpeed      ; if the paddle is not moving in the same direction
                          ; then reset the ball speed to normal
  LDA #03
  STA ballspeedy
  STA ballspeedx

  RTS

ApplyEnglishRightDown:
  LDA paddlemoving2
  CMP #$02
  BNE ResetBallSpeed        ; if the paddle is not moving in the same direction
                            ; then reset the ball speed to normal
  LDA #$03
  STA ballspeedy
  STA ballspeedx

  RTS

ApplyEnglishLeft:
  LDA ballup
  CMP #$01
  BNE ApplyEnglishLeftDown

  LDA paddlemoving1
  CMP #$01
  BNE ResetBallSpeed        ; if the paddle is not moving in the same direction
                            ; then reset the ball speed to normal
  LDA #$03
  STA ballspeedy
  STA ballspeedx

  RTS

ApplyEnglishLeftDown:
  LDA paddlemoving1
  CMP #$02
  BNE ResetBallSpeed        ; if the paddle is not moving in the same direction
                            ; then reset the ball speed to normal
  LDA #$03
  STA ballspeedy
  STA ballspeedx

  RTS

ResetBallSpeed:
  LDA #$02
  STA ballspeedy
  STA ballspeedx

  RTS

;;; SCORE UPDATES ;;;

CheckScore:

CheckScore1:
  LDA scoretens1
  CMP #$00
  BNE CheckScore2

  LDA scoreones1
  CMP #$05
  BNE CheckScore2

  LDA #$01
  STA gamewinner      ; player 1 wins
  LDA #STATE_END
  STA gamestate       ; go to end screen
  LDA #$00
  STA loadstate       ; redraw the screen
  LDA #$01
  STA nmistate        ; disable NMIs to redraw the screen

  JSR ClearGame

  JMP CheckScoreDone

CheckScore2:
  LDA scoretens2
  CMP #$00
  BNE CheckScoreDone

  LDA scoreones2
  CMP #$05
  BNE CheckScoreDone

  STA $0041
  LDA #$02
  STA gamewinner      ; player 2 wins
  LDA #STATE_END
  STA gamestate       ; go to end screen
  LDA #$00
  STA loadstate       ; redraw the screen
  LDA #$01
  STA nmistate        ; disable NMIs to redraw the screen

  JSR ClearGame

  JMP CheckScoreDone

ClearGame:
  LDA #$00
  STA scoretens1
  STA scoreones1
  STA scoretens2
  STA scoreones2      ; clear all the score values

  STA ballup
  STA balldown
  STA ballleft
  STA ballright       ; clear all ball movement, so it can move again

  RTS

CheckScoreDone:
  RTS

DrawScore:
  LDA $2002             ; reset the PPU
  LDA #$20
  STA $2006
  LDA #$41
  STA $2006             ; start at $2041 for player 1 score

  LDA scoretens1        ; get first digit for score 1
  STA $2007             ; if the digits aren't on the tilesheet starting at 00
                        ; then an offset would have needed to be used
  LDA scoreones1        ; get the second digit for score 1
  STA $2007

  LDA $2002
  LDA #$20
  STA $2006
  LDA #$5D
  STA $2006             ; start at $205D for player 2 score

  LDA scoretens2        ; get the first digit for score 2
  STA $2007
  
  LDA scoreones2        ; get the second digit for score 2
  STA $2007

  RTS

IncrementScore1:

IncOnes1:
  LDA scoreones1        ; ones place digit of the score for player 1
  CLC
  ADC #$01              ; add one
  STA scoreones1
  CMP #$0A              ; check if the new ones place is 10
  BNE IncDone           ; if the ones place isn't over 10, ya done baby

IncTens1:
  LDA #$00
  STA scoreones1        ; reset the ones place to 0

  LDA scoretens1        ; tens place digit of the score for player 1
  CLC
  ADC #$01            
  STA scoretens1        
  CMP #$0A              ; check if the tens place is 10
  BNE IncDone           ; if the tens place ain't over 10, ya done baby

IncrementScore2:

IncOnes2:
  LDA scoreones2        ; ones place digit of the score for player 2
  CLC
  ADC #$01              ; add one
  STA scoreones2
  CMP #$0A              ; check if ones place is 10
  BNE IncDone           ; if it's not over 10, just stop just stop!

IncTens2:
  LDA #$00
  STA scoreones2        ; reset the ones place to 0

  LDA scoretens2        ; tens place digit of the score for player 2
  CLC
  ADC #$01              ; add one
  STA scoretens2
  CMP #$0A              ; check if the tens place is 10
  BNE IncDone           ; if it ain't why don't you just skip on to being done

IncDone:
  JSR CheckScore
  JSR ServeBall
  JSR CenterBall

  RTS

;;; NMI ;;;

NMI:
  LDA #$00
  STA $2003               ; set the low byte ($00)
  LDA #$02
  STA $4014               ; set the hight byte ($02), start transfer of sprites

  LDA gamestate
  CMP #$01
  BNE SkipDrawScore
  JSR DrawScore

SkipDrawScore:
  LDA $2002
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006

  JSR ReadController1     ; get current button data for player 1
  JSR ReadController2     ; get current button data for player 2

  LDA nmistate
  CMP #$01
  BEQ NMISet
  JMP NMIClear

NMISet:                   ; set up background rendering
  LDA #$00
  STA $2001               ; disable rendering

  LDA $2002               ; reset PPU high/low latch
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006

  LDA #%00000000
  STA $2000               ; disable NMI

;;; GAME ENGINE ;;;

GameEngine:
  LDA gamestate
  CMP #STATE_TITLE
  BEQ EngineTitle         ; gamestate = x00, display title screen

  LDA gamestate
  CMP #STATE_PLAYING
  BEQ EnginePlaying       ; gamestate = x01, game time

  LDA gamestate
  CMP #STATE_END
  BEQ EngineEnd           ; gamestate = x02, game over

GameEngineDone:
  JSR UpdateSprites
  RTI                     ; return from interrupt

EngineTitle:
  LDA loadstate
  CMP #00
  BNE CheckTitle

  LDA #$78
  STA ballx
  LDA #$88
  STA bally               ; center the ball

  LDA #$80
  STA paddle1y
  LDA #$80
  STA paddle2y            ; center the paddles

  LDA #$68
  STA paddle1x
  LDA #$88
  STA paddle2x            ; put the balls on either side of the ball

  JSR LoadTitle
  JMP CheckTitle

EnginePlaying:
  LDA loadstate
  CMP #$00
  BNE CheckPlaying
  JSR LoadPlaying
  JMP CheckPlaying

EngineEnd:
  LDA loadstate
  CMP #$00
  BNE CheckEnd

  LDA #$FE
  STA ballx
  STA bally
  STA paddle1y
  STA paddle2y
  STA paddle1x
  STA paddle2x          ; send all the sprites off the screen

  JSR LoadEnd
  JMP CheckEnd

CheckTitle:
  LDA buttons1
  CMP #%00010000        ; A B Select Start Up Down Left Right
  BNE GameEngineDone    ; if start button is not pressed, go back to GameEngine loop
  JMP InitializeGame

CheckPlaying:
  JSR CheckInput
  JMP MoveBall

CheckEnd:
  LDA buttons1
  CMP #%00100000
  BNE GameEngineDone
  
  LDA #STATE_TITLE
  STA gamestate
  LDA #$00
  STA loadstate
  LDA #$01
  STA nmistate

  JMP GameEngineDone

InitializeGame:
  LDA #$00
  STA loadstate         ; reload the background
  LDA #$01
  STA gamestate         ; game is now in the playing state
  LDA #$01
  STA nmistate          ; block NMIs so that background loading can occur

  LDA #$80
  STA bally
  LDA #$78
  STA ballx             ; center the ball

  LDA #$01
  STA ballright         ; start the ball moving to the right
  STA balldown          ; start the ball moving down


  LDA #PADDLE1_X
  STA paddle1x
  LDA #PADDLE2_X
  STA paddle2x          ; place the paddles in the right horizontal positions

  JMP infinite

MoveBall:
  LDA ballright
  STA $0040
  LDA balldown
  STA $0041

MoveBallRight:
  LDA ballright
  BEQ MoveBallRightDone ; if ballright = 0, then skip this cause it aint movin

  LDA ballx
  CLC
  ADC ballspeedx        ; ballx += ballspeedx
  STA ballx

  LDA ballx
  CMP #RIGHT_WALL
  BCC MoveBallRightDone ; if ballx < right wall, then skip this cause it movin

  LDA #$00
  STA ballright         ; it's hit the right wall, so make it move left now
  LDA #$01
  STA ballleft

  JSR IncrementScore1

MoveBallRightDone:

MoveBallLeft:
  LDA ballleft
  BEQ MoveBallLeftDone  ; if ballleft = 0 then skip it cause it aint movin

  LDA ballx
  SEC
  SBC ballspeedx        ; ballx -= ballspeedx
  STA ballx

  LDA ballx
  CMP #LEFT_WALL
  BCS MoveBallLeftDone  ; if ballx > left_wall then skip cause it still movin

  LDA #$00
  STA ballleft          ; it's hit the left wall so make it move the other way
  LDA #$01
  STA ballright

  JSR IncrementScore2

MoveBallLeftDone:

MoveBallUp:
  LDA ballup
  BEQ MoveBallUpDone    ; it aint movin up

  LDA bally
  SEC
  SBC ballspeedy
  STA bally

  LDA bally
  CMP #TOP_WALL
  BCS MoveBallUpDone    ; bally > TOP_WALL, it's still movin

  LDA #$00
  STA ballup
  LDA #$01
  STA balldown

MoveBallUpDone:

MoveBallDown:
  LDA balldown
  BEQ MoveBallDownDone  ; it ain't movin

  LDA bally
  CLC
  ADC ballspeedy
  STA bally

  LDA bally
  CMP #BOTTOM_WALL
  BCC MoveBallDownDone  ; bally < BOTTOM_WALL, it's still movin

  LDA #$00
  STA balldown
  LDA #$01
  STA ballup

MoveBallDownDone:
  JMP GameEngineDone


NMIClear:
  LDA #%00011110
  STA $2001       ; enable rendering
  LDA #$00
  STA $2005
  STA $2005
  JMP GameEngine

ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
.Loop:
  LDA $4016
  LSR A         ; bit 0 -> carry
  ROL buttons1  ; bit 0 <- carry 
                ; (loads currently pressed buttons into buttons1 variable)
  DEX
  BNE .Loop     ; while X != 0, runs 8 times (8 bits for controller input)
  RTS

ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
.Loop:
  LDA $4017
  LSR A         ; bit 0 -> carry
  ROL buttons2  ; bit 0 <- carry
                ; (loads currently pressed buttons into buttons2 variable)
  DEX
  BNE .Loop     ; while X != 0, runs 8 times (8 bits for controller input)
  RTS


  .bank 1
  .org $E000

titlescreen:
  .include "graphics/titlescreen.asm"
titleattributes:
  .include "graphics/attributes.asm"

  ; $E400
gamescreen:
  .include "graphics/gamescreen.asm"
gameattributes:
  .include "graphics/attributes.asm"

  ; E800
endscreen:
  .include "graphics/endscreen.asm"
endattributes:
  .include "graphics/attributes.asm"

palettes:
  .include "graphics/palettes.asm"
gamesprites:
  .include "graphics/sprites.asm"

  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0

  .bank 2
  .org $0000
  .incbin "graphics/pong.chr"
