include "ggsound.inc"

Header:
.byte "NES",$1a
.byte $02 ;; 1x 16KB bank of PRG code
.byte $01 ;; 1x 8KB bank of CHR data
.byte $00|%0000 ;; mapper 0 = NROM, no bank swapping ;; background mirroring ($01 = VERT mirroring for HORIZONTAL scroll)
;;.byte %0001 ;;$00
.dsb 9, $00 ;clear the remaining bytes
;;.byte 0,0,0,0,0,0,0,0  ;; pad header to 16 bytes 
;>
Variables:
  .enum $0000       ;; put pointers in zero page
	pointerLo  			  .dsb 1 ;; pointer variables are declared in RAM
	pointerHi  			  .dsb 1 ;; low byte first, high byte immediately after
	enemySlotSprite		  .dsb 1 ;; keep in zero page
	hostBltSlotSprite	  .dsb 1 ;; keep in zero page
	EnemyDataLow		  .dsb 1 ;; keep in zero page (shadow)
	EnemyDataHigh		  .dsb 1 ;; keep in zero page (shadow)
	EventDataLow		  .dsb 1 ;;
	EventDataHigh		  .dsb 1 ;;
	rowLow				  .dsb 1 ;;
	rowHigh			  	  .dsb 1 ;;
	octant 				  .dsb 1 ;;used for atan2
	include "ggsound_zp.inc"

	.ende
	
  ;;$0200 is sprite shadow OAM (Object Attribute Memory)
	
  .enum $0300
	buttons1 			 	.dsb 1    ;;= $0300 ;; player 1 gamepad buttons, one bit per button
	buttons2 				.dsb 1    ;;= $0301 ;; player 2 gamepad buttons, one bit per button
	player_bullet_active 	.dsb 1    ;;= $0302
	player_bullet_x    		.dsb 1    ;;= $0303
	player_bullet_y      	.dsb 1    ;;= $0304
	
	player_bullet_col_x  	.dsb 2    ;;= $0305 ;; x2
	player_bullet_col_y  	.dsb 2    ;;= $0307 ;; x2
	player_col_x		 	.dsb 2    ;;= $0309 ;; x2
	player_col_y			.dsb 2    ;;= $030B ;; x2
									
	enemy_active			.dsb 10   ;;= $030D ;; x10
	enemy_x					.dsb 10   ;;= $0317 ;; x10
	enemy_y					.dsb 10   ;;= $0321 ;; x10
									
	enemyPatternPos		  	.dsb 10   ;;= $032B ;; x10
	enemyPtrnCurFrameX	    .dsb 10   ;;= $0335 ;; x10
	enemyPtrnCurFrameY	    .dsb 10   ;;= $033F ;; x10
	enemyPattern	 	    .dsb 10   ;;= $0349 ;; x10
	enemyPatternMirror      .dsb 10   ;;= $0353 ;; x10
	enemyPatternMirrorY     .dsb 10   ;;= $035D ;; x10
	enemyMovAmtWholeX	  	.dsb 10   ;;= $0367 ;; x10 amount to move each frame (whole number)
	enemyMovAmtFracX	  	.dsb 10   ;;= $0371 ;; x10 amount to move/60 each frame
	enemyMovIncAmtWholeX  	.dsb 10   ;;= $037B ;; x10 amount you'd increase/decrease enemyMovIntervalX by each frame (whole number)
	enemyMovIncAmtFracX   	.dsb 10   ;;= $0385 ;; x10 amount you'd increase/decrease enemyMovIntervalX by/60 each frame
	enemyMovAmtWholeY	  	.dsb 10   ;;= $038F ;; x10 amount to move each frame (whole number)
	enemyMovAmtFracY	  	.dsb 10   ;;= $0399 ;; x10 amount to move/60 each frame
	enemyMovIncAmtWholeY  	.dsb 10   ;;= $03A3 ;; x10 amount you'd increase/decrease enemyMovIntervalX by each frame (whole number)
	enemyMovIncAmtFracY   	.dsb 10   ;;= $03AD ;; x10 amount you'd increase/decrease enemyMovIntervalX by/60 each frame
	enemyPtrnMovDecelFlag 	.dsb 10   ;;= $03B7 ;; x10 a flag set to check whether the total frames increase or decrease    
	enemyMovMirrorDir	  	.dsb 1    ;;= $03C1 ;;.dsb 1    ;; Mirrored direction to move enemy  
	enemySlotLoop		    .dsb 1    ;;
	
	nextEvent_ID		  .dsb 1	;;
	nextEvent_StatID	  .dsb 1	;;
	nextEvent_Second	  .dsb 1	;;
	nextEvent_Frame		  .dsb 1	;;
	
	nextMovAmt			  .dsb 1    ;;
	lvl_timer_frame		  .dsb 1    ;;
	lvl_timer_second	  .dsb 1    ;;
	nextEnemy_ID		  .dsb 1    ;;
	nextEnemy_StatID	  .dsb 1    ;;
	nextEnemySecond	      .dsb 1    ;;
	nextEnemyFrame		  .dsb 1    ;;
	nextEnemySlot		  .dsb 1    ;;	
	
	enemyBltPattern		  .dsb 10   ;;
	hostBltActive	 	  .dsb 10   ;;
	hostBltIncX			  .dsb 10	;;
	hostBltIncY			  .dsb 10	;;
	hostBltX			  .dsb 10   ;;
	hostBltY			  .dsb 10   ;;
	hostBltPattern		  .dsb 10	;;
	hostBltHomingX		  .dsb 10	;;
	hostBltHomingY		  .dsb 10	;;
	hostBltColX		 	  .dsb 20   ;;
	hostBltColY			  .dsb 20   ;;
	hostBltCurFrameX	  .dsb 10   ;;
	hostBltCurFrameY	  .dsb 10   ;;
	hostBltSlotLoop	  	  .dsb 1    ;;
	nextHostBltSlot	 	  .dsb 1    ;;
	

	enemyPtrnCurSecondX   .dsb 10   ;;
	
	enemyCurFrame		  .dsb 10	;;
	enemyCurAniFrame	  .dsb 10	;;
	
	enemy_tile1			  .dsb 10   ;;
	enemy_tile2			  .dsb 10   ;;
	enemy_tile3			  .dsb 10   ;;
	enemy_tile4			  .dsb 10   ;;
	
	bgCurFrame			  .dsb 1	;;
	bgCurSecond			  .dsb 1	;;
									
	hostBltCurFrame  	  .dsb 10   ;;
	hostBltCurSecond  	  .dsb 10   ;;
	
	scroll				  .dsb 1 	;;
	scrollSpeed			  .dsb 1	;;
	nametable			  .dsb 1 	;;
	gameState			  .dsb 1 	;;
	buttonHeld			  .dsb 1 	;;
	playerLives			  .dsb 1 	;;
	playerImmunCount	  .dsb 1	;;
	playerImmun			  .dsb 1	;;
	flickerRotation		  .dsb 1	;;
	stateWaitCount		  .dsb 1	;;
	
	rbHeld				  .dsb 1	;;
	lbHeld				  .dsb 1	;;
	
	drawBuffer			  .dsb 20	;;
	ntInc				  .dsb 1	;;
	
	scrollPointerLo		  .dsb 1	;;
	scrollPointerHi		  .dsb 1	;;
	
	rowNumber			  .dsb 1	;;
	columnLow			  .dsb 1	;;
	columnHigh			  .dsb 1	;;
	sourceLow			  .dsb 1	;;
	sourceHigh			  .dsb 1	;;
	columnNumber		  .dsb 1	;;
	
	atan2x1               .dsb 10	;;
	atan2y1               .dsb 10	;;
	atan2x2               .dsb 10	;;
	atan2y2               .dsb 10	;;
	hostBltPassed		  .dsb 10	;;
	testAngle			  .dsb 10	;;
	
	include "ggsound_ram.inc"
  .ende  ;>  
HWDefines:
PPU_CTRL_REG1         = $2000
PPU_CTRL_REG2         = $2001
PPU_STATUS            = $2002
PPU_SPR_ADDR          = $2003
PPU_SPR_DATA          = $2004 ;;Sprite OAM
PPU_SCROLL_REG        = $2005
PPU_ADDRESS           = $2006
PPU_DATA              = $2007

SND_REGISTER          = $4000
SND_SQUARE1_REG       = $4000
SND_SQUARE2_REG       = $4004
SND_TRIANGLE_REG      = $4008
SND_NOISE_REG         = $400C
SND_DELTA_REG         = $4010
SND_MASTERCTRL_REG    = $4015

SPR_DMA               = $4014
JOYPAD_PORT           = $4016
JOYPAD_PORT1          = $4016
JOYPAD_PORT2          = $4017 ;> 
GameDefines:
;>
Constants:
  song_index_Hormel   = 1
  sfx_index_sfx_laser = 0
  sfx_index_sfx_zap   = 1
  RIGHTWALL      	  = $F7
  TOPWALL        	  = $04
  BOTTOMWALL     	  = $DD
  LEFTWALL       	  = $02 
  TITLESTATE	 	  = $00
  PLAYSTATE		 	  = $01
  GAMEOVERSTATE  	  = $02
  PAUSESTATE	 	  = $03
  SPINNER1		 	  = $00
  STARTENEMYSPRITE    = $1C	  ;;Shadow
  STARTHOSTBLTSPRITE  = $BC	  ;;Shadow
  PLAYERSPRITE	 	  = $020C ;;Shadow
  ;>  
GeneralMacros:
MACRO LDACMP: var cmpval
  LDA var
  CMP cmpval
ENDM ;>
MACRO CPYBEQ: cmpVal loc
  CPY cmpVal
  BEQ loc
ENDM ;>
MACRO CMPBNE: cmpVal loc
  CMP cmpVal
  BNE loc
ENDM ;>
MACRO LDSTA: val loc
  LDA val
  STA loc
ENDM ;>
MACRO LDSTX: val loc
  LDX val
  STX loc
ENDM ;>
MACRO LDSTY: val loc
  LDY val
  STY loc
ENDM ;>
MACRO LDAIY: val yInd
  LDY yInd
  LDA val, y
ENDM ;>
MACRO STAIY: val yInd
  LDY yInd
  STA val, y
ENDM ;>
MACRO ADCSTA: val loc
  CLC
  ADC val
  STA loc
ENDM ;>
MACRO SetSlotSpriteLoByte: slotSpriteName slotLoopName initialByte
  LDSTA initialByte, slotSpriteName ;;low byte of starting sprite address
  LDY slotLoopName   		  		;;current enemy slot active in loop
  CPYBEQ #$00, +           		;;slot 0
  ADCSTA #$10, slotSpriteName	;;jumps ahead $10 because there are 4 sprites per slot
  CPYBEQ #$01, +           		;;slot 1  		
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$02, +           		;;slot 2
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$03, +           		;;slot 3
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$04, +           		;;slot 4
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$05, +           		;;slot 5
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$06, +           		;;slot 6
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$07, +           		;;slot 7
  ADCSTA #$10, slotSpriteName		
  CPYBEQ #$08, +           		;;slot 8
  ADCSTA #$10, slotSpriteName
  CPYBEQ #$09, +           		;;slot 9
+
ENDM ;>
MACRO SetSlotSpriteLoByte2Spr: slotSpriteName slotLoopName initialByte
  LDSTA initialByte, slotSpriteName ;;low byte of starting sprite address
  LDY slotLoopName   		  		;;current enemy slot active in loop
  CPYBEQ #$00, +           		;;slot 0
  ADCSTA #$08, slotSpriteName	;;jumps ahead $08 because there are 2 sprites per slot
  CPYBEQ #$01, +           		;;slot 1  		
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$02, +           		;;slot 2
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$03, +           		;;slot 3
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$04, +           		;;slot 4
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$05, +           		;;slot 5
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$06, +           		;;slot 6
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$07, +           		;;slot 7
  ADCSTA #$08, slotSpriteName		
  CPYBEQ #$08, +           		;;slot 8
  ADCSTA #$08, slotSpriteName
  CPYBEQ #$09, +           		;;slot 9
+
ENDM ;>
MACRO ChangePalette: val1 val2 val3 val4
  ;;LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
  ;;LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
  ;;LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
  ;;LDA val1                
  ;;STA PPU_DATA
  ;;LDA val2                
  ;;STA PPU_DATA
  ;;LDA val3                
  ;;STA PPU_DATA
  ;;LDA val4                
  ;;STA PPU_DATA
  LDY #$00
  LDA #$04
  STA drawBuffer, y
  INY
  LDA #$3F
  STA drawBuffer, y
  INY
  LDA #$00
  STA drawBuffer, y
  INY
  LDA val1
  STA drawBuffer, y
  INY
  LDA val2
  STA drawBuffer, y
  INY
  LDA val3
  STA drawBuffer, y
  INY
  LDA val4
  STA drawBuffer, y
ENDM ;>
MACRO TimeInc: frmVar secVar
  LDA frmVar
  CLC
  ADC #$01
  STA frmVar
  CMP #$3C
  BNE +
  LDSTA #$00, frmVar
  LDA secVar
  ADCSTA #$01, secVar
  +
ENDM ;>
MACRO ScrollRegAdjust: 
  LDA scroll 			;;scroll starts at 0
  SEC
  SBC scrollSpeed
  CMP #$ef ;;#$f0				;;this is getting skipped over. need some kind of indicator that it's been passed for the first time since iterating through the complete loop
  BCC +					;;if new scroll amount is smaller than #$f0, skip ahead
  SBC #$10 ;;LDA #$e0
  INC ntInc
  TAX
  LDA nametable
  EOR #%00000010
  STA nametable
  TXA
+ sta scroll
  
  LDA #$00		       	;; horizontal scrolling (#$00 for none)
  STA PPU_SCROLL_REG    ;; write the horizontal scroll count register

  LDA scroll        	;; vertical scrolling (#$00 for none)
  STA PPU_SCROLL_REG 
  ;;LDSTA #$01, updating_background 
  ENDM ;>
MACRO AddToPointer: low high
	LDA pointerLo
	CLC
	ADC low
	STA pointerLo
	LDA pointerHi
	ADC high
	STA pointerHi
	ENDM ;>
;>
EnemyMacros:
MACRO addEnemyMovDigitsX: movIncAmtWhole movIncAmtFrac
@AddFracDigits
  LDA enemyMovAmtFracX, y
  CLC
  ADC movIncAmtFrac
  STA enemyMovAmtFracX, y
  CMP #$3B ;; check if above 59. If >= $3B, carry is set
  BCC @AddWholeDigits
  LDA enemyMovAmtFracX, y
  SEC
  SBC #$3B
  STA enemyMovAmtFracX, y
  LDA enemyMovAmtWholeX, y
  CLC
  ADC #$01
  STA enemyMovAmtWholeX, y
@AddWholeDigits
  LDA enemyMovAmtWholeX, y
  CLC
  ADC movIncAmtWhole
  STA enemyMovAmtWholeX, y
ENDM ;>
MACRO addEnemyMovDigitsY: movIncAmtWhole movIncAmtFrac
@AddFracDigits
  LDA enemyMovAmtFracY, y
  CLC
  ADC movIncAmtFrac
  STA enemyMovAmtFracY, y
  CMP #$3B ;; check if above 59. If >= $3B, carry is set
  BCC @AddWholeDigits
  LDA enemyMovAmtFracY, y
  SEC
  SBC #$3B
  STA enemyMovAmtFracY, y
  LDA enemyMovAmtWholeY, y
  CLC
  ADC #$01
  STA enemyMovAmtWholeY, y
@AddWholeDigits
  LDA enemyMovAmtWholeY, y
  CLC
  ADC movIncAmtWhole
  STA enemyMovAmtWholeY, y
ENDM ;>
MACRO frameSkipMoveEnemyX: speed moveEveryNoFrames
  LDA enemyPtrnCurFrameX, y ;;moves every 2 frames
  BNE @End
  LDA enemyPatternMirror, y
  BNE @MirroredPattern
    moveEnemyX speed, #$00 ;;1 pixel left
  JMP @End
@MirroredPattern
  moveEnemyX speed, #$01 ;;1 pixel right
@End
  JSR SetEnemyPtrnCurFrameX
ENDM ;>
MACRO incMoveEnemyX: startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac ;;decelFlag
  LDA enemyPtrnCurFrameX, y ;;check to see if first time moving this enemy
  CMP #$FF
  BNE @Begin
  LDA startMovAmtWhole
  STA enemyMovAmtWholeX, y
  LDA startMovAmtFrac
  STA enemyMovAmtFracX, y
@Begin
  addEnemyMovDigitsX movIncAmtWhole, movIncAmtFrac
  LDA enemyMovAmtWholeX, y
  BEQ @End
  STA nextMovAmt
  LDA enemyPatternMirror, y
  BNE @MirroredPattern
  moveEnemyX nextMovAmt, #$00
  JMP @End
@MirroredPattern
  moveEnemyX nextMovAmt, #$01
@End
  JSR SetEnemyPtrnCurFrameX
ENDM ;>
MACRO incMoveEnemyY: startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac direction ;;up/down ;;decelFlag
  LDA enemyPtrnCurFrameY, y ;;check to see if first time moving this enemy
  CMP #$FF
  BNE @Begin
  LDA startMovAmtWhole
  STA enemyMovAmtWholeY, y
  LDA startMovAmtFrac
  STA enemyMovAmtFracY, y
@Begin
  addEnemyMovDigitsY movIncAmtWhole, movIncAmtFrac
  LDA enemyMovAmtWholeY, y
  BEQ @End
  STA nextMovAmt
  LDA enemyPatternMirrorY, y
  BNE @MirroredPattern
  moveEnemyY nextMovAmt, direction
  JMP @End
@MirroredPattern
  LDA direction
  BNE @RevToZero
@RevToOne
  CLC
  ADC #$01
  STA enemyMovMirrorDir
  JMP @Move
@RevToZero
  SEC
  SBC #$01
  STA enemyMovMirrorDir
@Move
  moveEnemyY nextMovAmt, enemyMovMirrorDir
@End
  JSR SetEnemyPtrnCurFrameY
ENDM ;>
MACRO decMoveEnemyX: startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac ;;decelFlag
  LDA enemyPtrnCurFrameX, y ;;check to see if first time moving this enemy
  CMP #$FF
  BNE @Begin
  LDA startMovAmtWhole
  STA enemyMovAmtWholeX, y
  LDA startMovAmtFrac
  STA enemyMovAmtFracX, y
@Begin
  subEnemyMovDigitsX movIncAmtWhole, movIncAmtFrac
  LDA enemyMovAmtWholeX, y
  BEQ @End
  STA nextMovAmt
  LDA enemyPatternMirror, y
  BNE @MirroredPattern
  moveEnemyX nextMovAmt, #$00
  JMP @End
@MirroredPattern
  moveEnemyX nextMovAmt, #$01
@End
  JSR SetEnemyPtrnCurFrameX
ENDM ;>
MACRO decMoveEnemyY: startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac direction ;;up/down ;;decelFlag
  LDA enemyPtrnCurFrameY, y ;;check to see if first time moving this enemy
  CMP #$FF
  BNE @Begin
  LDA startMovAmtWhole
  STA enemyMovAmtWholeY, y
  LDA startMovAmtFrac
  STA enemyMovAmtFracY, y
@Begin
  subEnemyMovDigitsY movIncAmtWhole, movIncAmtFrac
  LDA enemyMovAmtWholeY, y
  BEQ @End
  STA nextMovAmt
  LDA enemyPatternMirrorY, y
  BNE @MirroredPattern
  moveEnemyY nextMovAmt, #$00
  JMP @End
@MirroredPattern
  moveEnemyY nextMovAmt, #$01
@End
  JSR SetEnemyPtrnCurFrameY
ENDM ;>
MACRO subEnemyMovDigitsX: movIncAmtWhole movIncAmtFrac
@SubFracDigits
  LDA enemyMovAmtFracX, y
  SEC
  SBC movIncAmtFrac
  STA enemyMovAmtFracX, y
  CMP #$3B ;; check if above 59. If >= $3B, carry is set
  BCC @SubWholeDigits
  LDA enemyMovAmtFracX, y
  SEC
  SBC #$3B
  STA enemyMovAmtFracX, y
  LDA enemyMovAmtWholeX, y
  CLC
  ADC #$01
  STA enemyMovAmtWholeX, y
@SubWholeDigits
  LDA enemyMovAmtWholeX, y
  SEC
  SBC movIncAmtWhole
  STA enemyMovAmtWholeX, y
ENDM ;>
MACRO subEnemyMovDigitsY: movIncAmtWhole movIncAmtFrac
@SubFracDigits
  LDA enemyMovAmtFracY, y
  SEC
  SBC movIncAmtFrac
  STA enemyMovAmtFracY, y
  CMP #$3B ;; check if above 59. If >= $3B, carry is set
  BCC @SubWholeDigits
  LDA enemyMovAmtFracY, y
  SEC
  SBC #$3B
  STA enemyMovAmtFracY, y
  LDA enemyMovAmtWholeY, y
  CLC
  ADC #$01
  STA enemyMovAmtWholeY, y
@SubWholeDigits
  LDA enemyMovAmtWholeY, y
  SEC
  SBC movIncAmtWhole
  STA enemyMovAmtWholeY, y
ENDM ;>
MACRO moveEnemyX: speed direction ;;0 left 1 right
  LDX direction
  BNE +
  LDA enemy_x, y ;;update x
  SEC
  SBC speed
  JMP ++
+ LDA enemy_x, y ;;update x
  CLC
  ADC speed
++ STA enemy_x, y
  JSR EnemyCollisionCheck
ENDM ;>
MACRO moveEnemyY: speed direction ;;0 down 1 up
  LDX direction
  BNE +
  LDA enemy_y, y ;;update y
  CLC
  ADC speed
  JMP ++
+ LDA enemy_y, y ;;update y
  SEC
  SBC speed
++ STA enemy_y, y
  JSR EnemyCollisionCheck
ENDM ;>
MACRO enemyPosInc: yVal 
  LDA enemy_y, y 		 ;;check if y reached position increment coordinate
  CMP yVal
  BCC @enemyPosIncDone
  LDA enemyPatternPos, y ;;increment to next position
  CLC
  ADC #$01
  STA enemyPatternPos, y
@enemyPosIncDone
ENDM ;>
MACRO revEnemyPosInc: yVal 
  LDA enemy_y, y 		 ;;check if y reached position increment coordinate
  CMP yVal
  BCS @enemyPosIncDone
  LDA enemyPatternPos, y ;;increment to next position
  CLC
  ADC #$01
  STA enemyPatternPos, y
@enemyPosIncDone
ENDM ;>
MACRO squareRoot: int
  LDSTX int, $sqrt
  LDSTY #$01, $lsby ;; lsby of first odd number = 1
  DEY
@again
  SEC
  LDA $sqrt   		;; save remainder in X register
  TAX 		  		;; subtract odd from int
  SBC $lsby
  STA $sqrt
  BCC @nomore 		;; no. increment square root
  INY 		  		;; square root = square root + 1
  LDA $lsby   		;; calculate next odd number (iterates #$01 - #$FF and back to zero, will only branch back to @again on zero, otherwise it will inc $23 before going @again)
  ADC #$01 	  		;; carry is set, so this subtracts 2
  STA $lsby
  BCC @again
@nomore
  STY $sqrt   		;; all done, store square root
  STX $rmdr   		;; and remainder
ENDM ;>
MACRO enemyAniInc: spriteSet
  LDA enemyCurFrame, y
  CMP #$03
  BCS @Ani2
  LDA #$0A
  STA enemy_tile1, y
  LDA #$0B
  STA enemy_tile2, y
  LDA #$1A
  STA enemy_tile3, y
  LDA #$1B
  STA enemy_tile4, y
  JMP @AniEnd
  @Ani2
  LDA enemyCurFrame, y
  CMP #$03
  BCS @Ani3
  LDA #$0C
  STA enemy_tile1, y
  LDA #$0D
  STA enemy_tile2, y
  LDA #$1C
  STA enemy_tile3, y
  LDA #$1D
  STA enemy_tile4, y
  JMP @AniEnd
  @Ani3
  LDA enemyCurFrame, y
  CMP #$03
  BCS @Ani4
  LDA #$0E
  STA enemy_tile1, y
  LDA #$0F
  STA enemy_tile2, y
  LDA #$1E
  STA enemy_tile3, y
  LDA #$1F
  STA enemy_tile4, y
  JMP @AniEnd
  @Ani4
  LDA #$0C
  STA enemy_tile1, y
  LDA #$0D
  STA enemy_tile2, y
  LDA #$1C
  STA enemy_tile3, y
  LDA #$1D
  STA enemy_tile4, y
  @AniEnd
  LDA enemyCurFrame, y
  INC #$01
  STA enemyCurFrame, y
  CMP #$10
  BNE @AniEnd2
  LDA #$00
  STA enemyCurFrame, y
  @AniEnd2
  ;;check if $04 and reset to zero if so after incrementing the frame of animation
  ;;also check frame of animation for termination byte and reset to $00 when reached
  
  ;; just set the tiles for now
  
  ;;use a pointer that I inc every 4 frames until reaching a termination byte ($FF)
ENDM ;>
;>

Boot:
  .org $8000
  include "ggsound.asm"
  include "songs10.asm"
  ;;include "HormelExportTest3.asm"
  ;;include "HormelExportTest3_dpcm.asm"
  ;;include "songs8.asm"
  ;;include "songs7_dpcm.asm"
  .org $C000
  RESET: 
    SEI          	  		 ;; disable IRQs
    CLD          	  		 ;; disable decimal mode
    LDSTX #$40, JOYPAD_PORT2 ;; disable APU frame IRQ   
    LDX #$FF
    TXS         	  		 ;; Set up stack
    INX         	  		 ;; now X = 0
    STX PPU_CTRL_REG1   	 ;; disable NMI
    STX PPU_CTRL_REG2   	 ;; disable rendering
    STX SND_DELTA_REG   	 ;; disable DMC IRQs
  ;>
  vblankwait1:       ;; First wait for vblank to make sure PPU is ready
    BIT PPU_STATUS
    BPL vblankwait1 ;>
  clrmem:
    LDA #$00
    STA $0000, x
    STA $0100, x
    STA $0300, x
    STA $0400, x
    STA $0500, x
    STA $0600, x
    STA $0700, x
    LDA #$FE
    STA $0200, x
    INX
    BNE clrmem ;>
  vblankwait2:       ;; Second wait for vblank to make sure PPU is ready
    BIT PPU_STATUS
    BPL vblankwait2
	LDSTA #$00, gameState
	STA playerImmun
	STA bgCurFrame
	STA bgCurSecond
	STA rbHeld
	STA lbHeld
	STA scrollSpeed
	STA scroll
	LDA #%00000010
	STA nametable
	
	LDSTA #$1D, columnNumber ;;29
	LDSTA #$03, playerLives
	LDSTA #$1E, stateWaitCount
	
	LDSTA #SOUND_REGION_NTSC, sound_param_byte_0
	;;LDSTA #<song_list, sound_param_word_0
	;;LDSTA #>song_list, sound_param_word_0+1
	;;LDSTA #<sfx_list, sound_param_word_1
	;;LDSTA #<sfx_list, sound_param_word_1+1
	;;JSR sound_initialize

    LDA #<(song_list)
    STA sound_param_word_0
    LDA #>(song_list)
    STA sound_param_word_0+1
    LDA #<(sfx_list)
    STA sound_param_word_1
    LDA #>(sfx_list)
    STA sound_param_word_1+1
    LDA #<(envelopes_list)
    STA sound_param_word_2
    LDA #>(envelopes_list)
    STA sound_param_word_2+1
    ;;lda #<dpcm_list
    ;;sta sound_param_word_3
    ;;lda #>dpcm_list
    ;;sta sound_param_word_3+1
    JSR sound_initialize
  ;>
  LoadTitleScreen:
    LoadTitlePalettes:
      LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
      LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
      LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address           
      LDX #$00                ;; start out at 0
    ;>
    LoadTitlePalettesLoop:
      LDA palette, x            ;; load data from address (palette + the value in x)
                                ;; 1st time through loop it will load palette+0
                                ;; 2nd time through loop it will load palette+1
                                ;; 3rd time through loop it will load palette+2
                                ;; etc
      STA PPU_DATA          	;; write to PPU
      INX                   	;; X = X + 1
      CPX #$20              	;; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
      BNE LoadTitlePalettesLoop ;; Branch to LoadPalettesLoop if compare was Not Equal to zero
                                ;; if compare was equal to 32, keep going down
    ;>
	LDY #$00
    LoadSprites:
      LDX #$00                 ;; start at 0
    @LoadSpritesLoop
      LDA sprites, x           ;; load data from address (sprites + x)
      STA $0200, x             ;; store into RAM address ($0200 + x)
      INX                      ;; X = X + 1
      CPX #$FF                 ;; Compare X to hex $10, decimal 16
      BNE @LoadSpritesLoop 	   ;; Branch to LoadSpritesLoop if compare was Not Equal to zero
	  CPY #$00
	  BNE LoadTitleBackground
	  INY
	  JMP @LoadSpritesLoop
    ;>     					   ;; if compare was equal to 16, keep going down
    LoadTitleBackground:
      LDA PPU_STATUS             	;; read PPU status to reset the high/low latch
      LDSTA #$20, PPU_ADDRESS		;; write the high byte of $2000 address     
      LDSTA #$00, PPU_ADDRESS		;; write the low byte of $2000 address    
      LDSTA #<titlebgcomp, pointerLo	;; put the low byte of the address of background into pointer
      LDSTA #>titlebgcomp, pointerHi	;; put the high byte of the address into pointer
      LDX #$00          			;; start at pointer + 0
      LDY #$00 
	;>
    OutsideTitleLoop: ;>
    InsideTitleLoop:				;; this runs 256 * 4 times
      JSR StaticBGLoop
	  ;;im incrementing that pointer high at a certain point... pretty sure I still need to do that
	  ;;high byte needs to increment by 1 4x total
	  
	  
      ;;LDA pointerHi
	  ;;CMP #>titlebgcomp+4
	  ;;BEQ InsideTitleLoop
	  
	  
	  ;;INX
      ;;CPX #$04	;;can compare pointerHi to #>titlebgcomp+4 instead to avoid the use of a register
      ;;BNE OutsideTitleLoop2  	  	  ;; run the outside loop 256 times before continuing down    
	  
	  
	  
	  ;;need to check if value is in range. if not, I gotta roll back and do the sequence again until X = 0, from which point I'll JMP to InsideTitleLoop
	  
      ;;STA PPU_DATA            		  ;; this runs 256 * 4 times
      ;;INY                	  		  ;; inside loop counter
      ;;CPY #$00	              		              		  
      ;;BNE InsideTitleLoop     	  	  ;; run the inside loop 256 times before continuing down
      ;;INC pointerHi      	  		  ;; low byte went 0 to 256, so high byte needs to be changed now
      ;;INX
      ;;CPX #$04
      ;;BNE OutsideTitleLoop     	  	  ;; run the outside loop 256 times before continuing down        
      ;;;;LDSTA #%00010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 2
      ;;;;LDSTA #%00001000, PPU_CTRL_REG2 ;; enable background
    ;>
	LoadTitleBackground2:
      LDA PPU_STATUS             	;; read PPU status to reset the high/low latch
      LDSTA #$28, PPU_ADDRESS		;; write the high byte of $2000 address     
      LDSTA #$00, PPU_ADDRESS		;; write the low byte of $2000 address    
      LDSTA #<stationbgcomp, pointerLo	;; put the low byte of the address of background into pointer
      LDSTA #>stationbgcomp, pointerHi	;; put the high byte of the address into pointer
      LDX #$00          			;; start at pointer + 0
      LDY #$00 ;>
    OutsideTitleLoop2: ;>
    InsideTitleLoop2:
    JSR StaticBGLoop				;;bg load (/w decompression?)
	;;  LDA (pointerLo), y 	  		  ;; copy one background byte from address in pointer plus Y
    ;;  STA PPU_DATA            		  ;; this runs 256 * 4 times
    ;;  INY                	  		  ;; inside loop counter
    ;;  CPY #$00	              		              		  
    ;;  BNE InsideTitleLoop2    	  	  ;; run the inside loop 256 times before continuing down
    ;;  INC pointerHi      	  		  ;; low byte went 0 to 256, so high byte needs to be changed now
    ;;  INX
    ;;  CPX #$04
    ;;  BNE OutsideTitleLoop2  	  	  ;; run the outside loop 256 times before continuing down        
    ;;  LDSTA #%00010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 2
    ;;  LDSTA #%00001000, PPU_CTRL_REG2 ;; enable background
	;>
	ReadCtrlTitle:
	  LDA stateWaitCount
	  BEQ @readCtrl
	  DEC stateWaitCount			;;decrements stateWaitCount by one if needed
	@readCtrl
	  LDA stateWaitCount
	  BNE TitleForever				;;endless loop until stateWaitCount down to zero
	  JSR ReadController1
	  LDA buttons1    				;;player 1 - Start
      AND #%00010000
      BNE @loadMainGame
	  ;;LDSTA #$00, buttonHeld
	  JMP TitleForever
	@loadMainGame
	  ;;LDA buttonHeld
	  ;;BNE TitleForever
	  LDSTA #$01, gameState
	  LDSTA #$1E, stateWaitCount
	  JMP LoadGameVisuals
	;>
	TitleForever:
	  LDA stateWaitCount
	  CMP #$1D
	  BNE TitleForever2
	  LDSTA #%10010001, PPU_CTRL_REG1 ;; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
      LDSTA #%00001000, PPU_CTRL_REG2 ;; enable sprites, enable background, no clipping on left side
	  LDSTA #song_index_soler42, sound_param_byte_0
	  JSR play_song
	TitleForever2:
	@soundupdate
	  soundengine_update
	@loop
	  JMP @loop
	;>
  ;>
  LoadGameVisuals: 	 ;; sets up and stores visuals into PPU RAM
	LDSTA #%00010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 2
	LDSTA #%00000000, PPU_CTRL_REG2 ;; disable background
	LDA PPU_STATUS             	;; read PPU status to reset the high/low latch
	LDSTA #$20, PPU_ADDRESS		;; write the high byte of $2000 address     
	LDSTA #$00, PPU_ADDRESS		;; write the low byte of $2000 address    
	LDSTA #<background, pointerLo	;; put the low byte of the address of background into pointer
	LDSTA #>background, pointerHi	;; put the high byte of the address into pointer
	LDX #$00          			;; start at pointer + 0
	LDY #$00
  @outsideTitleLoop
  @insideTitleLoop
	LDA (pointerLo), y 	  		  ;; copy one background byte from address in pointer plus Y
	STA PPU_DATA            		  ;; this runs 256 * 4 times
	INY                	  		  ;; inside loop counter
	CPY #$00	              		              		  
	BNE @insideTitleLoop     	  	  ;; run the inside loop 256 times before continuing down
	INC pointerHi      	  		  ;; low byte went 0 to 256, so high byte needs to be changed now
	INX
	CPX #$04
	BNE @outsideTitleLoop     	  	  ;; run the outside loop 256 times before continuing down
	LDA PPU_STATUS
    LDSTA #%10000000, PPU_CTRL_REG1 ;; enable NMI, sprites from Pattern Table 0, background from Pattern Table 0
    LDSTA #%00011110, PPU_CTRL_REG2 ;; enable sprites, enable background, no clipping on left side
  ;>
  StartMusic:
  LDSTA #song_index_Hormel, sound_param_byte_0
  JSR play_song ;>
  Forever:
  JMP Forever ;> ;;jump back to Forever, infinite loop
;>

NMI:
  LDA gameState			;;
  BNE @1
  JMP ReadCtrlTitle		;;jumps to paused controller-checking titlescreen?
  @1
  CMP #$01
  BEQ @gameplay
  CMP #PAUSESTATE
  BEQ @pause
  JMP ReadCtrlGameOver
  @pause
  ;;JSR pause_song ;; need to fix NMI disabling first
  LDA stateWaitCount
  BEQ @pause2
  DEC stateWaitCount
  @pause2
  LDA stateWaitCount
  BNE @pauseloop
  JSR ReadController1
  LDA buttons1    ;; player 1 - A
  AND #%00010000  ;; only look at bit 0
  BEQ @pauseloop
  LDSTA #$01, gameState
  LDSTA #$1E, stateWaitCount
  JSR resume_song
  ;;JMP LoadGameVisuals
  @pauseloop
  soundengine_update
  @pauseloop2
  JMP @pauseloop2
  @gameplay ;;To switch between different nametables
  ;;LDSTA #$1E, stateWaitCount
;>

WritePPUBuffer:
  ;;This needs to be before background scrolling for it to work
  LDY #$00
@loop1
  LDA drawBuffer, y
  BEQ @end
  TAX 						;;copy string length
  INY
  LDA PPU_STATUS
  LDA drawBuffer, y
  STA PPU_ADDRESS
  INY
  LDA drawBuffer, y
  STA PPU_ADDRESS
@loop2
  INY
  LDA drawBuffer, y
  STA PPU_DATA
  DEX
  TXA
  BNE @loop2
  INY
  JMP @loop1
@end
;>

NewColumnCheck:
  LDA scroll												;;this section needs some kind of way to check if it's the first time the lower bits for the column have been passed over
															;;add scrollspeed to scroll and if they are larger than the last #%000, then draw new column
  AND #%00000111          	;; throw away higher bits
  BEQ @incCol				;; draw new col every multiple of 8
  
  ADC scrollSpeed			;; otherwise, check and see if you rolled over the new col bit during last scroll
  CMP #$08
  BCC @NewColumnCheckDone
  ;;set a carry
  ;;subtract scrollspeed
  ;;if carry was set (or removed), draw new column
  ;;BNE @NewColumnCheckDone  ;; done if lower bits != 0
  
@incCol
  JSR DrawNewColumn 
  LDA columnNumber
  SEC
  SBC #$01             	   ;; go to next column
  BNE @store
  LDA #$1D
  ;;AND #%01111111         ;; only 128 columns of data, throw away top bit to wrap
@store
  STA columnNumber
@NewColumnCheckDone
;>

BGScroll:
  ;;wherever scroll is occuring... check to see if one cycle completed
  ;;if so, inc levelNT count by 1
  ;;check levelNT
  ;;if 1, start swapping the bg out (by setting a flag or something to be buffered)
    
;;Myth: You always need to use the PPUADDR ($2006) register to scroll. (Seen in Nestech.txt section 10: Programming the NES)
;;Fact: The proper way to set the scroll position is to write the upper bits of the X and Y coordinates to PPUCTRL ($2000), and then bits 0-7 of the X and Y coordinates to PPUSCROLL ($2005). The NES will update the VRAM address register for you near the end of the pre-render scanline (261 on NTSC, 311 on PAL).
  
  ;;Which is one way to reset the scroll (the wrong way, because it doesn't fully reset the scroll). the correct way is to select the name table through $2000 and then writing #$00 to $2005 (PPU_SCROLL_REG) twice, to clear both horizontal and vertical scroll.
  ;;Whenever you write to $2006 or $2007, you overwrite whatever is currently in the scroll registers, because $2005 and $2006 are accessing the same internal registers, just in different ways.
  ;;This is why you must reset scroll (write $2000, $2005, $2005) after you are finished making any updates via $2007.
  ;;It is not necessary to write 0 to both $2006 and $2005. This is redundant, as the writes go to the same place.
  
  ;;LDA PPU_STATUS
  ;;;;LDA #%10010000 
  ;;;;ORA nametable ;;#%10010010
  ;;LDA #%10010000 ;;nametable
  ;;STA PPU_CTRL_REG1
  ;;LDSTA #%00011110, PPU_CTRL_REG2
  ;;;;This HAS to come before ScrollRegAdjust
  ;;LDA #$00				   ;; clean up scroll registers  
  ;;STA PPU_SCROLL_REG
  ;;STA PPU_SCROLL_REG
  
  ScrollRegAdjust
  
  ;;Add a var to increment each scroll wrap and check to see if new nametable needs to be loaded. JSR to load if needed
  ;;Need to implement a check to see if scrolling increment is greater than 8 per frame, because I will need to disable the drawing of new columns (which means the nametables should already be good)
;;  LDA scroll  			   ;; checks if scroll wrapped 255 to ensure nametable swap occurs at proper point (this will only work in forward-direction so add additional ADC section if going in reverse)
;;  SEC
;;  SBC scrollSpeed
;;  CMP scroll
;;  BCC SwapNTDone		   ;; if new scroll val is smaller than previous, skip table swap
;;  ;;need to adjust scroll so that it is going through the entirety of both 0th and 2nd nametable locations
;;  ;;LDSTA #$EF, scroll 	   ;; was EF but was getting glitches with different speeds
;;  ;;LDA nametable
;;  ;;CMP #%10010000
;;  ;;BNE @flip
;;  ;;LDA #%10010000
;;  ;;STA nametable
;;  ;;JMP SwapNTDone
;;  ;;@flip
;;  ;;LDA #%10010000
;;  ;;STA nametable
;;  
;;  LDA nametable
;;  ;;EOR #$01
;;  EOR #%00000010
;;  STA nametable
;;  SwapNTDone: ;>
;>

SpriteDMA:
  ;;This needs to be after background scrolling for it to work
  LDSTA #$00, PPU_SPR_ADDR ;; set the low byte of address
  LDSTA #$02, SPR_DMA      ;; set high byte of address, start sprite DMA from $2000
;>

PPUCleanup:
  ;;this ensures rendering the next frame starts properly.
  LDA PPU_STATUS
  LDA #%10010000 				    ;; 7 NMI on vblank start, 5 8x8/8x16 sp, 4 bg pat tbl, 3 sp pat tbl, 2 VRAM inc1/dec32, 1/0 base nmtbl adr 
  ORA nametable						
  ;;LDA nametable
  STA PPU_CTRL_REG1
  LDSTA #%00011110, PPU_CTRL_REG2 ;; enable sprites, enable background, no clipping on left side (doesn't appear to be of use)
  ;;JSR CPUUsageBar
  ;;PLA ;restore the registers
  ;;TAY
  ;;PLA
  ;;TAX
  ;;PLA
;;  LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;;LDACMP #$00, #$00
;>

PlayerAnimation:
  ;;JSR PlayerLAniInc
  ;;JSR PlayerRAniInc
;>
BGLvl1IntroAni:
  ;;get background to brighten up, go black, fade the stars back in, hold awhile until enemies start coming then go to normal bg animation routine. flag set back to zero upon routine completion.
;>
BGAnimation:
  ;;LDA bgCurFrame
  ;;CMP #$08
  ;;BCS @Ani2
  ;;ChangePalette #$0F, #$2D, #$0F, #$0F
  ;;JMP @AniEnd
  ;;@Ani2
  ;;LDA bgCurFrame
  ;;CMP #$18
  ;;BCS @Ani3
  ;;ChangePalette #$0F, #$10, #$2D, #$0F
  ;;JMP @AniEnd
  ;;@Ani3
  ;;LDA bgCurFrame
  ;;CMP #$30
  ;;BCS @Ani4
  ;;ChangePalette #$0F, #$30, #$10, #$2D
  ;;JMP @AniEnd
  ;;@Ani4
  ;;ChangePalette #$0F, #$10, #$2D, #$0F
  ;;alter ChangePalette so that it always changes the bg bit on the first palette
;;  LDSTA #$01, updating_background
  ;;LDA bgCurFrame
  ;;CMP #$08
  ;;BCS @Ani2
  
  LDACMP bgCurSecond, #$00
  BNE @Ani2
  LDACMP bgCurFrame, #$00
  BNE @Ani2
  ChangePalette #$0F, #$2D, #$0F, #$0F
  JMP @AniEnd
@Ani2
  LDACMP bgCurSecond, #$00
  BNE @Ani3
  LDACMP bgCurFrame, #$01
  BNE @Ani3
  ChangePalette #$0F, #$10, #$2D, #$0F
  JMP @AniEnd
@Ani3
  LDACMP bgCurSecond, #$00
  BNE @Ani4
  LDACMP bgCurFrame, #$02
  BNE @Ani4
  ChangePalette #$0F, #$30, #$10, #$2D
  JMP @AniEnd  
@Ani4  
  LDACMP bgCurSecond, #$06
  BNE @Ani5
  LDACMP bgCurFrame, #$1E
  BNE @Ani5
  ChangePalette #$2D, #$30, #$30, #$10
  ;;Change bg bit
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$2D
;;  STA PPU_DATA
  JMP @AniEnd
@Ani5
  LDACMP bgCurSecond, #$06
  BNE @Ani6
  LDACMP bgCurFrame, #$2D
  BNE @Ani6
  ChangePalette #$10, #$30, #$30, #$30
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$10
;;  STA PPU_DATA
  JMP @AniEnd
@Ani6									;;Screen fully white
  LDACMP bgCurSecond, #$07
  BNE @Ani7
  LDACMP bgCurFrame, #$00
  BNE @Ani7
  ChangePalette #$30, #$30, #$30, #$30
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$30
;;  STA PPU_DATA
  JMP @AniEnd
@Ani7
  LDACMP bgCurSecond, #$0A
  BNE @Ani8
  LDACMP bgCurFrame, #$00
  BNE @Ani8
  ChangePalette #$10, #$10, #$10, #$10
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$10
;;  STA PPU_DATA
  JMP @AniEnd
@Ani8
  LDACMP bgCurSecond, #$0A
  BNE @Ani9
  LDACMP bgCurFrame, #$0F
  BNE @Ani9
  ChangePalette #$2D, #$2D, #$2D, #$2D
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$2D
;;  STA PPU_DATA
  JMP @AniEnd
@Ani9
  LDACMP bgCurSecond, #$0A
  BNE @Ani10
  LDACMP bgCurFrame, #$1E
  BNE @Ani10
  ChangePalette #$0F, #$0F, #$0F, #$0F
;;  LDA PPU_STATUS          ;; read PPU status to reset the high/low latch
;;  LDSTA #$3F, PPU_ADDRESS ;; write the high byte of $3F00 address
;;  LDSTA #$00, PPU_ADDRESS ;; write the low byte of $3F00 address
;;  LDA #$0F
;;  STA PPU_DATA
  JMP @AniEnd
@Ani10
  LDACMP bgCurSecond, #$0A
  BNE @Ani11
  LDACMP bgCurFrame, #$2D
  BNE @Ani11
  ChangePalette #$0F, #$0F, #$0F, #$0F
  JMP @AniEnd
@Ani11
  LDACMP bgCurSecond, #$0B
  BNE @Ani12
  LDACMP bgCurFrame, #$00
  BNE @Ani12
  ChangePalette #$0F, #$0F, #$0F, #$0F
  JMP @AniEnd
@Ani12									;;Screen fully black
  LDACMP bgCurSecond, #$0B
  BNE @Ani13
  LDACMP bgCurFrame, #$0F
  BNE @Ani13
  ChangePalette #$0F, #$0F, #$0F, #$0F
  JMP @AniEnd
@Ani13
  LDACMP bgCurSecond, #$0C
  BNE @Ani14
  LDACMP bgCurFrame, #$0F
  BNE @Ani14
  ChangePalette #$0F, #$2D, #$0F, #$0F
  JMP @AniEnd
@Ani14
  LDACMP bgCurSecond, #$0C
  BNE @Ani15
  LDACMP bgCurFrame, #$14
  BNE @Ani15
  ChangePalette #$0F, #$10, #$2D, #$0F
  JMP @AniEnd
@Ani15
  LDACMP bgCurSecond, #$0C
  BNE @AniEnd
  LDACMP bgCurFrame, #$19
  BNE @AniEnd
  ChangePalette #$0F, #$30, #$10, #$2D
  JMP @AniEnd  
  
@AniEnd
  TimeInc bgCurFrame, bgCurSecond
;>

Gameplay:
  LevelTimer:
    INC lvl_timer_frame
    LDA lvl_timer_frame
    CMP #$3C
    BNE @end
    INC lvl_timer_second
    LDSTA #$00, lvl_timer_frame
    @end ;>
  GameStateTimer:
    LDA stateWaitCount
    BEQ @end
    DEC stateWaitCount
    @end
  ;>
  EventTrigger:
  LDSTA #$00, nextEvent_StatID 	;;reset StatID
    JSR LoadEventStatLoop 	   	;;load nextEnemy second/inc StatID
    CMPBNE lvl_timer_second, + 	;;checks if ready to spawn
    JSR LoadEventStatLoop 	   	;;load nextEnemy frame/inc StatID
    CMPBNE lvl_timer_frame, +  	;;checks if ready to spawn
    JSR LoadEventStatLoop      
	;;CMP #$00
	BNE @01
  @00 						   	;;increment scroll by 1
	LDA scrollSpeed 			;;get back to single INC scrollSpeed at some point
	CLC
	ADC #$02
	STA scrollSpeed
	JMP @03
  @01 						   	;;decrement scroll by 1
	CMP #$01
	BNE @02
	DEC scrollSpeed
  @02						  	;;engages bg animation routine
  @03						  	;;disgengages bg animation routine
	INC nextEvent_ID
  +
;>
  EnemySpawn:
    LDSTA #$00, nextEnemy_StatID		 	 ;;reset StatID
    JSR LoadEnemyStatLoop 			         ;;load nextEnemy second/inc StatID
    CMPBNE lvl_timer_second, +			     ;;checks if ready to spawn
    JSR LoadEnemyStatLoop 			 	     ;;load nextEnemy frame/inc StatID
    CMPBNE lvl_timer_frame, + 			     ;;checks if ready to spawn
    JSR SelectOpenEnemySlot			 	     ;;selects the next available enemy slot
    JSR LoadEnemyStatLoop 				     ;;load X
    STAIY enemy_x, nextEnemySlot			 ;;store X
    JSR LoadEnemyStatLoop 				     ;;load Y
    STAIY enemy_y, nextEnemySlot             ;;store Y
    JSR LoadEnemyStatLoop 				     ;;load pattern
    STAIY enemyPattern, nextEnemySlot		 ;;store pattern
    JSR LoadEnemyStatLoop 				     ;;load X pattern mirroring
    STAIY enemyPatternMirror, nextEnemySlot  ;;store X pattern mirroring
    JSR LoadEnemyStatLoop 				     ;;load Y pattern mirroring
    STAIY enemyPatternMirrorY, nextEnemySlot ;;store Y pattern mirroring
    JSR LoadEnemyStatLoop 				     ;;load bullet pattern
    STAIY enemyBltPattern, nextEnemySlot	 ;;store bullet pattern
    LDA #$FF								 ;;reset pattern-specific vars and queue next enemy
    STA enemyPtrnCurFrameX, y 
    STA enemyPtrnCurFrameY, y
    LDA #$00
    STA enemyPtrnCurSecondX, y
    STA enemyPatternPos, y
	STA enemyCurFrame, y
	
	;;LDAIY enemyPattern, nextEnemySlot ;;was going to use this to enable events via the enemy pattern table, but decided to use a separate table
	;;CMP #$FF
	;;BNE @next
	;;LDA #$00
	;;STA enemy_active, y
	;;LDSTA <enemySlotSprite, EnemyDataLow
	;;LDSTA #$02, EnemyDataHigh 	


	;;LDA enemy_tile1, x
	;;STAIY (EnemyDataLow), #$66
	;;LDA enemy_tile2, x
	;;STAIY (EnemyDataLow), #$67
	;;LDA enemy_tile3, x
	;;STAIY (EnemyDataLow), #$76
	;;LDA enemy_tile4, x
	;;STAIY (EnemyDataLow), #$77
  ;;@next
    INC nextEnemy_ID
    +
  ;>
  ReadController:
    JSR ReadController1
    ReadA: 
      LDA buttons1    ;; player 1 - A
      AND #%10000000  ;; only look at bit 0
      BEQ ReadB   	  ;; branch to ReadB if button is NOT pressed (0)
                      ;; add instructions here to do something when button IS pressed (1)
      LDA player_bullet_active
      CMP #$00
      BNE ReadB
      ;;LDSTA $0203, player_bullet_x
      LDAIY PLAYERSPRITE, #$03
	  CLC
	  ADC #$04
	  STA player_bullet_x
	  LDSTA PLAYERSPRITE, player_bullet_y ;;$0200, player_bullet_y
      LDSTA #$01, player_bullet_active
	  LDSTA #sfx_index_sfx_laser, sound_param_byte_0
	  LDSTA #soundeffect_one, sound_param_byte_1
	  JSR play_sfx
    ;>
    ReadB: 
      LDA buttons1    ;; player 1 - A
      AND #%01000000  ;; only look at bit 0
      BEQ ReadSel     ;; branch to ReadSel if button is NOT pressed (0)
                      ;; add instructions here to do something when button IS pressed (1)
    ;>
    ReadSel: 
      LDA buttons1    ;; player 1 - A
      AND #%00100000  ;; only look at bit 0
      BEQ ReadSt      ;; branch to ReadSelDone if button is NOT pressed (0)
                      ;; add instructions here to do something when button IS pressed (1)
    ;>
    ReadSt: 
      LDA buttons1    ;; player 1 - A
      AND #%00010000  ;; only look at bit 0
      BEQ ReadUp
	  LDA stateWaitCount
	  BNE ReadUp
      LDSTA #PAUSESTATE, gameState
	  LDSTA #$1E, stateWaitCount
	  ;;LDSTA #%10010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 2
	  LDSTA #%00000000, PPU_CTRL_REG2 ;; disables the background
	  JSR pause_song
	  ;;JSR sound_stop	;;can't figure out how to get it back
	  soundengine_update
	  
	  JMP NMI
    ;>
    ReadUp: 
      LDA buttons1    ;; player 1 - A
      AND #%00001000  ;; only look at bit 0
      BEQ ReadDwn
	  
	  LDA playerImmunCount
	  BEQ @next
	  CMP #$96
	  BCC @next
	  JMP ReadDwn
	  
	  @next
      ;;LDA $0200       ;; load sprite Y position
      ;;CMP #TOPWALL    ;; branch to next if y < than top wall
      ;;BCC ReadDwn
      ;;SEC             ;; make sure carry flag is set
      ;;SBC #$02        ;; A = A - 1
      ;;STA $0200       ;; save sprite Y position
	  
	  LDA PLAYERSPRITE
	  CMP #TOPWALL
	  BCC ReadDwn
	  SEC
	  SBC #$02
	  STA PLAYERSPRITE
	  LDAIY PLAYERSPRITE, #$04
	  SEC
	  SBC #$02
	  STAIY PLAYERSPRITE, #$04
	  LDAIY PLAYERSPRITE, #$08
	  SEC
	  SBC #$02
	  STAIY PLAYERSPRITE, #$08
	  LDAIY PLAYERSPRITE, #$0C
	  SEC
	  SBC #$02
	  STAIY PLAYERSPRITE, #$0C
    ;>
    ReadDwn: 
      LDA buttons1    ;; player 1 - A
      AND #%00000100  ;; only look at bit 0
      BEQ ReadLft
      ;;LDA $0200       ;; load sprite Y position
      ;;CMP #BOTTOMWALL ;; branch to next if y > than bottom wall
      ;;BCS ReadLft
      ;;CLC             ;; make sure the carry flag is clear
      ;;ADC #$02        ;; A = A + 1
      ;;STA $0200       ;; save sprite Y position
	  
	  LDA PLAYERSPRITE
	  CMP #BOTTOMWALL
	  BCS ReadLft
	  CLC
	  ADC #$02
	  STA PLAYERSPRITE
	  LDAIY PLAYERSPRITE, #$04
	  CLC
	  ADC #$02
	  STAIY PLAYERSPRITE, #$04
	  LDAIY PLAYERSPRITE, #$08
	  CLC
	  ADC #$02
	  STAIY PLAYERSPRITE, #$08
	  LDAIY PLAYERSPRITE, #$0C
	  CLC
	  ADC #$02
	  STAIY PLAYERSPRITE, #$0C
    ;>
    ReadLft: 
      LDA buttons1    ;; player 1 - A
      AND #%00000010  ;; only look at bit 0
      BNE @move1
	  LDSTA #$00, lbHeld
	  JMP @noInc1
      ;;LDA $0203       ;; load sprite X position
      ;;CMP #LEFTWALL   ;; branch to next if x < than left wall
      ;;BCC ReadRt
      ;;SEC             ;; make sure carry flag is set
      ;;SBC #$02        ;; A = A - 1
      ;;STA $0203       ;; save sprite X position
	  @move1
	  LDAIY PLAYERSPRITE, #$03
	  CMP #LEFTWALL
	  BCC @noMove
	  JSR MovePlayerLeft
	  ;;SEC
	  ;;SBC #$02
	  ;;STAIY PLAYERSPRITE, #$03
	  ;;LDAIY PLAYERSPRITE, #$07
	  ;;SEC
	  ;;SBC #$02
	  ;;STAIY PLAYERSPRITE, #$07
	  ;;LDAIY PLAYERSPRITE, #$0B
	  ;;SEC
	  ;;SBC #$02
	  ;;STAIY PLAYERSPRITE, #$0B
	  ;;LDAIY PLAYERSPRITE, #$0F
	  ;;SEC
	  ;;SBC #$02
	  ;;STAIY PLAYERSPRITE, #$0F
	  @noMove
	  LDA lbHeld
	  CMP #$0C
	  BEQ @noInc1
	  LDA lbHeld
	  CLC
	  ADC #$01
	  STA lbHeld
	  @noInc1
	  JSR PlayerLAniInc
	  @end
    ;>
    ReadRt: 
      LDA buttons1     ;; player 1 - A
      AND #%00000001   ;; only look at bit 0
      BNE @move   	   ;; branch to @move if button IS pressed (0)
	  LDSTA #$00, rbHeld
	  JMP @noInc
      ;;LDA $0203        ;; load sprite X position
      ;;CMP #RIGHTWALL   ;; branch to next if x > than right wall
      ;;BCS ReadCtrlDone 
      ;;CLC              ;; make sure the carry flag is clear
      ;;ADC #$02         ;; A = A + 1
      ;;STA $0203        ;; save sprite X position
	  @move
	  LDAIY PLAYERSPRITE, #$07
	  CMP #RIGHTWALL
	  BCS @noMove
	  JSR MovePlayerRight
	  ;;LDAIY PLAYERSPRITE, #$03
	  ;;CLC
	  ;;ADC #$02
	  ;;STAIY PLAYERSPRITE, #$03
	  ;;LDAIY PLAYERSPRITE, #$07
	  ;;CLC
	  ;;ADC #$02
	  ;;STAIY PLAYERSPRITE, #$07
	  ;;LDAIY PLAYERSPRITE, #$0B
	  ;;CLC
	  ;;ADC #$02
	  ;;STAIY PLAYERSPRITE, #$0B
	  ;;LDAIY PLAYERSPRITE, #$0F
	  ;;CLC
	  ;;ADC #$02
	  ;;STAIY PLAYERSPRITE, #$0F
	  @noMove
	  LDA rbHeld
	  CMP #$0C
	  BEQ @noInc
	  LDA rbHeld
	  CLC
	  ADC #$01
	  STA rbHeld
	  @noInc
	  JSR PlayerRAniInc
    ;>
    ReadCtrlDone: ;>
  ;>
  PlayerImmunTimer:
    LDA playerImmunCount
	BNE @next
	LDSTA #$00, playerImmun
	JMP @end
	@next
	DEC playerImmunCount
	LDA flickerRotation
	BNE @loop2
	@loop1
	;;LDSTA #$01, $0201
	LDA #$01

	;;just do the stores to PLAYERSPRITE back in the animation routine and only black the sprite out in here if need be
	
	STAIY PLAYERSPRITE, #$01
	STAIY PLAYERSPRITE, #$05
	STAIY PLAYERSPRITE, #$09
	STAIY PLAYERSPRITE, #$0D
	LDSTA #$01, flickerRotation
	
	JMP @end
	@loop2
	LDSTA #$00, flickerRotation
	@end
  ;>
  PlayerRespawnMovement:
	LDA playerImmunCount
	BEQ @end
	CMP #$96
	BCC @end
	;;LDA $0200    
    ;;SEC         
    ;;SBC #$02     
    ;;STA $0200    
	LDA PLAYERSPRITE ;; load sprite Y position
	SEC				 ;; make sure carry flag is set
	SBC #$02		 ;; A = A - 1
	STA PLAYERSPRITE ;; save sprite Y position
	LDAIY PLAYERSPRITE, #$04
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$04
	LDAIY PLAYERSPRITE, #$08
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$08
	LDAIY PLAYERSPRITE, #$0C
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$0C
	@end
  ;>
  UpdateEnemy:
    LDSTA #$00, enemySlotLoop
    UpdateEnemyLoop:
      EnemyPatterns:
        CheckEnemyPattern:
          LDAIY enemy_active, enemySlotLoop
          BNE +
          JMP UpdateEnemySprites
          
        + LDAIY enemyPattern, enemySlotLoop
          BNE @01
          JMP EnemyPattern0
        @01
          CMP #$01
          BNE @02
          JMP EnemyPattern1
        @02
          CMP #$02
          BNE @03
          JMP EnemyPattern2 
        @03
          CMP #$03
          BNE @04
          JMP EnemyPattern3
        @04
          CMP #$04
          BNE @05
          JMP EnemyPattern4
        @05
          JMP EnemyPattern5 ;>
        EnemyPattern0: ;straight line
		  JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
          incMoveEnemyY #$00, #$0F, #$00, #$03, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac down/up
          JMP EnemyBltPatterns ;>
        EnemyPattern1: ;down in straight line and back up at left/right angle
		  JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
          LDA enemyPatternPos, y ;;set and move to proper pattern position
          BNE @Position2
          moveEnemyY #$02, #$00 ;;2 pixels down 
          enemyPosInc #$CC ;;check if y reached next position and update
          JMP EnemyBltPatterns
        @Position2
          moveEnemyY #$02, #$01 ;;2 pixels up
          frameSkipMoveEnemyX #$01, #$01 ;;moves 1 pixel every 2 frames
          JMP EnemyBltPatterns ;>
        EnemyPattern2: ;down at angle and back up at angle left/left right/right
          JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
		  LDA enemyPatternPos, y 		 ;;set and move to proper pattern position
          BNE @Position2
          moveEnemyY #$03, #$00 		 ;;3 pixels down
          frameSkipMoveEnemyX #$01, #$01 ;;moves 1 pixel every 2 frames
          enemyPosInc #$CC 				 ;;check if y reached next position and update
          JMP EnemyBltPatterns
        @Position2
          moveEnemyY #$01, #$01 		 ;;1 pixel up
          ;;frameSkipMoveEnemyX #$01, #$01 ;;moves 1 pixel every 2 frames
		  ;;incMoveEnemyX #$10, #$00, #$00, #$00 ;;, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac
          moveEnemyX #$01, #$00
		  JMP EnemyBltPatterns ;>
        EnemyPattern3: ;goes straight down before eventually curving off screen left/right
		  JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
          LDA enemyPatternPos, y ;;set and move to proper pattern position
          BEQ @Position1
          JMP @Position2
        @Position1
          incMoveEnemyY #$01, #$00, #$00, #$00, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac
          enemyPosInc #$88 ;;check if y reached next position and update
          JMP EnemyBltPatterns
        @Position2
          incMoveEnemyX #$01, #$00, #$00, #$03 ;;, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac
          decMoveEnemyY #$01, #$00, #$00, #$00 ;;, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac
          JMP EnemyBltPatterns ;>
        EnemyPattern4: ;goes straight down, curves up and to the left/right, then straight back down again
		  JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
          LDA enemyPatternPos, y ;;set and move to proper pattern position
          BNE @2
        @1
          JMP @Position1
        @2
          CMP #$01
          BNE @3
          JMP @Position2
        @3
          JMP @Position3
        @Position1
          incMoveEnemyY #$02, #$00, #$00, #$00, #$00 ;;startMovAmtWhole startMovAmtFrac movIncAmtWhole movIncAmtFrac down/up
          enemyPosInc #$88 ;;check if y reached next position and update
          JMP EnemyBltPatterns
        @Position2
          incMoveEnemyY #$02, #$00, #$00, #$00, #$01
          incMoveEnemyX #$00, #$00, #$00, #$0F
          revEnemyPosInc #$50
          JMP EnemyBltPatterns
        @Position3
          incMoveEnemyY #$02, #$00, #$00, #$00, #$00 
          JMP EnemyBltPatterns ;>
        EnemyPattern5: ;comes in at the side and goes straight to the left/right
		  JSR EnemyAniInc		 	 	 ;;sets the proper sprite in the animation sequence
          incMoveEnemyX #$01, #$00, #$00, #$00
          JMP EnemyBltPatterns ;>
	  ;>
      EnemyBltPatterns:
		CheckEnemyBltPattern:
		  JSR SetHostBltCurFrame
          LDAIY enemyBltPattern, enemySlotLoop
          BNE @01
          JMP EnemyBltPatternsDone
          @01
          CMP #$01
          BNE @02
          JMP EnemyBltPattern1
          @02
          CMP #$02
          BNE @03
          JMP EnemyBltPattern2 
          @03
          CMP #$03
          BNE @04
          JMP EnemyBltPattern3
          @04
          CMP #$04
          BNE @05
          JMP EnemyBltPattern4
          @05
          JMP EnemyBltPattern5 ;>
        EnemyBltPattern1: ;;straight line ;;need to set this up to pass whatever attributes to the individual bullet after spawn
          ;;THESE PATTERNS SHOULD ONLY SPAWN AND THEN PASS BULLET MOVEMENT ATTRIBUTES TO ADJACENT VARIABLES
          LDA hostBltCurFrame, y ;;time to wait before shooting the bullet
          CMP #$30
          BNE EnemyBltPatternsDone
          LDA hostBltCurSecond, y
          CMP #$01
          BNE EnemyBltPatternsDone
          LDY #$FF
          JSR FindOpenEnemyBltSlot
		  ;;LDSTA #$00, hostBltIncX
		  ;;LDSTA #$04, hostBltIncY
          JSR SpawnHostBullet
          JMP EnemyBltPatternsDone ;>
        EnemyBltPattern2: ;;spawns pointed towards player
		  LDA hostBltCurFrame, y ;;time to wait before shooting the bullet
          CMP #$30
          BNE EnemyBltPatternsDone
          LDA hostBltCurSecond, y
          CMP #$01
          BNE EnemyBltPatternsDone
          LDY #$FF
          JSR FindOpenEnemyBltSlot
		  JSR SpawnHostBullet
		  
		  ;;LDY enemySlotLoop
		  
		  LDA PLAYERSPRITE 	;;y
		  CLC
		  ADC #$08
		  ;;STA hostBltHomingY, y
		  STA atan2y2, y	;;need to figure out the coords of edge of screen at same angle
		  
		  LDA PLAYERSPRITE+3 ;;,  #$03 ;;x
		  CLC
		  ADC #$08
		  ;;STA hostBltHomingX, y
		  STA atan2x2, y	;;need to figure out the coords of edge of screen at same angle
		  
		  LDA #$00
		  STA hostBltPassed, y
		  
		  ;;do an atan2 to get the angle
		  ;;use the angle to determine the coordinates at which the bullet would arrive at the edge of the screen
			;;basically a formula that would figure out whether the bullet would arrive at the x or y positive or negative edge first
			  ;;would have to essentially simulate an entire run-through of UpdateHostBulletPos with an IF x or y bounds crossed case
		  
		  ;;sets up your atan2
		  
		  ;;LDAIY hostBltX, nextHostBltSlot
		  ;;STA atan2x1
          ;;LDAIY hostBltY, nextHostBltSlot
		  ;;STA atan2y1
		  ;;
		  ;;LDSTA #$28, atan2x1
		  ;;STA atan2y1
		  ;;LDSTA #$06, atan2x2
		  ;;STA atan2y2
		  ;;
		  ;;JSR ATAN2 	;;retrieve your angle using player and enemy cartesian coords
		  
		  
		  ;;angle stored in A reg
		  ;;now I need to calculate the velocity (speed and direction)
		  ;;if the result is the outside angle (should be), may need to keep that in mind during calculation
		  ;;units of x each frame
		  ;;units of y each frame
		  
          JMP EnemyBltPatternsDone
		  
		;;spawnHostBullet needs to store the bullet pattern number
		;;UpdateHostBulletPos needs to check for this pattern number and then jump to alternate movement for pattern 2
		
		
        ;; How to send the bullet towards the player...
        ;; take enemy location
        ;; set bullet position to that
        ;; enemy X = 30
        ;; enemy Y = 40
        ;; player X = 50
        ;; player Y = 10
        ;; enemy bullet speed = 1
        ;; check if enemy/player x is higher
        ;; set direction of bullet
        ;; sbc
        ;; do 3 above for y
        ;; 
        ;; X distance = 20
        ;; Y distance = 30
        ;; bxh = 600
        ;>
        EnemyBltPattern3: ;;fires every second or so
        ;>
        EnemyBltPattern4: ;;fires triple separate shots every few seconds
        ;>
        EnemyBltPattern5: ;;fires triple shots towards player every few seconds
        ;>
        EnemyBltPattern6: ;;constantly runs a check and only shoots once when close enough to player
        ;>
        EnemyBltPattern7: ;;fires an 8-point bullet hell-style torrential wave of neverending shots (subr the crap out of this) (remember you can only show 64 sprites at a time)
        ;>
        EnemyBltPatternsDone: ;>  
      ;>
      UpdateEnemySprites:
      SetSlotSpriteLoByte enemySlotSprite, enemySlotLoop, #STARTENEMYSPRITE ;;#$1C ;;#$0C
      + LDSTA <enemySlotSprite, EnemyDataLow ;;set low byte of EnemyDataLow with starting enemySlotSprite
        LDSTA #$02, EnemyDataHigh 			 ;;set low byte of EnemyDataHigh with starting enemySlotSprite
        LDX enemySlotLoop
        LDA enemy_y, x ;;sprite 1
		CLC
        ADC #$00
        STAIY (EnemyDataLow), #$00 ;;$0214 first vert
        LDA enemy_x, x
        ADC #$00
        STAIY (EnemyDataLow), #$03 ;;$0217 first horiz
        LDA enemy_y, x ;;sprite 2
        ADC #$00
        STAIY (EnemyDataLow), #$04 ;;$0218
        LDA enemy_x, x
        ADC #$08
        STAIY (EnemyDataLow), #$07 ;;$021B
        LDA enemy_y, x ;;sprite 3
        ADC #$08
        STAIY (EnemyDataLow), #$08 ;;$021C
        LDA enemy_x, x
        ADC #$00
        STAIY (EnemyDataLow), #$0B ;;$021F
        LDA enemy_y, x ;;sprite 4
        ADC #$08
        STAIY (EnemyDataLow), #$0C ;;$0220
        LDA enemy_x, x
        ADC #$08
        STAIY (EnemyDataLow), #$0F ;;$0223
	  ;>	  
	  UpdateEnemyTiles:
	    LDA enemy_tile1, x
		BEQ @end
		STAIY (EnemyDataLow), #$01
		LDA enemy_tile2, x
		STAIY (EnemyDataLow), #$05
		LDA enemy_tile3, x
		STAIY (EnemyDataLow), #$09
		LDA enemy_tile4, x
		STAIY (EnemyDataLow), #$0D
		@end
	  ;>
	  
      ;;make sure second/frame are reset on deactivation of enemy slot
      ;;the enemy will contain data on which bullet pattern. the pattern contains frequency, speed, and direction. speed and direction will be passed to the bullet upon spawning
      ;;a spawned bullet needs to have a speed, direction, coordinates, and a set active flag. it doesn't hold anything pertaining to patterns
      
      PlayerBulletCoordXfer:
        LDA player_bullet_y ;,y              ;;transfer player bullet collision coords
        CLC
        ADC #$00
        STA player_bullet_col_y ;,y
        LDA player_bullet_y;,y
        CLC
        ADC #$08
        STA player_bullet_col_y+1 ;,y
        LDA player_bullet_x
        CLC
        ADC #$01
        STA player_bullet_col_x ;,y
        LDA player_bullet_x
        CLC
        ADC #$06
        STA player_bullet_col_x+1 ;,y ;>
      BulletEnemyCollision:              ;;check collision with bullets and player
        LDAIY enemy_active, enemySlotLoop
        BNE @next
        JMP @nexts
      @next
        LDA enemy_y, y
        CLC
        ADC #$01
        CMP player_bullet_col_y+1 ;,y
        BCC @next1
        JMP @nexts
      @next1
        LDA enemy_y, y
        CLC
        ADC #$0E
        CMP player_bullet_col_y ;,y
        BCS @next2
        JMP @nexts
      @next2
        LDA enemy_x, y
        CLC
        ADC #$03
        CMP player_bullet_col_x+1 ;,y
        BCC @next3
        JMP @nexts
      @next3
        LDA enemy_x, y
        CLC
        ADC #$0C
        CMP player_bullet_col_x ;,y
        BCS @next4
        JMP @nexts
      @next4
        LDSTA #$FE, player_bullet_y
        STA player_bullet_x
        LDSTA #$00, player_bullet_active
        STA enemy_active, y
		LDSTA #sfx_index_sfx_zap, sound_param_byte_0
		LDSTA #soundeffect_one, sound_param_byte_1
		JSR play_sfx
      @nexts ;>
      PlayerCoordXfer:
        LDA PLAYERSPRITE ;;$0200             ;transfer player collision coords
        CLC
        ADC #$00
        STA player_col_y ;,y
        LDA PLAYERSPRITE
        CLC
        ADC #$10 ;;#$08
        STA player_col_y+1 ;,y
        LDAIY PLAYERSPRITE, #$03 ;;$0203
        CLC
        ADC #$01
        STA player_col_x ;,y
        LDAIY PLAYERSPRITE, #$03 ;;$0203
        CLC
        ADC #$10 ;;#$06
        STA player_col_x+1 ;,y ;>
      PlayerEnemyCollision:              ;;check collision with player
        LDAIY enemy_active, enemySlotLoop
        BNE @chkimmun
		JMP @nexts
	  @chkimmun
		LDA playerImmun
		BEQ @next
        JMP @nexts
      @next
        LDA enemy_y, y
        CLC
        ADC #$01
        CMP player_col_y+1 ;;,y
        BCC @next1
        JMP @nexts
      @next1
        LDA enemy_y, y
        CLC
        ADC #$0E
        CMP player_col_y ;;,y
        BCS @next2
        JMP @nexts
      @next2
        LDA enemy_x, y
        CLC
        ADC #$03
        CMP player_col_x+1 ;;,y
        BCC @next3
        JMP @nexts
      @next3
        LDA enemy_x, y
        CLC
        ADC #$0C
        CMP player_col_x ;;,y
        BCS @next4
        JMP @nexts
      @next4
		LDA PPU_STATUS
		LDSTA #$FE, PPU_CTRL_REG1
		LDSTA #$F7, PLAYERSPRITE ;;$0200
		LDA #$7F
		STAIY PLAYERSPRITE, #$03
		LDA #$F7
		STAIY PLAYERSPRITE, #$04
		LDA #$87
		STAIY PLAYERSPRITE, #$07
		LDA #$FF
		STAIY PLAYERSPRITE, #$08
		LDA #$7F
		STAIY PLAYERSPRITE, #$0B
		LDA #$FF
		STAIY PLAYERSPRITE, #$0C
		LDA #$87
		STAIY PLAYERSPRITE, #$0F
		LDA #$00
		STA enemy_active, y
		LDSTA #sfx_index_sfx_zap, sound_param_byte_0
		LDSTA #soundeffect_one, sound_param_byte_1
		JSR play_sfx
		DEC playerLives
		LDSTA #$01, playerImmun
		LDSTA #$B4, playerImmunCount
		LDA playerLives
		BNE @nexts
		LDSTA #$1E, stateWaitCount
        LDSTA #GAMEOVERSTATE, gameState ;;game over
		JMP LoadGameOverScreen
      @nexts ;>
      ClearEnemy:
        LDX enemySlotLoop
        LDA enemy_active, x
        CMP #$00
        BNE IncEnemyLoop
        LDA #$FE
        STAIY (EnemyDataLow), #$00
        STAIY (EnemyDataLow), #$03
        STAIY (EnemyDataLow), #$04
        STAIY (EnemyDataLow), #$07
        STAIY (EnemyDataLow), #$08
        STAIY (EnemyDataLow), #$0B
        STAIY (EnemyDataLow), #$0C
        STAIY (EnemyDataLow), #$0F ;>
      IncEnemyLoop:  
        INC enemySlotLoop
        LDA enemySlotLoop
        CMP #$0A
        BEQ @UpdateEnemyLoopDone
        JMP UpdateEnemyLoop
  	  @UpdateEnemyLoopDone ;> ;> ;>
  UpdateHostBullet: 
    HostBulletLoopSetup:
	  LDSTA #$00, hostBltSlotLoop
    ;>
	HostBulletLoop:
      CheckHostBulletActive:
		LDAIY hostBltActive, hostBltSlotLoop
		CMP #$00
		BNE UpdateHostBulletPos
		JMP EndHostBulletLoop
		
		;;REWRITE THIS SO IT CHECKS hostBltPattern AND JUMPS TO THE APPROPRIATE PATTERN UPDATE (TO BE ADDED)
	  ;>
	  UpdateHostBulletPos: ;;TEMP FOR IMPLEMENTING HOMING BULLET PATTERN
		;;calculate the vertical and horizontal speeds of the shot by multiplying the base speed by the cosine and sine of the angle
		;;speed * cos = vertical
		;;speed * sin = horizontal
		;;x1 - x2 = adjacent
		;;y1 - y2 = opposite
		;;cos = a/h
		;;sin = o/h
		;;velocity x = cos(***arctan(slope)***) and velocity y = sin(***arctan(slope)***)
		;;trig values (sin, cos, tan) are percentages
		;;sohcahtoa work because in a unit circle the radius or hypotenuse is always 1, so the height and length (sin and cos) are already representative of the percentage.
		;;arcs are inverse trig functions, used to get the angle when all you have is length and height
		
		;;LDA octant_adjust, y
		;;x+,y+, |x| > |y| ;;#63
		;;x+,y+, |x| < |y| ;;#0		;;everything above or to the right moves with this one set... definitely an x
		;;x+,y-, |x| > |y| ;;#192
		;;x+,y-, |x| < |y| ;;#255
		;;x-,y+, |x| > |y| ;;#64
		;;x-,y+, |x| < |y| ;;#127
		;;x-,y-, |x| > |y| ;;#191
		;;x-,y-, |x| < |y| ;;#128
		;;x+ y+ 0-63    0/1
		;;x+ y- 192-255 2/3
		;;x- y+ 64-127  4/5
		;;x- y- 128-191 6/7
		
		;;make sure all of your enemy and bullet slots are set up correctly... they haven't been
		
		;;an angle of #$00 is direct to the right and it goes counterclockwise from there in octants every #$1F and quadrants every #$3F
		
		;;everything seems better but something is off with the directions on about half the shots
		LDY hostBltSlotLoop ;;#$00 ;;changing this to hostBltSlotLoop fucks it up even more???
		LDA hostBltPassed, y
		CMP #$00
		BNE @moveBlt
		
		
		
		LDA atan2x1, y
		SBC atan2x2, y
		CMP #$08
		BCC @atan
		LDA atan2y1, y
		SBC atan2y2, y
		CMP #$08
		BCC @atan
		LDA #$01
		STA hostBltPassed, y
		
		
		
		@atan
		
		LDA hostBltX, y
		STA atan2x1, y
        LDA hostBltY, y
		STA atan2y1, y
		
		CLC
		JSR ATAN2 			;;retrieve your angle using player and enemy cartesian coords
		CLC
		
		
		;;LDY enemySlotLoop ;;this won't work here
		
		
		;;try sticking some kind of clause in here that goes 1. check for bullet being in same x/y coords as target and 2. if so, keep moving in x/y at previously set coords (this won't be super accurate, but check it out anyways as it might be good enough)
		
		@moveBlt
		LDY hostBltSlotLoop
		LDA testAngle, y
		CMP #$40
		;;CPY #$01 ;;everything above or to the right moves with this one set
		BCS @oct2
		LDX testAngle, y
		LDA cosX2, x				
		STAIY hostBltIncX, hostBltSlotLoop
		LDA hostBltX, y
		CLC
		ADC hostBltIncX, y
		STA hostBltX, y
		;;LDX testAngle, y
		LDA sinX2, x
		STAIY hostBltIncY, hostBltSlotLoop
		LDA hostBltY, y
		CLC
		ADC hostBltIncY, y
		STA hostBltY, y
		JMP @next
		
		@oct2 ;;
		LDA testAngle, y
		CMP #$7F
		BCS @oct3
		LDX testAngle, y
		LDA cosX2, x
		STAIY hostBltIncX, hostBltSlotLoop
		LDA hostBltX, y
		SEC
		SBC hostBltIncX, y
		STA hostBltX, y
		;;LDX testAngle, y
		LDA sinX2, x		
		STAIY hostBltIncY, hostBltSlotLoop
		LDA hostBltY, y
		CLC
		ADC hostBltIncY, y
		STA hostBltY, y
		JMP @next
		
		@oct3 ;;??? quadrant
		;;CPY #$03
		LDA testAngle, y
		CMP #$BE
		BCS @oct4
		LDX testAngle, y
		LDA cosX2, x					
		STAIY hostBltIncX, hostBltSlotLoop
		LDA hostBltX, y
		SEC
		SBC hostBltIncX, y
		STA hostBltX, y
		;;LDX testAngle, y
		LDA sinX2, x		
		STAIY hostBltIncY, hostBltSlotLoop
		LDA hostBltY, y
		SEC
		SBC hostBltIncY, y
		STA hostBltY, y
		JMP @next
		
		@oct4 ;;??? quadrant
		LDX testAngle, y
		LDA cosX2, x					
		STAIY hostBltIncX, hostBltSlotLoop
		LDA hostBltX, y
		CLC
		ADC hostBltIncX, y
		STA hostBltX, y
		;;LDX testAngle, y
		LDA sinX2, x		
		STAIY hostBltIncY, hostBltSlotLoop
		LDA hostBltY, y
		SEC
		SBC hostBltIncY, y
		STA hostBltY, y
		@next
		
		;;LDAIY cosX2, testAngle 				;;determines the velocity using sinX2 and cosX2 tables
		;;STAIY hostBltIncX, hostBltSlotLoop
		;;
		;;;;LDY hostBltSlotLoop
		;;LDA hostBltX, y
		;;CLC
		;;ADC hostBltIncX, y
		;;STA hostBltX, y
		;;;;STAIY hostBltIncX, hostBltSlotLoop
		;;LDAIY sinX2, testAngle
		;;STAIY hostBltIncY, hostBltSlotLoop
		;;
		;;;;LDY hostBltSlotLoop
		;;LDA hostBltY, y
		;;CLC
		;;ADC hostBltIncY, y
		;;STA hostBltY, y
		;;;;STAIY hostBltIncY, hostBltSlotLoop
		;;;;close
		

		;;draw angle value
	;;	LDA $2002
	;;	LDA #$20
	;;	STA $2006
	;;	LDA #$20
	;;	STA $2006          ; start drawing the score at PPU $2020

		
		;;#$45 %1000101
		;;LDA #$05      ; first digit
	;;	CLC
	;;	LDA testAngle ;;octant ;;testAngle
		;;AND #%11110000
		;;do a check and eliminate 
		;;LSR
		;;LSR
		;;LSR
		;;LSR
		;;ADC #$06
		;;LDA #$07
	;;	STA $2007
		;;LDA #$06      ; last digit
		;;LDA testAngle
		;;AND %00001111
		;;STA $2007
		
		
		;;LDA hostBltX, y
		;;CLC
		;;ADC hostBltIncX, y ;;I believe this is currently set to 0
		;;STA hostBltX, y
        ;;LDA hostBltY, y ;;adds a value to the Y coord of the current hostBlt, to be added to its respective sprite
        ;;CLC
        ;;ADC hostBltIncY, y ;;#$04 ;;hostBltIncY, y ;;#$04
        ;;STA hostBltY, y
		JSR HostBulletCollisionCheck
		SetSlotSpriteLoByte2Spr hostBltSlotSprite, hostBltSlotLoop, #STARTHOSTBLTSPRITE ;;#$AC
      + LDSTA <hostBltSlotSprite, EnemyDataLow ;;set low byte of EnemyDataLow with starting hostBltSlotSprite
        LDSTA #$02, EnemyDataHigh 		       ;;set high byte of EnemyDataHigh with starting hostBltSlotSprite
        LDX hostBltSlotLoop
        LDA hostBltY, x 					   ;;sprite 1
        CLC
		ADC #$00
        STAIY (EnemyDataLow), #$00 			   ;;$024C first
        LDA hostBltX, x
        ADC #$00
        STAIY (EnemyDataLow), #$03 			   ;;$024F first
        LDA hostBltY, x 					   ;;sprite 2
		ADC #$00
        STAIY (EnemyDataLow), #$04			   
        LDA hostBltX, x
        ADC #$08
        STAIY (EnemyDataLow), #$07			   
		
		JMP PlayerHostBulletCollision
		
        ;;LDA hostBltY, x 					   ;;sprite 3
        ;;ADC #$08
        ;;STAIY (EnemyDataLow), #$08			   
        ;;LDA hostBltX, x
        ;;ADC #$00
        ;;STAIY (EnemyDataLow), #$0B			   
        ;;LDA hostBltY, x 					   ;;sprite 4
        ;;ADC #$08
        ;;STAIY (EnemyDataLow), #$0C			   
        ;;LDA hostBltX, x
        ;;ADC #$08
        ;;STAIY (EnemyDataLow), #$0F			   
	  ;>   
      UpdateHostBulletPosOld:
;;		;;LDA hostBltX, y
;;		;;CLC
;;		;;ADC hostBltIncX, y
;;		;;STA hostBltX, y
;;        LDA hostBltY, y ;;adds a value to the Y coord of the current hostBlt, to be added to its respective sprite
;;        CLC
;;        ADC #$04 ;;hostBltIncY, y ;;#$04
;;        STA hostBltY, y
;;		JSR HostBulletCollisionCheck
;;      SetSlotSpriteLoByte2Spr hostBltSlotSprite, hostBltSlotLoop, #STARTHOSTBLTSPRITE ;;#$AC
;;      + LDSTA <hostBltSlotSprite, EnemyDataLow ;;set low byte of EnemyDataLow with starting hostBltSlotSprite
;;        LDSTA #$02, EnemyDataHigh 		       ;;set high byte of EnemyDataHigh with starting hostBltSlotSprite
;;        LDX hostBltSlotLoop
;;        LDA hostBltY, x 					   ;;sprite 1
;;        CLC
;;		ADC #$00
;;        STAIY (EnemyDataLow), #$00 			   ;;$024C first
;;        LDA hostBltX, x
;;        ADC #$00
;;        STAIY (EnemyDataLow), #$03 			   ;;$024F first
;;        LDA hostBltY, x 					   ;;sprite 2
;;		ADC #$00
;;        STAIY (EnemyDataLow), #$04			   
;;        LDA hostBltX, x
;;        ADC #$08
;;        STAIY (EnemyDataLow), #$07			   
;;        ;;LDA hostBltY, x 					   ;;sprite 3
;;        ;;ADC #$08
;;        ;;STAIY (EnemyDataLow), #$08			   
;;        ;;LDA hostBltX, x
;;        ;;ADC #$00
;;        ;;STAIY (EnemyDataLow), #$0B			   
;;        ;;LDA hostBltY, x 					   ;;sprite 4
;;        ;;ADC #$08
;;        ;;STAIY (EnemyDataLow), #$0C			   
;;        ;;LDA hostBltX, x
;;        ;;ADC #$08
;;        ;;STAIY (EnemyDataLow), #$0F			   
	  ;>  
	  PlayerHostBulletCollision:              ;;check collision with player
        LDAIY hostBltActive, hostBltSlotLoop
        BNE @chkimmun
		JMP @nexts
	  @chkimmun
		LDA playerImmun
		BEQ @next
        JMP @nexts
      @next
        LDA hostBltY, y
        CLC
        ADC #$01
        CMP player_col_y+1 ;;,y
        BCC @next1
        JMP @nexts
      @next1
        LDA hostBltY, y
        CLC
        ADC #$0E
        CMP player_col_y ;;,y
        BCS @next2
        JMP @nexts
      @next2
        LDA hostBltX, y
        CLC
        ADC #$03
        CMP player_col_x+1 ;;,y
        BCC @next3
        JMP @nexts
      @next3
        LDA hostBltX, y
        CLC
        ADC #$0C
        CMP player_col_x ;;,y
        BCS @next4
        JMP @nexts
      @next4
	    LDA PPU_STATUS
        LDSTA #$FE, PPU_CTRL_REG1
        LDSTA #$F7, PLAYERSPRITE ;;$0200
		LDA #$7F
		STAIY PLAYERSPRITE, #$03
		LDA #$F7
		STAIY PLAYERSPRITE, #$04
		LDA #$87
		STAIY PLAYERSPRITE, #$07
		LDA #$FF
		STAIY PLAYERSPRITE, #$08
		LDA #$7F
		STAIY PLAYERSPRITE, #$0B
		LDA #$FF
		STAIY PLAYERSPRITE, #$0C
		LDA #$87
		STAIY PLAYERSPRITE, #$0F
		LDY hostBltSlotLoop								;;clearing out the hostile bullet
		LDA #$00
		STA hostBltActive, y
		LDA #$FE										
		STA hostBltX, y									
		STA hostBltY, y									
		LDSTA #sfx_index_sfx_zap, sound_param_byte_0
		LDSTA #soundeffect_one, sound_param_byte_1
		JSR play_sfx
		DEC playerLives
		LDSTA #$01, playerImmun
		LDSTA #$B4, playerImmunCount
		LDA playerLives
		BNE @nexts
		LDSTA #$1E, stateWaitCount
        LDSTA #GAMEOVERSTATE, gameState ;;game over
		JMP LoadGameOverScreen
      @nexts ;>
	  ClearHostBullet:
	  LDY hostBltSlotLoop
	  LDA hostBltActive, y
	  BNE @endclear
	  LDA #$FE
      STAIY (EnemyDataLow), #$00
      STAIY (EnemyDataLow), #$03
      STAIY (EnemyDataLow), #$04
      STAIY (EnemyDataLow), #$07
	  ;;STA PLAYERSPRITE ;;$0200
	  ;;STAIY PLAYERSPRITE, #$03
	  ;;STAIY PLAYERSPRITE, #$04
	  ;;STAIY PLAYERSPRITE, #$07
	  ;;STAIY PLAYERSPRITE, #$08
	  ;;STAIY PLAYERSPRITE, #$0B
	  ;;STAIY PLAYERSPRITE, #$0C
	  ;;STAIY PLAYERSPRITE, #$0F
	  
	  @endclear
	  ;>
      EndHostBulletLoop: 
        INC hostBltSlotLoop
        LDA hostBltSlotLoop
        CMP #$0A
        BNE @loop
        LDSTA #$00, hostBltSlotLoop
        JMP @end
        @loop
        JMP HostBulletLoop
        @end
	  ;>
    ;>
  ;>
  UpdatePlayerBullet:
    LDA player_bullet_active ;;skip update if bullet not active
    BEQ ClearPlayerBullet
    LDA player_bullet_y 	 ;;branch to next if y > top wall
    CMP #TOPWALL
    BCS MovePlayerBullet
    LDSTA #$00, player_bullet_active
    MovePlayerBullet:
      LDA player_bullet_y
      SEC
      SBC #$04
      STA player_bullet_y
      STA $0208
      LDSTA player_bullet_x, $020B ;>
    ClearPlayerBullet:
      LDA player_bullet_active
      CMP #$00
      BNE @ClearPlayerBulletDone
      LDSTA #$FE, $0208
      STA $020B
      @ClearPlayerBulletDone
    ;>
  ;>
  UpdatePlayerLives:
	CLC
	LDA playerLives
	;;AND #%00001111 ;;bitmask rightmost digit
	;;do series of compares (or try using IF/ELSEIF here, and buffer the proper sprite for writing
	;;buffer update to match calculated life value (rightmost is 024C and leftmost is 0248)
	ADC #$A0
	;;LDA #$A3
	STA $02ED
	LDA playerLives
	AND #%11110000 ;;bitmask leftmost digit
	ROL
	ROL
	ROL
	ROL
	ADC #$A0
	STA $02E9
	;;do compares and buffer sprite update if needed
  ;>
  soundengine_update
;>
  
RTI     ;; return from NMI interrupt

Subroutines:
  ReadController1:
    LDSTA #$01, JOYPAD_PORT
    LDSTA #$00, JOYPAD_PORT
    LDX #$08 ;>
  ReadController1Loop:
    LDA JOYPAD_PORT
    LSR A            ;; bit0 -> Carry
    ROL buttons1     ;; bit0 <- Carry
    DEX
    BNE ReadController1Loop
    RTS ;>
  ReadController2:
    LDSTA #$01, JOYPAD_PORT
    LDSTA #$00, JOYPAD_PORT
    LDX #$08 ;>
  ReadController2Loop:
    LDA JOYPAD_PORT2
    LSR A            ;; bit0 -> Carry
    ROL buttons2     ;; bit0 <- Carry
    DEX
    BNE ReadController2Loop
    RTS ;>
  LoadEnemyStatLoop:
    LDAIY EnemyDataAddrLow, nextEnemy_StatID   ;;use offset to load pointer
    STA EnemyDataLow
    LDA EnemyDataAddrHigh, y
    STA EnemyDataHigh
    INC nextEnemy_StatID
    LDAIY (EnemyDataLow), nextEnemy_ID
    RTS ;>
  LoadEventStatLoop:
    LDAIY EventDataAddrLow, nextEvent_StatID   ;;use offset to load pointer
    STA EventDataLow
    LDAIY EventDataAddrHigh, nextEvent_StatID
    STA EventDataHigh
    INC nextEvent_StatID
    LDAIY (EventDataLow), nextEvent_ID
    RTS ;>
  SetEnemyPtrnCurFrameX:     ;;resets every 60 frames
    LDA enemyPtrnCurFrameX, y
    CLC
    ADC #$01
    STA enemyPtrnCurFrameX, y
    CMP #$3B
    BNE SetEnemyPtrnCurFrameXDone
    LDA #$00
    STA enemyPtrnCurFrameX, y 
    LDA enemyPtrnCurSecondX, y
    CLC
    ADC #$01
    STA enemyPtrnCurSecondX, y ;>
  SetEnemyPtrnCurFrameXDone:
    RTS ;>
  SetEnemyPtrnCurFrameY:     ;;resets every 60 frames
    LDA enemyPtrnCurFrameY, y
    CLC
    ADC #$01
    STA enemyPtrnCurFrameY, y
    CMP #$3B
    BNE SetEnemyPtrnCurFrameYDone
    LDA #$00
    STA enemyPtrnCurFrameY, y ;>
  SetEnemyPtrnCurFrameYDone:
    RTS ;>
  SetHostBltCurFrame:        ;;resets every 60 frames
    LDA hostBltCurFrame, y
    CLC
    ADC #$01
    STA hostBltCurFrame, y
    CMP #$3B
    BNE SetHostBltCurFrameDone
    LDA #$00
    STA hostBltCurFrame, y 
	LDA hostBltCurSecond, y
	CLC
	ADC #$01
	STA hostBltCurSecond, y ;>
  SetHostBltCurFrameDone:
    RTS ;>
  EnemyCollisionCheck:
    LDAIY enemy_y, enemySlotLoop
    CMP #TOPWALL
    BCS +
    LDA #$00
    STA enemy_active, y
  + LDA enemy_y, y
    CMP #BOTTOMWALL
    BCC +
    LDA #$00
    STA enemy_active, y
  + LDA enemy_x, y
    CMP #LEFTWALL
    BCS +
    LDA #$00
    STA enemy_active, y
  + LDA enemy_x, y
    CMP #RIGHTWALL
    BCC EnemyCollisionCheckDone
    LDA #$00
    STA enemy_active, y ;>
  EnemyCollisionCheckDone:
    RTS ;>
  SelectOpenEnemySlot:
    LDY #$FF
  @loop
    INY
    LDA enemy_active, y
    BNE @loop
    STY nextEnemySlot
    LDA #$01
    STAIY enemy_active, nextEnemySlot
    RTS ;>
  FindOpenEnemyBltSlot:
    LDY #$FF
  @loop
    INY
    LDA hostBltActive, y
    BNE @loop
    STY nextHostBltSlot
    RTS ;>
  SpawnHostBullet: 
          LDA #$01
          STAIY hostBltActive, nextHostBltSlot
          LDAIY enemy_x, enemySlotLoop
		  CLC
		  ADC #$04
          STAIY hostBltX, nextHostBltSlot
          LDAIY enemy_y, enemySlotLoop
          STAIY hostBltY, nextHostBltSlot
          RTS
  ;>
  HostBulletCollisionCheck:
    LDAIY hostBltY, hostBltSlotLoop
    CMP #TOPWALL
    BCS +
    LDA #$00
    STA hostBltActive, y
  + LDA hostBltY, y
    CMP #BOTTOMWALL
    BCC +
    LDA #$00
    STA hostBltActive, y
  + LDA hostBltX, y
    CMP #LEFTWALL
    BCS +
    LDA #$00
    STA hostBltActive, y
  + LDA hostBltX, y
    CMP #RIGHTWALL
    BCC HostBulletCollisionCheckDone
    LDA #$00
    STA hostBltActive, y
	HostBulletCollisionCheckDone:
		RTS
	;>
  ;>
  EnemyAniInc:
    LDA enemyCurFrame, y
    CMP #$04
    BCS @Ani2
    LDA #$0A
    STA enemy_tile1, y
    LDA #$0B
    STA enemy_tile2, y
    LDA #$1A
    STA enemy_tile3, y
    LDA #$1B
    STA enemy_tile4, y
    JMP @AniEnd
    @Ani2
    LDA enemyCurFrame, y
    CMP #$08
    BCS @Ani3
    LDA #$0C
    STA enemy_tile1, y
    LDA #$0D
    STA enemy_tile2, y
    LDA #$1C
    STA enemy_tile3, y
    LDA #$1D
    STA enemy_tile4, y
    JMP @AniEnd
    @Ani3
    LDA enemyCurFrame, y
    CMP #$0C
    BCS @Ani4
    LDA #$0E
    STA enemy_tile1, y
    LDA #$0F
    STA enemy_tile2, y
    LDA #$1E
    STA enemy_tile3, y
    LDA #$1F
    STA enemy_tile4, y
    JMP @AniEnd
    @Ani4
    LDA #$0C
    STA enemy_tile1, y
    LDA #$0D
    STA enemy_tile2, y
    LDA #$1C
    STA enemy_tile3, y
    LDA #$1D
    STA enemy_tile4, y    
    @AniEnd
    LDA enemyCurFrame, y
    CLC
    ADC	#$01
    STA enemyCurFrame, y
    CMP #$10
    BNE @AniEnd2
    LDA #$00
    STA enemyCurFrame, y
    @AniEnd2
    ;;check if $04 and reset to zero if so after incrementing the frame of animation
    ;;also check frame of animation for termination byte and reset to $00 when reached
    
    ;; just set the tiles for now
    
    ;;use a pointer that I inc every 4 frames until reaching a termination byte ($FF)
	RTS
  ;>
  PlayerRAniInc:
	LDA lbHeld
    CMP #$00
    BNE @AniEnd
    LDA rbHeld
    CMP #$05
    BCS @Ani2
    LDA #$6A
    STAIY PLAYERSPRITE, #$01
    LDA #$6B
    STAIY PLAYERSPRITE, #$05
    LDA #$7A
    STAIY PLAYERSPRITE, #$09
    LDA #$7B
    STAIY PLAYERSPRITE, #$0D
    JMP @AniEnd
    @Ani2
    LDA rbHeld
    CMP #$0A
    BCS @Ani3
    LDA #$4D
    STAIY PLAYERSPRITE, #$01
    LDA #$4E
    STAIY PLAYERSPRITE, #$05
    LDA #$5D
    STAIY PLAYERSPRITE, #$09
    LDA #$5E
    STAIY PLAYERSPRITE, #$0D
    JMP @AniEnd
    @Ani3
	LDA #$8A
    STAIY PLAYERSPRITE, #$01
    LDA #$8B
    STAIY PLAYERSPRITE, #$05
    LDA #$9A
    STAIY PLAYERSPRITE, #$09
    LDA #$9B
    STAIY PLAYERSPRITE, #$0D
    @AniEnd
	RTS
  ;>  
  PlayerLAniInc:
	LDA rbHeld
    CMP #$00
    BNE @AniEnd
    LDA lbHeld
    CMP #$05
    BCS @Ani2
    LDA #$6A
    STAIY PLAYERSPRITE, #$01
    LDA #$6B
    STAIY PLAYERSPRITE, #$05
    LDA #$7A
    STAIY PLAYERSPRITE, #$09
    LDA #$7B
    STAIY PLAYERSPRITE, #$0D
    JMP @AniEnd
    @Ani2
    LDA lbHeld
    CMP #$0A
    BCS @Ani3
    LDA #$86
    STAIY PLAYERSPRITE, #$01
    LDA #$87
    STAIY PLAYERSPRITE, #$05
    LDA #$96
    STAIY PLAYERSPRITE, #$09
    LDA #$97
    STAIY PLAYERSPRITE, #$0D
    JMP @AniEnd
    @Ani3
	LDA #$88
    STAIY PLAYERSPRITE, #$01
    LDA #$89
    STAIY PLAYERSPRITE, #$05
    LDA #$98
    STAIY PLAYERSPRITE, #$09
    LDA #$99
    STAIY PLAYERSPRITE, #$0D
    @AniEnd
	RTS
  ;>  
  MovePlayerLeft:
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$03
	LDAIY PLAYERSPRITE, #$07
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$07
	LDAIY PLAYERSPRITE, #$0B
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$0B
	LDAIY PLAYERSPRITE, #$0F
	SEC
	SBC #$02
	STAIY PLAYERSPRITE, #$0F
	RTS
  ;>
  MovePlayerRight:
	LDAIY PLAYERSPRITE, #$03
	CLC
	ADC #$02
	STAIY PLAYERSPRITE, #$03
	LDAIY PLAYERSPRITE, #$07
	CLC
	ADC #$02
	STAIY PLAYERSPRITE, #$07
	LDAIY PLAYERSPRITE, #$0B
	CLC
	ADC #$02
	STAIY PLAYERSPRITE, #$0B
	LDAIY PLAYERSPRITE, #$0F
	CLC
	ADC #$02
	STAIY PLAYERSPRITE, #$0F
	RTS
  ;>
  DrawNewColumn:
	LDA ntInc
    CMP #$01
    BCC @end
    CMP #$02
    BNE @2
	
	JMP @draw
	@2
	@end
	RTS
	@draw
  ;>
  DrawColumn:  
	;; x32 wide
    ;; x30 tall
    ;; 960 tiles total
    ;; need some form of inner loop
	;;scroll = holds current y scroll pos in pixels
	;;columnLow = holds current y scroll * 4 and uses that as PPU nametable addr location to begin to write to (is that correct?)
	;;columnHigh = holds PPU nametable high addr location to begin to write to
	;;columnNumber = count that is incremented every time scroll is divisible by 8
	;;sourceLow = currently just columnNumber
	;;sourceHigh = currently just $00
	;;pointerLo = nametable data location + sourceLow
	;;pointerHi = nametable data location + $00
	
	LDA scroll ;;232 or 11101000
	CLC
	ASL A
	ASL A ;;10100000
	STA columnLow
	
	LDA scroll
	CLC
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A
	LSR A ;;00000011
	STA sourceHigh 	;;to be appended to NT pointer
	STA columnHigh
	
	LDA nametable     ;; calculate new column address using current nametable
	EOR #$01          ;; invert low bit, A = $00 or $01
	ASL A             ;; shift up, A = $00 or $02
	ASL A             ;; $00 or $04
	ASL A			  ;; $00 or $08
	CLC
	ADC #$20          ;; add high byte of nametable base address ($2000)
	ADC columnHigh    ;;adds converted scroll value from previous calculation
	STA columnHigh    ;; now address = $20 or $24 for nametable 0 or 1
	
	LDA columnNumber
	STA sourceLow  	  ;;to be appended to NT pointer
	
	LDSTA #<background, pointerLo ;;$00
	LDSTA #>background, pointerHi ;;$E0
	
	AddToPointer columnLow, sourceHigh ;;sourceHigh ;;sourceLow, sourceHigh  ;;column data start + offset = address to load column data from
  ;>
  DrawColumnLoop: ;;sets the column in PPU to be written to using columnLow/High
	LDA $2002             ;; read PPU status to reset the high/low latch
	LDA columnHigh		  ;; sets the PPU to the beginning of its' nametable located at the address in columnHigh/Low
	STA $2006             ;; write the high byte of row address
	LDA columnLow
	STA $2006             ;; write the low byte of row address
	LDX #$20              ;; copy 32 bytes to the row
	LDY #$00 
	;>
  BlargColumnLoop:
	LDA (pointerLo), y
	STA $2007 			  ;;this goes backwards in horiz scrolling because scroll is currently decrementing
	INY
	DEX
	BNE BlargColumnLoop
	RTS
  ;>
  ATAN2:
	LDX #$00
	LDY #$00
	;;LDSTA #$FB, octant
	LDSTA #$00, octant	
	LDY hostBltSlotLoop ;;#$00 TEMP
	LDA atan2x1, y
	SBC atan2x2, y ;;once bullet arrives at target, it just jumps back and forth between two surrounding spots
	BCS @next1 	;;means jump to end of this section
	EOR #$FF	;;flips the bits
  @next1	
	TAX
	ROL octant
  ;;@next1
	LDY hostBltSlotLoop ;;#$00 TEMP
	LDA atan2y1, y
	SBC atan2y2, y ;;once bullet arrives at target, it just jumps back and forth between two surrounding spots
	BCS @next2		;;means jump to end of this section
	EOR #$FF		;;flips the bits
  @next2
	TAY
	ROL octant
  ;;@next2
	LDA log2_tab, x
	SBC log2_tab, y ;;if carry is set, it will be cleared upon borrowing
	BCC @next3		;;means jump to end of this section 
	EOR	#$FF		;;flips the bits
  @next3
	TAX
  ;;@next3
	LDA octant
  ;;@next3
	ROL
	AND #%111	;;???????
	;;ADC #$01    ;;I put this in here because I think the octant table is wrong for NES but it needs to be tested with different values after shots are firing!!!
				;;will likely need to physically shift the octants down by one if it does work for all but the last
	TAY
	STY octant ;;for testing purposes only
	
	LDA atan_tab, x
	EOR octant_adjust, y
	LDY hostBltSlotLoop
	STA testAngle, y
	
	RTS
  ;>
  StaticBGLoop:
	LDA (pointerLo), y 	  		;; copy one background byte from address in pointer plus Y
	CMP #$FF					;;exits loop when termination byte found
	BEQ @end
	AND #%10000000 				;;bitmask and check if bit 7 is on
	BNE @runblock
	@litblock
	LDA (pointerLo), y
	ADC #$01
	TAX
	@litloop
	INY
	CPY #$00
	BNE @next1
	INC pointerHi
	@next1
	LDA (pointerLo), y
	STA PPU_DATA            		
	DEX
	CPX #$00
	BNE @litloop
	INY
	CPY #$00
	BNE @next2
	INC pointerHi
	@next2
	JMP StaticBGLoop

	;;Try and use REPT/ENDR and IF/ENDIF and do a comparison of space used between the two
	
	@runblock
	LDA (pointerLo), y
	AND #%01111111
	ADC #$03 					;;this form of RLE starts the run length at 3
	TAX
	INY
	CPY #$00
	BNE @next3
	INC pointerHi
	@next3
	@runloop
	;;repeat single byte and dec x until it = 0
	LDA (pointerLo), y
	STA PPU_DATA
	DEX
	CPX #$00
	BNE @runloop
	INY
	CPY #$00
	BNE @next4
	INC pointerHi
	@next4
	;;CPY #$00
	;;BNE InsideTitleLoop
	;;INC pointerHi      	  	;; low byte went 0 to 256, so high byte needs to be changed now
	JMP StaticBGLoop
	@end
	RTS
  ;>
;>
DebugSubroutines:
  CPUUsageBar:
    LDX #%00011111 ;;sprites/bg/monochrome
    STX $2001
    LDY #21
    @loop
    DEY
    BNE @loop
    DEX
    STX $2001
	RTS
  ;>
  TestBeep:
	lda #%00000001
	sta SND_MASTERCTRL_REG ;;enable Square 1
	;;square 1
	lda #%10001111 ;;Duty 10, Length Counter Disabled, Saw Envelopes disabled, Volume F
	sta SND_SQUARE1_REG
	lda #$08    ;;Set Negate flag so low notes aren't silenced
	sta $4001
	
	lda #$C9    ;;0C9 is a C# in NTSC mode
	sta $4002   ;;low 8 bits of period
	lda #$00
	sta $4003   ;;high 3 bits of period

    ;;lda #%00000111 ;enable Sq1, Sq2 and Tri channels
    ;;sta SND_MASTERCTRL_REG
    ;;;Square 1
    ;;lda #%00111000 ;Duty 00, Volume 8 (half volume)
    ;;sta SND_SQUARE1_REG
    ;;lda #$C9 ;$0C9 is a C# in NTSC mode
    ;;sta $4002 ;low 8 bits of period
    ;;lda #$00
    ;;sta $4003 ;high 3 bits of period
    ;;;Square 2
    ;;lda #%01110110 ;Duty 01, Volume 6
    ;;sta SND_SQUARE2_REG
    ;;lda #$A9 ;$0A9 is an E in NTSC mode
    ;;sta $4006
    ;;lda #$00
    ;;sta $4007
    ;;;Triangle
    ;;lda #%10000001 ;Triangle channel on
    ;;sta SND_TRIANGLE_REG
    ;;lda #$42 ;$042 is a G# in NTSC mode
    ;;sta $400A
    ;;lda #$00
    ;;sta $400B


    RTS
  ;>
;>

LoadGameOverScreen:
  LoadGameOverBackground:
    LDSTA #%00010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 2
	LDSTA #%00000000, PPU_CTRL_REG2 ;; disable background
    LDA PPU_STATUS             		;; read PPU status to reset the high/low latch
    LDSTA #$24, PPU_ADDRESS			;; write the high byte of $2000 address     
    LDSTA #$00, PPU_ADDRESS			;; write the low byte of $2000 address    
    LDSTA #<gameoverbgcomp, pointerLo	;; put the low byte of the address of background into pointer
    LDSTA #>gameoverbgcomp, pointerHi	;; put the high byte of the address into pointer
    LDX #$00          				;; start at pointer + 0
    LDY #$00 ;>
    OutsideGameOverLoop: ;>
    InsideGameOverLoop:
	  JSR StaticBGLoop
      ;;LDA (pointerLo), y 	  		  ;; copy one background byte from address in pointer plus Y
      ;;STA PPU_DATA            		  ;; this runs 256 * 4 times
      ;;INY                	  		  ;; inside loop counter
      ;;CPY #$00	              		            		  
      ;;BNE InsideGameOverLoop     	  ;; run the inside loop 256 times before continuing down
      ;;INC pointerHi      	  		  ;; low byte went 0 to 256, so high byte needs to be changed now
      ;;INX
      ;;CPX #$04
      ;;BNE OutsideGameOverLoop     	  ;; run the outside loop 256 times before continuing down        
	  LDSTA #%10010001, PPU_CTRL_REG1 ;; disable NMI, sprites from Pattern Table 0, background from Pattern Table 1
      LDSTA #%00001000, PPU_CTRL_REG2 ;; enable background
	  STX #$00
    ;>
	ReadCtrlGameOver:
	  LDA stateWaitCount
	  BEQ @readCtrl
	  DEC stateWaitCount
	  @readCtrl
	  LDA stateWaitCount
	  BNE GameOverForever
	  JSR ReadController1
	  LDA buttons1    ;; player 1 - Start
      AND #%00010000
      BEQ GameOverForever
	  LDSTA #TITLESTATE, gameState
	  JMP RESET
	;>
	GameOverForever:
	  ;;LDSTA #%10010001, PPU_CTRL_REG1 ;; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
      ;;LDSTA #%00001000, PPU_CTRL_REG2 ;; enable sprites, enable background, no clipping on left side
	  JMP GameOverForever
	;>
;>

LevelData:
  EnemyDataAddrLow:
    .db <NextEn_Second, <NextEnemy_Frame, <NextEnemy_X, <NextEnemy_Y, <NextEnemy_Pattern, <NextEnemy_PatternMirror, <NextEnemy_PatternMirrorY, <NextEnemy_BulletPattern ;>
  EnemyDataAddrHigh:
    .db >NextEn_Second, >NextEnemy_Frame, >NextEnemy_X, >NextEnemy_Y, >NextEnemy_Pattern, >NextEnemy_PatternMirror, >NextEnemy_PatternMirrorY, >NextEnemy_BulletPattern ;>
  NextEn_Second:
    .db $01,$01,$01,$01,$02,$02,$02,$02,$03,$04,$05,$06,$07,$08 ;>
	  ;;$0E,$0E,$0E,$0E,$0F,$0F,$0F,$0F,$10,$11,$12,$13,$14,$15 ;>
  NextEnemy_Frame:
    .db $00,$0F,$1E,$2D,$00,$0F,$1E,$2D,$00,$0F,$1E,$2D,$00,$0F ;> ;;1E 1/2 second, 0F 1/4, 2D 3/4
  NextEnemy_X:
    .db $02,$F7,$02,$F7,$C0,$40,$C0,$40,$C0,$40,$40,$80,$40,$80,$80,$60,$70,$80,$B0,$40,$80,$60,$70,$80 ;> ;;leftwall = $02, rightwall = $F7
  NextEnemy_Y:
    .db $35,$35,$35,$35,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04 ;> ;;top = $04, bottom = ?
  NextEnemy_Pattern: 
    .db $05,$05,$05,$05,$02,$02,$03,$03,$02,$02,$02,$02,$00,$00 ;>
  NextEnemy_PatternMirror:
    .db $01,$00,$01,$00,$00,$01,$00,$01,$00,$01,$00,$01,$00,$00 ;>
  NextEnemy_PatternMirrorY:
    .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;>
  NextEnemy_BulletPattern:
    .db $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02 ;>
  EventDataAddrLow:
    .db <NextEvent_Second, <NextEvent_Frame, <NextEvent_Action ;>
  EventDataAddrHigh:
    .db >NextEvent_Second, >NextEvent_Frame, >NextEvent_Action ;>
  NextEvent_Second:
    .db $01,$02,$02,$03,$03,$04,$04,$05,$07,$07,$07,$07,$08,$08,$08,$08,$09,$09,$09,$09,$0A,$0A ;> ;;,$04,$04,$04,$04,$05,$05,$05,$05,$06,$06,$06,$06,$07,$07 ;; ;>
  NextEvent_Frame:                                                                                                 
    .db $00,$00,$2D,$0F,$2D,$0F,$2D,$0F,$00,$0F,$1E,$2D,$00,$0F,$1E,$2D,$00,$0F,$1E,$2D,$00,$0F ;>
  NextEvent_Action:                                                                                                
    .db $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;>
;>
MathTables:
	octant_adjust:
		.byte %00111111	;;x+,y+, |x| > |y| ;;3:00 - 4:30     ??????????????????????????????????
		.byte %00000000 ;;x+,y+, |x| < |y| ;;4:30 - 6:00???
		.byte %11000000 ;;x+,y-, |x| > |y|
		.byte %11111111 ;;x+,y-, |x| < |y| ;;4:30 - 6:00???
		.byte %01000000 ;;x-,y+, |x| > |y| ;;9:00 = 10:30
		.byte %01111111 ;;x-,y+, |x| < |y|
		.byte %10111111 ;;x-,y-, |x| > |y|
		.byte %10000000 ;;x-,y-, |x| < |y|
	;>
	atan_tab: ;;;;;;;; atan(2^(x/32))*128/pi ;;;;;;;; (confined to octant)
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $05,$05,$05,$05,$05,$05,$05,$05
		.byte $06,$06,$06,$06,$06,$06,$06,$06
		.byte $07,$07,$07,$07,$07,$07,$08,$08
		.byte $08,$08,$08,$08,$09,$09,$09,$09
		.byte $09,$0a,$0a,$0a,$0a,$0b,$0b,$0b
		.byte $0b,$0c,$0c,$0c,$0c,$0d,$0d,$0d
		.byte $0d,$0e,$0e,$0e,$0e,$0f,$0f,$0f
		.byte $10,$10,$10,$11,$11,$11,$12,$12
		.byte $12,$13,$13,$13,$14,$14,$15,$15
		.byte $15,$16,$16,$17,$17,$17,$18,$18
		.byte $19,$19,$19,$1a,$1a,$1b,$1b,$1c
		.byte $1c,$1c,$1d,$1d,$1e,$1e,$1f,$1f
	;>
	log2_tab: ;;;;;;;; log2(x)*32 ;;;;;;;; ;;check and see if a reversed RLE algorithm would save space on this specifically
		.byte $00,$00,$20,$32,$40,$4a,$52,$59
		.byte $60,$65,$6a,$6e,$72,$76,$79,$7d
		.byte $80,$82,$85,$87,$8a,$8c,$8e,$90
		.byte $92,$94,$96,$98,$99,$9b,$9d,$9e
		.byte $a0,$a1,$a2,$a4,$a5,$a6,$a7,$a9
		.byte $aa,$ab,$ac,$ad,$ae,$af,$b0,$b1
		.byte $b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9
		.byte $b9,$ba,$bb,$bc,$bd,$bd,$be,$bf
		.byte $c0,$c0,$c1,$c2,$c2,$c3,$c4,$c4
		.byte $c5,$c6,$c6,$c7,$c7,$c8,$c9,$c9
		.byte $ca,$ca,$cb,$cc,$cc,$cd,$cd,$ce
		.byte $ce,$cf,$cf,$d0,$d0,$d1,$d1,$d2
		.byte $d2,$d3,$d3,$d4,$d4,$d5,$d5,$d5
		.byte $d6,$d6,$d7,$d7,$d8,$d8,$d9,$d9
		.byte $d9,$da,$da,$db,$db,$db,$dc,$dc
		.byte $dd,$dd,$dd,$de,$de,$de,$df,$df
		.byte $df,$e0,$e0,$e1,$e1,$e1,$e2,$e2
		.byte $e2,$e3,$e3,$e3,$e4,$e4,$e4,$e5
		.byte $e5,$e5,$e6,$e6,$e6,$e7,$e7,$e7
		.byte $e7,$e8,$e8,$e8,$e9,$e9,$e9,$ea
		.byte $ea,$ea,$ea,$eb,$eb,$eb,$ec,$ec
		.byte $ec,$ec,$ed,$ed,$ed,$ed,$ee,$ee
		.byte $ee,$ee,$ef,$ef,$ef,$ef,$f0,$f0
		.byte $f0,$f1,$f1,$f1,$f1,$f1,$f2,$f2
		.byte $f2,$f2,$f3,$f3,$f3,$f3,$f4,$f4
		.byte $f4,$f4,$f5,$f5,$f5,$f5,$f5,$f6
		.byte $f6,$f6,$f6,$f7,$f7,$f7,$f7,$f7
		.byte $f8,$f8,$f8,$f8,$f9,$f9,$f9,$f9
		.byte $f9,$fa,$fa,$fa,$fa,$fa,$fb,$fb
		.byte $fb,$fb,$fb,$fc,$fc,$fc,$fc,$fc
		.byte $fd,$fd,$fd,$fd,$fd,$fd,$fe,$fe
		.byte $fe,$fe,$fe,$ff,$ff,$ff,$ff,$ff
	;>
	sinX2:
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$00,$00,$00,$00,$00,$00
	;>
	cosX2:
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$02,$02,$02,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $01,$01,$01,$01,$01,$01,$01,$01
		.byte $01,$01,$00,$00,$00,$00,$00,$00
		.byte $00,$00,$00,$00,$00,$01,$01,$01
		.byte $01,$01,$01,$01,$01,$01,$01,$02
		.byte $02,$02,$02,$02,$02,$02,$02,$02
		.byte $02,$02,$02,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$03,$03,$03,$03,$03
		.byte $03,$03,$03,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
		.byte $04,$04,$04,$04,$04,$04,$04,$04
	;>
;>
VisualData:
  .org $E000   	;;align the background data so the lower address is $00
  background:
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$05 
    .db $34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$05 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$31 ;;$05 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$05 
    .db $31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31 ;;$05 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$09 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$08 
    .db $31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$07 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31 ;;$06 
    .db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 ;;$05
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000 ;;attributes 8 x 8 = 64 bytes
    .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000 ;>
  titlebg: 		;;$F19C
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$1c,$1e,$1e,$1e,$1e,$1e,$19,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$1c,$1d,$0e,$1f,$0e,$31,$10,$0a,$16,$0e,$31,$31,$31,$31,$34,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$19,$1b,$0e,$1c,$1c,$31,$1c,$1d,$0a,$1b,$1d,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31
;;	.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31 
;;	.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;;attributes 8 x 8 = 64 bytes
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
;;    .db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  ;>
  titlebgcomp: 	;;EE3A 866 saved
	.db $ce, $31, $00, $34, $86, $31, $00, $34
	.db $85, $31, $00, $34, $8e, $31, $00, $34
	.db $90, $31, $00, $32, $92, $31, $00, $34
	.db $98, $31, $00, $34, $8d, $31, $00, $34
	.db $91, $31, $00, $32, $85, $31, $00, $1c
	.db $82, $1e, $00, $19, $9b, $31, $00, $34
	.db $82, $31, $00, $34, $8a, $31, $00, $34
	.db $a3, $31, $0b, $34, $31, $1c, $1d, $0e
	.db $1f, $0e, $31, $10, $0a, $16, $0e, $81
	.db $31, $00, $34, $b3, $31, $00, $34, $99
	.db $31, $0a, $19, $1b, $0e, $1c, $1c, $31
	.db $1c, $1d, $0a, $1b, $1d, $85, $31, $00
	.db $34, $93, $31, $00, $33, $96, $31, $00
	.db $34, $91, $31, $00, $33, $a1, $31, $00
	.db $34, $d4, $31, $00, $34, $89, $31, $00
	.db $34, $85, $31, $00, $34, $ac, $31, $00
	.db $34, $88, $31, $00, $34, $ae, $31, $00
	.db $34, $8d, $31, $00, $34, $c7, $31, $00
	.db $34, $87, $31, $00, $34, $a3, $31, $00
	.db $34, $a5, $31, $bd, $55, $ff ;>
  stationbgcomp:
  	.db $9c, $31, $00, $09, $9c, $31, $00, $08
	.db $8e, $31, $00, $34, $86, $31, $00, $34
	.db $80, $31, $00, $07, $81, $31, $00, $34
	.db $8e, $31, $00, $34, $85, $31, $00, $06
	.db $87, $31, $00, $32, $91, $31, $01, $05
	.db $34, $98, $31, $03, $34, $31, $31, $09
	.db $8a, $31, $00, $34, $8e, $31, $03, $08
	.db $31, $31, $32, $99, $31, $00, $07, $8d
	.db $31, $00, $34, $82, $31, $00, $34, $85
	.db $31, $00, $06, $81, $31, $00, $34, $97
	.db $31, $00, $05, $88, $31, $00, $34, $80
	.db $31, $01, $85, $85, $87, $31, $00, $34
	.db $80, $31, $00, $09, $8b, $31, $81, $85
	.db $8a, $31, $00, $08, $8b, $31, $81, $85
	.db $00, $34, $89, $31, $00, $07, $8b, $31
	.db $81, $85, $80, $31, $00, $34, $86, $31
	.db $03, $06, $31, $31, $34, $88, $31, $81
	.db $85, $84, $31, $00, $33, $82, $31, $00
	.db $05, $8b, $31, $81, $85, $01, $31, $34
	.db $88, $31, $00, $09, $85, $31, $00, $33
	.db $82, $31, $81, $85, $8a, $31, $00, $08
	.db $8a, $31, $00, $34, $81, $85, $8a, $31
	.db $00, $07, $8b, $31, $81, $85, $8a, $31
	.db $00, $06, $8b, $31, $81, $85, $8a, $31
	.db $00, $05, $82, $31, $00, $34, $85, $31
	.db $81, $85, $00, $34, $85, $31, $00, $34
	.db $80, $31, $00, $09, $8a, $31, $83, $85
	.db $89, $31, $00, $08, $88, $31, $01, $34
	.db $31, $83, $85, $81, $31, $00, $34, $84
	.db $31, $00, $07, $8a, $67, $83, $85, $89
	.db $67, $00, $06, $86, $31, $00, $34, $80
	.db $31, $83, $85, $84, $31, $00, $34, $81
	.db $31, $00, $05, $89, $67, $85, $85, $88
	.db $67, $00, $09, $89, $31, $85, $85, $88
	.db $31, $00, $08, $89, $67, $85, $85, $88
	.db $67, $00, $07, $89, $31, $85, $85, $80
	.db $31, $00, $34, $84, $31, $00, $06, $89
	.db $67, $85, $85, $88, $67, $00, $05, $bd
	.db $00, $ff ;>
  gameoverbgcomp:
	.db $ce, $31, $00, $34, $86, $31, $00, $34
	.db $85, $31, $00, $34, $8e, $31, $00, $34
	.db $90, $31, $00, $32, $92, $31, $00, $34
	.db $98, $31, $00, $34, $8d, $31, $00, $34
	.db $91, $31, $00, $32, $aa, $31, $00, $34
	.db $82, $31, $00, $34, $8a, $31, $00, $34
	.db $a2, $31, $15, $10, $0a, $16, $0e, $31
	.db $18, $1f, $0e, $1b, $0f, $15, $18, $20
	.db $21, $22, $23, $24, $25, $26, $27, $28
	.db $29, $8e, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $92, $31, $00, $34, $86, $31, $00
	.db $34, $81, $31, $85, $55, $b5, $00, $ff ;>
  gameoverbg:
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$10,$0a,$16,$0e,$31,$18,$1f,$0e,$1b,$0f,$15,$18,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29
	;;.db $2a,$2b,$2c,$2d,$2e,$2f,$2a,$2b,$2c,$2d,$2e,$2f,$2a,$2b,$2c,$2d,$2e,$2f,$2a,$2b,$2c,$2d,$2e,$2f,$2a,$2b,$2c,$2d,$2e,$2f,$38,$39
	;;.db $3a,$3b,$3c,$3d,$3e,$3f,$3a,$3b,$3c,$3d,$3e,$3f,$3a,$3b,$3c,$3d,$3e,$3f,$3a,$3b,$3c,$3d,$3e,$3f,$3a,$3b,$3c,$3d,$3e,$3f,$48,$49
	;;.db $4a,$4b,$4c,$4d,$4e,$4f,$4a,$4b,$4c,$4d,$4e,$4f,$4a,$4b,$4c,$2a,$2b,$2c,$2d,$2e,$2f,$4d,$4e,$4f,$4a,$4b,$4c,$4d,$4e,$4f,$58,$59
	;;.db $5a,$5b,$5c,$5d,$5e,$5f,$5a,$5b,$5c,$5d,$5e,$5f,$5a,$5b,$5c,$3a,$3b,$3c,$3d,$3e,$3f,$5d,$5e,$5f,$5a,$5b,$5c,$5d,$5e,$5f,$68,$69
	;;.db $6a,$6b,$6c,$6d,$6e,$6f,$6a,$6b,$6c,$6d,$6e,$6f,$6a,$6b,$6c,$4a,$4b,$4c,$4d,$4e,$4f,$6d,$6e,$6f,$6a,$6b,$2a,$2b,$2c,$2d,$2e,$2f
	;;.db $7a,$2a,$2a,$2b,$2c,$2d,$2e,$2f,$2c,$2d,$2e,$2f,$7a,$7b,$7c,$5a,$5b,$5c,$5d,$5e,$5f,$7d,$7e,$7f,$7a,$7b,$3a,$3b,$3c,$3d,$3e,$3f
	;;.db $8a,$3a,$3a,$3b,$3c,$3d,$3e,$3f,$3c,$3d,$3e,$3f,$8a,$8b,$8c,$6a,$6b,$6c,$6d,$6e,$6f,$8d,$8e,$8f,$8a,$8b,$4a,$4b,$4c,$4d,$4e,$4f
	;;.db $9a,$4a,$4a,$4b,$4c,$4d,$4e,$4f,$4c,$4d,$4e,$4f,$9a,$9b,$9c,$7a,$7b,$7c,$7d,$7e,$7f,$9d,$9e,$9f,$9a,$9b,$5a,$5b,$5c,$5d,$5e,$5f
	;;.db $aa,$5a,$5a,$5b,$5c,$5d,$5e,$5f,$5c,$5d,$5e,$5f,$aa,$ab,$ac,$8a,$8b,$8c,$8d,$8e,$8f,$ad,$ae,$af,$aa,$ab,$6a,$6b,$6c,$6d,$6e,$6f
	;;.db $ba,$6a,$6a,$6b,$6c,$6d,$6e,$6f,$6c,$6d,$6e,$6f,$ba,$bb,$bc,$9a,$9b,$9c,$9d,$9e,$9f,$bd,$be,$bf,$ba,$bb,$7a,$7b,$7c,$7d,$7e,$7f
	;;.db $ca,$7a,$7a,$7b,$7c,$7d,$7e,$7f,$7c,$7d,$7e,$7f,$ca,$cb,$cc,$aa,$ab,$ac,$ad,$ae,$af,$cd,$ce,$cf,$ca,$cb,$8a,$8b,$8c,$8d,$8e,$8f
	;;.db $da,$8a,$8a,$8b,$8c,$8d,$8e,$8f,$8c,$8d,$8e,$8f,$da,$db,$dc,$ba,$bb,$bc,$bd,$be,$bf,$dd,$de,$df,$da,$db,$9a,$9b,$9c,$9d,$9e,$9f
	;;.db $ea,$9a,$9a,$9b,$9c,$9d,$9e,$9f,$9c,$9d,$9e,$9f,$ea,$eb,$ec,$ca,$cb,$cc,$cd,$ce,$cf,$ed,$ee,$ef,$ea,$eb,$aa,$ab,$ac,$ad,$ae,$af
	;;.db $fa,$aa,$aa,$ab,$ac,$ad,$ae,$af,$ac,$2a,$2b,$2c,$2d,$2e,$2f,$2a,$2b,$2c,$2d,$2e,$2a,$2b,$2c,$2d,$2e,$2f,$2e,$2f,$bc,$bd,$be,$bf
	;;.db $31,$ba,$ba,$bb,$bc,$bd,$be,$bf,$bc,$3a,$3b,$3c,$3d,$3e,$3f,$3a,$3b,$3c,$3d,$3e,$3a,$3b,$3c,$3d,$3e,$3f,$3e,$3f,$cc,$cd,$ce,$cf
	;;.db $2a,$2b,$2c,$2d,$2e,$2f,$ce,$cf,$cc,$4a,$4b,$4c,$4d,$4e,$4f,$4a,$4b,$4c,$4d,$4e,$4a,$4b,$4c,$4d,$4e,$4f,$4e,$4f,$dc,$dd,$de,$df
	;;.db $3a,$3b,$3c,$3d,$3e,$3f,$de,$df,$dc,$5a,$5b,$5c,$5d,$5e,$5f,$5a,$5b,$5c,$5d,$5e,$5a,$5b,$5c,$5d,$5e,$5f,$5e,$5f,$ec,$ed,$ee,$ef
	;;.db $4a,$4b,$4c,$4d,$4e,$4f,$ee,$ef,$ec,$6a,$6b,$6c,$6d,$6e,$6f,$6a,$6b,$6c,$6d,$6e,$6a,$6b,$6c,$6d,$6e,$6f,$6e,$6f,$fc,$fd,$fe,$ff
	;;.db $5a,$5b,$5c,$5d,$5e,$5f,$fe,$ff,$fc,$7a,$7b,$7c,$7d,$7e,$7f,$7a,$7b,$7c,$7d,$7e,$7a,$7b,$7c,$7d,$7e,$7f,$7e,$7f,$31,$31,$31,$31
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;;attributes 8 x 8 = 64 bytes
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	;;.db $4a,$4b,$4c,$4d,$4e,$4f,$ee,$ef,$ec,$6a,$6b,$6c,$6d,$6e,$6f,$6a,$6b,$6c,$6d,$6e,$6a,$6b,$6c,$6d,$6e,$6f,$6e,$6f,$fc,$fd,$fe,$ff
	;;.db $5a,$5b,$5c,$5d,$5e,$5f,$fe,$ff,$fc,$7a,$7b,$7c,$7d,$7e,$7f,$7a,$7b,$7c,$7d,$7e,$7a,$7b,$7c,$7d,$7e,$7f,$7e,$7f,$01,$01,$01,$01

	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	
	;;.db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
	;; .db $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
 ;>
  stationbg:
  	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$09 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$07 ;;$31
	;;.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$06 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$05 ;;$31
	;;.db $34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$09 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $31,$31,$32,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$07 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$06 ;;$31
	;;.db $31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$05 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$09 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$07 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$06 ;;$31
	;;.db $31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$05 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$09 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$33,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$07 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$06 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$05 ;;$31
	;;.db $31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$34,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$09 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$85,$85,$85,$85,$85,$85,$31,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$07 ;;$31
	;;.db $67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$85,$85,$85,$85,$85,$85,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$06 ;;$67
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$85,$85,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$34,$31,$31,$31,$31,$05 ;;$31
	;;.db $67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$85,$85,$85,$85,$85,$85,$85,$85,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$09 ;;$67
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$85,$85,$85,$85,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$08 ;;$31
	;;.db $67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$85,$85,$85,$85,$85,$85,$85,$85,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$07 ;;$67
	;;.db $31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$85,$85,$85,$85,$85,$85,$85,$85,$31,$31,$31,$34,$31,$31,$31,$31,$31,$31,$31,$06 ;;$31
	;;.db $67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$85,$85,$85,$85,$85,$85,$85,$85,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$67,$05 ;;$67
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101 ;;attributes 8 x 8 = 64 bytes
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
	;;.db %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101, %01010101
  ;>
  attributes:   ;;8 x 8 = 64 bytes
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    ;;.db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
 
    ;;.db $24,$24,$24,$24, $47,$47,$24,$24 
    ;;.db $47,$47,$47,$47, $47,$47,$24,$24 
    ;;.db $24,$24,$24,$24 ,$24,$24,$24,$24
    ;;.db $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
    ;;.db $47,$47,$47,$47, $47,$47,$24,$24 
    ;;.db $24,$24,$24,$24 ,$24,$24,$24,$24
    ;;.db $24,$24,$24,$24, $55,$56,$24,$24
	;>
  palette:		;;The palettes start at PPU address $3F00 and $3F10
    .db $0F,$30,$3D,$17, $0F,$30,$10,$2D, $0F,$0F,$0F,$0F, $0F,$0F,$0F,$0F;;$0F,$36,$17,$0F, $0F,$30,$21,$0F;;, $0F,$27,$17,$0F   ;;background palette
    .db $0F,$30,$3D,$17, $0F,$08,$16,$36, $0F,$1C,$15,$14, $0F,$30,$10,$07;;, $0F,$02,$38,$3C   ;;sprite palette
  ;>
  sprites:
      ;;vert tile attr horiz
    .db $FE, $00, $00, $FE ;;sprite 0 main ship
    .db $FE, $33, $00, $FE ;;sprite 1 rock
    .db $FE, $05, $00, $FE ;;sprite 2 bullet
	;;.db $FE, $			   ;;sprite 3 level/lives on-screen
  ;>
  playersprite:
	.db $C0, $6A, $01, $80 ;;sprite 3 player top left (020C)
	.db $C0, $6B, $01, $88 ;;sprite 4 player top right
	.db $C8, $7A, $01, $80 ;;sprite 5 player bottom left 
	.db $C8, $7B, $01, $88 ;;sprite 6 player bottom right ;>
  playerleftsprite: ;>
  playerhardleftsprite: ;>
  playerrightsprite: ;>
  playerhardrightsprite: ;>
  ;>
  enemy:
	.db $FE, $68, $03, $FE 	 ;;#%00000011, $FE	 ;;$0C (enemy 0)
	.db $FE, $69, $03, $FE 	 ;;#%00000011, $FE
	.db $FE, $78, $03, $FE 	 ;;#%00000011, $FE	 
	.db $FE, $79, $03, $FE 	 ;;#%00000011, $FE
	.db $FE, $69, $03, $FE 	 ;;#%01000011, $FE	 ;;(enemy 1)
	.db $FE, $68, $03, $FE 	 ;;#%01000011, $FE	 
	.db $FE, $79, $03, $FE 	 ;;#%01000011, $FE
	.db $FE, $78, $03, $FE 	 ;;#%01000011, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 2)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 3)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 4)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 5)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 6)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 7)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 8)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE
	.db $FE, $66, $03, $FE	 ;;(enemy 9)
	.db $FE, $67, $03, $FE	 
	.db $FE, $76, $03, $FE
	.db $FE, $77, $03, $FE ;>
  bullet:
	.db $FE, $06, $00, $FE	 ;;$32 (bullet 0)
	.db $FE, $07, $00, $FE	 
	.db $FE, $06, $00, $FE	 ;;(bullet 1)
	.db $FE, $07, $00, $FE
	.db $FE, $06, $00, $FE	 ;;(bullet 2)
	.db $FE, $07, $00, $FE	 
	.db $FE, $06, $00, $FE	 ;;(bullet 3)
	.db $FE, $07, $00, $FE
	.db $FE, $06, $00, $FE	 ;;bullet 4
	.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE	 ;;bullet 5
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;bullet 6
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE	 ;;bullet 7
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 8) ;;this is sprite 64. Any more than this will not be rendered.
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 5)
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 6)
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 7)
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 8)
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE
	;;.db $FE, $06, $00, $FE	 ;;(bullet 9)
	;;.db $FE, $07, $00, $FE	 
	;;.db $FE, $06, $00, $FE
	;;.db $FE, $07, $00, $FE ;>
	lifebar:
	.db $08, $00, $00, $00	;;02E4
	.db $08, $A0, $00, $08	;;02E8
	.db $08, $A0, $00, $10	;;02EC
	.db $FE, $07, $00, $FE ;; placeholders
	.db $FE, $07, $00, $FE
	.db $FE, $07, $00, $FE
	.db $FE, $07, $00, $FE
    ;;.db $88, $35, $00, $88   ;;sprite 3
	;>
  ;>
  columnData:
  .db $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F
  .db $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $1D, $1E, $1F ;>
;>
;>
Vectors:
  .org $FFFA     ;;first of the three vectors starts here
  .dw NMI        ;;when an NMI happens (once per frame if enabled) the 
                   ;;processor will jump to the label NMI
  .dw RESET      ;;when the processor first turns on or is reset, it will jump
                   ;;to the label RESET
  .dw 0          ;;external interrupt IRQ is not used in this tutorial
;>
Includes:
  .incbin "space.chr"   ;;includes 8KB graphics file
;>