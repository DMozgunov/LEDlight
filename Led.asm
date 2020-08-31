#include <p16F887.inc>
__CONFIG    _CONFIG1, _LVP_OFF & _FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
;__CONFIG    _CONFIG1,  _LVP_OFF &_FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT

__CONFIG    _CONFIG2, _WRT_OFF & _BOR21V

; Define const
    CCP1                        EQU 2           ; RC2
    IsLongPush                  EQU 7           ; 7th bit of ButtonState
    Selected                    EQU 7           ; 7th bit of CurrentMode
    StableStateCounterMAX       EQU 0x0A        ; For button debounce - count state in a row
    OneShortHumanPushCounterMAX EQU 0x07        ; Count state to get one Human button push
    OneLongHumanPushCounter     EQU 0xF8        ; Add to counter to get Carry bit for a long Human push
    UserCommandSelectionDelay   EQU 0x5F        ; Wait this number of counts to give user time for choise
    PatternMaskInitial          EQU 0x01
    BrightnessMaxSteps          EQU 0x06        ; Brughtness table length

    FALSE	                    EQU 0
    TRUE	                    EQU 1

    OFF_MODE                    EQU b'00000000'
    LIGHT_ON                    EQU b'00000001'
    STROBE_ON                   EQU b'00000010'
    SOS_ON                      EQU b'00000011'
    RED_ON                      EQU b'00000100'


    Debug                       EQU TRUE
    ;Debug                       EQU FALSE

    WithRedLight                EQU FALSE
    ;WithRedLight                EQU TRUE


cblock 0x20

    BightnessSelection               ; Current LED brightness
    CurrentMode             ;
    ButtonState             ;
    StrobeMode              ;

    ; button debounce
    StableStateCounter
    ButtonPushCounterTMP
    ShortPushCounter        ; count of debounced pushes to be a Short human button push
    ShortPushCounterNumber  ; count Number of a Short human button pushes
    UserCommandSelectionCounter 
   
    ; strobe routine

    Pattern0
    Pattern1
    Pattern2

    Delay0
    Delay1
    Delay2

    PatternMask

    ; Interrupr service
	W_Save
	STATUS_Save
    PCLATH_Save

    ; Other
    Delay               ; 

    temp

endc
     

;===============================================================================
; executable CODE goes here
;===============================================================================
RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    MAIN_PROGRAM_CONFIG     ; go to beginning of program


; -----------------------------------------------------------------------
; *		Interrupts
; -----------------------------------------------------------------------

ISR       CODE    0x0004	    ; Interrupts

    movwf       W_Save		; Save context
    movf        STATUS,w
    movwf       STATUS_Save
    movf        PCLATH,w
    movwf       PCLATH_Save


    bcf         STATUS,RP0		


; *******************************************************************
; *		lightness in PWM %
; *******************************************************************
; PWM
	btfsc		PIR1,TMR2IF		    ; PWM timer overflow
    goto        ServiceTimer2
    goto        ServiceTimer1

ServiceTimer2:
	bcf			PIR1,TMR2IF		    ; clear timer2

    movf        CurrentMode,w       ; If current mode is 0x00 then PWM duty should not be changed
    btfsc       STATUS,Z
    goto        ServiceTimer1

    movfw       BightnessSelection            ; 
    call        SET_PWM_by_lookupTable        ; 
    movwf       CCPR1L                        ; 



;PWM - end


; *******************************************************************
; *		STROBE/SOS pattern processing routine
; *******************************************************************
; 
ServiceTimer1:

	btfsc		PIR1,TMR1IF		    ; PWM timer overflow
    goto        STROBE_SOS_Pattern
    goto        ExitISR

STROBE_SOS_Pattern:

; Timer1 int is used for delay
	bcf			PIR1,TMR1IF		    ; clear

    ; Check for current mode - if not Strobe/sos - exit (this should not happen - error)
    ;STROBE_ON                   EQU b'00000010'
    ;SOS_ON                      EQU b'00000011'

    btfss       CurrentMode,1                
    goto        ExitISR

    ; Check pattern and turn LED on or off
    movf        PatternMask,w
    andwf       Pattern0,w

    btfsc       STATUS,Z
    goto        SetCurrentLEDStepOFF    ;must be off
    goto        SetCurrentLEDStepON     ;must be on

SetCurrentLEDStepOFF:
    clrf		CCP1CON			    ; disable PWM


    goto Prepare_for_next_check

SetCurrentLEDStepON:
   	movlw		b'00101100'	  	    ;
	movwf		CCP1CON			    ; enable PWM 

Prepare_for_next_check:
    rlf         PatternMask,f

    btfss       STATUS,C            ; overflow of PatternMask, must be set to default
    ; implement cyclic pattern addr change here
    goto        $+3
    movlw       PatternMaskInitial
    movwf       PatternMask
    nop

    ;debug
    ;movf        PatternMask,w
    ;movwf       PORTD               ; 
    ;debug

; STROBE/SOS pattern processing routine END

ExitISR
    movf        STATUS_Save,w       ; Restore context
    movwf       STATUS
    movf        PCLATH_Save,w       
    movwf       PCLATH
 
    swapf       W_Save,f            ; swapf doesn't affect Status bits, but MOVF would
    swapf       W_Save,w


    retfie

; -----------------------------------------------------------------------
; 		Registers config
; -----------------------------------------------------------------------

MAIN_PROG CODE                   

MAIN_PROGRAM_CONFIG:



    bsf         STATUS,RP0     

    bsf         INTCON,GIE      ; global interrupts enable
    bsf         INTCON,PEIE

    bsf         PIE1, TMR1IE	; Timer1 overflow interrupt enable
    bsf         PIE1, TMR2IE	; Timer2 overflow interrupt enable

; PORT B
    movlw		b'00011111'
    movwf   	TRISB           ; RB0, RB1, RB2, RB3, RB4 - входы, кнопки

; PORT C
    movlw		b'10000000'
    movwf   	TRISC           ; RC7 - in - Rx, RC6 - out - Tx,  
								; RC1 and RC2 - PWM out

; PORT D 
    clrf        TRISD           ; Make PortD all output
   
; PWM config
    movlw	   	0x65			; PWM frequency 0,6 √ц
    movwf	   	PR2

; Outputs
    bsf			STATUS,RP1		; 
    clrf  		ANSELH          ; all the inputs are digital
 
    bcf			STATUS,RP0		; 
    
    bcf         CM1CON0,C1ON    ; disable comparators
    bcf         CM2CON0,C2ON

    bcf			STATUS,RP1
 
    ; remove 'trash' data from port output
    clrf        PORTA
    clrf        PORTB
    clrf        PORTC
    clrf        PORTD

; PWM config
 	clrf		CCP2CON			; PWM off 
 	clrf		CCP1CON			; PWM off
	clrf		TMR2			; 
	clrf		CCPR2L			; duty = 0
	clrf		CCPR1L			; duty = 0
	   	  
	bcf			PIR1,TMR2IF		; timer2 interrupt on
	
	clrf		T2CON		  
	bsf			T2CON,T2CKPS1	; prescaler = 16
	bsf			T2CON,TMR2ON	; activate timer

; Set default values

    movlw		b'00110000'     ; Timer1 config
	movwf		T1CON           ; Timer1 prescaler 1:8 and on

    call        TURN_OFF        ; initial state

    movlw       b'00101001'
    movwf       Pattern0

    movlw       b'10001111'
    movwf       Delay0

    movlw       PatternMaskInitial
    movwf       PatternMask

	bcf		    PIR1,TMR1IF		; timer1 interrupt on

; -----------------------------------------------------------------------
; 		Main program routine
; -----------------------------------------------------------------------
 
MAIN_PROGRAM_LOOP:

    ;debug
    movf        CurrentMode,w
    movwf       PORTD               ; 
    ;debug


; Button debounce and push time count
CHECK_BUTTON:
    
    clrf        ShortPushCounterNumber      ; Number of time periods when button was pushed and which counts as a short human push

    movlw       OneShortHumanPushCounterMAX
    movwf       ShortPushCounter

LookForOneStableState:
	movlw		StableStateCounterMAX		; we need to see this many stable 'button is pressed' states for debounce
	movwf		StableStateCounter
    clrf        ButtonPushCounterTMP

ButtonDebounce:		
    clrw                                    ; 
    btfss       PORTB,0                     ; wait for switch to go low
    incf        ButtonPushCounterTMP,w      ; if it's low, bump the stable state counter
    movwf       ButtonPushCounterTMP        ; 

    call		DELAY_1mS                   ; wait before next stable state check

	decfsz		StableStateCounter,f	    ; try it again
	goto		ButtonDebounce

	movf        ButtonPushCounterTMP,w      ; have we seen some stable state in a row?
    xorlw       StableStateCounterMAX
    btfss       STATUS,Z     
	goto		ButtonNotPushed

ButtonPushed:
    ; the way we count one Human 'button is pressed' state 
    ; we count all short Human pushes
    decfsz      ShortPushCounter,f
    goto        LookForOneStableState

    movlw       OneShortHumanPushCounterMAX
    movwf       ShortPushCounter

    incf        ShortPushCounterNumber,f

    btfsc       STATUS,C
    decf        ShortPushCounterNumber,f
    bcf         STATUS,C

    goto        LookForOneStableState    
    

ButtonNotPushed:
    ; also process state when button was pushed and released
    ; if it was not at all, ShortPushCounterNumber is 0x00
    movf        ShortPushCounterNumber,w 
    btfsc       STATUS,Z 
    goto        CHECK_BUTTON_END           

    ; proceed here if button was pushed and released
    movlw       OneLongHumanPushCounter        
    addwf       ShortPushCounterNumber,w

    bcf         ButtonState, IsLongPush                 ; Test for Long push 
    btfsc       STATUS,C
    bsf         ButtonState, IsLongPush            

    movlw       UserCommandSelectionDelay               ; Reset user choise time counter - delay before choise is processed
    movwf       UserCommandSelectionCounter

    incf        ButtonState
    ;movfw       ButtonState
    
    ;debug
    ;movwf       PORTD  

CHECK_BUTTON_END:


    ; next step is to decode user command
USER_CHOISE_SELECTION:

    ; delay for user to make a mode choise
    decf        UserCommandSelectionCounter   

    ; check if current mode is selected
    btfsc       CurrentMode,Selected
    goto        CheckLongPush

    movf        CurrentMode,w 
    btfsc       STATUS,Z                    ; 
    goto        CheckLongPush
        
    movf        UserCommandSelectionCounter,w
    btfsc       STATUS,Z
    bsf         CurrentMode,Selected

CheckLongPush:
    ; long push must be processed despite UserCommandSelectionCounter
    btfss       ButtonState, IsLongPush 
    goto        UserShortPush                         ; not a long push

    movf        CurrentMode,w                         ; if CurrentMode 0x00 then turn on red light or SOS(from power off)
    btfsc       STATUS,Z                              ; if 
    goto        TurnOnWithLongPush

TurnOFF:
    call        TURN_OFF

    goto        USER_CHOISE_SELECTION_END

TurnOnWithLongPush:

;    SOS_ON                      EQU b'00000011'
;    RED_ON                      EQU b'00000100'

    if ( WithRedLight )             ; either red light or SOS on long push from OFF state
        movlw   RED_ON
    else
        movlw   SOS_ON
    endif
 
    movwf       CurrentMode

    clrf        ButtonState                  ; processed - Mode changed

    bsf         CurrentMode,Selected


    ; if it is SOS -then max brightness
    if ( WithRedLight )             ; either red light or SOS on long push from OFF state
        
        movlw       0x01
        movwf       BightnessSelection
        
    else

        movlw       BrightnessMaxSteps
        movwf       BightnessSelection

        call        SET_PWM_by_lookupTable        ;  
        movwf       CCPR1L                        ; 

        btfsc       CurrentMode,1      ; Enable Timer1 for LED in SOS or Strobe mode
        bsf         T1CON,TMR1ON

    endif

    goto        USER_CHOISE_SELECTION_END

UserShortPush:
    ; process short counts
    movf        ButtonState,w 
    btfsc       STATUS,Z                    ; if ButtonState is 0x00 then no action
    goto        USER_CHOISE_SELECTION_END    

    clrf        ButtonState                 ; processed - Mode changed
    
    btfsc       CurrentMode,Selected         
    goto        BRIGHTNESS_OR_PATTERN_CHANGE

    incf        CurrentMode                 ; select next mode

    ;OFF_MODE                    EQU b'00000000'
    ;LIGHT_ON                    EQU b'00000001'
    ;STROBE_ON                   EQU b'00000010'
    ;SOS_ON                      EQU b'00000011'
    ;RED_ON                      EQU b'00000100'

    movf        CurrentMode,w

    if ( WithRedLight )             ; check if user wants to Turn OFF
       xorlw    b'00000101'
    else
       xorlw    b'00000100' 
    endif

    btfsc       STATUS,Z           ; Menu is cyclic - after last position turn OFF
    goto        TurnOFF
    
    btfsc       CurrentMode,1      ; Enable Timer1 for LED in SOS or Strobe mode
    bsf         T1CON,TMR1ON


    ; for SOS mode set brightest value, and least for others
    movf        CurrentMode,w                  
    
    xorlw       SOS_ON
    btfsc       STATUS,Z
    goto        $+3
    movlw       0x01
    goto        $+2
    movlw       BrightnessMaxSteps
    nop

    movwf       BightnessSelection      ; set up current brightness anyway

    call        SET_PWM_by_lookupTable        ;  
    movwf       CCPR1L                        ; 

    btfsc       CurrentMode,1      ; do not turn LED in SOS or Strobe mode - it should happen in ISR
    goto        USER_CHOISE_SELECTION_END 

	movlw		b'00101100'	  	;
	movwf		CCP1CON			; activate PWM 


USER_CHOISE_SELECTION_END:


    goto        MAIN_PROGRAM_LOOP


BRIGHTNESS_OR_PATTERN_CHANGE:
    ; button has been pushed while mode is already selected
    ; change mode settings

    ; make no change for red led

    btfsc       CurrentMode,2                  ; Red Light - no change at all
    goto        BRIGHTNESS_OR_PATTERN_CHANGE_END

    ; for SOS mode let's start from brightest setting and decrease
    ;SOS_ON                      EQU b'00000011'
    movf        CurrentMode,w                  ; SOS mode and 'selected' bit
    
    xorlw       b'10000011'
    btfsc       STATUS,Z
    goto        SOSReverseBrightnessChange

    ; default behavior with brightness increasing in circle from less to most bright setting
    incf        BightnessSelection,f
    movf        BightnessSelection,w
    xorlw       0x07
    btfsc       STATUS,Z
    clrf        BightnessSelection
    goto        ApplyBrightness

SOSReverseBrightnessChange

    movlw       BrightnessMaxSteps
    decf        BightnessSelection,f
    btfsc       STATUS,Z
    movwf       BightnessSelection


ApplyBrightness:
    movfw       BightnessSelection            ; 
    call        SET_PWM_by_lookupTable        ; 
    movwf       CCPR1L                        ; 

    goto        BRIGHTNESS_OR_PATTERN_CHANGE_END


    ;debug
;    movwf       PORTD                        ; 

;    btfss       CurrentMode, 4
;    goto        $+3
;    bcf         CurrentMode, 4
;    goto        $+2
;    bsf         CurrentMode, 4
;    nop
    ;debug




BRIGHTNESS_OR_PATTERN_CHANGE_END:

    goto        MAIN_PROGRAM_LOOP



; -----------------------------------------------------------------------
; *		Subroutines
; -----------------------------------------------------------------------


TURN_OFF:
    clrf      CurrentMode                   ;
    clrf      ButtonState                   ;
    clrf      StrobeMode                    ;
    clrf      BightnessSelection            ; 
    clrf      UserCommandSelectionCounter   ;

    ;set least brightness available
    clrw                                    ;
    clrf        CCPR1L                        ; 

 	clrf		CCP2CON			; PWM off 
 	clrf		CCP1CON			; PWM off
	clrf		TMR2			; 
	clrf		CCPR2L			; duty = 0
	clrf		CCPR1L			; duty = 0
	   	  
    clrf        PORTC           ; when PWM is off it can turn LED on as digital out

	bcf         T1CON,TMR1ON
    ;debug
    clrf        PORTD
    ;debug
    

    return


; *******************************************************************
; *		Delay  1ms
; *******************************************************************
DELAY_1mS:
     movlw     .71                 ; delay ~1000uS
     movwf     Delay
     decfsz    Delay,f             ; this loop does 215 cycles
     goto      $-1          
     decfsz    Delay,f             ; This loop does 786 cycles
     goto      $-1;
;DELAY_1mS - end
    return

     goto      MAIN_PROGRAM_LOOP


; *******************************************************************
; *		lightness in PWM %
; *******************************************************************

     org     0xf7                  ; force table to cross a 256 instruction boundary
SET_PWM_by_lookupTable:
     andlw     0x07                ; mask off invalid entries
     movwf     temp
     movlw     high TableStart     ; get high order part of the beginning of the table
     movwf     PCLATH
     movlw     low TableStart      ; load starting address of table
     addwf     temp,w              ; add offset
     btfsc     STATUS,C            ; did it overflow?
     incf      PCLATH,f            ; yes: increment PCLATH
     movwf     PCL                 ; modify PCL

TableStart:
     retlw     b'00000011'             ; 0
     retlw     b'00000111'             ; 1
     retlw     b'00001011'             ; 2
     retlw     b'00010011'             ; 3
     retlw     b'00100011'             ; 4
     retlw     b'01001111'             ; 5
     retlw     b'10100011'             ; 6
; SET_PWM_by_lookupTable_END

     
; END CODE
     end
     
