//CONNECTIONS
// LCD D0-D7 -> PC0-PC7
// LCD BE-RS -> PA0-PA3
//KEYPAD - PORTD
//Motor - PB7
//External interrupts- buton 1 PE4, button 2 PE5
//LED - PB0 - PB3 
//AUDIO ASD- PIN1, AIN - PB6

.include "m64def.inc"

.def temp = r16
.def temp2 = r17  ;ALSO LCD DATA VALUE
.def count = r18
.def count2 = r19
.def floor = r20
.def status = r21 ;fourth bit(direction bit): 1 - up, 0 - down    
				  ;fifth bit(moving bit): 1 - moving, 0 - not moving
				  ;bit one, two: 0, 0 - door closed  || 1, 0 - door opening
				  ;              0, 1 - door closing || 1, 1 -door opened
.def row =r22   ;ALSO USED AS A COUNT IN EMERGENCY
.def col =r23  
.def mask =r24  ;ALSO USED AS A COUNT IN EMERGENCY

.def emergencyFlash = r25

;Motor Constants
.equ FOURTYRPS = 0x8E ;FOURTY RPS OCR VALUE
.equ SEVENTYRPS = 0xDA ;SEVENTY RPS OCR VALUE
;Speaker constant
.equ SPEAKERSOUND = 0x10

;LCD protocol control bits
.equ LCD_RS = 3
.equ LCD_RW = 1
.equ LCD_E = 2
;LCD functions
.equ LCD_FUNC_SET = 0b00110000
.equ LCD_DISP_OFF = 0b00001000
.equ LCD_DISP_CLR = 0b00000001
.equ LCD_DISP_ON = 0b00001100
.equ LCD_ENTRY_SET = 0b00000100
.equ LCD_ADDR_SET = 0b10000000
;LCD function bits and constants
.equ LCD_BF = 7
.equ LCD_N = 3
.equ LCD_F = 2
.equ LCD_ID = 1
.equ LCD_S = 0
.equ LCD_C = 1
.equ LCD_B = 0
.equ LCD_LINE1 = 0
.equ LCD_LINE2 = 0x40

;KEYPAD CONSTANTS
.equ PORTDDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

;STATUS CONSTANTS
.equ DIRECTIONMASK = 0b00001000
.equ UP = 0b00001000
.equ DOWN = 0b00000000
.equ SETDOWN = 0b11110111
.equ STATEMASK = 0b00000011
.equ CLOSED = 0b00000000
.equ OPENING = 0b00000001
.equ CLOSING = 0b00000010
.equ OPENED = 0b00000011
.equ SETCLOSED = 0b11111100
.equ MOVINGMASK = 0b00010000
.equ MOVING = 0b00010000
.equ NOTMOVING = 0b00000000
.equ SETNOTMOVING = 0b11101111
.equ EMERGENCYMASK = 0b01000000
.equ EMERGENCYSTATE = 0b01000000

;EMERGENCY MESSAGE CONSTANTS
.equ LENGTHONE = 9
.equ LENGTHTWO = 8

.dseg
floorRequests: .byte 10 ;10 byte array for floor requests, 
						; corresponding byte is 0 if floor isnt requested, otherwise it is requested
.cseg
jmp RESET
jmp Default ; IRQ0 Handler
jmp Default ; IRQ1 Handler
jmp Default ; IRQ2 Handler
jmp Default ; IRQ3 Handler
jmp closeButton ; IRQ4 Handler
jmp emergency ; IRQ5 Handler
jmp Default ; IRQ6 Handler
jmp Default ; IRQ7 Handler
jmp Default ; Timer2 Compare Handler
jmp Default ; Timer2 Overflow Handler
jmp Default ; Timer1 Capture Handler
jmp Default ; Timer1 CompareA Handler
jmp Default ; Timer1 CompareB Handler
jmp Default ; Timer1 Overflow Handler
jmp Default ; Timer0 Compare Handler
jmp Timer0  ; Timer0 Overflow Handler

Default: reti

Timer0:    ;Counts for one two and three seconds
push temp  ;Push stack
push temp2
mov temp, status   
andi temp, STATEMASK    ;Set temp to show state
mov temp2, status
andi temp2, EMERGENCYMASK  ;Set temp2 to show emergency state
//Start count
inc count
cpi count, 239
brne timerReturn
clr count
inc count2
//Count one second
cpi count2, 15
breq oneSecondPassed
//Count another second
cpi count2, 30
breq twoSecondPassed
//Count another second
cpi count2, 45
breq threeSecondPassed
jmp timerReturn

oneSecondPassed: ;When one second has passed:
cpi temp2, EMERGENCYSTATE ;If in emergency state, flash
brne continueOne
call flash
rjmp continueOneTwo
continueOne:
out OCR1AL, emergencyFlash
continueOneTwo:
cpi temp, OPENING   ;If elevator was opening, carry out opening state
breq openingState
cpi temp, CLOSING   ;If elevator was closing, carry out closing state
breq closingState
jmp timerReturn
twoSecondPassed:  ;When two seconds have passed
cpi temp2, EMERGENCYSTATE ;If in emergency state, flash
brne continueTwo
call flash
continueTwo:
cpi temp, CLOSED   ;If elevator was closed, carry out closed state
breq closedState
jmp timerReturn
threeSecondPassed:  ;When three seconds have passed
cpi temp2, EMERGENCYSTATE ;If in emergency state, flash
brne continueThree
call flash
continueThree:
cpi temp, OPENED  ;If elevator was opened, carry out opened state
breq openedState
jmp timerReturn

timerReturn:	;Timer return/finish
pop temp2   ;Pop stack
pop temp
reti  ;Finish interrupt routine

//Carry out appropriate action according to state
openingState:  ;Opening State
call holdDoor   ;Hold the door, change to Opened state
rjmp endStateAction
closingState:  ; Closing State
call changeFloor  ;Change the floor, change to Closed state
rjmp endStateAction
closedState:  ;Closed state
cpi temp2, EMERGENCYSTATE ;If emergency state, open the door but without activating speakers
breq emergencyClosedState
call openDoor  ;Otherwise, open door, change to Opened state
rjmp endStateAction 
openedState:  ;Opened state
call closeDoor  ;Close the door, change to closed state
rjmp endStateAction
emergencyClosedState:  ;Open door without activating speakers
call openDoorEmergency
rjmp endEmergencyStateAction ;end without showing status
endStateAction:  ;Finish changing the states
mov temp2, status   
andi temp2, EMERGENCYMASK   ;Check if emergency
cpi temp2, EMERGENCYSTATE
breq endEmergencyStateAction  ;If emergency, dont display status
out PORTB, status   ;Display status
endEmergencyStateAction:
clr count    ;Reset timer
clr count2
jmp timerReturn  ;finish

flash:   ;Flashes the emergency signal
cpi emergencyFlash, 0  ;Toggle flash on and off
breq setFlash
clr emergencyFlash  ;If flash was on, turn off
out OCR1AH, emergencyFlash ;Turn off speaker
out OCR1AL, emergencyFlash
rjmp flashFinish
setFlash:  ;If flash was off, turn on
ldi emergencyFlash, SPEAKERSOUND
out OCR1AL, emergencyFlash
ldi emergencyFlash, 0b00011111
flashFinish:
out PORTB, emergencyFlash   ;Flash lights and speaker
ret  ;Finish

.MACRO delay   ; Delay routine
loop: subi temp, 1
sbci temp2, 0
nop
nop
nop
nop
brne loop ; taken branch takes two cycles.
; one loop time is 8 cycles = ~1.08us
.ENDMACRO

;Function lcd_write_com: Write a command to the LCD. The temp2 reg stores the value to be written.
.MACRO lcd_write_com
out PORTC, temp2 ; set the data port's value up
clr temp
out PORTA, temp ; RS = 0, RW = 0 for a command write
nop ; delay to meet timing (Set up time)
sbi PORTA, LCD_E ; turn on the enable pin
nop ; delay to meet timing (Enable pulse width)
nop
nop
cbi PORTA, LCD_E ; turn off the enable pin
nop ; delay to meet timing (Enable cycle time)
nop
nop

.ENDMACRO
;Function lcd_write_data: Write a character to the LCD. The temp2 reg stores the value to be written.
.MACRO lcd_write_data
out PORTC, temp2 ; set the data port's value up
ldi temp, 1 << LCD_RS
out PORTA, temp ; RS = 1, RW = 0 for a data write
nop ; delay to meet timing (Set up time)
sbi PORTA, LCD_E ; turn on the enable pin
nop ; delay to meet timing (Enable pulse width)
nop
nop
cbi PORTA, LCD_E ; turn off the enable pin
nop ; delay to meet timing (Enable cycle time)
nop
nop

.ENDMACRO
;Function lcd_wait_busy: Read the LCD busy flag until it reads as not busy.
.MACRO lcd_wait_busy
clr temp
out DDRC, temp ; Make PORTC be an input port for now
out PORTC, temp
ldi temp, 1 << LCD_RW
out PORTA, temp ; RS = 0, RW = 1 for a command port read
busy_loop:
nop ; delay to meet timing (Set up time / Enable cycle time)
sbi PORTA, LCD_E ; turn on the enable pin
nop ; delay to meet timing (Data delay time)
nop
nop
in temp, PINC ; read value from LCD
cbi PORTA, LCD_E ; turn off the enable pin
sbrc temp, LCD_BF ; if the busy flag is set
rjmp busy_loop ; repeat command read
clr temp ; else
out PORTA, temp ; turn off read mode,
ser temp
out DDRC, temp ; make PORTC an output port again
 ; and return
.ENDMACRO
; Function delay: Pass a number in registers r18:r19 to indicate how many microseconds
; must be delayed. Actual delay will be slightly greater (~1.08us*r18:r19).
; r18:r19 are altered in this function.
; Code is omitted
;Function lcd_init Initialisation function for LCD.
.MACRO lcd_init
ser temp
out DDRC, temp ; PORTC, the data port is usually all otuputs
out DDRA, temp ; PORTA, the control port is always all outputs
ldi temp, low(15000)
ldi temp2, high(15000)
delay ; delay for > 15ms
; Function set command with N = 1 and F = 0
ldi temp2, LCD_FUNC_SET | (1 << LCD_N)
lcd_write_com ; 1st Function set command with 2 lines and 5*7 font
ldi temp, low(4100)
ldi temp2, high(4100)
delay ; delay for > 4.1ms
ldi temp2, LCD_FUNC_SET | (1 << LCD_N)
lcd_write_com ; 2nd Function set command with 2 lines and 5*7 font
ldi temp, low(100)
ldi temp2, high(100)
delay ; delay for > 100us
ldi temp2, LCD_FUNC_SET | (1 << LCD_N)
lcd_write_com ; 3rd Function set command with 2 lines and 5*7 font
lcd_write_com ; Final Function set command with 2 lines and 5*7 font
lcd_wait_busy ; Wait until the LCD is ready
ldi temp2, LCD_DISP_OFF
lcd_write_com ; Turn Display off
lcd_wait_busy ; Wait until the LCD is ready
ldi temp2, LCD_DISP_CLR
lcd_write_com ; Clear Display
lcd_wait_busy ; Wait until the LCD is ready
; Entry set command with I/D = 1 and S = 0
ldi temp2, LCD_ENTRY_SET | (1 << LCD_ID)
lcd_write_com ; Set Entry mode: Increment = yes and Shift = no
lcd_wait_busy ; Wait until the LCD is ready
; Display on command with C = 0 and B = 1
ldi temp2, LCD_DISP_ON | (1 << LCD_C)
lcd_write_com ; Turn Display on with a cursor that doesn't blink
.ENDMACRO

.MACRO load_floor_temp ;Load a floor request into temp
cpi temp2, 0		   ;Temp2 = floor requested, temp = floor return value
brne nextFloor1			
ldd temp, Z+0			;Manually scans to return an offset from floor 0
rjmp loadFloorEnd
nextFloor1:
cpi temp2, 1
brne nextFloor2
ldd temp, Z+1
rjmp loadFloorEnd
nextFloor2:
cpi temp2, 2
brne nextFloor3
ldd temp, Z+2
rjmp loadFloorEnd
nextFloor3:
cpi temp2, 3
brne nextFloor4
ldd temp, Z+3
rjmp loadFloorEnd
nextFloor4:
cpi temp2, 4
brne nextFloor5
ldd temp, Z+4
rjmp loadFloorEnd
nextFloor5:
cpi temp2, 5
brne nextFloor6
ldd temp, Z+5
rjmp loadFloorEnd
nextFloor6:
cpi temp2, 6
brne nextFloor7
ldd temp, Z+6
rjmp loadFloorEnd
nextFloor7:
cpi temp2, 7
brne nextFloor8
ldd temp, Z+7
rjmp loadFloorEnd
nextFloor8:
cpi temp2, 8
brne nextFloor9
ldd temp, Z+8
rjmp loadFloorEnd
nextFloor9:
cpi temp2, 9
brne loadFloorEnd
ldd temp, Z+9
loadFloorEnd:
.ENDMACRO

.MACRO set_floor_request ;Set a floor request
cpi temp2, 0			 ;Temp2 = floor to be set, temp = value to set floor request to
brne nextFloor11
std Z+0, temp				;Manually scans each floor to store a value to an offset from floor 0
rjmp clearFloorEnd
nextFloor11:
cpi temp2, 1
brne nextFloor21
std Z+1, temp
rjmp clearFloorEnd
nextFloor21:
cpi temp2, 2
brne nextFloor31
std Z+2, temp
rjmp clearFloorEnd
nextFloor31:
cpi temp2, 3
brne nextFloor41
std Z+3, temp
rjmp clearFloorEnd
nextFloor41:
cpi temp2, 4
brne nextFloor51
std Z+4, temp
rjmp clearFloorEnd
nextFloor51:
cpi temp2, 5
brne nextFloor61
std Z+5, temp
rjmp clearFloorEnd
nextFloor61:
cpi temp2, 6
brne nextFloor71
std Z+6, temp
rjmp clearFloorEnd
nextFloor71:
cpi temp2, 7
brne nextFloor81
std Z+7, temp
rjmp clearFloorEnd
nextFloor81:
cpi temp2, 8
brne nextFloor91
std Z+8, temp
rjmp clearFloorEnd
nextFloor91:
cpi temp2, 9
brne clearFloorEnd
std Z+9, temp
clearFloorEnd:
.ENDMACRO

.MACRO check_floors  ;Check all floors, to see if any requests exist
clr temp2				;Returns temp = 1 if a floor request exists
checkFloorLoop:			;Returns temp = 0 if no floor request exists
load_floor_temp			;Iteratively scans all floors to check if any requests exist
cpi temp, 0
brne floorRequestsAvailable
inc temp2
cpi temp2, 10
brne checkFloorLoop
ldi temp, 0
rjmp checkFloorsEnd
floorRequestsAvailable:
ldi temp, 1
checkFloorsEnd:
.ENDMACRO

.MACRO check_floors_direction ;Check if any floors are requested in the current direction
mov temp2, floor				;Returns temp = 1 if a floor is requested in the current direction
mov temp, status				;Returns temp = 0 if a floor is not requested in the current direction
andi temp, DIRECTIONMASK		
cpi temp2, 0					;Iteratively scans up or down from the current floor,
breq firstFloorCheck			;depending on the current direction.
cpi temp2, 9
breq lastFloorCheck
cpi temp, DOWN
breq checkFloorsDown
cpi temp, UP
breq checkFloorsUp
firstFloorCheck:   ;If first floor and direction down, no requests
cpi temp, DOWN
breq jumpNotAvailable
jmp floorDirectionAvailable
lastFloorCheck:    ;If last floor and direction up, no requests
cpi temp, UP
breq jumpNotAvailable
jmp floorDirectionAvailable
jumpNotAvailable: ;Bridge to jump to notAvailable
jmp floorDirectionNotAvailable
CheckFloorsUp:    ;Check floors above current floor
inc temp2
load_floor_temp
cpi temp, 0
brne floorDirectionAvailable
cpi temp2, 9
brne CheckFloorsUp
jmp floorDirectionNotAvailable
CheckFloorsDown:    ;Check floors below current floor
dec temp2
load_floor_temp
cpi temp, 0
brne floorDirectionAvailable
cpi temp2, 0
brne CheckFloorsDown
jmp floorDirectionNotAvailable
floorDirectionAvailable:  ;Floor direction is available, temp = 1
ldi temp, 1
rjmp checkFloorsDirectionEnd
floorDirectionNotAvailable:  ;Floor direction isnt available, temp = 0
ldi temp, 0
checkFloorsDirectionEnd:
.ENDMACRO

RESET:
//Set up everything
//Stack pointer
ldi temp, low(RAMEND)
out SPL, temp
ldi temp, high(RAMEND)
out SPH, temp
//Set up data pointers
ldi ZL, low(floorRequests<<1)  ;Z pointer will be used to refer to floorRequests
ldi ZH, high(floorRequests<<1)
clr temp
clr temp2
initialiseRequests:   ;Sets all floor requests to 0 (not requested)
set_floor_request ;Sets floor-temp2 to request-temp
inc temp2
cpi temp2, 10
brne initialiseRequests
//Set up keypad
ldi temp, PORTDDIR ; columns are outputs, rows are inputs
out DDRD, temp
//Set up LCD
lcd_wait_busy
lcd_init
ldi temp2, '0'
lcd_wait_busy
lcd_write_data
//Set up motor PWM and LED and Speaker PWN
ser temp
out DDRB, temp ; Bit 7 will function as OC2. 6 as OC1
ldi temp, (1<< WGM20)|(1<<COM21)|(1<<CS20) ;Phase correct mode
out TCCR2, temp
ldi temp, (1<<COM1A1) | (1<<WGM10) ;Phase correct mode
out TCCR1A, temp
ldi temp, (1 << CS11)    ;Set up prescaler for speaker sound
out TCCR1B, temp
//Set up external interrupts
ldi temp, (2<<ISC40) | (2<<ISC50)         ;setting the interrupts for falling edge
out EICRB, temp                       ;storing them into EICRB
in temp, EIMSK                        ;taking the values inside the EIMSK  
ori temp, (1<<INT4) | (1<<INT5)            ; oring the values with INT4 and INT5  
out EIMSK, temp                       ; enabling interrput4 and interrupt5
//Set up registers to wait for button press
clr emergencyFlash
clr count
clr count2
clr floor
ldi status, 0b00001000  ;Set status to Up, Not Moving and Closed.
out PORTB, status ;Show status on LED's
//Set up Speaker
//Set up Timer
ldi temp, 0b00000010     ; 
out TCCR0, temp          ; Prescaling value=8  ;256*8/7.3728( Frequency of the clock 7.3728MHz, for the overflow it should go for 256 times)
ldi temp, 1<<TOIE0       ; =278 microseconds
out TIMSK, temp          ; T/C0 interrupt enable
sei  ;Enable interrupts
main:
//Scan keypad
//Count seconds to action in background
ldi mask, INITCOLMASK
clr col 
colloop:  ;Scans the columns
out PORTD, mask
ldi temp, 0xFF 
keypadDelay:  ;a delay so the hardware can stabilize
dec temp
brne keypadDelay
in temp, PIND      
andi temp, ROWMASK ;Read rows in column
cpi temp, 0xF      ;Check if any rows active
breq nextcol       ;If none, change column
ldi mask, INITROWMASK 
clr row 
rowloop:    ;Scan rows to find which row was pressed
mov temp2, temp  ;Check row bit
and temp2, mask 
brne skipconv 

rcall convert 
jmp main 
skipconv:  ;Increment row bit to check next row
inc row 
lsl mask 
jmp rowloop
nextcol:   ;Change to check next column
cpi col, 3   ;If last column, restart
breq main 
sec 
rol mask 
inc col 
jmp colloop 

convert:   ;Convert keypad input and carry out according action
cpi col, 3  ;Column 3 = letters
breq letters
cpi row, 3   ;Row 3 = symbols or zero
breq symbols
number:  ;Otherwise a number
in temp2, PIND        ;Convert row and column to corresponding number
andi temp2, ROWMASK
and temp2, mask
breq number
mov temp, row
lsl temp 
add temp, row
add temp, col
inc temp            ;number = row*3 + col + 1
//Add floor request corresponding to input
mov temp2, temp
ser temp
set_floor_request
jmp convert_end
letters: ;Letters do nothing
jmp convert_end
symbols: 
cpi col, 0  ;Col 0 means star
breq star
cpi col, 1  ;Col 1 means zero
breq zero
hash:  ;Otherwise a hash
jmp convert_end
star:  ;Star will operate as an open button
call openButton   ;Activate open button
jmp convert_end
zero: ;Add floor request to floor zero
ser temp
st Z, temp
convert_end:
ret ; return to caller

changeFloor:
//set status to closed
ldi temp, 0  ;Stop motor
out OCR2, temp
andi status, SETCLOSED  ;Set status to closed
//check if no floor requests, if so set status to not moving and return
check_floors
cpi temp, 0
brne checkCurrentLevel		;Otherwise, if floor requests exist, check
andi status, SETNOTMOVING
jmp endChangeFloor
checkCurrentLevel:   ;If current level has been requested again, re open door
mov temp2, floor
load_floor_temp   ;check current floor
cpi temp, 0
breq checkLastLevel  ;Last level of checking if current floor isnt requested
call openDoorEmergency		;Otherwise open door
jmp endChangeFloor  ;End process
checkLastLevel:
//check if no more floor requests in direction
check_floors_direction
cpi temp, 0
brne checkDirection ;If floor requests do exist in the curent direction, check and change floor
//Change direction bit
changeDirection:  ;Otherwise, change the direction
mov temp, status
andi temp, DIRECTIONMASK	;Read current direction
cpi temp, UP
breq changeToDown  ;If up, change to down
cpi temp, DOWN
breq changeToUp    ;If down, change to up
changeToDown:
andi status, SETDOWN  ;Change direction to down
rjmp changeFloorLevelDown  ;Change floor level down
changeToUp:
ori status, UP   ;Change direction to up
rjmp changeFloorLevelUp  ;Change floor level up
checkDirection:  ;Check direction and change floor
mov temp, status
andi temp, DIRECTIONMASK ;Read direction
cpi temp, UP   ;If up, change floor level up
breq changeFloorLevelUp
cpi temp, DOWN  ;If down, change floor level down
breq changeFloorLevelDown
changeFloorLevelUp: ;Increment floor level
inc floor
rjmp changeFloorFinish
changeFloorLevelDown:  ;Decrement floor level
dec floor
changeFloorFinish:  ;Finish changing floor
mov temp, status
andi temp, MOVINGMASK	;Check if elevator was moving
cpi temp, MOVING
breq endChangeFloor    ;If so return
mov temp, status
andi temp, EMERGENCYMASK   ;Check if emergency state
cpi temp, EMERGENCYSTATE  
breq setToMoving           ;If not set to moving and return
ldi temp, SPEAKERSOUND	   ;Otherwise Sound Speaker to signal departure of floor
out OCR1AL, temp
setToMoving:
//set status to moving
ori status, MOVING
endChangeFloor:
//return
ret


openDoor:
//display floor level
lcd_wait_busy
ldi temp2, LCD_DISP_CLR
lcd_write_com
lcd_wait_busy
mov temp2, floor	;convert floor level to ascii
ldi temp, '0'
add temp2, temp
lcd_write_data
//If floor is requested, open door
mov temp2, floor
load_floor_temp
cpi temp, 0
breq notRequestedBridge  ;Otherwise call change floor
mov temp, status		;Check if elevator was moving
andi temp, MOVINGMASK
cpi temp, NOTMOVING
breq setToOpening		;If it was moving, sound speaker, otherwise continue
ldi temp, SPEAKERSOUND	;Sound speaker
out OCR1AL, temp
rjmp setToOpening		;Continue
openDoorEmergency:
//Check if floor is requested, if so open door. However, speaker cannot be sounded
mov temp2, floor
load_floor_temp
cpi temp, 0
breq notRequested
notRequestedBridge: ;Bridge to get to not Requested
breq notRequested
setToOpening:  ;Sets status to opening, and clears floor request
//Set status to opening and not moving
andi status, SETNOTMOVING
andi status, SETCLOSED
ori status, OPENING
//Unset floor request
mov temp2, floor
clr temp
set_floor_request
//Spin motor at 70 rps
ldi temp, SEVENTYRPS
out OCR2, temp
rjmp endOpenDoor
notRequested:
//Call change floor
call changeFloor
endOpenDoor:
//return
ret

closeDoor:
//set status to closing
andi status, SETCLOSED
ori status, CLOSING
//Spin motor at 40 rps
ldi temp, FOURTYRPS
out OCR2, temp
//return
ret

holdDoor:
//set status to opened
ori status, OPENED
//Stop motor
clr temp
out OCR2, temp
//return
ret

closeButton:
//Debounce for 10ms
ldi temp, low(10000)
ldi temp2, high(10000)
delay

push temp  ;Push stack
push temp2
//Check conditions - (Opened)
mov temp, status
andi temp, STATEMASK
cpi temp, OPENED
brne endCloseButton		;if opened, close door, otherwise end
//Close Door
call closeDoor  ;Close the door
clr count ;Reset timer
clr count2
endCloseButton:
pop temp2  ;Pop stack
pop temp
//display status on LED
out PORTB, status
//return
reti

openButton:
cli   ;Clear interrupts to allow hold
//If not moving AND opened, hold door open until button is released
mov temp, status
andi temp, MOVINGMASK ;Check if not moving and opened
cpi temp, MOVING	.
breq endOpenButton		;If moving, end
mov temp, status
andi temp, STATEMASK
cpi temp, OPENED
brne nextOpenButtonCheck  ;If not opened, check next conditions
holdButton:    ;Otherwise, hold door opened
in temp2, PIND 
andi temp2, ROWMASK		;Loops until button is released
and temp2, mask			
breq holdButton
rjmp endOpenButton   ;End open button
nextOpenButtonCheck:
//Else if not moving AND closing, or not moving AND closed
cpi temp, 0b00000000
breq openDoorAgain   ;If closed, open door
cpi temp, 0b00000010
breq openDoorAgain   ;If closing, open door
rjmp endOpenButton   ;Otherwise end
openDoorAgain:
//spin motor ar 70 rps and reset counts
ldi temp, SEVENTYRPS
out OCR2, temp
//set status to opening
andi status, SETCLOSED
ori status, OPENING
clr count
clr count2
endOpenButton:
out PORTB, status  ;Display status
sei   ;Enable interrupts
//return
ret

emergency:
/*Debounce for 10ms
ldi temp, low(10000)
ldi temp2, high(10000)
delay*/
clr temp
out EIMSK, temp			;Cancel external interrupts
//Show emergency message
lcd_wait_busy
ldi temp2, LCD_DISP_CLR
lcd_write_com
ldi ZL, low(stringOne<<1)        ; point Z at the first string
ldi ZH, high(stringOne<<1)      
ldi count, LENGTHONE               ; initialise counter 
stringOneLoop: 
lpm temp2, Z+                    ; read a character from the string 
lcd_wait_busy
lcd_write_data            ; write the character to the screen
dec count                       ; decrement character counter
cpi count, 0
brne stringOneLoop                  ; loop again if there are more characters

lcd_wait_busy
ldi temp2, LCD_ADDR_SET | LCD_LINE2
lcd_write_com                     ; move the insertion point to start of line 2

ldi ZL, low(stringTwo<<1)        ; point Z at the second string
ldi ZH, high(stringTwo<<1)      
ldi count, LENGTHTWO               ; initialise counter 
stringTwoLoop: 
lpm temp2, Z+                    ; read a character from the string 
lcd_wait_busy
lcd_write_data            ; write the character to the screen
dec count                       ; decrement character counter
cpi count, 0
brne stringTwoLoop                  ; loop again if there are more characters

ldi temp, (1 << CS11) | (1 << CS10)  ;Change speaker sound by changing prescaler
out TCCR1B, temp

ori status, EMERGENCYSTATE   ;Activate emergency state
clr mask  ;Mask will be used a count, it is no longer needed as main is not running
clr row		;As above

call flash ;Flash signal
//set up conditions
emergencyInitialise:  ;Sets zero floor to requested, cancel all other requests
ldi ZL, low(floorRequests<<1)  
ldi ZH, high(floorRequests<<1)
clr count
ser temp
st Z+, temp   ;Set zero floor to requested
clr temp
emergencyLoop:  ;Set remaining requests to not requested
st Z+, temp
inc count
cpi count, 9
brne emergencyLoop
ldi ZL, low(floorRequests<<1)  ;Reallocate pointers
ldi ZH, high(floorRequests<<1)
//Close door if not closing or closed
closeEmergencyDoor:
mov temp, status
andi temp, STATEMASK	;Check state
cpi temp, CLOSED		
breq goToZero			;If closed skip
cpi temp, CLOSING
breq goToZero			;If closing skip
call closeDoor			;Close door
clr count				;Reset timer
clr count2
goToZero:			;Enable interrupts, wait until floor zero
sei
zeroLoopCount:  ;Ensures status is opened for a small fraction time
inc row			;Inc row when status is opened
cpi row, 20		;when row is 20 continue
brsh holdUntilFinished
zeroLoop:			;Wait until status is Opened
mov temp, status
andi temp, STATEMASK	;read status
cpi temp, OPENED		;if opened increment row
brne zeroLoop			;Otherwise loop
rjmp zeroLoopCount
holdUntilFinishedCount:   ;Ensures status is closed for a small fraction time
inc mask					;inc mask when status is closed
cpi mask, 20				;when mask is 20 finish
brsh finishEmergency
holdUntilFinished:   ;Wait until status is closed
mov temp, status
andi temp, STATEMASK	;read status
cpi temp, CLOSED		;if closed, increment mask
brne holdUntilFinished	;otherwise loop
rjmp holdUntilFinishedCount
finishEmergency:	;Infinite loop
rjmp finishEmergency


stringOne: .db "Emergency   "	;Emergency Strings
stringTwo : .db "Call 000    "
