org 0x7c00

jmp 0:start

WELCOME_MSG_1: db 'Enter your ', 0x0  ; string ends with newline(0xd, 0xa) and nullem (0x0)
WELCOME_MSG_2: db 'name', 0xd, 0xa, 0x0
HELLO_STR: db 0xd, 0xa, 'Hello ', 0x0
NEW_LINE_STR: db 0xd, 0xa, 0x0
BUFFER times 64 db 0                     ; init 64-bit buffer
NAME times 64 db 0

print_char: ; print letter from al register
    mov ah, 0xe
    int 0x10
    ret

print_string:	; assumes string is in si register
    next_character:	; read letter
        mov al, [si]	; take char from current si reg value
        inc si		; move si reg to next value
        or al, al	; if al is zero - that's string end char, finish
        jz exit_function ; as above
        call print_char ; more chars there to print
        jmp next_character	; and move on
    exit_function:
      ret

sleep:
    mov cx, 001eH
    mov dx, 8480H
    mov ah, 0x86
    int 0x15
    ret

sub_bx: ; substracts 1 from bx if it's > 0
  cmp bx, 0
  je sub_ret
  sub bx, 1
  sub_ret:
  ret

on_enter:
  cmp bx, 3
  jl ignored_input
  jmp print_greeting_and_launch

on_backspace:
    call sub_bx
    mov al, 0x08
    call print_char
    mov al, ' '
    call print_char
    mov al, 0x08
    call print_char
    jmp ignored_input

read:
    mov ah,0h   ;get character from keyboard
    int 16h     ;and store it in AL

    cmp al, 0x0d
    je on_enter

    cmp al, 0x08
    je on_backspace

    cmp bx, 12
    je ignored_input

    mov [NAME+bx], al ; set next name char
    add bx, 1
    call print_char

    ignored_input:
    ret

save_name:
  mov ah, 0x03
  mov al, 0x01
  mov ch, 0x00
  mov cl, 0x03
  mov dh, 0x00
  mov dl, 0x80
  mov bx, NAME
  int 0x13
  ret

print_greeting_and_launch:
  mov si, HELLO_STR
  call print_string
  mov [NAME+bx], word 0x0
  mov si, NAME
  call print_string
  mov si, NEW_LINE_STR
  call print_string
  jmp after_greeting

print_error:
  mov si, HELLO_STR
  call print_string
  ret

copy_bootloader:
  mov ah, 0x02
  mov al, 0x01
  mov ch, 0x00
  mov cl, 0x01
  mov dh, 0x00
  mov dl, 0x80
  mov bx, 0x7e00
  int 0x13
  lea ax, [run_org_bootloader + 0x0200]
  jmp ax

run_org_bootloader: ; After my job just run default bootloader
  mov ah, 0x02
  mov al, 0x01
  mov ch, 0x00
  mov cl, 0x02
  mov dh, 0x00
  mov dl, 0x80
  mov bx, 0x7c00
  int 0x13

  mov ax, 0x7c00
  jmp ax

start:
    mov ax, cs      ; zeroes segment registers
    mov ds, ax
    mov es, ax
    mov bx, ax
    mov ss, ax     ; after this instruction processor blocks interrupts until next statement
    mov sp, 0x8000 ; sp register must be loaded immediately afetr ss

  welcome_msg_print:
    mov si, WELCOME_MSG_1                       ; store buffer address in si
		call print_string
    mov si, WELCOME_MSG_2
    call print_string

  main:
      call read
      jmp main
  after_greeting:
      call sleep
      call save_name
      jmp copy_bootloader

; trick with substraction to keep partition table as it was
;times 440 - ($-$$) db 0 ; Fill with zeros
;dw 0xAA55 ; Add bootloader sign
