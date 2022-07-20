cls

jmp start

first_call:
  ldi 0, 0
  ldi 1, 0
  ldi 2, 1
  set-digit 2
  draw 0, 1, 5
  call second_call
  ret

second_call: 
  ldi 0, 10
  ldi 1, 5
  ldi 2, 2
  set-digit 2
  draw 0, 1, 5
  call third_call
  ret

third_call: 
  ldi 0, 20
  ldi 1, 10
  ldi 2, 3
  set-digit 2
  draw 0, 1, 5
  ret


start:
  call first_call
  ldi 0, 30
  ldi 1, 15
  ldi 2, 4
  set-digit 2
  draw 0, 1, 5

loop:
  jmp loop
