cls

ldi 0, 2
ldi 1, 0
ldi 2, 0

jmp loop

next_row:
  ldi 0, 2
  addi 1, 5
  ldi 2, 0

loop:
  set-digit 2
  draw 0, 1, 5
  addi 0, 5
  addi 2, 1
  
  skip-neq-i 0, 62
  jmp next_row

  jmp loop
