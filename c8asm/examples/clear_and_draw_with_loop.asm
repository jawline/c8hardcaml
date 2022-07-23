cls

ldi 0, 2
ldi 1, 0
ldi 2, 0

jmp loop

exit:
  jmp exit

draw_next_character:
  set-digit 2
  draw 0, 1, 5
  addi 0, 5
  addi 2, 1
  ret

next_row:
  ldi 0, 2
  addi 1, 5
  ldi 2, 0
  skip-neq-i 1, 30
  jmp exit

loop:
  call draw_next_character
  skip-neq-i 0, 62
  jmp next_row
  jmp loop
