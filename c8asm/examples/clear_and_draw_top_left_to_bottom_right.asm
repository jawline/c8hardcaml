cls

ldi 0, 2
ldi 1, 2
ldi 2, 0

jmp loop

draw_next_character:
  set-digit 2
  draw 0, 1, 5
  addi 0, 8
  addi 1, 3
  addi 2, 1
  ret

finished:
  jmp finished

loop:
  call draw_next_character
  skip-neq-i 1, 32
  jmp finished
  jmp loop
