cls

ldi 0, 20
ldi 1, 20
ldi 2, 0
set-digit 2
draw 0, 1, 5

loop:
  jmp loop
