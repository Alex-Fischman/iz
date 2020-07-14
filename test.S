.global main
main:
push %rbp
mov %rsp, %rbp
jmp jump_printnum
printnum:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
.data
numfmt: .asciz "%d "
.text
mov 16(%rbp), %rsi
lea numfmt(%rip), %rdi
mov $0, %rax
call printf
mov %rbp, %rsp
pop %rbp
ret
jump_printnum:
lea printnum(%rip), %rax
jmp jump_return
return:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
mov %rbp, %rsp
pop %rbp
mov %rbp, %rsp
pop %rbp
ret
jump_return:
lea return(%rip), %rax
jmp jump_add
add:
push %rbp
mov %rsp, %rbp
mov 24(%rbp), %rax
mov %rax, %rcx
mov 16(%rbp), %rax
add %rcx, %rax
mov %rbp, %rsp
pop %rbp
ret
jump_add:
lea add(%rip), %rax
jmp jump_sub
sub:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
mov %rax, %rcx
mov 24(%rbp), %rax
sub %rcx, %rax
mov %rbp, %rsp
pop %rbp
ret
jump_sub:
lea sub(%rip), %rax
jmp jump_mul
mul:
push %rbp
mov %rsp, %rbp
mov 24(%rbp), %rax
mov %rax, %rcx
mov 16(%rbp), %rax
imul %rcx, %rax
mov %rbp, %rsp
pop %rbp
ret
jump_mul:
lea mul(%rip), %rax
jmp jump_div
div:
push %rbp
mov %rsp, %rbp
mov 24(%rbp), %rax
push %rax
mov 16(%rbp), %rax
mov %rax, %rcx
pop %rax
cdq
idiv %rcx
mov %rbp, %rsp
pop %rbp
ret
jump_div:
lea div(%rip), %rax
mov $3, %rax
push %rax
mov $2, %rax
push %rax
call sub
add $16, %rsp
push %rax
mov $4, %rax
push %rax
mov $2, %rax
push %rax
call div
add $16, %rsp
push %rax
mov $3, %rax
push %rax
call mul
add $16, %rsp
push %rax
mov $5, %rax
push %rax
call sub
add $16, %rsp
push %rax
mov $3, %rax
push %rax
mov $2, %rax
push %rax
call mul
add $16, %rsp
push %rax
call add
add $16, %rsp
push %rax
call add
add $16, %rsp
push %rax
mov -8(%rbp), %rax
push %rax
mov $3, %rax
push %rax
call mul
add $16, %rsp
push %rax
mov $48, %rax
push %rax
mov -16(%rbp), %rax
push %rax
call mul
add $16, %rsp
mov %rax, -16(%rbp)
mov $5, %rax
push %rax
jmp jump_func
func:
push %rbp
mov %rsp, %rbp
mov 24(%rbp), %rax
push %rax
mov 16(%rbp), %rax
push %rax
call mul
add $16, %rsp
push %rax
mov -8(%rbp), %rax
push %rax
call printnum
add $8, %rsp
mov $1, %rax
push %rax
mov -8(%rbp), %rax
push %rax
call add
add $16, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_func:
lea func(%rip), %rax
jmp jump_applyTwice
applyTwice:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
push %rax
call *24(%rbp)
add $8, %rsp
push %rax
call *24(%rbp)
add $8, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_applyTwice:
lea applyTwice(%rip), %rax
jmp jump_square
square:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
push %rax
mov 16(%rbp), %rax
push %rax
call mul
add $16, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_square:
lea square(%rip), %rax
jmp jump_print10
print10:
push %rbp
mov %rsp, %rbp
mov $10, %rax
push %rax
call printnum
add $8, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_print10:
lea print10(%rip), %rax
jmp jump_incr
incr:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
push %rax
mov $1, %rax
push %rax
call add
add $16, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_incr:
lea incr(%rip), %rax
mov -24(%rbp), %rax
push %rax
call printnum
add $8, %rsp
mov $2, %rax
push %rax
mov $3, %rax
push %rax
call func
add $16, %rsp
push %rax
call printnum
add $8, %rsp
jmp jump_f0
f0:
push %rbp
mov %rsp, %rbp
mov 16(%rbp), %rax
push %rax
mov $3, %rax
push %rax
call add
add $16, %rsp
mov %rbp, %rsp
pop %rbp
ret
jump_f0:
lea f0(%rip), %rax
push %rax
mov $2, %rax
push %rax
call applyTwice
add $16, %rsp
push %rax
call printnum
add $8, %rsp
mov $3, %rax
push %rax
call square
add $8, %rsp
push %rax
call printnum
add $8, %rsp
call print10
add $0, %rsp
mov $10, %rax
push %rax
call incr
add $8, %rsp
push %rax
call printnum
add $8, %rsp
mov -8(%rbp), %rax
push %rax
mov $4, %rax
push %rax
call add
add $16, %rsp
push %rax
call printnum
add $8, %rsp
mov -16(%rbp), %rax
push %rax
call printnum
add $8, %rsp
mov $0, %rax
push %rax
call return
add $8, %rsp
