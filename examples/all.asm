; Show off what we support.
call message
jmp message
jb message
jnb message
jz message
jnz message
jbe message
jnbe message
stosb
stosw
lodsb
lodsw
cbw
push ax
pop bx
ret
add al,ah
and ah,al
xor bx,sp
cmp ax,cx
mov ax,bx
add al,0o123
and ah,0o123
xor bx,0o123
cmp cx,0o123
mov bh,0o123
mov bx,0o123
int 0o20
message:
db 0o0
 
