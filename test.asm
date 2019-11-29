mov si,message
xor ax,ax
lp:
    lodsb
    push si
    push ax
    int 0o42
    pop ax
    pop si
    and al,al
    jnz lp
    int 0o40
message:
db 0o110
db 0o145
db 0o154
db 0o154
db 0o157
db 0o54
db 0o40
db 0o167
db 0o157
db 0o162
db 0o154
db 0o144
db 0o41
db 0o0
