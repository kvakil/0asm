;;; # 0asm
;;;
;;; 0asm is a 16-bit x86 assembler written in 16-bit x86 assembly which fits
;;; in under 512 bytes of x86 machine code! It runs on
;;; [bootOS](https://github.com/nanochess/bootOS/). 
;;;
;;; ## Assembling
;;;
;;; You can assemble and run 0asm using nasm and QEMU (respectively):
;;;
;;;     make run
;;;
;;; This also copies the input file `test.asm` into the disk under `H`. One
;;; can then run the assembler in bootOS:
;;;
;;;     $0asm
;;;
;;; and then run the output program under `P`:
;;;
;;;     $P
;;;     Hello, world! $
;;;
;;; Further examples are available under the `examples/` directory.
;;;
;;; ## Anticipated FAQ
;;;
;;; ### What does it support?
;;;
;;; 0asm supports several common instructions, labels (absolute and relative
;;; relocations), and octal literals (but only octal literals). It also
;;; supports all 16-bit x86 general purpose registers, but does not support
;;; segment registers.
;;; 
;;; Many jump instructions are supported (call, jmp, jb, jnb, jz, jnz, jbe,
;;; jnbe), along with several memory addressing instructions (stosb, stosw,
;;; lodsb, lodsw, cbw), several stack instructions (push, pop, ret), a
;;; few fundamental arithmetic instructions in both register/register and
;;; register/immediate form (add, and, xor, cmp, mov), and some special
;;; instructions (int, the pseudo-instruction db).
;;;
;;; Errors are not really handled, but the assembler does usually exit cleanly
;;; instead of producing garbage.
;;;
;;; ### How does it work?
;;;
;;; 0asm is a simple 2-pass assembler. The first pass collects labels into a
;;; "symbol table", and the addresses of instructions which need to be fixed
;;; into a "fixup table". The first pass also outputs the machine code for
;;; instructions which do not require any relocation.
;;;
;;; ### Is it self-hosting?
;;;
;;; Unfortunately not yet, for several reasons.
;;;
;;; 1. The underlying bootOS only supports 512 byte files.
;;; 2. The instruction encoding produced is not optimal, so it will not fit
;;; in 512 bytes once assembled.
;;; 3. Shift instructions are not yet supported.
;;;
;;; These problems are not insurmountable, although it seems difficult.
;;; We could easily move the goalpost by typing the entire program using db,
;;; but of course that would be no fun.
;;;
;;; ## Development
;;;
;;; The code is decently commented. Many of the files in this repo are
;;; generated from `0asm.asm`, including the README. You can also debug by
;;; running `make debug` (to start QEMU in one pane) and `make gdbdebug`
;;; (to start GDB along with some helpful default scripts).
;;;
;;; ## License
;;;
;;; Copyright (c) 2019 Keyhan Vakil
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Vocabulary

;; hash: a formula which maps identifiers to a 16-bit value (for shorter, but
;; lossy, string comparisons). See the hash procedure for its computation.

;; table: an associative array of (key, value) pairs. Keys are typically
;; outputs of the hash function, while values are any 16-bit values.

;; clobbered: indicates that a register is changed by a procedure in some
;; unspecified fashion.

;; consumed: as we read characters from our input buffer, we "consume"
;; them. We generally keep the invariant that al is the next character in
;; the buffer, and si points to the rest of the buffer. Note that consuming
;; is different than clobbering, as consuming means that we maintain this
;; invariant!

;; ";!": this comment symbol indicates that some code or functionality is
;; easily added, but was removed so that the program fits in 512 bytes.

bits 16
fixup_table:    equ 0x4000
symbol_table:   equ 0x5000
register_table: equ 0x6000
input_start:    equ 0x6b00
output_start:   equ 0x6d00
int_exit:       equ 0x20
int_read_file:  equ 0x23
int_save_file:  equ 0x24

%macro hash_s 1
%assign hash_result 35
%strlen %%len %1
%assign %%i 1
%rep %%len
%substr %%ch %1 %%i
%assign hash_result (31 * hash_result + %%ch)
%assign hash_result (hash_result & 0xffff)
%assign %%i (%%i+1)
%endrep
%endmacro

%macro dw_hash 1
hash_s %1
dw hash_result
%endmacro

global _start
_start:

;; Zero out fixup, symbol table, and the stack. Many of the functions below
;; (like lookup and add_table) assume that tables are zero-terminated, and
;; this initialization ensures that this is the case.
initialize_tables:
    ; This zeros out 0x4000 to 0x6f00.
    ; 0x4000 is the address of the fixup table.
    ; 0x5000 is the address of the symbol table.
    ; 0x6f00 is the start of the stack.
    mov di,fixup_table
    ; We set only ch here, which saves us a byte. This means that the lower
    ; cl might be garbage, but this is OK -- the maximum value is 0x2fff,
    ; which would have us write up to 0x7000, right below bootOS. The minimum
    ; value is 0x2f00, which still zeros out everything we need.
    ;
    ; In any case, error-checking is definitely not the priority of this
    ; assembler...
    mov ch,0x2f
    xor ax,ax
    rep stosb

;; Unlike other tables, the register table is generated at runtime, since the
;; lookup values are sequential, we can save bytes by only storing the keys.
initialize_register_table:
    ; It's shorter to do this manually than to use add_table.
    mov si,partial_register_table
    mov di,register_table
    ; ax is zero from above.
.initialize_register_table_loop:
    ; Copy over partial register table key.
    movsw
    ; Store the index of this register.
    stosw
    inc ax
    ; If al == 16, then we've finished copying partial_register_table.
    ; From initialize_tables above, we know that this table was zero.
    ; It is important that we don't overwrite this final zero entry, since
    ; we are using it as a sentinel value.
    cmp al,0x10
    jne .initialize_register_table_loop
.initialize_register_table_end:

;; Initialize si to point to the input buffer and di to point to the output
;; buffer. Read in the input file into the input buffer.
init:
    ; Start stack pointer right below input buffer. This leaves space for
    ; 0x200 input bytes, which is the size of files in bootOS. The output
    ; buffer will start at 0x6d00.
    mov sp,input_start
    mov di,sp
    ; Read in the file "H".
    mov bx,infile
    int int_read_file
    jc done_error
    ; All registers except sp could be clobbered at this point.
    mov si,sp
    ; Give space for 0x200 output bytes, until 0x6f00.
    mov di,output_start

;; Keep reading instructions. This lets us use "ret" when we successfully
;; parse an instruction, which ends up saving many bytes over
;; "jmp read_instruction", because we need to jump back so often.
read_instruction_loop:
    call read_instruction
    jmp read_instruction_loop

;; Read and assemble a single instruction.
read_instruction:
    lodsb

;; This block will be executed when we are finished parsing the entire file.
done:
    ; The input file is NUL terminated. If this is not a NUL, then we need to
    ; keep parsing the file.
    and al,al
    jne done_end
.fixup_labels:
    mov si,fixup_table
.fixup_labels_loop:
    ; Load key from fixup table.
    lodsw
    and ax,ax
    jz .fixup_labels_done
    ; Lookup key in symbol table.
    mov bp,si
    mov si,symbol_table
    mov cx,ax
    call lookup
    ; Error: could not find a label for this address we need to fix.
    jnc done_error

    ; Currently there is a "fixup hint" (f) stored at the address of the
    ; immediate we are fixing (i), and the label address is d. This code sets
    ; *i = f + d - i. This allows us to support both relative and absolute
    ; relocations, by choosing the fixup hint appropriately. (See jmp_and_call
    ; as well as parse_2x.parse_2x_label for relative and absolute relocations
    ; respectively.)

    ; Store the label address.
    push ax
    ; Get the location to fixup.
    lodsw
    mov di,ax
    sub word [di],ax
    pop ax
    add word [di],ax
    jmp .fixup_labels_loop
.fixup_labels_done:
    ; Original start of output buffer.
    mov di,output_start
    ; Output to the file "P".
    mov bx,outfile
    int int_save_file
done_error:
    int int_exit
done_end:

    ; Skip space or space-ish character
    cmp al,' '
    jbe read_instruction

;; If we see a comment, read until a newline.
read_comment:
    cmp al,';'
    jnz .read_comment_end
.read_comment_loop:
    lodsb
    cmp al,`\n`
    jnz .read_comment_loop
    ret
.read_comment_end:

    call hash

    ; At this point, al equals the first non-identifier character,
    ; and si points to right after al.
    ;
    ; Some examples follow. The position of al is marked a, the position
    ; of si is marked s. $ is used to indicate a newline.
    ;
    ;      mov ax,cx$
    ;         as
    ;
    ;      hello:$
    ;           as
    ;
    ;      ret$
    ;         as

    ; Hashes are critical for the memory efficiency of 0asm. The computed hash
    ; is used as an index in an associative array. The code in hash.c is
    ; helpful for computing the hash of many instructions at once.

;; If the next character in the buffer is a colon, this adds the label to our
;; symbol table.
add_to_label:
    cmp al,':'
    jne add_to_label_end

    mov dh,symbol_table >> 8
    ; Inlining the tail call to add_table here saves us a two bytes of a jmp.

;; Add (cx, di) to the table at cx.
;; Inputs:
;;   cx is the key to write.
;;   dh is the (high part) of the address of the table to write to.
;;      the low part will be ignored and zero'd out.
;;   di is the value to write.
;; Outputs:
;;   ax is clobbered to the initial value of di.
;;   dl is set to zero.
add_table:
    ; Save old values.
    push di
    xor dl,dl
    ; Set di equal to the start of the table.
    mov di,dx
    xor ax,ax
.keep_scanning:
    scasw
    jnz .keep_scanning
    ; Decrementing twice is shorter than subtracting 2.
    dec di
    dec di
    ; Store the key-value pair.
    mov ax,cx
    stosw
    ; Pop old di into ax to store correct value.
    pop ax
    stosw
    mov di,ax
    ret
add_to_label_end:

    ; db
parse_db:
    hash_s 'db'
    cmp cx,hash_result
    je store_odigit_byte
.parse_db_end:

    ; int
parse_int:
    hash_s 'int'
    cmp cx,hash_result
    jne parse_int_end
    mov al,0xcd
    stosb
store_odigit_byte:
    call odigit
; This "jump chain" allows us to jump to a single statement (done_error), from
; multiple places, without requiring long jumps (signed displacement greater
; than one byte). Each individual jump in the chain is within 128 bytes of the
; previous jump.
done_error_chain_0:
    jnc done_error
    stosb
    ret
parse_int_end:

    ; All calls to lookup expect a saved value of si in bp, which is restored
    ; on success.
    mov bp,si

;; Deals with jmp and call instructions, encoded as:
;;    - opcode (1 or 2 bytes) - from the table.
;;    - immediate (2 bytes) - needs to be relocated by linker.
jmp_and_call:
    mov si,jmp_and_call_table
    call lookup
    jnc .jmp_and_call_end
.jmp_and_call_match:
    ; Blindly store BOTH bytes into the buffer. (We'll fix this below.)
    stosw
    ; If this is a one byte opcode, then decrement di. This effectively makes
    ; the stosw above act as a stosb, since the high 0 byte will be overwritten
    ; by the immediate below.
    and ah,ah
    jnz .jmp_and_call_two_bytes
    dec di
.jmp_and_call_two_bytes:
    ; Hash the label and add it to the fixup table.
    call hash_pre
    mov dh,fixup_table >> 8
    call add_table
    ; Add a fixup hint of -2. (This effectively creates a relative relocation,
    ; see done.fixup_labels for details.)
    mov ax,0xfffe
    stosw
    ret
.jmp_and_call_end:

;; Deals with single-byte no argument instructions, encoded as just the opcode.
parse_10:
    ; Note we don't need to set si or bp here.
    ; If the lookup above fails, then si points to one after jmp_and_call_table
    ; (i.e., table10), and bp is still correctly saved.
    call lookup
    jnc .parse_10_end
    stosb
    ret
.parse_10_end:

;; Deals with single-byte single-register instructions, encoded as just the
;; opcode plus a register number.
parse_11:
    call lookup
    jnc parse_11_end
    push ax
    call accept_register
done_error_chain_1:
    jnc done_error_chain_0
    pop dx
    add ax,dx
    stosb
    ret
parse_11_end:

;; Deals with instructions which take two arguments: either a
;; register-register, or a register-immediate. This also handles the case of
;; register-label (which is encoded as an absolute relocation of a
;; register-immediate).
;;
;; This is the trickiest case, consult an Intel manual for details.
parse_2x:
    call lookup
done_error_chain_2:
    jnc done_error_chain_1
    ; Save the opcode.
    push ax
    ; The first argument MUST be a register, regardless.
    call accept_register
    ; (Note accept_register clobbers the comma in ax.)
done_error_chain_3:
    jnc done_error_chain_2

    ; Restore opcode in dx.
    pop dx

    ; Check if this is a 16-bit register or an 8-bit register.
    ; bl will be 1 if we should use a 16-bit immediate, and 0 if we should use
    ; an 8-bit immediate. It also encodes if this is operating on an 8-bit or
    ; 16-bit register. Technically x86 makes a distinction between these two,
    ; but we treat them the same (leading to longer but still valid instruction
    ; encodings).

    ; Move 4th bit into carry flag.
    shl al,0x5
    ; Set bx to the carry flag.
    adc bx,0x0
    ; Make al the same as before, except without 4th bit set.
    shr al,0x5

    ; Set LSB of opcode byte correctly.
    add dl,bl

    ; Try to get another register.
    push ax
    push si
    call accept_register
    ; (Note accept_register clobbers the newline in ax.)
    ; If it's not a register, it must be an immediate or a label.
    jnc .parse_2x_immediate

    ; In this case, we are parsing a register-register instruction.
    ; We store this as:
    ;     - opcode (1 byte)
    ;     - Mod R/M byte (1 byte)

    ; The parse was successful, so we need to clean the old si from the stack.
    ; We don't use cx from now on, so pop into it.
    pop cx

    ; Compute the Mod R/M byte:
    ;     11 src[0:3] dst[0:3]
    ; where src and dst are the register numbers.
    ; Right now ax=src, and dst is on the top of stack.

    ; Shifting up by three here can overflow the second MSB, but it's
    ; overwritten below anyway.
    shl al,0x3
    ; Note that we always set the top two bits, as we only support register
    ; addressing here.
    or al,0xc0

    ; Get the destination register off the stack.
    pop cx
    or cl,al
    ; Opcode byte
    mov al,dl
    ; Mod R/M byte
    mov ah,cl
    stosw
    ret

.parse_2x_immediate:
    ; Backtrack to point to the immediate / label.
    pop si

    ; In the immediate case, the opcode byte is USUALLY reusable as the Mod R/M
    ; byte. (The exception is the case when we have a MOV instruction.) First,
    ; we can remove the LSB for the opcode byte (which indicates if the
    ; instruction operates on 8-bit or 16-bit registers). The middle three
    ; bits of this opcode byte select the correct operation for all Group 1
    ; instructions.

    ; The top of our lookup table stores the corresponding opcode for immediate
    ; version of the instruction.
    mov al,dh
    cmp al,0x80
    je .parse_group1_immediate

    ; In this case it's a MOV instruction, we kill the old top bits of the
    ; opcode. This effectively allows us to reuse the opcode byte for Group 1
    ; instructions, and makes the MOV instruction a "raw" encoding which uses
    ; an opcode byte of 0. It also maintains whether this instruction is
    ; supposed to operate on 8-bit or 16-bit registers, which is useful for bl
    ; below.
    mov dl,bl

.parse_group1_immediate:
    add al,bl
    stosb
    ; Construct the Mod R/M byte using the old register.
    pop ax
    add al,dl
    ; Clear off the last bit of the opcode.
    sub al,bl
    ; Set the top two bits of Mod R/M to indicate registers.
    or al,0xc0
    stosb
    ; Parse off the octal number.
    push si
    call odigit
    ; If it's not an octal number, it must be a register.
    jnc .parse_2x_label
    ; The parse was successful, so we need to clean the old si from the stack.
    ; We don't use cx from now on, so pop into it.
    pop cx

.parse_2x_append_immediate:
    ; Blindly store both bytes of the immediate.
    stosw
    ; If bx = 0, we want to decrement di, so that we effectively store only
    ; one byte of the immediate.
    ; If bx = 1, then we don't want to change di.
    ; Because we have bx available, this is shorter than the similar code in
    ; jmp_and_call.jmp_and_call_match.
    add di,bx
    dec di
    ret

.parse_2x_label:
    ; Backtrack.
    pop si
    ; Get the hash of this label.
    call hash_pre
    mov dh,fixup_table >> 8
    call add_table
    ; We want to add a value to the relocation such that, when di is added by
    ; the fixup, it will correspond to the absolute address of the loaded
    ; label. This is the start of the output buffer (output_start), minus the
    ; actual location the binary will start (0x7c00).
    add ah,0xf
    stosw
    ret

;; Accept a register pointed to by the buffer.
;;
;; Inputs:
;;   al is the first character in the buffer.
;;   si is a buffer pointing to the input after al.
;; Outputs:
;;   ax is DESTROYED. It now indicates the register number and does not point
;;      to the buffer, therefore ignoring the character after this register.
;;   bx is clobbered.
;;   bp is clobbered.
;;   si is consumed.
accept_register:
    call hash_pre
    mov bp,si
    mov si,register_table
    ; FALLTHROUGH to lookup (saves us two bytes)

;; Lookup in the given table. A table is an array of key-value pairs, where
;; each key and value occupies a single word. The keys must be non-zero,
;; because a zero key is reserved to indicate the end. The table must be
;; terminated by a zero key.
;;
;; Inputs:
;;   cx is the key to lookup.
;;   si points to the first key of the table.
;;   bp is the old value of si.
;; Outputs:
;;   ax is the returned value, or 0 on failure.
;;   si is modified.
;;      if the lookup is successful, it is set to bp.
;;      if the lookup fails, it points one word after the end of the table.
;;   carry flag is set iff the lookup was successful.
;;   
lookup:
    lodsw
    and ax,ax
    jz .lookup_not_found
.lookup_keep_going:
    cmp ax,cx
    lodsw
    jne lookup
.lookup_done:
    stc
    mov si,bp
.lookup_not_found:
    ret

;; Compute a hash for table indexing. Stops at the first non-identifier
;; character after al, consuming all characters and incrementing si.
;;
;; Inputs:
;;   al is the first character in the buffer.
;;   si is a buffer pointing to the input after al.
;; Outputs:
;;   al is consumed.
;;   si is consumed.
;;   cx is the returned hash value.
hash_pre:
    lodsb
hash:
    mov cx,0x23
.hash_loop:
    ; Using cbw here lets us use ax as al, which we need (because we want to
    ; use the full 16-bit for the hash to reduce collisions).
    cbw
    ; cx = 31 * cx + (next character)
    sub ax,cx
    neg ax
    shl cx,0x5
    sub cx,ax
    lodsb
    ; Stop if we see a non-identifier character.
    cmp al,'@'
    jg .hash_loop
    ret

;; Convert ASCII octal.
;; The representation must start with "0o".
;; Inputs:
;;   al is the first character in the buffer.
;;   si is a buffer pointing to the input after al.
;; Outputs:
;;   ax is the returned value. It is clobbered if the parse fails.
;;   cx is clobbered.
;;   si is consumed.
;;   the carry flag is set if the parse is successful.
odigit:
    lodsb
    cmp al,'0'
    jne .odigit_bad
    ;! Error checking on o removed for byte count.
    lodsb
    xor cx,cx
.odigit_loop:
    lodsb
    ; Convert by subtracting off ASCII 0.
    sub al,'0'
    ; Note if al < '0', then CF is set to 1. This indicates we are now dealing
    ; with a whitespace character, and the parse was successful.
    ;! We don't check that the value is not too large.
    jb .odigit_good
    shl cx,0x3
    ; Note that the bottom three bits of cl are zero and al is at most 7,
    ; so adding only the lower bytes here is fine; we never overflow cl.
    add cl,al
    jmp .odigit_loop
.odigit_bad:
    ; Clear the carry flag to show parse was bad.
    clc
.odigit_good:
    mov ax,cx
    ret

;; THESE TABLES MUST APPEAR IN THIS ORDER. ;;

;; Lookup table for jump and call instructions (anything requiring relative
;; relocation with 16-bit addresses). Keys are the hashes, and values are the
;; opcode, which may be multiple bytes long.
jmp_and_call_table:
    dw_hash 'call'
    dw 0xe8
    dw_hash 'jmp'
    dw 0xe9
    dw_hash 'jb'
    dw 0x820f
    dw_hash 'jnb'
    dw 0x830f
    dw_hash 'jz'
    dw 0x840f
    dw_hash 'jnz'
    dw 0x850f
    dw_hash 'jbe'
    dw 0x860f
    dw_hash 'jnbe'
    dw 0x870f
    ; NOT FOUND
    dw 0x0

;; Lookup table for instructions which take up one byte and have no arguments.
;; Keys here are the hashes, values are simply the opcode.
table10:
    dw_hash 'stosb'
    dw 0xaa
    dw_hash 'stosw'
    dw 0xab
    dw_hash 'lodsb'
    dw 0xac
    dw_hash 'lodsw'
    dw 0xad
    dw_hash 'ret'
    dw 0xc3
    dw_hash 'cbw'
    dw 0x98
    ; NOT FOUND
    dw 0x0

;; Lookup table for instructions with one opcode byte and one argument.
;; Keys here are the hashes. Values are the opcode which will be added to
;; (note that because we start numbering 16-bit registers at 0x8, most of
;; the values here are shifted down by 0x8).
table11:
    dw_hash 'push'
    ; Nasty trick here. We overlap infile and outfile with the push and pop
    ; opcodes respectively. This makes our input filename "H" and our output
    ; filename "P", and saves us three bytes.
infile:
    dw 0x48
    dw_hash 'pop'
outfile:
    dw 0x50
    dw_hash 'inc'
    dw 0x38
    dw_hash 'dec'
    dw 0x40
    ; NOT FOUND
    dw 0x0

;; Lookup table for instructions which take two arguments. Keys here are the
;; hashes. Values have two parts: the low-byte is the opcode byte for
;; non-immediate (register/register) forms, while the the high-byte is the
;; opcode for the immediate (register/immediate) form.
table2x:
    dw_hash 'add'
    dw 0x8000
    dw_hash 'and'
    dw 0x8020
    dw_hash 'xor'
    dw 0x8030
    dw_hash 'cmp'
    dw 0x8038
    dw_hash 'mov'
    dw 0xc688
    ; NOT FOUND
    dw 0x0

;; The "partial" register table contains only keys -- no values. The actual
;; register table is initialized using this and initialize_register_table.
partial_register_table:
    dw_hash 'al'
    dw_hash 'cl'
    dw_hash 'dl'
    dw_hash 'bl'
    dw_hash 'ah'
    dw_hash 'ch'
    dw_hash 'dh'
    dw_hash 'bh'
    dw_hash 'ax'
    dw_hash 'cx'
    dw_hash 'dx'
    dw_hash 'bx'
    dw_hash 'sp'
    dw_hash 'bp'
    dw_hash 'si'
    dw_hash 'di'
    ; NOT FOUND not required.

bytes_left_message:
%assign bytes_left 0x200-($-$$)
%warning Have bytes_left bytes left.
times bytes_left db 0x90
