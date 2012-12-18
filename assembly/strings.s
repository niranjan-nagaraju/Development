.data
    helloStr:
        .asciz "Welcome to the world of tomorrow!"
    hello:
        .asciz "Hello"


.bss
    .lcomm Destination, 100
    .lcomm DestinationUsingRep, 100
    .lcomm DestinationUsingStos, 100

.text
    .globl _start

    _start:
        nop

        # 1. copy using movsb, movsw, mpvsl

        movl $helloStr, %esi
        movl $Destination, %edi

        movsb
        movsw
        movsl


        # Set/Clear the DF flag -- Direction flag
        # 1 => decrement both esi, edi
        # 0 => increment 

        std # set the DF flag
        cld # clear the DF flag

        # Using Rep

        movl $helloStr, %esi
        movl $DestinationUsingRep, %edi
        movl $34, %ecx # Length of string in helloStr
        cld # clear DF
        rep movsb # Repeat until ECX becomes 0
        std


        # Load string from memory into EAX
        cld
        leal helloStr, %esi # Load effective address of helloStr
        lodsb
        movb $0, %al

        dec %esi
        lodsw
        movw $0, %ax

        subl $2, %esi # make esi point to original string
        lodsl

       
       # Stroring strings into memory from EAX
       leal DestinationUsingStos, %edi
       stosb
       stosw
       stosl

       # Comparing strings
       cld
       leal helloStr, %esi
       leal hello, %edi
       cmpsb

       dec %esi
       dec %edi
       cmpsw

       subl $2, %esi
       subl $2, %edi
       cmpsl

       # Exit()
       movl $1, %eax
       movl $10, %ebx # Exit(10)
       int $0x80


