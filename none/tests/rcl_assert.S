
                .text
                .globl  main
        main:
                xorl    %eax, %eax
                rcll    %eax
                imull   %eax, %eax
                ret
