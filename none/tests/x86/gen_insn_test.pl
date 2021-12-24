#!/usr/bin/perl

use 5.006;
use strict;
use warnings;

our %ArgTypes = (
                 r8 => "reg8_t",
                 r16 => "reg16_t",
                 r32 => "reg32_t",
                 mm => "reg64_t",
                 xmm => "reg128_t",
                 m8 => "reg8_t",
                 m16 => "reg16_t",
                 m32 => "reg32_t",
                 m64 => "reg64_t",
                 m128 => "reg128_t",
                 eflags => "reg32_t",
                 st => "reg64_t",
                 fpucw => "reg16_t",
                 fpusw => "reg16_t"
                 );

our %SubTypeFormats = (
                       sb => "%d",
                       ub => "%u",
                       sw => "%d",
                       uw => "%u",
                       sd => "%ld",
                       ud => "%lu",
                       sq => "%lld",
                       uq => "%llu",
                       ps => "%.16g",
                       pd => "%.16g"
                       );

our %SubTypeSuffixes = (
                        sb => "",
                        ub => "U",
                        sw => "",
                        uw => "",
                        sd => "L",
                        ud => "UL",
                        sq => "LL",
                        uq => "ULL",
                        ps => "F",
                        pd => ""
                        );

our %RegNums = (
                al => 0, ax => 0, eax => 0,
                bl => 1, bx => 1, ebx => 1,
                cl => 2, cx => 2, ecx => 2,
                dl => 3, dx => 3, edx => 3,
                ah => 4,
                bh => 5,
                ch => 6,
                dh => 7,
                st0 => 0, st1 => 1, st2 => 2, st3 => 3,
                st4 => 4, st5 => 5, st6 => 6, st7 => 7
                );

our %RegTypes = (
                 al => "r8", ah => "r8", ax => "r16", eax => "r32",
                 bl => "r8", bh => "r8", bx => "r16", ebx => "r32",
                 cl => "r8", ch => "r8", cx => "r16", ecx => "r32",
                 dl => "r8", dh => "r8", dx => "r16", edx => "r32"
                 );

our @IntRegs = (
                { r8 => "al", r16 => "ax", r32 => "eax" },
                { r8 => "bl", r16 => "bx", r32 => "ebx" },
                { r8 => "cl", r16 => "cx", r32 => "ecx" },
                { r8 => "dl", r16 => "dx", r32 => "edx" },
                { r8 => "ah" },
                { r8 => "bh" },
                { r8 => "ch" },
                { r8 => "dh" }
                );

print <<EOF;
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

typedef union {
  char sb[1];
  unsigned char ub[1];
} reg8_t;

typedef union {
  char sb[2];
  unsigned char ub[2];
  short sw[1];
  unsigned short uw[1];
} reg16_t;

typedef union {
  char sb[4];
  unsigned char ub[4];
  short sw[2];
  unsigned short uw[2];
  long int sd[1];
  unsigned long int ud[1];
  float ps[1];
} reg32_t;

typedef union {
  char sb[8];
  unsigned char ub[8];
  short sw[4];
  unsigned short uw[4];
  long int sd[2];
  unsigned long int ud[2];
  long long int sq[1];
  unsigned long long int uq[1];
  float ps[2];
  double pd[1];
} reg64_t __attribute__ ((aligned (8)));

typedef union {
  char sb[16];
  unsigned char ub[16];
  short sw[8];
  unsigned short uw[8];
  long int sd[4];
  unsigned long int ud[4];
  long long int sq[2];
  unsigned long long int uq[2];
  float ps[4];
  double pd[2];
} reg128_t __attribute__ ((aligned (16)));

static sigjmp_buf catchpoint;

static char state[108];

static void handle_sigill(int signum)
{
   siglongjmp(catchpoint, 1);
}

__attribute__((unused))
static int eq_float(float f1, float f2)
{
   return f1 == f2 || fabsf(f1 - f2) < fabsf(f1) * 1.5 * pow(2,-12);
}

__attribute__((unused))
static int eq_double(double d1, double d2)
{
   return d1 == d2 || fabs(d1 - d2) < fabs(d1) * 1.5 * pow(2,-12);
}

EOF

my %tests;
my @tests;

while (<>)
{
    next if /^#/;

    my $insn;
    my $presets;
    my $args;
    my $results;

    if (/^(\S+)\s+(?:(\S+(?:\s+\S+)*)\s+:\s+)?((?:\S+\s+)*?)(?:=>\s+(\S+(?:\s+\S+)*))?$/)
    {
        $insn = $1;
        $presets = $2 || "";
        $args = $3 || "";
        $results = $4 || "";

#        print STDERR "insn: $insn\n";
#        print STDERR "presets: $presets\n";
#        print STDERR "args: $args\n";
#        print STDERR "results: $results\n";
    }
    else
    {
        die "Can't parse test $_";
    }
    
    $tests{$insn}++;
    
    my $test = "${insn}_$tests{$insn}";
    
    push @tests, $test;
    
    print qq|static void $test(void)\n|;
    print qq|\{\n|;

    my @intregs = @IntRegs;
    my @mmregs  = map { "mm$_" }  (6,7,0,1,2,3,4,5);
    my @xmmregs = map { "xmm$_" } (4,5,0,1,2,3,6,7);
    my @fpregs  = map { "st$_" }  (0 .. 7);

    my @presets;
    my $presetc = 0;
    my $eflagsmask;
    my $eflagsset;
    my $fpucwmask;
    my $fpucwset;
    my $fpuswmask;
    my $fpuswset;

    foreach my $preset (split(/\s+/, $presets))
    {
        if ($preset =~ /^([abcd][lh]|[abcd]x|e[abcd]x)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
        {
            my $name = "preset$presetc";
            my $type = $RegTypes{$1};
            my $regnum = $RegNums{$1};
            my $register = $intregs[$regnum];
            my $subtype = $2;
            my @values = split(/,/, $3);
    
            die "Register $1 already used" unless defined($register);

            my $preset = {
                name => $name,
                type => $type,
                subtype => $subtype,
                register => $register
            };

            delete($intregs[$regnum]);

            push @presets, $preset;
            
            print qq|   $ArgTypes{$type} $name = \{ .$subtype = \{|;
            
            my $valuec = 0;
            
            foreach my $value (@values)
            {
                print qq|,| if $valuec > 0;
                print qq| $value$SubTypeSuffixes{$subtype}|;
                $valuec++;
            }
            
            print qq| \} \};\n|;

            $presetc++;
        }
        elsif ($preset =~ /^st([0-9]+)\.(ps|pd)\[([^\]]+)\]$/)
        {
            my $name = "preset$presetc";
            my $type = "st";
            my $regnum = $1;
            my $register = $fpregs[$regnum];
            my $subtype = $2;
            my @values = split(/,/, $3);

            die "Register st$1 already used" unless defined($register);

            my $preset = {
                name => $name,
                type => $type,
                subtype => $subtype,
                register => $register
            };

            delete($fpregs[$regnum]);

            push @presets, $preset;
            
            print qq|   $ArgTypes{$type} $name = \{ .$subtype = \{|;
            
            my $valuec = 0;
            
            foreach my $value (@values)
            {
                print qq|,| if $valuec > 0;
                print qq| $value$SubTypeSuffixes{$subtype}|;
                $valuec++;
            }
            
            print qq| \} \};\n|;

            $presetc++;
        }
        elsif ($preset =~ /^(eflags)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my @values = split(/,/, $2);

            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;

            $eflagsmask = sprintf "0x%08x", $values[0] ^ 0xffffffff;
            $eflagsset = sprintf "0x%08x", $values[1];
        }
        elsif ($preset =~ /^(fpucw)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my @values = split(/,/, $2);

            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;

            $fpucwmask = sprintf "0x%04x", $values[0] ^ 0xffff;
            $fpucwset = sprintf "0x%04x", $values[1];
        }
        elsif ($preset =~ /^(fpusw)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my @values = split(/,/, $2);

            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;

            $fpuswmask = sprintf "0x%04x", $values[0] ^ 0xffff;
            $fpuswset = sprintf "0x%04x", $values[1];
        }
        else
        {
            die "Can't parse preset $preset";
        }
    }

    my @args;
    my $argc = 0;
    
    foreach my $arg (split(/\s+/, $args))
    {
        my $name = "arg$argc";

        if ($arg =~ /^([abcd]l|[abcd]x|e[abcd]x|r8|r16|r32|mm|xmm|m8|m16|m32|m64|m128)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
        {
            my $type = $RegTypes{$1} || $1;
            my $regnum = $RegNums{$1};
            my $register = $intregs[$regnum] if defined($regnum);
            my $subtype = $2;
            my @values = split(/,/, $3);
            
            die "Register $1 already used" if defined($regnum) && !defined($register);

            my $arg = {
                name => $name,
                type => $type,
                subtype => $subtype
            };

            if (defined($register))
            {
                $arg->{register} = $register;
                delete($intregs[$regnum]);
            }

            push @args, $arg;
            
            print qq|   $ArgTypes{$type} $name = \{ .$subtype = \{|;
            
            my $valuec = 0;
            
            foreach my $value (@values)
            {
                print qq|,| if $valuec > 0;
                print qq| $value$SubTypeSuffixes{$subtype}|;
                $valuec++;
            }

            print qq| \} \};\n|;
        }
        elsif ($arg =~ /^st([0-9]+)\.(ps|pd)\[([^\]]+)\]$/)
        {
            my $type = "st";
            my $regnum = $1;
            my $register = $fpregs[$regnum] if defined($regnum);
            my $subtype = $2;
            my @values = split(/,/, $3);
            
            die "Register st$1 already used" if defined($regnum) && !defined($register);

            my $arg = {
                name => $name,
                type => $type,
                subtype => $subtype
            };

            if (defined($register))
            {
                $arg->{register} = $register;
                delete($fpregs[$regnum]);
            }

            push @args, $arg;
            
            print qq|   $ArgTypes{$type} $name = \{ .$subtype = \{|;
            
            my $valuec = 0;
            
            foreach my $value (@values)
            {
                print qq|,| if $valuec > 0;
                print qq| $value$SubTypeSuffixes{$subtype}|;
                $valuec++;
            }

            print qq| \} \};\n|;
        }
        elsif ($arg =~ /^(imm8|imm16|imm32)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my $value = $2;
            
            my $arg = {
                type => $type,
                value => $value
            };

            push @args, $arg;
        }
        else
        {
            die "Can't parse argument $arg";
        }

        $argc++;
    }
    
    foreach my $arg (@presets, @args)
    {
        if ($arg->{type} =~ /^(r8|r16|r32|m8|m16|m32)$/)
        {
            while (!exists($arg->{register}) || !defined($arg->{register}))
            {
                $arg->{register} = shift @intregs;
            }

            $arg->{register} = $arg->{register}->{$arg->{type}};
        }
        elsif ($arg->{type} =~ /^(mm|m64)$/)
        {
            $arg->{register} = shift @mmregs;
        }
        elsif ($arg->{type} =~ /^(xmm|m128)$/)
        {
            $arg->{register} = shift @xmmregs;
        }
        elsif ($arg->{type} =~ /^st$/)
        {
            while (!exists($arg->{register}) || !defined($arg->{register}))
            {
                $arg->{register} = shift @fpregs;
            }
        }
    }

    my @results;
    my $resultc = 0;
    
    foreach my $result (split(/\s+/, $results))
    {
        my $name = "result$resultc";
    
        if ($result =~ /^(\d+)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
        {
            my $index = $1;
            my $type = $args[$index]->{type};
            my $subtype = $2;
            my @values = split(/,/, $3);
            
            die "Argument $index not specified" unless exists($args[$index]);

            my $result = {
                name => $name,
                type => $type,
                subtype => $subtype,
                arg => $args[$index],
                register => $args[$index]->{register},
                values => [ @values ]
            };

            push @results, $result;

            print qq|   $ArgTypes{$type} $name|;
            print qq| = arg$index| if $type =~ /^m(8|16|32|64|128)$/;
            print qq|;\n|;

            $args[$index]->{result} = $result;
        }
        elsif ($result =~ /^([abcd][lh]|[abcd]x|e[abcd]x)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
        {
            my $register = $1;
            my $type = $RegTypes{$register};
            my $subtype = $2;
            my @values = split(/,/, $3);
                        
            my $result = {
                name => $name,
                type => $type,
                subtype => $subtype,
                register => $register,
                values => [ @values ]
            };

            push @results, $result;

            print qq|   $ArgTypes{$type} $name;\n|;
        }
        elsif ($result =~ /^(st[0-9]+)\.(ps|pd)\[([^\]]+)\]$/)
        {
            my $register = $1;
            my $type = "st";
            my $subtype = $2;
            my @values = split(/,/, $3);
                        
            my $result = {
                name => $name,
                type => $type,
                subtype => $subtype,
                register => $register,
                values => [ @values ]
            };

            push @results, $result;

            print qq|   $ArgTypes{$type} $name;\n|;
        }
        elsif ($result =~ /^eflags\[([^\]]+)\]$/)
        {
            my @values = split(/,/, $1);
            
            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;
            
            my $result = {
                name => $name,
                type => "eflags",
                subtype => "ud",
                values => [ map { sprintf "0x%08x", $_ } @values ]
            };

            push @results, $result;
            
            print qq|   $ArgTypes{eflags} $name;\n|;

            if (!defined($eflagsmask) && !defined($eflagsset))
            {
                $eflagsmask = sprintf "0x%08x", $values[0] ^ 0xffffffff;
                $eflagsset = sprintf "0x%08x", $values[0] & ~$values[1];
            }
        }
        elsif ($result =~ /^fpucw\[([^\]]+)\]$/)
        {
            my @values = split(/,/, $1);
            
            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;
            
            my $result = {
                name => $name,
                type => "fpucw",
                subtype => "ud",
                values => [ map { sprintf "0x%04x", $_ } @values ]
            };

            push @results, $result;
            
            print qq|   $ArgTypes{fpucw} $name;\n|;

            if (!defined($fpucwmask) && !defined($fpucwset))
            {
                $fpucwmask = sprintf "0x%04x", $values[0] ^ 0xffff;
                $fpucwset = sprintf "0x%04x", $values[0] & ~$values[1];
            }
        }
        elsif ($result =~ /^fpusw\[([^\]]+)\]$/)
        {
            my @values = split(/,/, $1);
            
            $values[0] = oct($values[0]) if $values[0] =~ /^0/;
            $values[1] = oct($values[1]) if $values[1] =~ /^0/;
            
            my $result = {
                name => $name,
                type => "fpusw",
                subtype => "ud",
                values => [ map { sprintf "0x%04x", $_ } @values ]
            };

            push @results, $result;
            
            print qq|   $ArgTypes{fpusw} $name;\n|;

            if (!defined($fpuswmask) && !defined($fpuswset))
            {
                $fpuswmask = sprintf "0x%04x", $values[0] ^ 0xffff;
                $fpuswset = sprintf "0x%04x", $values[0] & ~$values[1];
            }
        }
        else
        {
            die "Can't parse result $result";
        }
        
        $resultc++;
    }
    
    my $argnum = 0;

    foreach my $result (@results)
    {
        if ($result->{type} eq "xmm")
        {
            $result->{argnuml} = $argnum++;
            $result->{argnumh} = $argnum++;
        }
        else
        {
            $result->{argnum} = $argnum++;
        }
    }
    
    foreach my $arg (@presets, @args)
    {
        if (defined($arg->{name}))
        {
            if ($arg->{type} eq "xmm")
            {
                $arg->{argnuml} = $argnum++;
                $arg->{argnumh} = $argnum++;
            }
            else
            {
                $arg->{argnum} = $argnum++;
            }
        }
    }

    my $stateargnum = $argnum++;

    print qq|\n|;
    print qq|   if (sigsetjmp(catchpoint, 1) == 0)\n|;
    print qq|   \{\n|;
    print qq|      asm\(\n|;
    print qq|         \"fsave %$stateargnum\\n\"\n|;
    
    my @fpargs;

    foreach my $arg (@presets, @args)
    {
        if ($arg->{type} eq "r8")
        {
            print qq|         \"movb %$arg->{argnum}, %%$arg->{register}\\n\"\n|;
        }
        elsif ($arg->{type} eq "r16")
        {
            print qq|         \"movw %$arg->{argnum}, %%$arg->{register}\\n\"\n|;
        }
        elsif ($arg->{type} eq "r32")
        {
            print qq|         \"movl %$arg->{argnum}, %%$arg->{register}\\n\"\n|;
        }
        elsif ($arg->{type} eq "mm")
        {
            print qq|         \"movq %$arg->{argnum}, %%$arg->{register}\\n\"\n|;
        }
        elsif ($arg->{type} eq "xmm")
        {
            print qq|         \"movlps %$arg->{argnuml}, %%$arg->{register}\\n\"\n|;
            print qq|         \"movhps %$arg->{argnumh}, %%$arg->{register}\\n\"\n|;
        }
        elsif ($arg->{type} eq "st")
        {
            $fpargs[$RegNums{$arg->{register}}] = $arg;
        }
    }
    
    foreach my $arg (reverse @fpargs)
    {
        if (defined($arg))
        {
            if ($arg->{subtype} eq "ps")
            {
                print qq|         \"flds %$arg->{argnum}\\n\"\n|;
            }
            elsif ($arg->{subtype} eq "pd")
            {
                print qq|         \"fldl %$arg->{argnum}\\n\"\n|;
            }
        }
        else
        {
            print qq|         \"fldz\\n\"\n|;
        }
    }

    if (defined($eflagsmask) || defined($eflagsset))
    {
        print qq|         \"pushfl\\n\"\n|;
        print qq|         \"andl \$$eflagsmask, (%%esp)\\n\"\n| if defined($eflagsmask);
        print qq|         \"orl \$$eflagsset, (%%esp)\\n\"\n| if defined($eflagsset);
        print qq|         \"popfl\\n\"\n|;
    }

    if (defined($fpucwmask) || defined($fpucwset))
    {
        print qq|         \"subl \$2, %%esp\\n\"\n|;
        print qq|         \"fstcw (%%esp)\\n\"\n|;
        print qq|         \"andw \$$fpucwmask, (%%esp)\\n\"\n| if defined($fpucwmask);
        print qq|         \"orw \$$fpucwset, (%%esp)\\n\"\n| if defined($fpucwset);
        print qq|         \"fldcw (%%esp)\\n\"\n|;
        print qq|         \"addl \$2, %%esp\\n\"\n|;
    }

    print qq|         \"$insn|;
    
    my $prefix = " ";
    
    foreach my $arg (@args)
    {
        next if $arg->{type} eq "eflags";

        if ($arg->{type} =~ /^(r8|r16|r32|mm|xmm)$/)
        {
            print qq|$prefix%%$arg->{register}|;
        }
        elsif ($arg->{type} =~ /^st$/)
        {
            my $register = $arg->{register};

            $register =~ s/st(\d+)/st\($1\)/;

            print qq|$prefix%%$register|;
        }
        elsif ($arg->{type} =~ /^(m(8|16|32|64|128))$/)
        {
            if (exists($arg->{result}))
            {
                print qq|$prefix%$arg->{result}->{argnum}|;
            }
            else
            {
                print qq|$prefix%$arg->{argnum}|;
            }
        }
        elsif ($arg->{type} =~ /^imm(8|16|32)$/)
        {
            print qq|$prefix\$$arg->{value}|;
        }

        $prefix = ", ";
    }

    print qq|\\n\"\n|;

    my @fpresults;

    foreach my $result (@results)
    {
        if ($result->{type} eq "r8")
        {
            print qq|         \"movb %%$result->{register}, %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "r16")
        {
            print qq|         \"movw %%$result->{register}, %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "r32")
        {
            print qq|         \"movl %%$result->{register}, %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "mm")
        {
            print qq|         \"movq %%$result->{register}, %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "xmm")
        {
            print qq|         \"movlps %%$result->{register}, %$result->{argnuml}\\n\"\n|;
            print qq|         \"movhps %%$result->{register}, %$result->{argnumh}\\n\"\n|;
        }
        elsif ($result->{type} eq "st")
        {
            $fpresults[$RegNums{$result->{register}}] = $result;
        }
        elsif ($result->{type} eq "eflags")
        {
            print qq|         \"pushfl\\n\"\n|;
            print qq|         \"popl %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "fpucw")
        {
            print qq|         \"fstcw %$result->{argnum}\\n\"\n|;
        }
        elsif ($result->{type} eq "fpusw")
        {
            print qq|         \"fstsw %$result->{argnum}\\n\"\n|;
        }
    }
    
    foreach my $result (@fpresults)
    {
        if (defined($result))
        {
            if ($result->{subtype} eq "ps")
            {
                print qq|         \"fstps %$result->{argnum}\\n\"\n|;
            }
            elsif ($result->{subtype} eq "pd")
            {
                print qq|         \"fstpl %$result->{argnum}\\n\"\n|;
            }
        }
        else
        {
            print qq|         \"fincstp\\n\"\n|;
        }
    }

    print qq|         \"frstor %$stateargnum\\n\"\n|;
    print qq|         \"cld\\n\"\n|;
    
    print qq|         :|;

    $prefix = " ";

    foreach my $result (@results)
    {
        if ($result->{type} eq "xmm")
        {
            print qq|$prefix\"=m\" \($result->{name}.uq[0]\), \"=m\" \($result->{name}.uq[1]\)|;
        }
        else
        {
            print qq|$prefix\"=m\" \($result->{name}\)|;
        }

        $prefix = ", ";
    }

    print qq|\n|;
    
    $prefix = "         : ";
    
    foreach my $arg (@presets, @args)
    {
        if (defined($arg->{name}))
        {
            if ($arg->{type} eq "xmm")
            {
                print qq|$prefix\"m\" \($arg->{name}.uq[0]\), \"m\" \($arg->{name}.uq[1]\)|;
            }
            else
            {
                print qq|$prefix\"m\" \($arg->{name}\)|;
            }

            $prefix = ", ";
        }
    }

    print qq|$prefix\"m\" \(state[0]\)\n|;

    $prefix = "         : ";

    foreach my $arg (@presets, @args)
    {
        if ($arg->{register} && $arg->{type} ne "st")
        {
            print qq|$prefix\"$arg->{register}\"|;
            $prefix = ", ";
        }
    }

    print qq|\n|;
    
    print qq|      \);\n|;                          
    print qq|\n|;
    
    if (@results)
    {
        print qq|      if \(|;
        
        $prefix = "";
        
        foreach my $result (@results)
        {
            my $type = $result->{type};
            my $subtype = $result->{subtype};
            my $suffix = $SubTypeSuffixes{$subtype};
            my @values = @{$result->{values}};
            
            if ($type eq "eflags")
            {
                print qq|${prefix}\($result->{name}.ud[0] & $values[0]UL\) == $values[1]UL|;
            }
            elsif ($type =~ /^fpu[cs]w$/)
            {
                print qq|${prefix}\($result->{name}.uw[0] & $values[0]\) == $values[1]|;
            }
            else
            {
                foreach my $value (0 .. $#values)
                {
                    if ($subtype eq "ps")
                    {
                        print qq|${prefix}eq_float($result->{name}.$subtype\[$value\], $values[$value]$suffix)|;
                    }
                    elsif ($subtype eq "pd")
                    {
                        print qq|${prefix}eq_double($result->{name}.$subtype\[$value\], $values[$value]$suffix)|;
                    }
                    else
                    {
                        print qq|${prefix}$result->{name}.$subtype\[$value\] == $values[$value]$suffix|;
                    }
                    
                    $prefix = " && ";
                }
            }
            
            $prefix = " &&\n          ";
        }
        
        print qq| \)\n|;
        print qq|      \{\n|;
        print qq|         printf("$test ... ok\\n");\n|;
        print qq|      \}\n|;
        print qq|      else\n|;
        print qq|      \{\n|;
        print qq|         printf("$test ... not ok\\n");\n|;
        
        foreach my $result (@results)
        {
            my $type = $result->{type};
            my $subtype = $result->{subtype};
            my $suffix = $SubTypeSuffixes{$subtype};
            my @values = @{$result->{values}};
            
            if ($type eq "eflags")
            {
                print qq|         printf("  eflags & 0x%lx = 0x%lx (expected 0x%lx)\\n", $values[0]UL, $result->{name}.ud\[0\] & $values[0]UL, $values[1]UL);\n|;
            }
            elsif ($type =~ /^fpu[cs]w$/)
            {
                print qq|         printf("  $type & 0x%x = 0x%x (expected 0x%x)\\n", $values[0], $result->{name}.uw\[0\] & $values[0], $values[1]);\n|;
            }
            else
            {
                foreach my $value (0 .. $#values)
                {
                    print qq|         printf("  $result->{name}.$subtype\[$value\] = $SubTypeFormats{$subtype} (expected $SubTypeFormats{$subtype})\\n", $result->{name}.$subtype\[$value\], $values[$value]$suffix);\n|;
                }
            }
        }
        
        print qq|      \}\n|;
    }
    else
    {
        print qq|      printf("$test ... ok\\n");\n|;
    }

    print qq|   \}\n|;
    print qq|   else\n|;
    print qq|   \{\n|;
    print qq|      printf("$test ... failed\\n");\n|;
    print qq|   \}\n|;
    print qq|\n|;
    print qq|   return;\n|;
    print qq|\}\n|;
    print qq|\n|;
}

print qq|int main(int argc, char **argv)\n|;
print qq|\{\n|;
print qq|   signal(SIGILL, handle_sigill);\n|;
print qq|\n|;

foreach my $test (@tests)
{
    print qq|   $test();\n|;
}

print qq|\n|;
print qq|   exit(0);\n|;
print qq|\}\n|;

exit 0;
