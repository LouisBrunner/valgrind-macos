#!/usr/bin/perl

use strict;
use warnings;

our %ArgTypes = (
                 r => "reg_t",
                 mm => "mm_reg_t",
                 xmm => "xmm_reg_t",
                 m32 => "reg_t",
                 m64 => "mm_reg_t",
                 m128 => "xmm_reg_t",
                 eflags => "reg_t"
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

our @IntRegs = ( "eax", "ebx", "ecx", "edx" );

print <<EOF;
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

typedef union {
  long int sd[1];
  unsigned long int ud[1];
  float ps[1];
} reg_t;

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
} mm_reg_t __attribute__ ((aligned (8)));

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
} xmm_reg_t __attribute__ ((aligned (16)));

static sigjmp_buf catchpoint;

static void handle_sigill(int signum)
{
   siglongjmp(catchpoint, 1);
}

static int eq_float(float f1, float f2)
{
   return f1 == f2 || fabsf(f1 - f2) < fabsf(f1) * 1.5 * pow(2,-12);
}

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

    my @args = split(/\s+/, $_);
    my $insn = shift @args;
    my $result = pop @args;
    
    $tests{$insn}++;
    
    my $test = "${insn}_$tests{$insn}";
    
    push @tests, $test;
    
    print qq|static void $test(void)\n|;
    print qq|\{\n|;

    my $argc = 0;
    my @argtypes;
    my @argsubtypes;
    my @argindexes;
    my @argvalues;
    
    foreach my $arg (@args)
    {
        if ($arg =~ /^(r|mm|xmm|m32|m64|m128)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my $subtype = $2;
            my @values = split(/,/, $3);
            
            push @argtypes, $type;
            push @argsubtypes, $subtype;
            push @argvalues, undef;

            print qq|   $ArgTypes{$type} arg$argc = \{|;
            
            my $valuec = 0;
            
            foreach my $value (@values)
            {
                print qq|,| if $valuec > 0;
                print qq| .$subtype\[$valuec\] = $value$SubTypeSuffixes{$subtype}|;
                $valuec++;
            }

            print qq| \};\n|;
        }
        elsif ($arg =~ /^(imm8)\[([^\]]+)\]$/)
        {
            my $type = $1;
            my $value = $2;
            
            push @argtypes, $type;
            push @argsubtypes, undef;
            push @argvalues, $value;
        }
        else
        {
            die "Can't parse argument $arg";
        }

        $argc++;
    }
    
    my $resultarg;
    my $resulttype;
    my $resultsubtype;
    my @resultvalues;
    
    if ($result =~ /^(\d+)\.(sb|ub|sw|uw|sd|ud|sq|uq|ps|pd)\[([^\]]+)\]$/)
    {
        $resultarg = $1;
        $resulttype = $argtypes[$resultarg];
        $resultsubtype = $2;
        @resultvalues = split(/,/, $3);
    }
    elsif ($result =~ /^eflags\[([^\]]+)\]$/)
    {
        $resultarg = -1;
        $resulttype = "eflags";
        $resultsubtype = "ud";
        @resultvalues = split(/,/, $1);
    }
    else
    {
        die "Can't parse result $result";
    }
    
    print qq|   $ArgTypes{$resulttype} result;\n|;
    print qq|   char state\[108\];\n|;
    print qq|\n|;
    print qq|   if (sigsetjmp(catchpoint, 1) == 0)\n|;
    print qq|   \{\n|;
    print qq|      asm\(\n|;
    print qq|         \"fsave %\[state\]\\n\"\n|;
    
    foreach my $arg (0 .. $#args)
    {
        if ($argtypes[$arg] eq "r")
        {
            print qq|         \"movl %\[arg$arg\], %%$IntRegs[$arg]\\n\"\n|;
        }
        elsif ($argtypes[$arg] eq "mm")
        {
            print qq|         \"movq %\[arg$arg\], %%mm$arg\\n\"\n|;
        }
        elsif ($argtypes[$arg] eq "xmm")
        {
            print qq|         \"movlps 0%\[arg$arg\], %%xmm$arg\\n\"\n|;
            print qq|         \"movhps 8%\[arg$arg\], %%xmm$arg\\n\"\n|;
        }
    }
    
    if ($resulttype eq "m32")
    {
        print qq|         \"movl %\[arg$resultarg\], %%$IntRegs[$resultarg]\\n\"\n|;
        print qq|         \"movl %%$IntRegs[$resultarg], %\[result\]\\n\"\n|;
    }
    elsif ($resulttype eq "m64")
    {
        print qq|         \"movq %\[arg$resultarg\], %%mm$resultarg\\n\"\n|;
        print qq|         \"movq %%mm$resultarg, %\[result\]\\n\"\n|;
    }
    elsif ($resulttype eq "m128")
    {
        print qq|         \"movlps 0%\[arg$resultarg\], %%xmm$resultarg\\n\"\n|;
        print qq|         \"movhps 8%\[arg$resultarg\], %%xmm$resultarg\\n\"\n|;
        print qq|         \"movlps %%xmm$resultarg, 0%\[result\]\\n\"\n|;
        print qq|         \"movhps %%xmm$resultarg, 8%\[result\]\\n\"\n|;
    }

    print qq|         \"$insn|;
    
    my $prefix = " ";
    
    foreach my $arg (0 .. $#args)
    {
        if ($argtypes[$arg] eq "r")
        {
            print qq|$prefix%%$IntRegs[$arg]|;
        }
        elsif ($argtypes[$arg] eq "mm")
        {
            print qq|$prefix%%mm$arg|;
        }
        elsif ($argtypes[$arg] eq "xmm")
        {
            print qq|$prefix%%xmm$arg|;
        }
        elsif ($argtypes[$arg] eq "m32" || $argtypes[$arg] eq "m64" || $argtypes[$arg] eq "m128")
        {
            if ($arg == $resultarg)
            {
                print qq|$prefix%\[result\]|;
            }
            else
            {
                print qq|$prefix%\[arg$arg\]|;
            }
        }
        elsif ($argtypes[$arg] eq "imm8")
        {
            print qq|$prefix\$$argvalues[$arg]|;
        }

        $prefix = ", ";
    }

    print qq|\\n\"\n|;

    if ($resulttype eq "r")
    {
        print qq|         \"movl %%$IntRegs[$resultarg], %\[result\]\\n\"\n|;
    }
    elsif ($resulttype eq "mm")
    {
        print qq|         \"movq %%mm$resultarg, %\[result\]\\n\"\n|;
    }
    elsif ($resulttype eq "xmm")
    {
        print qq|         \"movlps %%xmm$resultarg, 0%\[result\]\\n\"\n|;
        print qq|         \"movhps %%xmm$resultarg, 8%\[result\]\\n\"\n|;
    }
    elsif ($resulttype eq "eflags")
    {
        print qq|         \"pushfl\\n\"\n|;
        print qq|         \"popl %\[result\]\\n\"\n|;
    }

    print qq|         \"emms\\n\"\n|;
    print qq|         \"frstor %\[state\]\\n\"\n|;

    print qq|         :|;

    if ($resulttype eq "m32" || $resulttype eq "m64" || 
        $resulttype eq "m128" || $resulttype eq "eflags")
    {
        print qq| \[result\] \"=m\" \(result\)|;
    }

    print qq|\n|;
    
    $prefix = "         : ";
    
    foreach my $arg (0 .. $#args)
    {
        if (!defined($argvalues[$arg]))
        {
            print qq|$prefix\[arg$arg\] \"m\" \(arg$arg\)|;
            $prefix = ", ";
        }
    }
    
    if ($resulttype eq "r" || $resulttype eq "mm" ||
        $resulttype eq "xmm" || $resulttype eq "elags")
    {
        print qq|$prefix\[result\] \"m\" \(result\)|;
    }

    print qq|, \[state\] \"m\" \(state[0]\)\n|;

    $prefix = "         : ";

    foreach my $arg (0 .. $#args)
    {
        if ($argtypes[$arg] eq "r" ||
            ($arg == $resultarg && $argtypes[$arg] eq "m32"))
        {
            print qq|$prefix\"$IntRegs[$arg]\"|;
            $prefix = ", ";
        }
        elsif ($argtypes[$arg] eq "mm" ||
               ($arg == $resultarg && $argtypes[$arg] eq "m64"))
        {
            print qq|$prefix\"mm$arg\"|;
            $prefix = ", ";
        }
        elsif ($argtypes[$arg] eq "xmm" ||
               ($arg == $resultarg && $argtypes[$arg] eq "m128"))
        {
            print qq|$prefix\"xmm$arg\"|;
            $prefix = ", ";
        }
    }

    print qq|\n|;
    
    print qq|      \);\n|;                          
    print qq|\n|;
    
    print qq|      if \(|;
    
    if ($resulttype eq "eflags")
    {
        print qq|\(result.ud[0] & $resultvalues[0]UL\) == $resultvalues[1]UL|;
    }
    else
    {
        $prefix = " ";
    
        foreach my $value (0 .. $#resultvalues)
        {
            if ($resultsubtype eq "ps")
            {
                print qq|${prefix}eq_float(result.$resultsubtype\[$value\], $resultvalues[$value]$SubTypeSuffixes{$resultsubtype})|;
            }
            elsif ($resultsubtype eq "pd")
            {
                print qq|${prefix}eq_double(result.$resultsubtype\[$value\], $resultvalues[$value]$SubTypeSuffixes{$resultsubtype})|;
            }
            else
            {
                print qq|${prefix}result.$resultsubtype\[$value\] == $resultvalues[$value]$SubTypeSuffixes{$resultsubtype}|;
            }

            $prefix = " && ";
        }
    }
    
    print qq| \)\n|;
    print qq|      \{\n|;
    print qq|         printf("$test ... ok\\n");\n|;
    print qq|      \}\n|;
    print qq|      else\n|;
    print qq|      \{\n|;
    print qq|         printf("$test ... not ok\\n");\n|;
    
    if ($resulttype eq "eflags")
    {
        print qq|         printf("  eflags & 0x%lx = 0x%lx (expected 0x%lx)\\n", $resultvalues[0]UL, result.ud\[0\] & $resultvalues[0]UL, $resultvalues[1]UL);\n|;
    }
    else
    {
        foreach my $value (0 .. $#resultvalues)
        {
            print qq|         printf("  $resultsubtype\[$value\] = $SubTypeFormats{$resultsubtype} (expected $SubTypeFormats{$resultsubtype})\\n", result.$resultsubtype\[$value\], $resultvalues[$value]$SubTypeSuffixes{$resultsubtype});\n|;
        }
    }

    print qq|      \}\n|;
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
