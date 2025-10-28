#!/usr/bin/perl
# Perl test example file

use strict;
use warnings;
use Test::More tests => 15;

# Regular helper function (not a test)
sub add {
    my ($a, $b) = @_;
    return $a + $b;
}

# Another helper function
sub multiply {
    my ($x, $y) = @_;
    return $x * $y;
}

# Helper function
sub factorial {
    my ($n) = @_;
    return 1 if $n <= 0;
    return $n * factorial($n - 1);
}

# Test::More basic tests
is(add(2, 3), 5, 'addition works correctly');
is(multiply(4, 5), 20, 'multiplication works');

# Regular package (not a test)
package Calculator;

sub new {
    my ($class, $initial) = @_;
    $initial //= 0;
    my $self = {
        value => $initial,
    };
    return bless $self, $class;
}

sub add {
    my ($self, $n) = @_;
    $self->{value} += $n;
    return $self;
}

sub get_value {
    my ($self) = @_;
    return $self->{value};
}

sub reset {
    my ($self) = @_;
    $self->{value} = 0;
}

package main;

# More tests
is(factorial(5), 120, 'factorial of 5 is 120');
is(factorial(0), 1, 'factorial of 0 is 1');

# Subtest block
subtest 'Calculator tests' => sub {
    plan tests => 5;
    
    my $calc = Calculator->new();
    is($calc->get_value(), 0, 'Calculator starts with zero');
    
    my $calc2 = Calculator->new(10);
    is($calc2->get_value(), 10, 'Calculator can be initialized with value');
    
    $calc->add(5)->add(10);
    is($calc->get_value(), 15, 'Addition works correctly');
    
    my $calc3 = Calculator->new();
    $calc3->add(-5);
    is($calc3->get_value(), -5, 'Handles negative numbers');
    
    my $calc4 = Calculator->new();
    $calc4->add(100);
    $calc4->reset();
    is($calc4->get_value(), 0, 'Reset works');
};

# Helper function
sub process_array {
    my (@arr) = @_;
    return map { $_ * 2 } grep { $_ > 0 } @arr;
}

# More tests
subtest 'String operations' => sub {
    plan tests => 3;
    
    is(reverse_string("hello"), "olleh", 'String reversal works');
    is(uc("world"), "WORLD", 'Uppercase works');
    is(reverse_string(""), "", 'Empty string handled');
};

# Helper function
sub reverse_string {
    my ($str) = @_;
    return scalar reverse $str;
}

# Regular helper
sub format_number {
    my ($n) = @_;
    return sprintf("%.2f", $n);
}

# Data processing helper
package StringHelper;

sub reverse {
    my ($str) = @_;
    return scalar reverse $str;
}

sub capitalize {
    my ($str) = @_;
    return uc $str;
}

package main;

done_testing();