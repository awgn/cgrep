#!/usr/bin/perl
use strict;
use warnings;
use Test::More;

sub compute_cgrep {
    my $cgrep_prod_1 = 1;
    return 2;
}

subtest 'my test' => sub {
    my $cgrep_test_1 = 1;
    ok(1);
};

sub runTestHarness_cgrep {
    my $cgrep_prod_2 = 2;
    return 1;
}
