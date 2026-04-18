#!/usr/bin/perl
use strict;
use warnings;
use Test::More;

sub compute_cgrep {
    my $CGREP_IDENTIFIER = 1;
    return 2;
}

subtest 'my test' => sub {
    my $CGREP_IDENTIFIER_TEST = 1;
    ok(1);
};

sub runTestHarness_cgrep {
    my $CGREP_IDENTIFIER = 2;
    return 1;
}
