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

# Production code using a variable/hash key or method named `test` followed by an `if` block:
# Must NOT swallow the `if` block as a test!
sub check_status {
    my ($self, $flag) = @_;
    my $res = $self->test($flag);
    if ($res) {
        my $cgrep_prod_3 = 3;
        return $cgrep_prod_3;
    }
    return 0;
}

# Production hash configuration with `test => ...` followed by a sub:
# Must NOT cross `;` and swallow the subsequent production sub!
my %config = ( test => 1 );
sub after_config_prod {
    my $cgrep_prod_4 = 4;
    return $cgrep_prod_4;
}

# Test::Class style test method with `: Test` / `: Tests(...)` attribute
sub verify_addition : Test(2) {
    my $cgrep_test_2 = 2;
    is(1 + 1, 2);
}

# Test::Spec style `describe` / `it` blocks
describe 'Calculator' => sub {
    my $cgrep_test_3 = 3;
    it 'adds two numbers' => sub {
        my $cgrep_test_4 = 4;
        ok(1);
    };
};
