# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..5\n"; }
END {print "not ok 1\n" unless $loaded;}
use Lingua::ZH::HanConvert qw(trad simple);
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my $testno = 2;

my $trad = "萬與醜專業叢東絲兩嚴喪個豐";
my $simp = "万与丑专业丛东丝两严丧个丰";

test_eq(simple($trad), $simp, $testno++);
test_eq(trad($simp), $trad, $testno++);
test_eq(simple("hello"), "hello", $testno++);
test_eq(trad("hello"), "hello", $testno++);

sub test_eq {
    print (((shift eq shift) ? "ok " : "not ok ") , shift, "\n");
}
