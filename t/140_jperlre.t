# This file is encoded in Latin-1.
die "This file is not encoded in Latin-1.\n" if q{あ} ne "\x82\xa0";

use Latin1;
print "1..1\n";

my $__FILE__ = ___FILE__;

if ('あxyzう' =~ /(あ.*う)/) {
    if ("$1" eq "あxyzう") {
        print "ok - 1 $^X $___FILE__ ('あxyzう' =~ /あ.*う/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あxyzう' =~ /あ.*う/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あxyzう' =~ /あ.*う/).\n";
}

__END__
