# This file is encoded in Latin-1.
die "This file is not encoded in Latin-1.\n" if q{あ} ne "\x82\xa0";

use Latin1;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あxyzえ' =~ /(あ.*う)/) {
    print "not ok - 1 $^X $__FILE__ not ('あxyzえ' =~ /あ.*う/).\n";
}
else {
    print "ok - 1 $^X $__FILE__ not ('あxyzえ' =~ /あ.*う/).\n";
}

__END__
