# This file is encoded in Latin-1.
die "This file is not encoded in ShiftJIS.\n" if q{あ} ne "\x82\xa0";

use Latin1;
print "1..1\n";

my $__FILE__ = __FILE__;

# 修飾子 C<i>, C<I> および C<j> は、C<\p{}>, C<\P{}>, POSIX C<[: :]>.
# (例えば C<\p{IsLower}>, C<[:lower:]> など) には作用しません。
# そのため、C<re('\p{Lower}', 'iI')> の代わりに
# C<re('\p{Alpha}')> を使用してください。

# Sjis ソフトウェアに C<\p{}>, C<\P{}>, POSIX C<[: :]> の機能がもともと存在しない。

print "ok - 1 $^X $__FILE__ (NULL)\n";

__END__

http://search.cpan.org/dist/ShiftJIS-Regexp/
