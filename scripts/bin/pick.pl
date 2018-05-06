use 5.016;
use strict;
use warnings;
use autodie;

use List::Util qw(shuffle);

opendir my $dh, '.';

mkdir 'temp';

my @files = shuffle grep { -f } readdir($dh);

for (1..20) {
  my $file = shift @files;

  rename $file, "temp/$file";
}
