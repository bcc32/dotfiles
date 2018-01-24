#!/usr/bin/perl
#line 2 "/home/nomad/perl5/bin/par.pl"
eval 'exec /usr/bin/perl  -S $0 ${1+"$@"}'
    if 0; # not running under some shell

package __par_pl;

# --- This script must not use any modules at compile time ---
# use strict;

#line 158

my ($par_temp, $progname, @tmpfile);
END { if ($ENV{PAR_CLEAN}) {
    require File::Temp;
    require File::Basename;
    require File::Spec;
    my $topdir = File::Basename::dirname($par_temp);
    outs(qq{Removing files in "$par_temp"});
    File::Find::finddepth(sub { ( -d ) ? rmdir : unlink }, $par_temp);
    rmdir $par_temp;
    # Don't remove topdir because this causes a race with other apps
    # that are trying to start.

    if (-d $par_temp && $^O ne 'MSWin32') {
        # Something went wrong unlinking the temporary directory.  This
        # typically happens on platforms that disallow unlinking shared
        # libraries and executables that are in use. Unlink with a background
        # shell command so the files are no longer in use by this process.
        # Don't do anything on Windows because our parent process will
        # take care of cleaning things up.

        my $tmp = new File::Temp(
            TEMPLATE => 'tmpXXXXX',
            DIR => File::Basename::dirname($topdir),
            SUFFIX => '.cmd',
            UNLINK => 0,
        );

        print $tmp "#!/bin/sh
x=1; while [ \$x -lt 10 ]; do
   rm -rf '$par_temp'
   if [ \! -d '$par_temp' ]; then
       break
   fi
   sleep 1
   x=`expr \$x + 1`
done
rm '" . $tmp->filename . "'
";
            chmod 0700,$tmp->filename;
        my $cmd = $tmp->filename . ' >/dev/null 2>&1 &';
        close $tmp;
        system($cmd);
        outs(qq(Spawned background process to perform cleanup: )
             . $tmp->filename);
    }
} }

BEGIN {
    Internals::PAR::BOOT() if defined &Internals::PAR::BOOT;

    eval {

_par_init_env();

my $quiet = !$ENV{PAR_DEBUG};

# fix $progname if invoked from PATH
my %Config = (
    path_sep    => ($^O =~ /^MSWin/ ? ';' : ':'),
    _exe        => ($^O =~ /^(?:MSWin|OS2|cygwin)/ ? '.exe' : ''),
    _delim      => ($^O =~ /^MSWin|OS2/ ? '\\' : '/'),
);

_set_progname();
_set_par_temp();

# Magic string checking and extracting bundled modules {{{
my ($start_pos, $data_pos);
{
    local $SIG{__WARN__} = sub {};

    # Check file type, get start of data section {{{
    open _FH, '<', $progname or last;
    binmode(_FH);

    # Search for the "\nPAR.pm\n signature backward from the end of the file
    my $buf;
    my $size = -s $progname;
    my $offset = 512;
    my $idx = -1;
    while (1)
    {
        $offset = $size if $offset > $size;
        seek _FH, -$offset, 2 or die qq[seek failed on "$progname": $!];
        my $nread = read _FH, $buf, $offset;
        die qq[read failed on "$progname": $!] unless $nread == $offset;
        $idx = rindex($buf, "\nPAR.pm\n");
        last if $idx >= 0 || $offset == $size || $offset > 128 * 1024;
        $offset *= 2;
    }
    last unless $idx >= 0;

    # Seek 4 bytes backward from the signature to get the offset of the 
    # first embedded FILE, then seek to it
    $offset -= $idx - 4;
    seek _FH, -$offset, 2;
    read _FH, $buf, 4;
    seek _FH, -$offset - unpack("N", $buf), 2;
    read _FH, $buf, 4;

    $data_pos = (tell _FH) - 4;
    # }}}

    # Extracting each file into memory {{{
    my %require_list;
    while ($buf eq "FILE") {
        read _FH, $buf, 4;
        read _FH, $buf, unpack("N", $buf);

        my $fullname = $buf;
        outs(qq(Unpacking file "$fullname"...));
        my $crc = ( $fullname =~ s|^([a-f\d]{8})/|| ) ? $1 : undef;
        my ($basename, $ext) = ($buf =~ m|(?:.*/)?(.*)(\..*)|);

        read _FH, $buf, 4;
        read _FH, $buf, unpack("N", $buf);

        if (defined($ext) and $ext !~ /\.(?:pm|pl|ix|al)$/i) {
            my $filename = _tempfile("$crc$ext", $buf, 0755);
            $PAR::Heavy::FullCache{$fullname} = $filename;
            $PAR::Heavy::FullCache{$filename} = $fullname;
        }
        elsif ( $fullname =~ m|^/?shlib/| and defined $ENV{PAR_TEMP} ) {
            my $filename = _tempfile("$basename$ext", $buf, 0755);
            outs("SHLIB: $filename\n");
        }
        else {
            $require_list{$fullname} =
            $PAR::Heavy::ModuleCache{$fullname} = {
                buf => $buf,
                crc => $crc,
                name => $fullname,
            };
        }
        read _FH, $buf, 4;
    }
    # }}}

    local @INC = (sub {
        my ($self, $module) = @_;

        return if ref $module or !$module;

        my $filename = delete $require_list{$module} || do {
            my $key;
            foreach (keys %require_list) {
                next unless /\Q$module\E$/;
                $key = $_; last;
            }
            delete $require_list{$key} if defined($key);
        } or return;

        $INC{$module} = "/loader/$filename/$module";

        if ($ENV{PAR_CLEAN} and defined(&IO::File::new)) {
            my $fh = IO::File->new_tmpfile or die $!;
            binmode($fh);
            print $fh $filename->{buf};
            seek($fh, 0, 0);
            return $fh;
        }
        else {
            my $filename = _tempfile("$filename->{crc}.pm", $filename->{buf});

            open my $fh, '<', $filename or die "can't read $filename: $!";
            binmode($fh);
            return $fh;
        }

        die "Bootstrapping failed: cannot find $module!\n";
    }, @INC);

    # Now load all bundled files {{{

    # initialize shared object processing
    require XSLoader;
    require PAR::Heavy;
    require Carp::Heavy;
    require Exporter::Heavy;
    PAR::Heavy::_init_dynaloader();

    # now let's try getting helper modules from within
    require IO::File;

    # load rest of the group in
    while (my $filename = (sort keys %require_list)[0]) {
        #local $INC{'Cwd.pm'} = __FILE__ if $^O ne 'MSWin32';
        unless ($INC{$filename} or $filename =~ /BSDPAN/) {
            # require modules, do other executable files
            if ($filename =~ /\.pmc?$/i) {
                require $filename;
            }
            else {
                # Skip ActiveState's sitecustomize.pl file:
                do $filename unless $filename =~ /sitecustomize\.pl$/;
            }
        }
        delete $require_list{$filename};
    }

    # }}}

    last unless $buf eq "PK\003\004";
    $start_pos = (tell _FH) - 4;
}
# }}}

# Argument processing {{{
my @par_args;
my ($out, $bundle, $logfh, $cache_name);

delete $ENV{PAR_APP_REUSE}; # sanitize (REUSE may be a security problem)

$quiet = 0 unless $ENV{PAR_DEBUG};
# Don't swallow arguments for compiled executables without --par-options
if (!$start_pos or ($ARGV[0] eq '--par-options' && shift)) {
    my %dist_cmd = qw(
        p   blib_to_par
        i   install_par
        u   uninstall_par
        s   sign_par
        v   verify_par
    );

    # if the app is invoked as "appname --par-options --reuse PROGRAM @PROG_ARGV",
    # use the app to run the given perl code instead of anything from the
    # app itself (but still set up the normal app environment and @INC)
    if (@ARGV and $ARGV[0] eq '--reuse') {
        shift @ARGV;
        $ENV{PAR_APP_REUSE} = shift @ARGV;
    }
    else { # normal parl behaviour

        my @add_to_inc;
        while (@ARGV) {
            $ARGV[0] =~ /^-([AIMOBLbqpiusTv])(.*)/ or last;

            if ($1 eq 'I') {
                push @add_to_inc, $2;
            }
            elsif ($1 eq 'M') {
                eval "use $2";
            }
            elsif ($1 eq 'A') {
                unshift @par_args, $2;
            }
            elsif ($1 eq 'O') {
                $out = $2;
            }
            elsif ($1 eq 'b') {
                $bundle = 'site';
            }
            elsif ($1 eq 'B') {
                $bundle = 'all';
            }
            elsif ($1 eq 'q') {
                $quiet = 1;
            }
            elsif ($1 eq 'L') {
                open $logfh, ">>", $2 or die "XXX: Cannot open log: $!";
            }
            elsif ($1 eq 'T') {
                $cache_name = $2;
            }

            shift(@ARGV);

            if (my $cmd = $dist_cmd{$1}) {
                delete $ENV{'PAR_TEMP'};
                init_inc();
                require PAR::Dist;
                &{"PAR::Dist::$cmd"}() unless @ARGV;
                &{"PAR::Dist::$cmd"}($_) for @ARGV;
                exit;
            }
        }

        unshift @INC, @add_to_inc;
    }
}

# XXX -- add --par-debug support!

# }}}

# Output mode (-O) handling {{{
if ($out) {
    {
        #local $INC{'Cwd.pm'} = __FILE__ if $^O ne 'MSWin32';
        require IO::File;
        require Archive::Zip;
    }

    my $par = shift(@ARGV);
    my $zip;


    if (defined $par) {
        # increase the chunk size for Archive::Zip so that it will find the EOCD
        # even if more stuff has been appended to the .par
        Archive::Zip::setChunkSize(128*1024);

        open my $fh, '<', $par or die "Cannot find '$par': $!";
        binmode($fh);
        bless($fh, 'IO::File');

        $zip = Archive::Zip->new;
        ( $zip->readFromFileHandle($fh, $par) == Archive::Zip::AZ_OK() )
            or die "Read '$par' error: $!";
    }


    my %env = do {
        if ($zip and my $meta = $zip->contents('META.yml')) {
            $meta =~ s/.*^par:$//ms;
            $meta =~ s/^\S.*//ms;
            $meta =~ /^  ([^:]+): (.+)$/mg;
        }
    };

    # Open input and output files {{{
    local $/ = \4;

    if (defined $par) {
        open PAR, '<', $par or die "$!: $par";
        binmode(PAR);
        die "$par is not a PAR file" unless <PAR> eq "PK\003\004";
    }

    CreatePath($out) ;
    
    my $fh = IO::File->new(
        $out,
        IO::File::O_CREAT() | IO::File::O_WRONLY() | IO::File::O_TRUNC(),
        0777,
    ) or die $!;
    binmode($fh);

    $/ = (defined $data_pos) ? \$data_pos : undef;
    seek _FH, 0, 0;
    my $loader = scalar <_FH>;
    if (!$ENV{PAR_VERBATIM} and $loader =~ /^(?:#!|\@rem)/) {
        require PAR::Filter::PodStrip;
        PAR::Filter::PodStrip->new->apply(\$loader, $0)
    }
    foreach my $key (sort keys %env) {
        my $val = $env{$key} or next;
        $val = eval $val if $val =~ /^['"]/;
        my $magic = "__ENV_PAR_" . uc($key) . "__";
        my $set = "PAR_" . uc($key) . "=$val";
        $loader =~ s{$magic( +)}{
            $magic . $set . (' ' x (length($1) - length($set)))
        }eg;
    }
    $fh->print($loader);
    $/ = undef;
    # }}}

    # Write bundled modules {{{
    if ($bundle) {
        require PAR::Heavy;
        PAR::Heavy::_init_dynaloader();
        init_inc();

        require_modules();

        my @inc = grep { !/BSDPAN/ } 
                       grep {
                           ($bundle ne 'site') or
                           ($_ ne $Config::Config{archlibexp} and
                           $_ ne $Config::Config{privlibexp});
                       } @INC;

        # Now determine the files loaded above by require_modules():
        # Perl source files are found in values %INC and DLLs are
        # found in @DynaLoader::dl_shared_objects.
        my %files;
        $files{$_}++ for @DynaLoader::dl_shared_objects, values %INC;

        my $lib_ext = $Config::Config{lib_ext};
        my %written;

        foreach (sort keys %files) {
            my ($name, $file);

            foreach my $dir (@inc) {
                if ($name = $PAR::Heavy::FullCache{$_}) {
                    $file = $_;
                    last;
                }
                elsif (/^(\Q$dir\E\/(.*[^Cc]))\Z/i) {
                    ($file, $name) = ($1, $2);
                    last;
                }
                elsif (m!^/loader/[^/]+/(.*[^Cc])\Z!) {
                    if (my $ref = $PAR::Heavy::ModuleCache{$1}) {
                        ($file, $name) = ($ref, $1);
                        last;
                    }
                    elsif (-f "$dir/$1") {
                        ($file, $name) = ("$dir/$1", $1);
                        last;
                    }
                }
            }

            next unless defined $name and not $written{$name}++;
            next if !ref($file) and $file =~ /\.\Q$lib_ext\E$/;
            outs( join "",
                qq(Packing "), ref $file ? $file->{name} : $file,
                qq("...)
            );

            my $content;
            if (ref($file)) {
                $content = $file->{buf};
            }
            else {
                open FILE, '<', $file or die "Can't open $file: $!";
                binmode(FILE);
                $content = <FILE>;
                close FILE;

                PAR::Filter::PodStrip->new->apply(\$content, $file)
                    if !$ENV{PAR_VERBATIM} and $name =~ /\.(?:pm|ix|al)$/i;

                PAR::Filter::PatchContent->new->apply(\$content, $file, $name);
            }

            outs(qq(Written as "$name"));
            $fh->print("FILE");
            $fh->print(pack('N', length($name) + 9));
            $fh->print(sprintf(
                "%08x/%s", Archive::Zip::computeCRC32($content), $name
            ));
            $fh->print(pack('N', length($content)));
            $fh->print($content);
        }
    }
    # }}}

    # Now write out the PAR and magic strings {{{
    $zip->writeToFileHandle($fh) if $zip;

    $cache_name = substr $cache_name, 0, 40;
    if (!$cache_name and my $mtime = (stat($out))[9]) {
        my $ctx = eval { require Digest::SHA; Digest::SHA->new(1) }
            || eval { require Digest::SHA1; Digest::SHA1->new }
            || eval { require Digest::MD5; Digest::MD5->new };

        # Workaround for bug in Digest::SHA 5.38 and 5.39
        my $sha_version = eval { $Digest::SHA::VERSION } || 0;
        if ($sha_version eq '5.38' or $sha_version eq '5.39') {
            $ctx->addfile($out, "b") if ($ctx);
        }
        else {
            if ($ctx and open(my $fh, "<$out")) {
                binmode($fh);
                $ctx->addfile($fh);
                close($fh);
            }
        }

        $cache_name = $ctx ? $ctx->hexdigest : $mtime;
    }
    $cache_name .= "\0" x (41 - length $cache_name);
    $cache_name .= "CACHE";
    $fh->print($cache_name);
    $fh->print(pack('N', $fh->tell - length($loader)));
    $fh->print("\nPAR.pm\n");
    $fh->close;
    chmod 0755, $out;
    # }}}

    exit;
}
# }}}

# Prepare $progname into PAR file cache {{{
{
    last unless defined $start_pos;

    _fix_progname();

    # Now load the PAR file and put it into PAR::LibCache {{{
    require PAR;
    PAR::Heavy::_init_dynaloader();


    {
        #local $INC{'Cwd.pm'} = __FILE__ if $^O ne 'MSWin32';
        require File::Find;
        require Archive::Zip;
    }
    my $zip = Archive::Zip->new;
    my $fh = IO::File->new;
    $fh->fdopen(fileno(_FH), 'r') or die "$!: $@";
    $zip->readFromFileHandle($fh, $progname) == Archive::Zip::AZ_OK() or die "$!: $@";

    push @PAR::LibCache, $zip;
    $PAR::LibCache{$progname} = $zip;

    $quiet = !$ENV{PAR_DEBUG};
    outs(qq(\$ENV{PAR_TEMP} = "$ENV{PAR_TEMP}"));

    if (defined $ENV{PAR_TEMP}) { # should be set at this point!
        foreach my $member ( $zip->members ) {
            next if $member->isDirectory;
            my $member_name = $member->fileName;
            next unless $member_name =~ m{
                ^
                /?shlib/
                (?:$Config::Config{version}/)?
                (?:$Config::Config{archname}/)?
                ([^/]+)
                $
            }x;
            my $extract_name = $1;
            my $dest_name = File::Spec->catfile($ENV{PAR_TEMP}, $extract_name);
            if (-f $dest_name && -s _ == $member->uncompressedSize()) {
                outs(qq(Skipping "$member_name" since it already exists at "$dest_name"));
            } else {
                outs(qq(Extracting "$member_name" to "$dest_name"));
                $member->extractToFileNamed($dest_name);
                chmod(0555, $dest_name) if $^O eq "hpux";
            }
        }
    }
    # }}}
}
# }}}

# If there's no main.pl to run, show usage {{{
unless ($PAR::LibCache{$progname}) {
    die << "." unless @ARGV;
Usage: $0 [ -Alib.par ] [ -Idir ] [ -Mmodule ] [ src.par ] [ program.pl ]
       $0 [ -B|-b ] [-Ooutfile] src.par
.
    $ENV{PAR_PROGNAME} = $progname = $0 = shift(@ARGV);
}
# }}}

sub CreatePath {
    my ($name) = @_;
    
    require File::Basename;
    my ($basename, $path, $ext) = File::Basename::fileparse($name, ('\..*'));
    
    require File::Path;
    
    File::Path::mkpath($path) unless(-e $path); # mkpath dies with error
}

sub require_modules {
    #local $INC{'Cwd.pm'} = __FILE__ if $^O ne 'MSWin32';

    require lib;
    require DynaLoader;
    require integer;
    require strict;
    require warnings;
    require vars;
    require Carp;
    require Carp::Heavy;
    require Errno;
    require Exporter::Heavy;
    require Exporter;
    require Fcntl;
    require File::Temp;
    require File::Spec;
    require XSLoader;
    require Config;
    require IO::Handle;
    require IO::File;
    require Compress::Zlib;
    require Archive::Zip;
    require PAR;
    require PAR::Heavy;
    require PAR::Dist;
    require PAR::Filter::PodStrip;
    require PAR::Filter::PatchContent;
    require attributes;
    eval { require Cwd };
    eval { require Win32 };
    eval { require Scalar::Util };
    eval { require Archive::Unzip::Burst };
    eval { require Tie::Hash::NamedCapture };
    eval { require PerlIO; require PerlIO::scalar };
    eval { require utf8 };
}

# The C version of this code appears in myldr/mktmpdir.c
# This code also lives in PAR::SetupTemp as set_par_temp_env!
sub _set_par_temp {
    if (defined $ENV{PAR_TEMP} and $ENV{PAR_TEMP} =~ /(.+)/) {
        $par_temp = $1;
        return;
    }

    foreach my $path (
        (map $ENV{$_}, qw( PAR_TMPDIR TMPDIR TEMPDIR TEMP TMP )),
        qw( C:\\TEMP /tmp . )
    ) {
        next unless defined $path and -d $path and -w $path;
        my $username;
        my $pwuid;
        # does not work everywhere:
        eval {($pwuid) = getpwuid($>) if defined $>;};

        if ( defined(&Win32::LoginName) ) {
            $username = &Win32::LoginName;
        }
        elsif (defined $pwuid) {
            $username = $pwuid;
        }
        else {
            $username = $ENV{USERNAME} || $ENV{USER} || 'SYSTEM';
        }
        $username =~ s/\W/_/g;

        my $stmpdir = "$path$Config{_delim}par-".unpack("H*", $username);
        mkdir $stmpdir, 0755;
        if (!$ENV{PAR_CLEAN} and my $mtime = (stat($progname))[9]) {
            open (my $fh, "<". $progname);
            seek $fh, -18, 2;
            sysread $fh, my $buf, 6;
            if ($buf eq "\0CACHE") {
                seek $fh, -58, 2;
                sysread $fh, $buf, 41;
                $buf =~ s/\0//g;
                $stmpdir .= "$Config{_delim}cache-" . $buf;
            }
            else {
                my $ctx = eval { require Digest::SHA; Digest::SHA->new(1) }
                    || eval { require Digest::SHA1; Digest::SHA1->new }
                    || eval { require Digest::MD5; Digest::MD5->new };

                # Workaround for bug in Digest::SHA 5.38 and 5.39
                my $sha_version = eval { $Digest::SHA::VERSION } || 0;
                if ($sha_version eq '5.38' or $sha_version eq '5.39') {
                    $ctx->addfile($progname, "b") if ($ctx);
                }
                else {
                    if ($ctx and open(my $fh, "<$progname")) {
                        binmode($fh);
                        $ctx->addfile($fh);
                        close($fh);
                    }
                }

                $stmpdir .= "$Config{_delim}cache-" . ( $ctx ? $ctx->hexdigest : $mtime );
            }
            close($fh);
        }
        else {
            $ENV{PAR_CLEAN} = 1;
            $stmpdir .= "$Config{_delim}temp-$$";
        }

        $ENV{PAR_TEMP} = $stmpdir;
        mkdir $stmpdir, 0755;
        last;
    }

    $par_temp = $1 if $ENV{PAR_TEMP} and $ENV{PAR_TEMP} =~ /(.+)/;
}


# check if $name (relative to $par_temp) already exists;
# if not, create a file with a unique temporary name, 
# fill it with $contents, set its file mode to $mode if present;
# finaly rename it to $name; 
# in any case return the absolute filename
sub _tempfile {
    my ($name, $contents, $mode) = @_;

    my $fullname = "$par_temp/$name";
    unless (-e $fullname) {
        my $tempname = "$fullname.$$";

        open my $fh, '>', $tempname or die "can't write $tempname: $!";
        binmode $fh;
        print $fh $contents;
        close $fh;
        chmod $mode, $tempname if defined $mode;

        rename($tempname, $fullname) or unlink($tempname);
        # NOTE: The rename() error presumably is something like ETXTBSY 
        # (scenario: another process was faster at extraction $fullname
        # than us and is already using it in some way); anyway, 
        # let's assume $fullname is "good" and clean up our copy.
    }

    return $fullname;
}

# same code lives in PAR::SetupProgname::set_progname
sub _set_progname {
    if (defined $ENV{PAR_PROGNAME} and $ENV{PAR_PROGNAME} =~ /(.+)/) {
        $progname = $1;
    }

    $progname ||= $0;

    if ($ENV{PAR_TEMP} and index($progname, $ENV{PAR_TEMP}) >= 0) {
        $progname = substr($progname, rindex($progname, $Config{_delim}) + 1);
    }

    if (!$ENV{PAR_PROGNAME} or index($progname, $Config{_delim}) >= 0) {
        if (open my $fh, '<', $progname) {
            return if -s $fh;
        }
        if (-s "$progname$Config{_exe}") {
            $progname .= $Config{_exe};
            return;
        }
    }

    foreach my $dir (split /\Q$Config{path_sep}\E/, $ENV{PATH}) {
        next if exists $ENV{PAR_TEMP} and $dir eq $ENV{PAR_TEMP};
        $dir =~ s/\Q$Config{_delim}\E$//;
        (($progname = "$dir$Config{_delim}$progname$Config{_exe}"), last)
            if -s "$dir$Config{_delim}$progname$Config{_exe}";
        (($progname = "$dir$Config{_delim}$progname"), last)
            if -s "$dir$Config{_delim}$progname";
    }
}

sub _fix_progname {
    $0 = $progname ||= $ENV{PAR_PROGNAME};
    if (index($progname, $Config{_delim}) < 0) {
        $progname = ".$Config{_delim}$progname";
    }

    # XXX - hack to make PWD work
    my $pwd = (defined &Cwd::getcwd) ? Cwd::getcwd()
                : ((defined &Win32::GetCwd) ? Win32::GetCwd() : `pwd`);
    chomp($pwd);
    $progname =~ s/^(?=\.\.?\Q$Config{_delim}\E)/$pwd$Config{_delim}/;

    $ENV{PAR_PROGNAME} = $progname;
}

sub _par_init_env {
    if ( $ENV{PAR_INITIALIZED}++ == 1 ) {
        return;
    } else {
        $ENV{PAR_INITIALIZED} = 2;
    }

    for (qw( SPAWNED TEMP CLEAN DEBUG CACHE PROGNAME ARGC ARGV_0 ) ) {
        delete $ENV{'PAR_'.$_};
    }
    for (qw/ TMPDIR TEMP CLEAN DEBUG /) {
        $ENV{'PAR_'.$_} = $ENV{'PAR_GLOBAL_'.$_} if exists $ENV{'PAR_GLOBAL_'.$_};
    }

    my $par_clean = "__ENV_PAR_CLEAN__               ";

    if ($ENV{PAR_TEMP}) {
        delete $ENV{PAR_CLEAN};
    }
    elsif (!exists $ENV{PAR_GLOBAL_CLEAN}) {
        my $value = substr($par_clean, 12 + length("CLEAN"));
        $ENV{PAR_CLEAN} = $1 if $value =~ /^PAR_CLEAN=(\S+)/;
    }
}

sub outs {
    return if $quiet;
    if ($logfh) {
        print $logfh "@_\n";
    }
    else {
        print "@_\n";
    }
}

sub init_inc {
    require Config;
    push @INC, grep defined, map $Config::Config{$_}, qw(
        archlibexp privlibexp sitearchexp sitelibexp
        vendorarchexp vendorlibexp
    );
}

########################################################################
# The main package for script execution

package main;

require PAR;
unshift @INC, \&PAR::find_par;
PAR->import(@par_args);

die qq(par.pl: Can't open perl script "$progname": No such file or directory\n)
    unless -e $progname;

do $progname;
CORE::exit($1) if ($@ =~/^_TK_EXIT_\((\d+)\)/);
die $@ if $@;

};

$::__ERROR = $@ if $@;
}

CORE::exit($1) if ($::__ERROR =~/^_TK_EXIT_\((\d+)\)/);
die $::__ERROR if $::__ERROR;

1;

#line 1010

__END__
PK     \8L               lib/PK     \8L               script/PK    \8L�<  �     MANIFESTu��N�0E���i+��*6VyI]�D��H�(HN�4F~ۥ��NLv�f����k ��(RU�Vr
��;�q�8�1��y�@�#\̗w��0	�gDZ(�:�DQ��W,Vst���OVP'�z�lwSi�Y,���4=���`ik��R�'P�+Rg�O�a�ժ�*-r;Ĥ��~U<"�ǲ�J&��d~�M��l��W6ҝ{���jzk�km�RW�ۛ���jۺ�L�)>��=���Q:pnG�7���5�h�r|E�ت���\Op8��9�;ۆ�Y���<%�j�\�3����� SLr���Wg�q�PK    \8LV��h�   �      META.yml-�K�0��=���Htӝ7 ^�)�H&�)�!!ƻ;�����c��M�g��Y���\�G WN�)�vA�ĭ]��J4�B�M�W�V��ߥF�{�4tjB�dz3���v�z�n�/LY�зݥo��!gU�9n���h�@d�ؖ���4��0�B˙�y� �W�PK    \8L�;�,�.  n�     lib/Stow.pm�}�{�6����+�]K�,%��v֙�I��o'����&S}��\S�JRv}S�o��� ��Iڭ�ގC��yᜃ��;�e����t��y���ĩ���Wev>\�776��4:����P-����Ʋ��(�xRާ�ϣ<����>�z���yo��;I��S1ɳ���3Y"���9�����Ɖ��{�-.��<;�΋W9��x���'ls,����������}_�	�+d9�ʣ��8�g2���y��ߕ�(��l*7D���Y��QyR��e��8b�?�(�i�#g�\l�ȳ�<�Fs)��r�s�_�������C|���ߎ�����GO��>�?�����6C컛d�(ٍ�SZ����o���q����é�Eˤ��1���X��(�����ţ��|���x���1�lE�~�x}"߀Q�H�z�lQ�YZ�`XF�Z�E�9�9}�d��G�?}=~��5`�|B��4� %��_��z\��e�R���d~���z�KO�Ǔl�&�ֆ���>D�Z���,K�@�;�c�O��_���@�yO���%s����w�ˍ�by$Ry.�R����.d2L'�XM?�$0� �Y����o�qc���l�pc���8J�ߊˁ�Z��ݯ�F\��$[��%4��^��"�\���s9G����M�\��Y��j�|b�Mb�+ ��'%����<ɖ��q���:�e)�7�Þ˔&-���m���N�������5|�T& zE�K�C�z�����z4h���3��_{#�À�i��mox��9��-�:+ X��$;N��������8SD����c����߰P�D<��c��|a֍ˋ^�ߴ5��Z�5.U+~�mQ⍁�zΛ8���
R�"��2O����Kf,g
�,�t�W0#%s.5���L�~w}j�?�����W�Ǡ�y�j_-��<��1,J�9����&S
����e&��\�FgQ�d�V��$���;����7�϶G��4R���b�ګ�&�z�Ш�D��]�� p*���]
��g��r�Q��,Eݳ{�+�ɂ*ܔ��SO	<��	T��W�Q�y�$�'0��5#�>6�&�Մ7�H�fdk ��@�fJ����E`~OQ{��I}u5��JO�9h:����"�&@��O!��%�����~�E]�T���Q:DF��,�;	��W���;p��Ÿ̈�.i���+�e�@?Y	�E����
I��#j��]N�9 .ZBK�GdI�9+P�l�{A��Ɔ@F7,.��R�:�*.�R�a�h�F3���_,�l�8J�e:�Τx$e*@��R��PϹ"�ⴸ���L���f�Ag��7���ũ����
Ys��9��4���!Ѵ��j,�m)�e<��3���Lr	�C��\����_}�oy�@���yʭ�=�Ę׍���L�_��mS�9:�U�,M.H����v
"ކg�q�hѪ.�I1�]J���ߏ,�<Z ni���'H�9�"K��D� '@�@e�	�/a2��>amY2�Lt���)�z��}�\�V��	��zJs��?PV6�a���C�3�OQ���Z���s�oq��p��p�X%�N�?aʇ�_���\��/`>�G.P�O��-��]�"�K���1Nϲ�E�`�@+E�H���7������v�$J�JJ����F�Z� Dl�YO��YaW
]�ڷ�3�l��G�e{`m�{u�70��� hW�Y)v�� ��m��׺W�)���|1B1\F +�G�	lz3p��JͿ T��D+%��>(�7>4��J����2�I�	��TAx�,��{�mG ��vjj�;Ý�����I!;�����K	��֨CO��p�YՈǼ�[���O�[����ȫkp��Ow!�f�-�n �_9�<�����֛�z_{�� ����<"\������5��C��|�~�=зr�,��J�Zo�IpTȜ����Ah�H�'��/������g`{ ��ٲD&�Ǩ�@/ig]�\�QDn�-��c�� ��2w���;-�䁅��N�`�{
6ɕ�f:��fڲVi��"�)i������}z�zQ_�+��E�w\�~���fx�p� ��D)ЈZ�����W��ˀK��=�8���ڍ49A���0��Nm�<��7P��������P�^������������x�7��`��[�,��msܣU5��İ���s�+�|6#	��Zh�<S7�X�g-#�����8�,s��fu(���쀳I���݆q�(�L�ϵ��}1��>�e������w�OE#�HjAZ���P_�ܨ9{4P�9x�)�������sD�����`eЈ$t�\�iO�p[V$fQ%*�����f�%�B��X�uN���C=j"�=@\�G6|=B�X5^4`h�X}Z}Mq�WT�T�g�L�_�䰬`SH�EHh�mk��FA��3�R�U�9И��������SGﱲnN�e2��B�eڢ����{@�D0>����Ϥt-��s�A�$�Tȁ����R�����������w���{R[;������Q�2BX�{� ��	��݊�qz��6+š��ڤ�5،@#2���r �#f'��B�����'/<��zS܊�Z��[��"a
E!�)�G�J�& �G������=�K�*p���P�l�?���ԁ����_�����n���M�M�I�	��-�z���Âc�hY<sـr��}{���[!�ao"o��ꢄ�n��Q��a2��o2�<�6�e@v� �C�5_Y
���Z�+���DR{Zu��p\Ф�����:��������Bkd��F��*��I�n�'n@K�m*�Y��e�V���ʑ��C�#;�䔕:�v���:"��@O`�(�ׂ�hH�%�%��o���N�߾y4�9Y@M�u�������Tc����a��;I7I&���|T�oe�k����j��j1�
��/A_pp�k�L�q¨d
m�OdN_��@���_;k�C7�W����w��ﯬ��a+�-�@�|*�PR��i~300�s�"z�0�a�6.�$+�e:ňpz��?�*��ë�,���?V�Ze��b�5"�+ač��U��to��ʫ4	�Hv�r�!VK��y��j���Ob�Zs��d͸FM���0�@�E�>�����G�� ,߀,Կʹ���+^�vm�̔HM�/M���ܶ��B8�WPfC���sy��)ۄrE���� 	r[��:�r�	<�.�� �s�1l�����V<����yD"���9'#��L������Z@,�!Vk����e�J�,�*�z�7�h6�n���螊s��M�	'z=�p�u���m)�Z��B�7Ơ�ßc*��#]jB�ƈg~�:SZv���AKÕ�S��/-�­y�>[�k~ݍă������N��Y+�I3�ZHC�z�,6�\�R�5��f�Pl�2h���+�N�s�
�R���� ����I��s�޳Hgm1N�k�ł�`i#�}x��f�v��ږV_6Zl���݂z+�E����]7�WG�
���f�pǪ|��g�yK�׍+��S	��{	f�4�m��`!㯅�C$��>@s	�����w����A��k��6mp��h�]�B�~�*��z�}�~�.��E[�T��H����"?'��$��)�C�w����|���R4��L��x2�/������2_bj�l�'�&f�Y*o�#�amڝ���my=כ��S�^�=���h�鳖ͼxTl�������w@jz�Y3����\�М,��BܫzRS�G�����]�����ӫ́O��N�B�60�և}�	o��.(>֑4@�&�ZC�J���t��p�kV�*3C�
c��Y�Q��RI���wߦD��L%i���R���n^J�[��7�3��¡���:�}�`�?��|�����Z�Nm�D��;���������t}��bC����	t�IL�dO��#���A�o��Mf��uׁCg��%Ra�oE���i�����\��͵��ՈA�fY6����A/�p�ģ�t�C�5#�;�_,N��:��g\T��������k��I)^C{Z���?S���R����z�H]�7��?�En[W{��-��Z��v^����A-��t����C�V��JӅ�\?�RѤ��ύ�_%�F4ۋ\�^���'����R���l-qBV��~3�B抠�q)�,�Ђh�����Y=?�;�s9��礟Y[�����cn�"R�V9������N7~OL����#o#7�n5,~顐�����H���N�zۀ�܏�3�v@�K��.���y^�ʴ8���t'����_���<��S[�h*g���R��D:T̊�Z1�������	��
h��"W %��a\�tq�_'���e_іӘU	��b6?��fV��������6���z�l�C��e�����q�ܹu]��λ�͸�~l�c:R0��e�$�*��M����F�a)oW�-��3+�jj^B<J����b���A� psG��4��Ia��f���y���V�ٟǻ~'^�uh��V�qFq�\��Ц���������a�?�#��P�A��muY�2|�ן7�]��<��x�S8P^#fg�V��q�9��ݑ��vHCoѹss
�z�r:d��:�;����x���/�X��r9�"��g�{�t(o ��Y}��4�벛��?0jWO��ep����4����Zc9�����3�F���0���N�7�����N�X�=8y���k��k�����i����X��J�c�3:9�]"XI�FuԚ�F�v'㯙�~g�e$�1i��х6J�$���`���\Ώ�H]�m��R�4'�-U���Q���!�P٣X����j&��N�O�6��+��9��@d���;;�I�IV�<�$�<�ً�]׿�=XpM��7֗詚�!e[���c����['��
��i6RN���$�k�UR�x�����%�Mp��.N"�h�ȳhr2h��)��_t�������}�W�;�L�n-����fjN���PUT����lI��=�,u+����_�ң�!F�0�iU���B�����}��Z`��[�b�h����2���ǯ��#F����~D��KLo�Hvd��h�w�L�x�TX)n#�����Ve�٬$��u�bd�*�5�j<w_�G��R��,�זkꕌ��	a����ub�N�|�n���퀵�L��������T���},;;UHA}1�ɟ�}v9��9:n��]1��-�lQ�D�ݿ��[y���,�%|�vh/��o�=���>`I�*��3� ��gH���S�x���i���k��_t8�1�r0�5��0�5o��?F�g���!@6�*F�'V���\�ۮ�]�W|&���m����~Uo�^���YdH���3@}���d�a-b�d�r�F�6���3N��	�#f�����Z!��γ�W*,Y��
~h.<��u���� ��=4��<,����p�|Q��:�k��8l5�|�G�٨��Aգ��п+Ԯ����7P@��,��]Gg�luӰ�8u� K�X1�s*d4�ٔj[5_{��������-�r����ud�:�C[��ʸ�ke����Ӷ�o܆#���K��r�8W��}GI69�e�|˕�oe�ZgC219������YUJЏe�k0��"ϖ&��gG���HE���b��@/2�ߧ$lfև3���M��1�CW�.`N�QR���&L2�޳3�@D�x��	���:�bi���e����k��$��.l3L�ok�.l��a�sR "7�h�e�zv��׃�'7`��Qn�c�P�@ ��W�5�^�����_��s/0z[u���v�_䖽)�-Ǎ�S�e�3Di:iv~�D<ec�W�5Κ�B���j�� �� ��\�x[(9�?���?���ѳ�]�b[��ǐ:kQ�W���A:�k��N	�MK�b�2o׷���>���4�mײv�G�IӔš[�^w.<�����h&��j���sH��q4�Wu�FV#������u;���zT7*�Ŀ9�U�%�
��Խ�^�V�6�j��T�J�@^80���Z4֧��w4�43���ݪ��)B�P��m�eMg^����B3�MRjo��k�5'����E�݊�Szne��Ee,)�V����2QU�I FIx�E�|J%�,�Y�}'�W ����yu�����"�>Z����;��mљ����*/��dpwj�KCOڙ���lx�����=���9�F,�3�������㭩`�3�`�Oe��$�6e���a�E��$4Q�T.d=��$C� �t"��<��)��D:�B;�0���,�q�P1��No�<z3|3]Z�U�����A�+��Q;m/���j*��h�m��{�%�vvՂ: M�zVk��Ukb��cF&����*>�mjy���hF����}����������&���dO����6Y�v��|���cl��x43�t�*[XJ˄utY�"D�ο�%3�P=ǽK��Zu�݄4�L�G��?@�}�ZmV��,�j��˹b���z}�n��������B�4Kb_�� @uC�/4�ե�ʯ�N�P$#Η$s�Ԃ�Ex��$�*��r�qK��L�{+�s�,��7 p�����O�ƽ hu��JW�pƦKj������P�.�o��%��c�,N�÷{^V�hݴ�S5�{��_����[El���'�Y�K��U�s�2B��8#���j�A�`��v��*�����Lڹ������������`�Jп|S�����&��(W�թg&4�m:U#Z��"�ε��YL����#Lѥ7�w�Pܟ�׳i���W���5�̊����A���o�,���_<z��G��ǘ뻷�ɹ.|�݉����L�1K����M�b9� l��TXՏ}S의�T�Ѫh�9!�i:)�ӕvwy�
R���~�p\�9e)Y�A{%b�E��Pԃ@��'��jS�챝Ԑ��X�A����9͏x�� �Q��֪���bsd?m9o�t��:����W�b��v�yÛMhp�g����_7�p�Tc�׷�n	XJ�>�
I9Z,�����Fw�ܯO��b������G�9�>����Kxv|��y__�_['��㈠����\!,'��I�GI9���+���#*[�Wpd7*��C!�1t�/��;�ff���
�(�b$�<J��t(P$��|7�n���%0T��RZ����D�/w���8����Y��^B{��� AסE�yt�6�s�8]�MSRs-�����p~��d(��$���=|w�����ۃ��V�Ɵ�d��T����oj��O	@�����u�	@X�3%O�,���>��Qڎ1�`�={<f�-�xj���_;���8�Xlݻ�0��'-�܅����%�H��I9V�s���p��z����5pr���<�K�%�s`з�#gѹ6��s9���'�B���/�e�7�����e�����^���ӋWEQ͓�6�U�~��CESws����o�PƃVb;��Tͫo�|z�3���^�mV#�ۥ�.�n����)AekD�k�k�����L}b 9R#��L`�j���� ��v}��|dQO��ZiԎzu;`���#�Y3|�R���79��
���7�g���l�Oȋ5�yk�>K6�{��>&����nV��,ǉ���Ut8 �����㛭�������E���b�-񔔧%�
�����ߧ���3@ ��D/[#76�ƩS�e�-)���mjf�����ו��7ۛ���:�6_ �y&�^��1��[I� ���<tmŢr%[V�z�u�"�ީ�����dٯ�m�a�������z��a&F�΋n��\���Q�A���.^�,��y4�����0:���Ω�c��q.���3��V;��������v��c}����}r����=��ݧ�!F�է�L������Q����{���l���l~j�(�����2m[�n+(X�K�Do�/�H�i�aK�Y���".0�I7)`��uj���i��f}�$�����-�M�� �#�W-�R삅��S,�S�7�Uy�B�7Q݇��B	�=b��]@t��x�@p�h2ͤJ��|�����x�&��{�\tXD%��xE!�yt*����;�c���E�,6
"��I��o��E/b�Śz������eM���%0A��r���J�̴��<�V�S ����Hʰ���sj'����)��G��w{W��+��N\�5������_�6h��� ����mlm����Oҡ�B��A8����Lt���E��
��ԧ�gT|��TdlӒ��(�\�f%f����aA��4�`H�<bG��B��M�8����ۭ7�g�8�j�r�V�hr"���u�R�xEE��*�V�ޚ�\�_iT<�q��ć�m�4��5½��ĕ,��p8�����%Ѡ�Z�#�ENU�m\ڱS�'�,�=6��KL����;�2qE׃��}hSgkgG���vO�a�f=DQ�QZ�^�(X�=�]����`��� �]�Þp��r���z�:�H�V�oC��U�%���d_�L�g����`�N�t�����)�ES��2����;�7	�@�r� ,
ǅo����$ME�l���੪���)�#J�[��.�j@��+��Z<��X���R��b!�!ખ�knT���0��/���~�:_Ue
��^)�
�s�*��k�Og�f�+[������u9p����7���)%�I�����j�,�B��&j�N�N�r���\^� (��+
��qJ�� ���W��)y����G��@3��i���^�^�&L���4[�P�Od�����z<��n�Y�ҍY*�ayB�'�?��-�!�E�~��Y�ڈaopj�]����D�]^�j��:�m]���1�i�q:��}X��^ik�8P�H>+��m��P������Cl���ps��KR��Ԝ��,���c�H��~��-�b͉XM�vx���<��q�a0������P������Jɫ-���`?VTr��n��tFyO�0�\\�(?�u�ؑ�P��~-�%7��қ�>����z3���G�zMl���c<>�E�/��|&bv��g�U^��\q�mu���'�����N�C�W���������Y��꠯q�cA�^�(�s��m�|��z�-7���45�A\�`n���N�&��Z:��Iͤ�f��U'�`�pN�2��y�r�:���������*�c�F�9��@��p�:n�run] R�$���c�������c5��内|���(_OR�T�X7~d]�W��am\W���͟�M�wEO���fPÆ��f�;�j����'zʋ�eRM����������^`�UU��:��Ra�+�rj6�S<����˜Ky����T�́LPO�8tx[�&��װ�{�T�t�a�zUoB�5^�xk��:�u}y�V%�`��*I���G�B�s�p��Wa�H�R���7 :���Mr���+Λ�t-�Ibݟ֕�-<����x�X�xuKWhi���C���Z�?M�"t�ڭI
*�QH4�0�߉l�vY�p���@�u:UU�ⴛ0���jABm��������sd{����4\��EPM�%ՖxT˹��O�B{c�Uz�����Ak%��4�Z�4�Il�����Y+E���K9\6Z�'��o�����7��NRW�DR�ܔ
���ЄQ7R���iץ�}�풝��Nd�ޕ}^0y�y��� ��?�Ԝ\H]W����߼��	���1*�z!�*eކ�";�+�Xg�%�P����և5[�����&a���f�o�J�V,�
*�FLDG�(Q2��d�I����O˳8�$߃��B��(Wt�شV�!��%�и3Y��mqN� �q���0q����t�/0ev��	rA԰��%���⸀��A?
�7���u�d}H0ϥ�����zItq�j�u�.�/`U�227��I�CZ����K"�SK�U5�h����&oӗB`�ЙN�i�;������I���nݚKYeQ�M"IT�i f7kåe�]�˖MTg��J�U�0&�e:��Nyg��))v}�e-ń��RL�y`P�xp<�U��Ȱ���=k�Ꞿ��=�,C]V��uF$�`rLb=�p�w)������Ք��>��^���n�1�BZ�n�GI�G�M�v�[��#9��h�n�Q܌jF��R���~v�s'�X��C�q�l�z�$�T(��뵾>]6��&�*CVM�-K��� �6)��?V��@�>=8��+����M��$� ��Jh2�k���Y��l��2M���C�XT��ո�VP9̎3uA	0����c�e�Um�o��Jԯ��Zyi��b�5�']HԍK["} �����������e��@Q�@��*�ۛ\5���/�$��zE�%��k�Ʊ�x�B��0�q�o̍�֜+nakkS�>�PU�6l���uѐ9ɐޗA�*H3�ݒwݠ��h�:�*W�c~�Xsm����Dw�aK<���V��9P��ݾ��2X��V-Т�%�޷���GXr�+49�WXod�)^��OЂ3�ɄCgk͆[5�
^|k���y�#�r�1����jF)�� s�V����t���گY����7��>����a�G�v��%P���]�ڎ^K�N�f�r�,���k�C�WG0o��uN���`�"& �>�FBE<_�6���3�.����Y��U+v��Ʒ�ͫ�%�7���%�{�smݵ6����'�Y�6��A���nZڕaG
��	Uj��?����E�ˬ�Ztf����Ժ�Uٝ?�V	*����ܢ�
kw��R�T���=�����*��b�w�������|�����G�����g��;l,H?5��|�� ���a�qcj��L����{TC���PQ����z�Z��jh�.s}5�����A󳦝%�]˛Z�ߙ��Q�C�U��m3���1���w��4���j)�۽�2�D��9?2@ni�x��j�|2 쭫���#h�ā���?�RQ��2.U�g���@-Kq�3G�mu�w��'�>\Wx�T:�;�
S�u	�Sap�v߈�O���нnz��2��*��Z��X�&B̑t��'��w,t�\�D?uD\�u����Xff��$t��+$1U�	�b��y��l�{=a�>O�.O3]3�:����B�-i~��[�f�AR���t��
�I6-��`��x�	��*o�����ݺS�.^��ܠ�����;_�<����}���o^>������U��ǥk_���´R2��4p� �u��6��];�c]����$h�VS����R]�T򿷍_��0ʏ�z6������o]5-69�j���勽�$K�#=}�T,������[ ��yv��u�9|�����S����s��Ӱ̍���-���P���C��m�����Һ�k������x%�8)���ht~~><N��,?٬<�r9B�;"�<�\3���L��P��_�uc�|����,�c�0�؃G@��A`���	��.��O��t]�'���2�b۳x�����iO����J��b�nn^!N�x/Z��m<!N&�<�[�P(?�N%Eg76�c,�=۔�,rf�)Jo,}�K����j7^>~�1�lp�����Wo�o���)�
0~xU]�ɷA���G��[:�G�ɔ6߀g��x�|sr������[?{Wi�-�hj��@������Z 
,��m6~�������S�������W����/�����PK    \8Lh���  v     lib/Stow/Util.pm�Wmo"7�ί�t�J D�SJ�S����GD�ꪦBf�����{�.���;��$�JM�v�cϋ�g<��DB�ʪ��7���<k6r}K'��x�hx�7�Fa��Id���Z&ri�~�rr��|�t�h�͢[������s��EMrUh8��p9�^�&?é�n �ZiX�X�A;�Y�kL�ݢ��L-���V���®�B��	��4�*�3��\q6[�Z-��ދ�j�46m�N����ջ�{'>�ӧ�B�Ѡ���H��6�	�HeW�Ep΀�1J�$�j�%��By�''��)�e��8��-tZ�ҙ�=8zi����٘g	B3L�.���tM��:�6~����7�+ï_��)��?�#<cA�Qm��z�Mij�����<0C�Qz��84���3^��7z�{����S�G�����p��Я�݃Vf�\
3pV� t�����7�Рy���9U3�ۧZn8�_�c:u~���i�=���A�a����ti�й2�S����(���2Y�Vtj�`�T|�oN6���q��>��m�h-Mm�DIPqm��V㽒A��T�Q2N��ʏUX*E�B.%�+��j#Z�#hm�K���\@[�~y���2��4qA���ܿ{��� 6�����Rr�\I����ڤE�H-��L��z�Ԙ��W0�y{���4����V�tQ�b*�2BoDc���s�6ݭ�[s��G��������,�GtKX蘜J/dw���{��!7$~����ǯ�O���{Ī�z*��,�s���X��}*�|���������͇��/�(�5�����p��qBggWX]��$��cS��
� c�R;d�.��'��O���i�foUV�u@f��$O�U!?�M�D�Ѕm FA��1*��,�u�9Pӳ� S���4�<�Y��n�S�H���«0"���	��4�v=�c8s��¢WK"�8��\T��<���C�ͧo��t���c_$1W*�V���8]Q/wx�1=&c߾  �n7��{7�����7�U}�V�DW�T!��4w��Y8D��#����lvw���
Z�N��e ,X�ʅ,ًM�:l��X������������#�cf\0+A��Z�Ĭ�Cٶc�GԎ� �E9h��N�<E��]/E�uJ/��>$d��Cn�FpBr�(Nw�d\{���PK    \8L� �M$  �     script/main.plM��j�0���T���� ōA�e����Sh��X�Y�}ю������K<�%�Y�1閦EA���.#���H�3�β}�n�7Ȭ�녭Tg�Vql�k�'J[��v�h4u�8h,�����x6|��d$�4𿅆G�C�l�~�x�ßV�}{���l�z���J�0I>��W��ҧ�MF�N��=���a����6�Y�e��m�[�� �q�����x&�B�	�����.p[��K W�v����0Qr�����ȩ�����$�ل��w0�� PK    \8L�Q�   �_     script/stow�<ks�F���W��NIڐ��dw+�ĵlɎjI%�ɦ�$�"� �`@ъ���ׯy c'���:$8����������.m�;J�݅)3��Wgo�UU�tOϓ<�5����J�,��"��TϗY�.2�m1�VIi�"���@�(�ez;�������7_u�߯�߿ѿף{���WY:~g+��k�>y����W�_΍��(ۇ�C�+�ei��-&��h����&�\_-RS�0�z�Z�(�����iiM}_,�8�ui&���t����N��nQ�y1I�� -�)�W�)���_���Ln�$��P�_�c�[�X�ؙ� ; Nx�\9��, .q}�M
���p�]x�x]]� c;��R����kظ0��Jy p�M�g�w }+�=2zi�t�u5�(?�^{��Z���8��<:��q c�Y����3)�/� Me�W��:L����ŷ0��������������J�<��G������ś�G��������I_�+cgQ\�y;��NL���e���Y6ѳ�����Mzx%z���=IV�D!�,�t���j�̪j����Z�����_������þR�3�L����w'JYV��*Օ����S�~<;��:���?�~[�����O�^\�^\���)E{ �@�GN�����i���q2B,E$��V�ل5S�g!���g'f��i<W�b��W<�#��2ǌ�u��`�r 2�UQd5�>�R)��s��])�y�J��$����^W��}}��"�n��'����c�)��T�}�dA;D�s2U:�́\�neW/�ŉ�*A��F$p^� Nӌ�I��,��W��^��E>]Za��e� ����,)�(z|�ϵh@�K`!���
99.�s��+������<^/�Qd�`J��H-r�F\����(�X��@��u��W���}��t�i�Q�"ˊ������������颿7��q��Z��^,ύ{�gO3���>""/,�BΊ���Ţ�hط`7Q��<�F(N�35��oM%�7��:ݥ��f�a%�,�mʜ&�Q�?�#�o夤L@� ���(r:J��p��:e�+�C��dg�A�ӻt���{�V�Y$ +�C�x�7P�� �G;U�v������/��%jC�q����Q��1K�wb��c߸X��D�R{K��[��8�J��8�#m;
�@�E�\����i��p�oz���tR [��O�b��ٲB�G_HWT�N� \��b�F��\L�I(�4�\��eI��,�ѐ-.�4S"�ς]��0S(���l_��Z�Ϛ|RzȀi�a�P{�|]��dޛ�JF��.�J���+��g{A��>�Bc��Vw��v�Q��")ss�BPf@�s؍c� ��!"oA�Fhq������LM���%nzi~Y���(�Nz`_��@��ߒ�����˻�ڠ��Az�;n��T+4�`݁ [(b���$9�S2U�\&���!�#2d��Y��D�t�D�x�O'@~�1˻C$:�hL���@3�"��#3�,jDi7^�:�d��z�	�J��mӿ�w����.���r��\���{��ꁙ��ޕfA��E/)Ѽ�0� ����ӳ���~T�HwD�:�K���D-�DĀ�l�p�c�"u
�D�u��$A�\�^O3B�[�5�T�1�#�}�"��b����{ۂN:���+����aW<�P�DG���ra��K,@n�5��x#�"_Bށ�T�z&��o��IH��/��=D�[2(E�:���W�r������	j��A>"T�x����d������5q�Dd�����dΔ�q��s�A��z�ٖ5|�,K�"3((B��@���H�� �G��:LR+��������.E��p��b�0DS�����N-�M�N��,��8q��)oYAh�b��br����--9��:Fژ��n���{���m8F�BAH7+<Ǵ�<���(~���k�҂Z1�uAY����1U0L7����O{������]���Y&���'q0�WR�E'iӉa�V�`"�H)��I�`��|}��$���S�Gf�B�je#E�ںF@^��bW>����9w<�H��-��+��u���6MEj�b �V���2|m�3�)�@\%b]\����1$/[Q�?��.|�8�`���� F^V�CA|䛟�;�P���X5H�R��/�*�kS�(G��4��,%�(j�w�I�t+�.�\U� A;�ڢ�%6d�F�G�!��(�A�ܜS��ӧ��"�]�9a�TN�%�ҥ|������7ǧ�Ce�,r4U0�LI|1�BXcM�����	g�� :8_�_��)�S(��\T��*��gb����u-e����j>�C܁�k� ���a,i�l��{�����IEyj����!�Q�'4�Xs9��A����C�E�^�?��B��$V�Q
���p፴�K�!��� �3�{�� ��q+�BB�����2� -aM �\��48d�8@���l��>��LFfŘ��SJt޹����X)Ŋ b�r����� ���H@]��l�������
k~:<��ѓ'"�JA�v"�$�D��,JW�D�iK��&<���i>V`2��2KB������iWź��@?q�-��7(T@X�n�:zH'����5 "�8��?��l���b�$�����#w#��5�s�F�7*��I� �p�&� Ѥb�V�IS:%A�Y�j4�� ����FP�9�{����Ph�.��.�s$�����H/h�Ror��4{��&���"Ũ�6���d3�J]2�~��iZZ<��R*cRn!�	�p>BQ�-�%ņza��]×��d��0U�\�W�%���1`�L�J=?�!)q��5#22���%�df�N�%����4������F���Tإ�ff�OY&l%WI^I%?�%�H<Dp|�(N�#��|\,a]LZ8kN2П�=���2\b҉Zd�d���L���1X0�'�i�{F{�û�r�M� ��0s+�-� ��C`��J
�S��S����Y�D��3�2�T�K����@�dI��I��)K�&�C��%��T3Q�p���!M��l���� kg�z,G�:u(S�S�|6�N+�p�dFЬ��j:A9Vǩ��H���f�@�z�hU�L�a���A\���YG%���7��'�N���)}��Be  �}?6GSS���T	$�HE$('%��
|,"�JG���g]�1X)!�qK�,ʱ�[�VWN�>c��k�ϕ�Axú����|��_(x�zU�?�g&[ē]�h�!w~_�<<||~zvu}�����+}q��_G�N$Vox.����`-� r��2��z|d�Iy'T�����2�'�!狊V��|�ж�Y�%�@j�\�O*VE9�d�A'-���YH�B�w��WREk1�Ot���xVl�&b�"ǭ��Q��ܨE�Xbї�QZ�2.��S7Z+/����{S�yj\�����ײ1D�	Z�������,*9I������`
A&���րc>�����BF(���G�"\ɋ�ȟd�V���n��})�\�kW�qM
��es7|�ޞ�Q�-��b�%����˃~��V"���\f�Άv���� ��u̺��UY�%��|*Fw�ܓ���E@+��.|PM�5?!�lQ@a�c�"�#�c*&!^��uH���t�H�ش����(3�uh�kZ��[�ѫE�&%k�Ҩ�Q'�k��F�:#E�6�ƃ����ǁ�@�ΧÐM��G ��\�6!ի����Ő�R���t��Ĕ��T��S�>�����.��%ن��`r����Ct��	}/��S/fiV�b1���bn��F��S/��8�\߫���D8}��X�0�Ō9��,�'"f�k�F�`�q��y���;�b)�u��wo�����Z�����ZQr��*��;���U!7ccmBn�������]�j�TZ-��3�&1���8��P�+�f'�T4b/>�N]y��M�PR��)!k�P�r�8�� ��Pza�Jpn�8E�+�;����i.,	*�r�k]q��BV,M=�@l)�	y���7V��Zx��(���c�� :t����Q����m&�K����3��~�	Xl�j�
x<�Ow]J�נ����lN��7��$��B�|�	5T�^�蘭�*A�>ԼO]��k3 �T�S�_�I����y���!���P��׻�UII((߻fP)�Y����/%5�A=:j1|(����a�Fw�L�?�h� ��1�L_��-?�F�+�`����!��)5-�5%C8'[���J�s����
.�J���j xږ�YG:��u��^�Ō�i䔚�/����jl+�<�X��|��).ep����|S~fE'U�f���a�nb%}��-'M��)p�&�,B%:�lŶ�(@�;�@����w��_pOO~J�{�T2@��Ғ��)�?[c��q�!�N���j8����EV��F;!�ͨ6�x%��������̵�IU�����_�\�3y�e>I�}���Q�����*��m��x#�jB�"y����c�~̟�b#���?�JB��0�o;��S�Ԏ�r�lx���y��&T�[ �+U�4@�m�>pX���	�S���6Na�x6}I��$%�<Ǝ|��&PV)@����
�[ˀ�u��؍�'��\�Fy�Ԕz��;���P������%X��)�	�c�Fl��b���+Q���P��
�� ��}J����kA`�O�������kA��g0���ȱy�~G�n�H}��&��5YN�,��8��Gy
��^��B��ĸ�(�NN���s���'a��;��N���+�]��37�S	�8q�np�-m��t��Q2�.�ش�T
�Ji��e�dH�t�L�����?��7M��7���]dx>,]%�%���~�� �86�������aj
�^������ 0(��_;��+�ġt��� �1���8exy0-�a(z�H�޿cꝁ��#l`���]��K3{lq`�@=Ơ��6��DR����\��P�7 RBQ?=xy�VF1��S�,_�`y������O.�4e�� �Ho��<������QD@#Bbv��Nv֫���.`��,�%%^"�r(�X­e_*�|��;G���l�&��#>���fa8�v��b �T�A�_y�U�5���3��K�c��	�D,��Mn�܌�7�Z܌8�)�C,k�ޖ�[�˜���j�~l� i��r�]��_B09`sQ5���q����ѩ�\b����Ĕ��)�5�$:y��-�S+x&T�>�H�/�Y(w�r��/�9��H���ƭh&˪7N��2��,�B�y_qA�Ul,��ψ��\{~��7t��f�Q"�]�r�a��(��!��w5�JqXڳ.ܤ���ut�
��zU�/ΏQ��-�T�7P��-�^?O �H�.B�B�A� ����/�j�8����.^�s�t�E�N��ºuEe��7;��cJFD$\���V�ufv�%��/9��x=�'4�`���i�|�<v(��ؚ*��U&n��{�6@���4]�*��7/o�?�է�����bt���kMW |F}�Փ�~����<�����}��]���(x��?^����m|�X}�Eg}0*F�_��?�wpq}`�p���0�q�~��P������h����{R灘��Z�>�\�M��p���o��#`��Ģd�=G�$��Pob�Kt`@/�t?�=��wuk�����"��m�H]vx�XΙא��A�&)7����'����jO��D���>�������>D쓲a3�.�6J����[5�f�\3OZx��R��d�Z` ����en3�nl0X*��eI~�$��'p��}����>�n�
�<��C���yT��k��(�, ��xY)E���q5��+��q7$���O���q��/ίN��Ym�3�&;<񕩊E�����oa =C�0����T��0Q:������~|��FS��񓁪?��������#N ���3!��4G�� m?���]�x����TōkC���,C-?���)p��>�2;b����p̣�����*3B�����b�\��~�@P �#		��q�/\��"�x",~��P�7���a�a܄��H�������ޢ����e��wz�~�����.Eٶ�Ͻ3�V����\�P8u燣˳ӳWя?�h46wsI���wk�h�5��EVz���'������󬕔�D�4�d+�U�����3�����`�կ��&"G�7p,��"�D�7�`�9����P۔}���C�M��2)B�vpߎ�q�����e>�h��b���HeyT��O_�y~y�����D��?�H�顿�u[�_,K:�ߓ�6��e(L������9�<��4w���j�}�4[�	i?���,V��l5��K��gt�L���bz�|N� a<3��gG������v�A�W�����C�	w�p�~��.Ï�l���f�Gg�Ū�i\b�9����v�3��A1�]�~��{�A�Ż����O�&2wzG9��&��q�W��ʋn���F4�#pј��gt�|l�� �\��:�?ڒv����/
�>�Ș�?�y�U��A���8
krI������&�?�[{��[�I��sha�����$3< �h��6�V0��K��]��`b K#��҇C��n~pc���O{?���big`��!��P?~��_�m���ם��D�o���?	W�x{ē7��=�2��x&Qa��e�+��9�C|L�'E�	rW��'��F�_��;������˨1ܟaq�^�f�5�l���Y|��d=���b���	y�p?�ꁟ��)��x��_�ذ�����oA=�5h>�;��\{����'�����g~��,k1��OO~^=�7�G(Q4�6���Y[�|�g��]�A�\b�����
�fUB]1�0$ O?
������+�ޭx�-�G��ƕQ�?p��~_1��	%����
3�g7QF!]{Q��� 6~�G���=�_��>9N��W7�5JmN<t���Oξ��.�~��l>�ל�� �7�b߷�2���������D�������ߊBa�׺,��P�F��f��(���c77�j�9���+>$��:�1��f�~��I�E>k�[��YTI���1VxM�*�J>=�6K���K6,�>����Q��ݹ�tl*�c����Pwbg��Ja�9��එ�ȑ����C]T�u���D����`a��� ��(�f�EX���<H|��@"ȭ��
:[$�[�[8f���Gю�sjDuNsO�0�0t��� �%E|	.�a_o_s�m�����.��l'�_w��+�$�������<���s�+N�6�u0�d'l�&9㻃��ߞw��1����iTz%Ol��.�	WteUjD�	kE j�C<^��>� à�kn�����ʽI��2H�q��kK�fD".Brp�h���z,1�A���6EF��.7�N�̨�.�5��m��%z���f}���� ���͊��E�:���M�4F�:���ְ�����媔���NkP�%�M�6��Ė�s�ت7��\�3
�5%]�����r�9a@h���� ���N\
gv�ͽ�������N�z��^e��o#>���'�W�T��})W����KT�?��zW�˟]{">��X)yo���:t���J|'�;�-����O`gr(��0�ʁ	�V`0�3oHx��[jGp���{b�tM�u�]K/����/�@��?'����ܲ�&�D{���|�ٕ#�v����+� ������z[���~�w�[A�~חA����>^Tk��~c�©������95�����M�.6�a��q/	z_k�#�L���]�!��(M\�`���&�{��(I~����1ʻtE��D*�(� ��/9=��_�+��r׭����Q^��c��������W�'7�wGu$['�:zo�6� ���.k{Ht�6�~g��̺r?;^���d���˨U�*����������m
�������c�Y𽆡rG�:�~�Ew�ځ�bOS����8�DwG�� ��t����=>� G��ȥt~�+�~�Vc�{X����T������W����x�ÿ���#<C���N~�񿽔�
ݣ��_�s0d8�.���iuH��PK     \8L                      �AW^  lib/PK     \8L                      �Ay^  script/PK    \8L�<  �             ���^  MANIFESTPK    \8LV��h�   �              �� `  META.ymlPK    \8L�;�,�.  n�             ���`  lib/Stow.pmPK    \8Lh���  v             ����  lib/Stow/Util.pmPK    \8L� �M$  �             ����  script/main.plPK    \8L�Q�   �_             ��ҕ  script/stowPK      �  Զ    7a59c4f30d07568672f448f2a3e2155843da1e4a CACHE  Z�
PAR.pm
