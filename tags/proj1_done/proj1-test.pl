#!/usr/bin/perl -w

# Perl script for testing CSCE 531 proj1 submissions

# edit as needed
$submissionRoot = "./submissions";
$testSuiteDir = ".";
$hwDir = 'pas-proj1';
$testFilePrefix = 'T1L';
$timeout = 11;			# seconds
$prog_name = 'ppc3';

############ You should not have to edit below this line ##############

# Hash to hold the test results
%error_counts = ();
%strip_error_counts = ();

# Check existence of test suite directory
die "No test suite directory $testSuiteDir\n"
    unless -d $testSuiteDir;

#sub main
{
    # Get test files
    opendir DIR, $testSuiteDir;
    @filenames = readdir DIR;
    @testfilenames = ();
    while (@filenames) {
        $name = shift @filenames;
	next if $name !~ /^($testFilePrefix.*)\.pas$/;
	push @testfilenames, ($1);
    }

    # Typical element is $test_file_hash{$level}->{$ok_or_err} = $base
    %test_file_hash = ();

    foreach $base (@testfilenames) {
	$base =~ /^$testFilePrefix(\d+[^-]*)_(err|ok)$/;
	$test_file_hash{$1}{$2} = $&;
    }

    if (!@ARGV) {
	print "Usage:\n  proj1-test.pl --self-test <your_proj1_directory>\n\n";
	print "For example:\n  \$ ./proj1-test.pl --self-test pas-proj1\n\n";
	print "This program must be invoked from the bash shell.\n";
	print "When in doubt, run \"bash\" first, then \"exit\" after.\n\n";
    }
    elsif ($ARGV[0] eq '--self-test') {
	shift @ARGV;
	die "proj1-test.pl: missing directory name\n"
	    unless @ARGV;

	local $proj1_dir = shift @ARGV;

	if ($proj1_dir !~ /^(~|\/)/) {
	    # relative path name -- prepend pwd
	    $pwd = `pwd`;
	    chomp $pwd;
	    $proj1_dir = "$pwd/$proj1_dir";
	}
	# convert home directory-relative pathname
	$proj1_dir =~ s/^~/$ENV{HOME}/e;
	# strip off final slash, if any
	$proj1_dir =~ s/\/$//;

	$uname = 'self-test';
	process_user();
    }
    elsif ($ARGV[0] ne '--all') {
	while (@ARGV) {
	    $uname = shift @ARGV;
	    process_user();
	}
    }
    else {
	# $ARGV[0] eq 'all'
	shift @ARGV;
	opendir DIR, $submissionRoot
	    or die "Cannot open submission directory $submissionRoot ($!)\n";
	@usernames = readdir DIR;
	closedir DIR;

	while (@usernames) {
	    $uname = shift @usernames;
	    next if $uname =~ /^\./;
	    next unless -d "$submissionRoot/$uname";
	    process_user();
	}
    }
}


sub process_user {
    print(STDERR "Processing $uname\n\n");
    if ($uname eq 'self-test') {
	$udir = $proj1_dir;
    }
    else {
	$udir = "$submissionRoot/$uname/$hwDir";
    }
    die "No subdirectory corresponding to $uname ($!)\n"
	unless -d $udir;

    open(COMMENTS, "> $udir/comments.txt");

    cmt("Comments for $uname -------- " . now() . "\n");

    chdir $udir;

    # try make clean, regardless of what happens
    system("make", "clean");

    opendir DIR, "$udir"
	or die "Cannot open $udir directory ($!)\n";
    @filenames = readdir DIR;
    closedir DIR;
    #    cmt("    Extra files found:");
    $count = 0;
    while (@filenames) {
	$filename = shift @filenames;
	chomp $filename;
	next if $filename =~ /^\./;
	if ($filename =~ /lex\.yy|y\.tab|\.o$|parse\.c|scan\.c|ppc3/) {
	    cmt("Removing illegal file: $filename\n");
	    unlink $filename;
	    $count++;
	}
    }
    cmt("No illegal files found\n")
	if $count == 0;

    test_prog($prog_name);

    report_summary();

    close COMMENTS;

    # try cleaning -- don't care what happens
    system("make", "clean");

    print(STDERR "\nDone.\nComments are in $uname/comments.txt\n");
}


sub test_prog {
    my ($prog) = @_;

    print(STDERR "Errors msgs for $uname/$prog:\n");
    print(STDOUT "System msgs for $uname/$prog:\n");

    cmt("Testing $prog:\n");

    $error_counts{$prog} = 0;
    $strip_error_counts{$prog} = 0;

    $rc = test_make($prog);

    if (!$rc) { # if couldn't make original prog
	cmt("Make of $prog failed\n");
	$error_counts{$prog}++;
	return;
    }

    foreach $level (sort by_extracted_number (keys(%test_file_hash))) {

	cmt("\n\nLEVEL $level:\n\n");

	foreach $key (sort { $b cmp $a; } (keys(%{$test_file_hash{$level}}))) {

	    $base = $test_file_hash{$level}{$key};

	    -e "$testSuiteDir/$base.pas" || die "$base.pas does not exist ($!)\n";
	    cmt("Running $prog with input $base.pas ...");
	    print(STDERR "----$base.pas:\n");
	    $testFile = "$testSuiteDir/$base.pas";
	    unlink "$base.s"
		if -e "$base.s";
	    unlink "$base.err"
		if -e "$base.err";
	    $error_counts{$base} = 0;
	    eval {
		local $SIG{ALRM} = sub { die "timed out\n" };
		alarm $timeout;
		$rc = system("./$prog < $testFile > $base.s 2> $base.err");
		alarm 0;
	    };
	    if ($@ && $@ eq "timed out\n") {
		cmt(" $@");		# program timed out before finishing
		$error_counts{$base}++;
		unlink "$base.s"
		    if -e "$base.s";
		unlink "$base.err"
		    if -e "$base.err";
		next;
	    }
	    elsif ($rc >> 8) {
		cmt(" nonzero termination status\n");
	    }
	    else {
		cmt(" zero termination status\n");
	    }

	    # Test error file in any case; contains the symbol table dump
	    if (!(-e "$base.err")) {
		cmt("  error message file $base.err does not exist\n");
		$error_counts{$base}++;
	    }
	    else {
		cmt("  $base.err exists\n  Comparing with solution file ...");
		$report = `diff $base.err $testSuiteDir/out/$base.err`;
		unlink "$base.err";
		chomp $report;
		if ($report eq '') {
		    cmt(" files match\n");
		}
		else {
		    cmt(" files differ:\nvvvvv\n$report\n^^^^^\n");
		    $error_counts{$base}++;
		}
	    }

	    if ($base =~ /_err$/) {
		# We don't care about the assembly output when checking an
		# Pascal file with errors
		unlink "$base.s"
		    if -e "$base.s";
		next;
	    }

	    if (!(-e "$base.s")) {
		cmt("  output file $base.s does not exist\n");
		$error_counts{$base}++;
		next;
	    }

	    cmt("  $base.s exists\n  Comparing with solution file ...");
	    $report = `diff $base.s $testSuiteDir/out/$base.s`;
	    unlink "$base.s";
	    chomp $report;
	    if ($report eq '') {
		cmt(" files match\n");
		next;
	    }
	    cmt(" files differ:\nvvvvv\n$report\n^^^^^\n");
	    $error_counts{$base}++;
	}
    }
}


# Tries to make the given executable program.  Returns true iff success
sub test_make {
    my ($prog) = @_;

    cmt("  Attempting to build $prog ...");
    $rc = system("make", $prog);
    if ($rc >> 8) {
	cmt(" make $prog failed\n");
	return 0;
    }

    if (!(-e $prog)) {
	cmt(" $prog executable does not exist\n");
	return 0;
    }

    cmt(" ok\n");
    return 1;
}


sub report_summary {
    cmt("######################################################\n");
    cmt("Summary for $uname:\n");

    foreach $base (sort(keys %error_counts)) {
	cmt("  $base: ");
	$cnt = $error_counts{$base};
	if ($cnt > 0) {
	    cmt(" problem(s) found");
	}
	elsif ($base !~ /_err$/) {
	    cmt(" ok");
	}
	else {
	    cmt(" error message(s) (check appropriateness)");
	}
	cmt("\n");
    }
    cmt("######################################################\n");
    cmt(" 80% level: \n");
    cmt(" 90% level: \n");
    cmt("100% level: \n");
}


sub cmt {
    my ($str) = @_;
#  print $str;
    print(COMMENTS $str);
}


sub now {
    my $ret;

    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime;
    $ret = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat')[$wday];
    $ret .= " ";
    $ret .= ('Jan','Feb','Mar','Apr','May','Jun','Jul',
	     'Aug','Sep','Oct','Nov','Dec')[$mon];
    $ret .= " $mday, ";
    $ret .= $year + 1900;
    $ret .= " at ${hour}:${min}:${sec} ";
    if ( $isdst ) {
	$ret .= "EDT";
    } else {
	$ret .= "EST";
    }
    return $ret;    
}


sub by_extracted_number
{
    my ($n1,$n2);
    $a =~ /^\d+/;
    $n1 = $&;
    $b =~ /^\d+/;
    $n2 = $&;
    $n1 <=> $n2;
}
