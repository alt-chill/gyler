#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Long;
use File::Basename;
use File::Find;

# ------------------------------------------------------------
# Parse command-line arguments
# ------------------------------------------------------------
my ($files_mode, $macros_mode);
GetOptions(
    "files"  => \$files_mode,
    "macros" => \$macros_mode,
) or die "Usage: $0 (--files|--macros) MACRO_NAME PATH\n";

# Exactly one of --files / --macros must be specified,
# and we need two positional arguments: MACRO_NAME and PATH (file or directory)
die "Usage: $0 (--files|--macros) MACRO_NAME PATH\n"
  unless (($files_mode xor $macros_mode) && @ARGV == 2);

my ($macro_name, $path) = @ARGV;

# ------------------------------------------------------------
# Gather all files to process
# ------------------------------------------------------------
my @files;

if (-d $path) {
    # Recursively traverse directories
    find(
        sub {
            return unless -f $_;
            # Process only Haskell-related source files
            return unless /\.(hs|lhs|hsc)$/;
            push @files, $File::Find::name;
        },
        $path
    );
} elsif (-f $path) {
    push @files, $path;
} else {
    die "Error: '$path' is not a valid file or directory.\n";
}

# ------------------------------------------------------------
# Define a function to extract macros from one file
# ------------------------------------------------------------
sub extract_macros {
    my ($filename, $macro_name) = @_;

    open my $fh, '<', $filename or return ();
    local $/;
    my $text = <$fh>;
    close $fh;

    # Remove Haskell-style comments
    $text =~ s/\{\-.*?\-\}//sg;    # Multi-line {- ... -}
    $text =~ s/^\s*\-\-.*$//mg;    # Lines starting with --

    # Find macros of the form $(MACRO_NAME ...)
    my @matches;
    while ($text =~ /
            ^[ \t]*\$\(                # Allow spaces before '$('
            \s*${macro_name}\b         # Match given macro name
            (?<body>                   # Capture 'body' recursively
                (?:
                    [^()]++            # Non-parenthesis chars
                  | \( (?&body) \)     # Nested parentheses
                )*
            )
            \)                         # Closing parenthesis
    /xmsg) {
        push @matches, "\$(${macro_name}$+{body})";
    }

    return @matches;
}

# ------------------------------------------------------------
# Process each file and output results
# ------------------------------------------------------------
foreach my $file (@files) {
    my @found = extract_macros($file, $macro_name);

    if ($files_mode) {
        # Print filename if it contains at least one match
        print $file, "\n" if @found;
    } elsif ($macros_mode) {
        # Print each macro found, prefixed by the file name for clarity
        foreach my $m (@found) {
            print "$m\n\n";
        }
    }
}

