#!/bin/perl

sub uniq {
    my %seen;
    grep !$seen{$_}++, @_;
}

sub find_stream {
    my $len = @_[0];
    for (my $i = 0; $i < length($line) - ($len - 1); $i++) {
        $str = substr($line, $i, $len);
        my @chars = split("", $str);
        my $size1 = @chars;
        my @filtered = uniq(@chars);
        my $size2 = @filtered;
        if ($size1 eq $size2) {
            print($i + $len, "\n");
            last;
        }
    }
}

open(fh, '<', "input");
$line = <fh>;
close(fh);

find_stream(4);
find_stream(14);
