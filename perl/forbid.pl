#!/usr/bin/env perl

use strict;
use warnings;
use utf8;

# cpan JSON
use JSON;

my %forbid_record;

my $file = $ARGV[0];

sub say {print @_, "\n"}

sub to_str {

    my $ans = "";
    my $split = "";
    foreach (@_) {
        $ans .= $split .= $_;
        $split = ":";
    }

    return $ans;
}

sub filter_forbid {
    my ($key, $endtime) = @_;

    if ( exists($forbid_record{$key}) ) {
        if ($endtime == 0) {
            delete $forbid_record{$key};
        } elsif ($forbid_record{$key} < $endtime) {
            $forbid_record{$key} = $endtime;
        }
    } else {
        my $cur_time = time();
        if ($endtime != 0 and $endtime > $cur_time ) {
            $forbid_record{$key} = $endtime;
        }
    }
}

sub dump_json_file {
    my ($file) = @_;

    open my $fh, ">", $file or die "Counld open $file: $!";
    print $fh encode_json \%forbid_record;
    close $fh;
}

open my $info, $file or die "Could not open $file: $!";

while( my $line = <$info> ) {

    my $domain = "";
    my $app = "all";
    my $stream = "all";
    my $end_time = "";

    if ($line =~ "/stream_forbid/domain") {
        # filter domain
        if ($line =~ "domain=(?<domain>[^&]*)") {
            $domain = $+{domain};
            # filter end_time
            if ($line =~ "end_time=(?<end_time>\\d+)") {
                $end_time = $+{end_time};
                # handle
                filter_forbid(to_str($domain, $app, $stream), $end_time);
            }
        }
    } elsif($line =~ "/stream_forbid/app") {
        # filter domain
        if ($line =~ "domain=(?<domain>[^&]*)") {
            $domain = $+{domain};
            # filter app
            if ($line =~ "app=(?<app>[^&]*)") {
                $app = $+{app};
                # filter end_time
                if ($line =~ "end_time=(?<end_time>\\d+)") {
                    $end_time = $+{end_time};
                    # handle
                    filter_forbid(to_str($domain, $app, $stream), $end_time);
                }
            }
        }
    } elsif($line =~ "/stream_forbid/stream") {
        # filter domain
        if ($line =~ "domain=(?<domain>[^&]*)") {
            $domain = $+{domain};
            # filter app
            if ($line =~ "app=(?<app>[^&]*)") {
                $app = $+{app};
                # filter stream
                if ($line =~ "stream=(?<stream>[^&]*)") {
                    $stream = $+{stream};
                    # filter end_time
                    if ($line =~ "end_time=(?<end_time>\\d+)") {
                        $end_time = $+{end_time};
                        # handle
                        filter_forbid(to_str($domain, $app, $stream), $end_time);
                    }
                }
            }
        }
    } else {
        say $line
    }
}

dump_json_file("/tmp/forbid.json");

close $info;
