#!/usr/bin/perl

# Run the ifconfig command, results into the file ifconfig.out
system ( "rm -f ifconfig.out; ifconfig >ifconfig.out" ) == 0 or die "ifconfig failed\n";
# Read in the results of the ifconfig command
open(IFCONFIGOUT, "<ifconfig.out"); # open for input
my(@lines) = <IFCONFIGOUT>; # read file into list
close(IFCONFIGOUT);
# Get the IP address out of it
my(@inet) = grep ( /inet/, @lines );
my ($addrLine) = $inet[0];
my(@colon) = split /[:]/, $addrLine;
my(@space) = split /[ ]/, $colon[1];
my($addr) = $space[0]; # And here is the IP address
print "$addr\n";
