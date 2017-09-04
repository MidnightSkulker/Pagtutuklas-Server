#!/usr/bin/perl
use Getopt::Long;

#  Produce file "rr", to run the RpxMe server, and
#  produce file "rb", to run the BaseUrl server.
#  Produce file "rd", to run the Discovery server.

# First build the system. The last line of the output from a successful
# build has the directory where the executable is stored, we will get that
# line and use it to construct the command to execute.
system ( "rm -f temp rr rb rd; coup install | tee temp" ) == 0 or die "coup install failed\n";
print "coup install was successful...\n";

# Run the ifconfig command, results into the file ifconfig.out
# This will give us the IP address to use for the "listen" parameter to
# the command. We will need to fish this address out of the results
# of the ifconfig command.
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
my($ip) = $space[0]; # And here is the IP address
print "ip = $ip\n";

$eqs = "=" x 70;
print "\n\n$eqs\n\n";
open(MYINPUTFILE, "<temp"); # open for input
my(@lines) = <MYINPUTFILE>; # read file into list
foreach $line (@lines) # loop thru list
  {
  print "$line"; # print in input order
  }
close(MYINPUTFILE);
print "\n\n$eqs\n\n";

# Get the command to run, from the long and complicated path
# used by coup
$last = pop ( @lines );
chop ($last);
$rbcmd = $last . "/BaseUrl";
$rrcmd = $last . "/RpxMe";
$rdcmd = $last . "/Disco";

# Add some arguments to the command
$defProt    = "--default-protocol=http";
$dbConnSz   = "--db-conn-pool-size=2";
$dbConn     = "--db-conn-string=\"host=localhost password=hackme user=murphy dbname=accelerator_development\"";
$listenBaseUrl = "--listen=$ip:3000";
$listenRpxMe = "--listen=$ip:3010";
$listenDisco = "--listen=$ip:3020";
$maxConn    = "--max-connections=3";
$mailServer = "--mail-server=smtp.frontier.com";
$mailFrom   = "--mail-from=peter\@janrain.com";
$mailTo     = "--mail-to=peter\@janrain.com";
$mailDom    = "--mail-domain=SMTP";
$beanPoolSize = "--beanstalk-pool-size=3";
$beanTubeName = "--beanstalk-tube-name=default";
$beanJobLimit = "--beanstalk-job-limit=2";
$beanOverDir = "--beanstalk-overflow-dir=/mnt/hgfs/peter/rpx/rpxd/Snap-Refactor/OverFlow";
# Arguments for the BaseUrl server
$prot   = " \\\n" . "  " . $defProt;
$dbcz   = " \\\n" . "  " . $dbConnSz;
$dbcn   = " \\\n" . "  " . $dbConn;
$blist  = " \\\n" . "  " . $listenBaseUrl;
$rlist  = " \\\n" . "  " . $listRpxMe;
$dlist  = " \\\n" . "  " . $listDisco;
$maxc   = " \\\n" . "  " . $maxConn;
$mails  = " \\\n" . "  " . $mailServer;
$mailf  = " \\\n" . "  " . $mailFrom;
$mailt  = " \\\n" . "  " . $mailTo;
$maild  = " \\\n" . "  " . $mailDom;
$beanps = " \\\n" . "  " . $beanPoolSize;
$beantn = " \\\n" . "  " . $beanTubeName;
$beanjl = " \\\n" . "  " . $beanJobLimit;
$beanod = " \\\n" . "  " . $beanOverDir;
$rb = $rbcmd . $prot . $blist . $dbcz . $dbcn . $maxc . $mails . $mailf . $mailt . $maild;
$rr = $rrcmd . $prot . $rlist . $dbcz . $dbcn . $maxc . $mails . $mailf . $mailt . $maild . $beanps . $beantn . $beanjl . $beanod;
$rd = $rdcmd . $prot . $dlist . $mails . $mailf . $mailt . $maild;
# Prepare a script to run the BaseUrl executable
open (RBFILE, '>>rb');
print RBFILE "#!/bin/bash\n";
print RBFILE "$rb\n";
close (RBFILE);
chmod 0755, "rb";  # Make it executable
# Prepare a script to run the RpxMe executable
open (RRFILE, '>>rr');
print RRFILE "#!/bin/bash\n";
print RRFILE "$rr";
close (RRFILE);
chmod 0755, "rr";  # Make it executable
# Prepare a script to run the Disco server executable
open (RRFILE, '>>rd');
print RRFILE "#!/bin/bash\n";
print RRFILE "$rd";
close (RRFILE);
chmod 0755, "rd";  # Make it executable

