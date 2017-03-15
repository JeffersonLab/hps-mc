#!/usr/bin/perl -w

################################################################################
# addvertex.pl
#  Natalia Toro
#
# DESCRIPTION : script to add decay vertices in MG/ME event files
# USAGE :
# addvertex.pl infile.lhe outfile.lhe particleID vertexLength
################################################################################

use Compress::Zlib;
use List::Util qw[min max];

print "Running script addvertex.pl to replace particle codes in event file.\n";

###########################################################################
# Initialisation
###########################################################################
# Set tag names for later
my $begin_header = '<header>';
my $end_header   = '</header>';
my $begin_event  = '<event>';
my $end_event    = '</event>';
my $begin_init   = '<init>';
my $end_init     = '</init>';

# Parse the command line arguments
if ( $#ARGV != 3 ) {
  die "Usage: replace.pl infile.lhe outfile.lhe particleID vertexLength\n";
}
my $vertexLength = pop(@ARGV);
my $particleID = pop(@ARGV);
my $outfile = pop(@ARGV);
my $infile = pop(@ARGV);

open INFILE, "<$infile" or die ("Error: Couldn't open file $infile\n");


###########################################################################
# Go through file and pick vertices
###########################################################################

open OUTFILE, ">$outfile" or die ("Error: Couldn't open file $outfile for writing\n");

# No. events and cross-section numbers file
$nevents = $xsec = $trunc = $unitwgt = -1;

# Keep track in which block we are
$initblock = 0;
$headerblock = 0;
$eventblock = 0;
$eventcount = 0;
$rdnseed = 41;

while (my $line = <INFILE>) {

    # Extract <MGGenerationInfo> information
    if ($line =~ m/$end_header/) { 
        $headerblock = 0; 
        print OUTFILE "<AddVertexInfo>\n";
        printf OUTFILE "#  ID Vertexed             : %11i\n",$particleID;
        printf OUTFILE "#  Unit vtx                : %11.4E\n",$vertexLength;
        if($rdnseed > 0){
            print OUTFILE " ", $rdnseed+1, "  = gseed ! Random seed for next iteration of replace.pl\n";}
        print OUTFILE "</ReplaceParticleInfo>\n";
        if($rdnseed > 0) {print "Initialize random seed with $rdnseed\n";srand($rdnseed);}
        else {print "Warning: Random seed 0, use default random seed (unreproducible)\n";}
    }
    if ($line =~ m/$end_init/) { $initblock=0; }
    if ($line =~ m/$end_event/) { 
        $eventcount++;
        $eventblock=0; 
    }

    if ($line =~ m/$begin_event/) { 
        $foundDecay = 0;
    }
    
    
    if ($headerblock == 1) {
    } elsif ($initblock > 0) {
    } elsif ($eventblock > 0) {
        # In <event> block
        # Remove leading whitespace and split
        $line  =~ m/^\s+(.*)/;
        if($line=~/^\#/){
        }
        else{
            @param = split(/\s+/, $1);
            if($eventblock == 1){  $ipart=0; }
            else {
                $ipart++;
#	    @param = split(/\s+/, $1);
                if ($#param != 12) { die "Error: Wrong number of params in event $eventcount \($#param / 12\)"; }
                if($param[0]==$particleID){
	   if($foundDecay==1) { die "Error can't handle two decaying particles\n";}
	   $foundDecay++;

	   # this is the particle that decays
	   # Randomly choose decay length in rest frame for this event
	   $rnumber = rand(1.);
	   $rlength = log(1/$rnumber)*$vertexLength;

	   @decayBoost=@param[6,7,8,9,10];
	   my @vertex=(0,0,0,0,0);
	   for($i=0;$i<5;++$i){
	       $decayBoost[$i]/=$decayBoost[4];
	       $vertex[$i]=$decayBoost[$i]*$rlength;
	   }
#	   print "\n BOOST: @decayBoost\n";
#	   print "VERTEX ($rlength) : @vertex\n";

	   $newline = sprintf "%9i %4i %4i %4i %4i %4i %18.11E %18.11E %18.11E %18.11E %18.11E %1.0f. %2.0f.\n",
		    999,9,$ipart,9,9,9,$vertex[0],$vertex[1],$vertex[2],$vertex[3],$vertex[4],0.,0.;
	   $line="$line$newline";
                }
            }
        }
        $eventblock++;
    }
	    
    if ($line =~ m/$begin_header/) { $headerblock = 1; }
    if ($line =~ m/$begin_init/) { $initblock=1; }
    if ($line =~ m/$begin_event/) { $eventblock=1; }

    print OUTFILE "$line";
}

close OUTFILE;
close INFILE;

print "Wrote $eventcount events\n";
if( $eventcount < $nevents ) { print "Warning: $infile ended early\n"; }

exit(0);


