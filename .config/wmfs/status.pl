#!/usr/bin/perl -w

use strict;
use WMFS;
 
use String::Scanf;
use Sys::Statistics::Linux;
 
my $lxs  = Sys::Statistics::Linux->new ( cpustats => 1, memstats => 1, diskusage => 1 );
my $stat = $lxs->get;
 
sub separator
{
     my ($x) = @_;

     my $sep = WMFS::text ($x, 12, "#4C8244", "||");
     return $sep;
}

sub time_date
{
     $lxs->settime ("%H:%M");

     my $sep = &separator (1231);
     my $date_time = WMFS::text (1245, 13, "#F5F5F5", $lxs->gettime);
     return $sep.$date_time;
}

#sub oss
#{
#     open (OSSMIX, "ossmix vmix0-outvol |");
#     my $output = join (' ', <OSSMIX>);
#     close (OSSMIX);
#
#     my @values = sscanf ("Value of mixer control vmix0-outvol is currently set to %f (dB)", $output);
#
#     my $sep = &separator (1158);
#     my $img = WMFS::image (1170, 2, 0, 0, $ENV{HOME}."/.config/wmfs/icons-1/vol.png");
#     my $bar = WMFS::progressbar (1186, 3, 40, 6, "#000000", "#555555", "#3F8EB4", $values[0], 25);
#     return $sep.$img.$bar;
#}

sub cpu
{
     my $perc0 = $stat->cpustats->{cpu0}->{total};
#     my $perc1 = $stat->cpustats->{cpu1}->{total};
     my $sep  = &separator (1096);
     my $img  = WMFS::image (1105, 1, 0, 0, $ENV{HOME}."/.config/wmfs/icons-1/cpu.png");
     my $cpu0 = WMFS::progressbar (1124, 3, 30, 3, "#999999", "#555555", "#B4000E", $perc0, 100);
#     my $cpu1 = WMFS::progressbar (1124, 7, 30, 3, "#999999", "#555555", "#B4000E", $perc1, 100);

     return $sep.$img.$cpu0;
}

sub mem
{
     my $free  = $stat->memstats->{memfree};
     my $used  = $stat->memstats->{memused};
     my $total = $stat->memstats->{memtotal};

     my $percfree = $free / $total * 100;
     my $percused = $used / $total * 100;

     my $barfree = WMFS::progressbar (1061, 7, 30, 3, "#999999", "#555555", "#3F8EB4", $percfree, 100);
     my $barused = WMFS::progressbar (1061, 3, 30, 3, "#999999", "#555555", "#B4000E", $percused, 100);
     my $perc    = WMFS::text (1031, 13, "#F5F5F5", int ($percused)."%");
     my $img     = WMFS::image (1012, 1, 0, 0, $ENV{HOME}."/.config/wmfs/icons-1/mem.png");
     my $sep     = &separator (1001);

     return $sep.$img.$perc.$barused.$barfree;
}

sub hdd
{
     my $root = $stat->diskusage->{"/dev/root"}->{usageper};
     my $sdb1 = $stat->diskusage->{"/dev/sda1"}->{usageper};

     my $bar0 = WMFS::progressbar (967, 3, 30, 3, "#999999", "#555555", "#858523", $root, 100);
     my $bar1 = WMFS::progressbar (967, 7, 30, 3, "#999999", "#555555", "#858523", $sdb1, 100);
     my $img  = WMFS::image (950, 3, 12, 12, $ENV{HOME}."/.config/wmfs/icons-2/hdd.png");
     my $sep  = &separator (940);

     return $sep.$img.$bar0.$bar1;
}

#sub mpd
#{
#     open (MPC, "mpc |");
#     my @output = <MPC>;
#     close (MPC);

     my $img;
     my $playing;
     my $pbar;

     if (@output == 1) # MPD: stop
     {
          $img     = WMFS::image (685, 3, 12, 12, $ENV{HOME}."/.config/wmfs/icons-1/mpd-stop.png");
          $playing = WMFS::text (699, 12, "#4C8244", "{").WMFS::text (707, 13, "#AFD744", "No playlist").WMFS::text (875, 12, "#4C8244", "}");
          $pbar    = WMFS::positionbar (884, 4, 50, 4, 3, "#999999", "#555555", "#111111", 0, 100);
     }
     else
     {
          # Get song name
          my $song;

          # If the name is too long, short it
          if (length ($output[0]) > 25)
          {
               $song = substr ($output[0], 0, 25)."...";
          }
          else
          {
               $song = $output[0];
          }

          chomp ($song); # Remove trailing \n

          # Get percent
          my @tmp = split (/\(/, $output[1]);
          @tmp = split (/%/, $tmp[1]);
          my $perc = $tmp[0];

          if ($output[1] =~ m/^\[playing\]/) # MPD: play
          {
               $img     = WMFS::image (685, 3, 12, 12, $ENV{HOME}."/.config/wmfs/icons-2/mpd-play.png");
               $playing = WMFS::text (699, 12, "#4C8244", "{").WMFS::text (707, 13, "#AFD744", $song).WMFS::text (875, 12, "#4C8244", "}");
               $pbar    = WMFS::positionbar (884, 4, 50, 4, 3, "#999999", "#555555", "#111111", $perc, 100);
          }
          else # MPD: Pause
          {
               $img     = WMFS::image (685, 3, 12, 12, $ENV{HOME}."/.config/wmfs/icons-2/mpd-pause.png");
               $playing = WMFS::text (699, 12, "#4C8244", "{").WMFS::text (707, 13, "#AFD744", $song).WMFS::text (875, 12, "#4C8244", "}");
               $pbar    = WMFS::positionbar (884, 4, 50, 4, 3, "#999999", "#555555", "#111111", $perc, 100);
          }
     }

     return $img.$playing.$pbar;
}

while (1)
{
     $stat = $lxs->get;
     WMFS::statustext_send (-1, &mpd.&hdd.&mem.&cpu.&oss.&time_date);
     sleep (1);
}
