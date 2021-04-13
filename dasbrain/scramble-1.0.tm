# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: scramble-1.0.tm

# Scramble game

# Configs:
package require Tcl 8.6

package provide dasbrain::scramble 1.0


namespace eval ::dasbrain::scramble {

	variable solvetime 300
	variable wordlist /usr/share/dict/words
	
	setudef flag scramble
	
	variable words [list]

	bind pubm - * [namespace current]::checksolution
	
	if {![info exists state]} {
		variable state [dict create]
	}
	
	proc checksolution {nick uhost hand chan word} {
		variable state
		if {![channel get $chan scramble] || ![dict exists $state $chan]} {
			return 1; # No log
		}
		set cstate [dict get $state $chan]
		set word [string tolower [stripcodes * $word]]
		if {$word eq [dict get $cstate word]} {
			variable solvetime
			set points [expr {$solvetime  - ([clock seconds] - [dict get $cstate start])}]
			addpoints $nick $uhost $chan scramble $points
			reset $chan
			return 1;
		}
	}
	
	proc addpoints {nick uhost chan game points} {
		putmsg $chan "Congratulations [bold]$nick[bold], you got [bold]$points[bold] useless internet [bold]points[bold]."
	}
	
	proc reset {args} {
		variable state
		if {$args eq {}} {set args [dict keys $state]}
		foreach chan $args {
			set ut [dict get $state $chan utimer]
			catch {killutimer $ut}
			dict unset state $chan
		}
	}
	
	proc unsolved {chan} {
		variable state
		putmsg $chan "Unfortunately, nobody found the right word. The word was [bold][dict get $state $chan word][bold]."
		reset $chan
	}
	
	bind pub - !scramble [namespace current]::pub_start
	proc pub_start {nick uhost hand chan arg} {
		if {![channel get $chan scramble]} {
			return 0; # No log
		}
		if {$arg eq {reset}} {
			if {[matchattr $hand o|o $chan]} {
				reset $chan
				putnotc $nick "Scrabble on $chan reseted."
			} else {
				putnotc $nick "Access denied"
			}
			return 1; # Log
		}
		variable state
		if {[dict exists $state $chan]} {
			putnotc $nick "A scramble game is already running in $chan. Please wait until it is done."
			return 0; # Don't log
		}
		variable words
		if {[llength $words] <= 0} {
			putnotc $nick "Not enough words loaded."
			return 0
		}
		dict set state $chan start [clock seconds]
		variable solvetime
		dict set state $chan utimer [utimer $solvetime [list [namespace current]::unsolved $chan]]
		
		set word [lindex $words [expr {int(rand()*[llength $words])}]]
		dict set state $chan word $word
		set s [scramble $word]
		putmsg $chan "$::botnick drops a word. The letters [bold]$s[bold] are now lying on the ground. What was the word?"
	}
	
	proc scramble {word} {
		set list [split $word {}]
		set len [llength $list]
		while {$len} {
			set n [expr {int($len*rand())}]
			set tmp [lindex $list $n]
			lset list $n [lindex $list [incr len -1]]
			lset list $len $tmp
		}
		return [join $list {}]
	}
	
	bind evnt - rehash [namespace current]::loadwordlist
	proc loadwordlist {{type {}}} {
		variable wordlist
		variable words
		set words [list]
		set fd [open $wordlist r]
		while {[gets $fd line] >= 0} {
			if {![regexp {^[a-z]{4,}$} $line]} {
				continue
			}
			lappend words $line
		}
		close $fd
		putlog "Scramble loaded [llength $words] words"
	}
	
	proc bold {} {
		return \002
	}
	
}

putlog "scramble by DasBrain (#John @ Quakenet) loaded"
