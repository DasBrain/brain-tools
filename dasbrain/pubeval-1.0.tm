# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: pubeval-1.0.tm

# Public safe interp eval

# Configs:
package require Tcl 8.5
package require dasbrain::interp

package provide dasbrain::pubeval 1.0


namespace eval ::dasbrain::pubeval {

	setudef flag interp-public-eval

	bind pub - $::nick [namespace current]::pubeval
	bind pub - ${::nick}, [namespace current]::pubeval
	bind pub - ${::nick}: [namespace current]::pubeval
	
	set maxlines 3
	
	proc pubeval {nick uhost hand chan param} {
		variable maxlines
		if {![channel get $chan interp-public-eval] && ![matchattr $hand n|n]} {
			return 0
		}
		catch {::dasbrain::interp::cinterp eval $chan $param} res opt
		set code [lindex {ok error return break continue} [dict get $opt -code]]
		set lc 0
		foreach line [split $res \r\n] {
			if {$lc > $maxlines} {break}
			incr lc
			putmsg $chan $line
		}
		return 1
	}
	
}

putlog "public interp by DasBrain (#John @ Quakenet) loaded"
