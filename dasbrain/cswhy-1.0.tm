# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: cswhy-1.0.tm

# Asks chanserv what access someone has when they get ops from ChanServ.
# Designed for use with Atheme.

# Configs:

package require eggdrop 1.6
package require Tcl 8.5

package provide dasbrain::cswhy 1.0

namespace eval ::dasbrain::cswhy {
}

setudef flag cswhy

bind mode S "% +o" ::dasbrain::cswhy::onmode
bind notc S * ::dasbrain::cswhy::onnotice

proc ::dasbrain::cswhy::onmode {nick uhost hand chan mc target} {
	if {[channel get $chan cswhy]} {
		putserv "CS WHY $chan $target"
	}
}

proc ::dasbrain::cswhy::onnotice {nick uhost handle msg target} {
	if {[regexp {has flags \002[^\002]+\002 in \002([^\002]+)\002 } $msg - chan]
		|| [regexp {has no special access to \002([^\002]+)\002} $msg - chan]} {
		if {[channel get $chan cswhy]} {
			putnotc @$chan $msg
		}
	}
}