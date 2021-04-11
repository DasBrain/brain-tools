# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: chanserv-1.0.tm

# Public safe interp eval

# Configs:
package require Tcl 8.5

package provide dasbrain::chanserv 1.0


namespace eval ::dasbrain::chanserv {

	setudef flag chanserv-invite
	setudef flag chanserv-unban
	setudef flag chanserv-op
	setudef flag chanserv-hop
	setudef flag chanserv-protect
	setudef flag chanserv-owner

	bind need - * [namespace current]::need
	
	variable needdata {
		op {
			op hop protect owner
		}
		key invite
		limit invite
		invite invite
		unban {
			unban
			invite
		}
	}
	
	proc need {channel what} {
		variable needdata
		if {[dict exists $needdata $what]} {
			foreach act [dict get $needdata $what] {
				if {[channel get $channel chanserv-$act]} {
					putquick "CS $act $channel"
				}
			}
		}
	}
	
}

putlog "chanserv by DasBrain (#John @ Quakenet) loaded"
