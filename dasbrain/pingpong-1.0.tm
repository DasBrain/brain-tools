# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: pingpong-1.0.tm

# Ping pong game

# Configs:
package require Tcl 8.5

package provide dasbrain::pingpong 1.0


namespace eval ::dasbrain::pingpong {

	setudef flag pingpong
	setudef int pingpong-kickchance

	bind pub - ping [namespace current]::ping
	
	proc ping {nick uhost handle chan arg} {
		if {![channel get $chan pingpong]} {return}
		if {[matchatttr $handle f|f $chan] || ![botisop $chan] || (rand() * 100 > [channel get $chan pingpong-kickchance])} {
			putmsg $chan "PONG"
		} else {
			putkick $chan $nick "PONG"
		}
	}
	
}

putlog "pingpong by DasBrain (#John @ Quakenet) loaded"
