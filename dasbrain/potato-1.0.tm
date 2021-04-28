# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: potato-1.0.tm

# Hot potato game

# Configs:
package require Tcl 8.5
package require eggdrop 1.9.0

package provide dasbrain::potato 1.0


namespace eval ::dasbrain::potato {

	setudef flag potato
	setudef str potato-protect-flags
	setudef int potato-cooldown
	setudef int potato-time
	setudef int potato-punish-maskhost
	
	bind pub - !potato [namespace current]::trigger
	bind nick - * [namespace current]::nickchange
	bind join - * [namespace current]::onjoin

	if {![info exists ::cap-request] || "away-notify" ni ${::cap-request}} {
		lappend ::cap-request away-notify
	}
	
	if {![info exists state]} {
		variable state [dict create]
	}
	
	if {![info exists punishlist]} {
		variable punishlist [dict create]
	}
	
	proc potato {nick uhost handle chan arg} {
		variable state
		if {![channel get $chan potato]} {
			dict unset state $chan
			return 0
		}
		if {![botisop $chan]} {
			putnotc $nick "I'm not op on this channel - game is disabled."
			if {[dict exists $state $chan utimer]} {
				killutimer [dict get $state $chan utimer]
			}
			dict unset state $chan
		}
		# Cooldown
		if {[dict exists $state $chan cooldown]} {
			set cooldown [dict get $state $chan cooldown]
			set diff [expr {$cooldown - [clock seconds]}]
			if {$diff < 0} {
				dict unset state $chan
			} else {
				putmsg $chan "$nick: The next hot potatos are still being heated. ETA: [duration $diff]"
				return 0
			}
		}
		if {[dict exists $state $chan potato]} {
			if {$nick ne [dict get $chan potato]} {
				putnotc $nick "You don't have the potato. Wait for your turn."
			}
		}
		set target [lindex [split $arg] 0]
		set target [normalizenick $chan $target]
		if {![validtarget $nick $chan $target]} {
			return 0
		}
		set potatotime [channel get $chan potato-time]
		if {[dict exists $state $chan utimer]} {
			killutimer [dict get $state $chan utimer]
		}
		putquick "PRIVMSG $chan :$nick gives the hot potato to $target. $target has now [duration $potatotime] to give the potato to someone else."
		dict set state $chan utimer [utimer $potatotime [list timout $chan]]
		dict set state $chan potato $target
		dict set state $chan potatouhost $target![getchanhost $target $chan]
	}
	
	proc timout {chan} {
		variable state
		if {![dict exists $state $chan potato]} {
			return
		}
		set victim [dict get $state $chan potato]
		if {![onchan $victim $chan]} {
			set bantype [channel get $chan potato-punish-maskhost]
			if {$bantype >= 0} {
				# Punish them when they join the next time
				set mask [maskhost [dict get $state $chan potatouhost] $bantype]
				dict set punishlist $chan $mask nick $victim
				dict set punishlist $chan $mask time [clock seconds]
			}
		} else {
			putkick $chan $victim "It burns!!!"
		}
		dict unset state $chan
		dict set state $chan cooldown [expr {[clock seconds] + [channel get $chan potato-cooldown]}]
	}
	
	proc validtarget {src chan nick} {
		if {$nick eq $::botnick} {
			putmsg $chan "HAHA. Nope. Give it to someone else."
			return 0
		} elseif {![onchan $nick $chan]} {
			putmsg $chan "$src, $nick is not on $chan"
			return 0
		} elseif {[matchattr [nick2hand $nick $chan] [channel get $chan potato-protect-flags] $chan]} {
			putmsg $chan "$src, $nick doesn't seem to like a hot potato."
			return 0
		} elseif {[isaway $nick $chan]} {
			putmsg $chan "$src, $nick seems to be busy with other things."
		} else {
			return 1;
		}
	}
	
	proc nickchange {nick uhost handle chan newnick} {
		variable state
		if {[dict exists $state $chan potato] && [dict get $state $chan potato] eq $nick} {
			dict set state $chan potato $newnick
		}
	}
	
	proc normalizenick {chan nick} {
		foreach n [chanlist $chan] {
			if {[rfcequal $n $nick]} {
				return $n
			}
		}
		return $nick
	}
	
	proc onjoin {nick uhost handle chan args} {
		if {![channel get $chan potato]} {return}
		if {[matchattr $handle [channel get $chan potato-protect-flags] $chan]} {return}
		variable punishlist
		set mask $nick!$uhost
		if {![dict exists $punishlist $chan]} {return}
		dict for {k v} [dict get $punishlist $chan] {
			if {[matchaddr $k $mask]} {
				dict unset punishlist $k
				putkick $chan $nick "[dict get $v nick] left with a hot potato in their pockets @ [clock format [dict get $v time]]."
				return
			}
		}
	}
	
}

putlog "pingpong by DasBrain (#John @ Quakenet) loaded"
