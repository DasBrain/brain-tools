# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: clonecheck-1.0.tm

# Checks for clones. Sees through vhosts. Only works on UnrealIRCds.

# Configs:
package require Tcl 8.5

package provide dasbrain::clonecheck 1.0


namespace eval ::dasbrain::clonecheck {

	setudef str clonecheck
	
	if {![info exists lastnick]} {
		variable lastnick {}
	}
	if {![info exists cloneips]} {
		variable cloneips {}
	}
	
	if {![info exists users]} {
		variable users [dict create]
	}
	if {![info exists ips]} {
		variable ips [dict create]
	}
	
	bind raw - 340 [namespace current]::userip_response
	
	bind join - * [namespace current]::get_userip
	bind nick - * [namespace current]::nick_chg
	bind part - * [namespace current]::user_part
	bind sign - * [namespace current]::user_quit
	bind kick - * [namespace current]::user_kick
	
	#>> :irc.example.com 340 DasBrain :Nick=+lamest@127.0.0.1
	#>> :irc.example.com 340 DasBrain :Nick*=+johannnes@127.0.0.1 (IRC-Op)
	proc userip_response {from key param} {
		variable users
		variable ips
		variable lastnick
		variable cloneips
		set idx [string first " :" $param]
		set ipdata [string range [split $param] $idx+2 end]
		foreach ip $ipdata {
			set data [split $ip {=@}]
			set nick [lindex $data 0]
			set cip [lindex $data 2]
			if {[string index $nick end] == {*}} {set nick [string range $nick 0 end-1]}
			if {![dict exists $users $nick]} {
				dict set users $nick $cip
				dict set ips $cip $nick {}
				set info [dict get $ips $cip]
				if {[dict size $info] > 1} {
					if {$lastnick eq {}} {
						reportclones $cip
					} else {
						lappend cloneips $cip
					}
				}
				if {$nick eq $lastnick} {
					set lastnick {}
					foreach clip $cloneips {
						reportclones $clip
					}
					set cloneips {}
				}
			}
		}
	}
	
	proc reportclones cip {
		variable ips
		set reported {}
		set info [dict get $ips $cip]
		foreach nick [dict keys $info] {
			foreach chan [channels] {
				if {$chan in $reported} {continue}
				if {![onchan $nick $chan]} {continue}
				lappend reported $chan
				set target [channel get $chan clonecheck]
				if {$target eq {}} {continue}
				putmsg $target "Clones: [join [dict keys $info]]"
			}
		}
	}
	
	proc get_userip {nick uhost handle channel} {
		variable users
		if {[isbotnick $nick]} {
			utimer 4 [list [namespace current]::getips_chan $channel]
		} else {
			if {![dict exists $users $nick]} {putserv "USERIP $nick"}
		}
	}
	
	proc getips_chan {chan} {
		variable users
		variable ips
		variable lastnick
		if {$lastnick ne {}} {
			utimer 4 [list [namespace current]::getips_chan $chan]
		}
		set getips {}
		foreach u [chanlist $chan] {
			if	{![dict exists $users $u]} {
				lappend getips $u
				if {[llength $getips] == 5} {
					putserv "USERIP [join $getips]"
					set getips {}
				}
				set lastnick $u
			}
		}
		if {[llength $getips] > 0} {putserv "USERIP [join $getips]"}
	}
	
	proc nick_chg {nick uhost hand chan newnick} {
		variable users
		variable ips
		if {![dict exists $ips $nick]} {
			set cip [dict get $users $nick]
			dict unset users $nick
			dict set users $newnick $cip
			dict unset ips $cip $nick
			dict set ips $cip $newnick {}
		}
	}
	
	proc user_part {nick uhost hand chan reason} {
		utimer 0 [list [namespace current]::check_left $nick]
	}
	
	proc user_quit {nick uhost hand chan reason} {
		utimer 0 [list [namespace current]::check_left $nick]
	}
	
	proc user_kick {nick uhost hand chan target reason} {
		utimer 0 [list [namespace current]::check_left $target]
	}
	
	proc check_left {nick} {
		variable users
		variable ips
		if {![onchan $nick] && [dict exists $users $nick]} {
			set cip [dict get $users $nick]
			dict unset ips $cip $nick
			if {[dict size [dict get $ips $cip]] <= 0} {
				dict unset ips $cip
			}
			dict unset users $nick
		}
	}
	
	proc reset {} {
		variable lastnick {}
		variable cloneips {}
		variable ips {}
		variable users {}
	}
	
}

putlog "clonecheck by DasBrain (#John @ Quakenet) loaded"
