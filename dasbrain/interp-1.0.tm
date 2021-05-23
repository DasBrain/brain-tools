# Brain-Tools
# Copyrigt DasBrain @ #John @ irc.quakenet.org

# File: interp.tm

# Creates save interp for channels
# Used Userflags: +S Service

# Configs:
# maxtime (int) time in seconds
package require Tcl 8.5

package provide dasbrain::interp 1.0

source [file join [file dirname [info script]] safe.tcl]

namespace eval ::dasbrain::interp {

	setudef str parent-interp

	namespace export cinterp
	namespace ensemble create -map {register register eval evalInterp get getInterp} -command cinterp
	namespace ensemble create -map {variable registerVar alias registerCmd command registerCmd cmd registerCmd} -command register
	
	if {![info exists ips]} {array set ips {}}
	variable registeredAliases {}
	variable registeredVars {}
	if {![info exists state]} {
		variable state [dict create]
	}
	
	# Returns a channel specific 
	proc getInterp {chan} {
		variable ips
		if {[info exists ips($chan)]} {
			return $ips($chan)
		} else {
			return [set ips($chan) [createInterp $chan]]
		}
	}
	
	proc getPath {chan} {
		set parent [channel get $chan parent-interp]
		if {$parent == {}} {
			return $chan
		}
		return [concat [getInterp $parent] $chan]
	}
	
	proc SplitIrc line {
		# set line [split $line]
		if {[string index $line 0] eq ":"} {
			set f [string first " " $line]
			set src [string range $line 1 $f-1]
			set line [string range $line $f+1 end]
		} else {
			set src {}
		}
		if {[set pos [string first " :" $line]] > -1} {
			set tail [list [string range $line $pos+2 end]]
			set line [string range $line 0 $pos-1]
		} else {
			set tail [list]
		}
		return [concat [list $src] [split $line] [list $tail]]
	}
	
	# Creates a new safe Interpreter for the irc channel
	proc createInterp {chan} {
		# variable ips
		variable maxtime
		variable registeredAliases
		variable registeredVars
		# default interp name
		set ip [::safe::interpCreate [getPath $chan] -home /home -mount [list [list /home [file join filesys [string tolower [string range $chan 1 end]]]]]]
		interp limit $ip time -command [list [namespace current]::timeup $chan]
		interp bgerror $ip bgerror
		interp alias $ip bgerror {} [namespace current]::bgerror $chan
		set ns [list]
		foreach {alias cmd} $registeredAliases {
			# set alias [lindex [split $cmd ::] end]
			interp alias $ip $alias {} $cmd $chan
			if {[namespace tail $alias] ne $alias} {
				lappend ns [namespace qualifiers $alias]
				interp eval $ip [list namespace eval [namespace qualifiers $alias] [list namespace export [namespace tail $alias]]]
			}
		}
		foreach n $ns {
			interp eval $ip [list namespace eval $n [list namespace ensemble create -command ::[namespace tail $n]]]
		}
		interp alias $ip tclLog {} putloglev d $chan
		foreach {alias var} $registeredVars {
			interp eval $ip "trace add variable $alias {read write unset} {traceHandler $alias}"
		}
		# interp hide $ip vwait
		interp eval $ip {
			proc traceHandler {var n1 n2 op} {
				switch -exact -- $op {
					write {
						return -code error "read-only variable"
					}
					read {
						set $var [getVar $var]
					}
					unset {
						return -code error "read-only variable"
					}
				}
			}
		}
		
		return $ip
	}
	
	proc registerCmd {cmd {alias {}}} {
		variable ips
		variable registeredAliases
		if {$alias == {}} {
			set alias [namespace tail $cmd]
		}
		dict set registeredAliases $alias $cmd
		foreach {chan ip} [array get ips] {
			interp alias $ip $alias {} $cmd $chan
			if {[namespace tail $alias] ne $alias} {
				set ns [namespace qualifiers $alias]
				interp eval $ip [list namespace eval $ns [list namespace export [namespace tail $alias]]]
				interp eval $ip [list namespace eval $ns [list namespace ensemble create -command ::[namespace tail $ns]]]
			}
		}
	}
	
	proc parseRaw {chan cmd} {
		set rest [lassign [split $cmd] cmd]
		
	}
	
	namespace eval aliases {
		namespace path [list [namespace parent]]
		
		proc putserv {chan text {options {}}} {
			
		}
		
		proc putmsg {chan target message} {
			if {[validchan $target]} {
				if {[string tolower $chan] == [string tolower $target]} {
					::putmsg $target $message
				} else {
					return -code error "You can not send a message to $target"
				}
			} else {
				#User
				if {[onchan $target $chan]} {
					if {[matchattr [nick2hand $target] +S]} {
						return -code error "You can not send a message to $target"
					}
					# TODO: Change when an interface for cprivmsg is written
					::putmsg $target $message
				} else {
					return -code error "You can not send a message to $target"
				}
			}
		}
		
		proc putnotc {chan target message} {
			if {[validchan $target]} {
				if {[string tolower $chan] == [string tolower $target]} {
					::putnotc $target $message
				} else {
					return -code error "You can not send a message to $target"
				}
			} else {
				#User
				if {[onchan $target $chan]} {
					if {[matchattr [nick2hand $target] +S]} {
						ereturn -code rror "You can not send a message to $target"
					}
					# TODO: Change when an interface for cnotice is written
					::putnotc $target $message
				} else {
					return -code error "You can not send a message to $target"
				}
			}
		}
		
		
		proc putchan {chan target message} {
			putmsg $chan $target $message
		}
		
		proc putact {chan target message} {
			putmsg $chan $target "\001ACTION ${message}\001"
		}
		
		proc putkick {chan chan2 target {reason {}}} {
			if {[string tolower $chan2] == [string tolower $chan]} {
				::putkick $chan $target $reason
			} else {
				return -code error "You can not kick peoples from $chan2"
			}
		}
		
		proc pushmode {chan channel mode {arg {}}} {
			if {[string tolower $chan] == [string tolower $channel]} {
				if {$arg == {}} {
					::pushmode $chan $mode
				} else {
					::pushmode $chan $mode $arg
				}
			} else {
				return -code error "You can not change the modes for $channel"
			}
		}
		
		proc putlog {chan text} {
			putloglev $chan o $chan $text
		}
		
		proc putcmdlog {chan text} {
			putloglev $chan c $chan $text
		}
		
		proc putxferlog {chan text} {
			putloglev $chan  x $chan $text
		}
		
		proc putloglev {chan level channel text} {
			# TODO: Log this message 
			if {[string tolower $chan] != [string tolower $channel]} {
				# return -code error "Permission denied"
				# We have to ignore this silently
				return
			}
			::putloglev $level $chan "Interp for $chan: $text"
		}
		
		proc unixtime {{chan {}}} {return [::unixtime]}
		
		# TODO: unbind support...
		proc bind {chan type attr filter callback} {
			switch $type {
				pub {
					::bind pubm $attr "$chan [string map {* \* % \% ~ \~ ? \?} $filter]" [list [namespace parent]::bind_callback $chan pub $callback]
					::bind pubm $attr "$chan [string map {* \* % \% ~ \~ ? \?} $filter] *" [list [namespace parent]::bind_callback $chan pub $callback]
					return $filter
				}
				pubm -
				time -
				join -
				part -
				sign -
				topc -
				kick -
				nick -
				mode -
				ctcp {
					::bind $type $attr $filter [list [namespace parent]::bind_callback $chan $type $callback]
				}
				default {
					# ignore silent - many scripts define some dcc or other commands :/
					putloglev d $chan "Ignored Bind for $chan: [list bind $type $attr $filter $callback]"
				}
			}
		}
		
		proc maskhost {chan host} {
			return [::maskhost $host]
		}
		
		proc rand {chan number} {
			return [::rand $number]
		}
		
		proc utimer {chan time script} {
			::utimer $time [list [namespace parent]::timer_callback $chan $script]
		}
		
		proc timer {chan time script} {
			::timer $time [list [namespace parent]::timer_callback $chan $script]
		}
		
		proc utimers {chan} {
			set res {}
			foreach t [lsearch -all -inline -exact -nocase -index {1 1} [lsearch -all -inline -exact -index {1 0} [::utimers] [namespace parent]::timer_callback] $chan] {
				lappend res [list [lindex $t 0] [lindex $t 1 2] [lindex $t 2]]
			}
			return $res
		}
		
		proc timers {chan} {
			set res {}
			foreach t [lsearch -all -inline -exact -nocase -index {1 1} [lsearch -all -inline -exact -index {1 0} [::timers] [namespace parent]::timer_callback] $chan] {
				lappend res [list [lindex $t 0] [lindex $t 1 2] [lindex $t 2]]
			}
			return $res
		}
		
		proc killtimer {chan tid} {
			# TODO: Check if $tid is a valid timer
			::killtimer $tid
		}
		
		proc killutimer {chan tid} {
			::killutimer $tid
		}
		
		proc duration {chan args} {
			::duration {*}$args
		}
		
		proc topic {chan channel} {
			if {[string tolower $chan] == [string tolower $channel]} {
				return [::topic $chan]
			} else {
				return -code error "You can not get the topic of $channel"
			}
		}
		
		namespace eval eggdrop::channel {
			proc info {chan {channel {-}}} {
				if {$channel eq {-}} {::set channel $chan}
				if {[string tolower $chan] != [string tolower $channel]} {
					return
				}
				return [::channel info $chan]
			}
			proc set {chan channel args} {
				if {[string tolower $chan] != [string tolower $channel]} {
					return
				}
				::set buf {}
				while {[llength $args]} {
					::set args [lassign $args opt]
					::set flag [expr {[string index $opt 0] in {+ -}}]
					if {$flag} {
						::set name [string range $opt 1 end]
					} else {
						::set name $opt
						::set args [lassign $args value]
					}
					::set skip [expr {[string match -nocase need-* $name] || [string match -nocase security-* $name]}]
					if {!$skip} {
						lappend buf $opt
						if {!$flag} {lappend buf $value}
					}
				}
				return [::channel set $chan {*}$buf]
			}
			proc get {chan channel {option -}} {
				if {[string tolower $chan] != [string tolower $channel]} {
					return
				}
				if {$option eq {-}} {
					return [::channel get $chan]
				} else {
					return [::channel get $chan $option]
				}
			}
			proc add {chan channel args} {
				if {[string tolower $chan] != [string tolower $channel]} {
					return
				}
				::channel set $channel {*}$args
			}
			proc remove {args} {}
		}
	}
	
	
	register alias [namespace current]::getVar
	proc RegisterSubNS {ns skip} {
		foreach cmd [info commands ${ns}::*] {
			register alias $cmd [string range $cmd $skip end]
		}
		foreach n [namespace children $ns] {RegisterSubNS $n $skip}
	}
	RegisterSubNS [namespace current]::aliases [string length [namespace current]::aliases]
	
	proc getVar {chan name} {
		variable registeredVars
		# This line rises a error if not registed
		set var [dict get $registeredVars $name]
		return [set $var]
	}
	
	proc registerVar {var {alias {}}} {
		variable registeredVars
		variable ips
		if {$alias == {}} {
			set alias ::[namespace tail $var]
		}
		dict set registeredVars $alias $var
		foreach {chan} [array names ips] {
			catch {evalInterp $chan [list trace add variable $alias [list read write unset] [list traceHandler $alias]]}
		}
	}
	register var ::botnick
	register var chan ::channel
	
	proc evalInterp {chan args} {
		variable state
		set ip [getInterp $chan]
		dict set state $chan invoke 1
		interp limit $ip time -seconds [clock add [clock seconds] 3 second]
		after idle [list [namespace current]::resetInvoke $chan]
		interp eval $ip {*}$args
	}
	
	proc bgerror {chan args} {
		putlog "BGError for $chan: $args"
	}
	
	proc timeup {chan} {
		set tr {}
		set lv [info frame]
		for {set i 0} {$i < $lv} {incr i} {lappend tr [info frame $i]}
		putloglev d $chan "timeup: $lv $tr"
		variable state
		if {![dict exists $state $chan invoke]} {
			set ip [cinterp get $chan]
			dict set state $chan invoke 1
			after idle [list [namespace current]::resetInvoke $chan]
			interp limit $ip time -seconds [clock add [clock seconds] 3 second]
		}
	}
	
	proc resetInvoke {chan} {
		variable state
		if {[info frame] > 2} {return}
		if {[dict exists $state $chan invoke]} {
			dict unset state $chan invoke
		}
	}
	
	proc bind_callback {chan type callback args} {
		switch $type {
			pub {
				# This is called by a pubm event
				lassign $args nick uhost handle channel param
				if {[string tolower $chan] ne [string tolower $channel]} {return}
				set param [join [lrange [split $param] 1 end]]
				putloglev d $chan "Invoke pub bind callback for interp $chan: [list {*}$callback $nick $uhost $handle $channel $param]"
				cinterp eval $chan [list {*}$callback $nick $uhost $handle $channel $param]
			}
			pubm -
			time -
			join -
			part -
			sign -
			topc -
			kick -
			nick -
			mode -
			ctcp {
				if {[string tolower $chan] ne [string tolower [lindex $args 3]]} {return}
				putloglev d $chan "Invoke $type bind callback for interp $chan: [concat $callback $args]"
				cinterp eval $chan [concat $callback $args]
			}
		}
	}
	
	proc timer_callback {chan script} {
		cinterp eval $chan $script
	}
}

putlog "interpreter factory by DasBrain (#John @ Quakenet) loaded"
