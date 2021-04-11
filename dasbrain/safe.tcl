# We canot use TclVFS because this works on a per process level
# SafeBase (::safe::*) has bugs and is hard to handle, so I write my own SafeBase
# Of course, some of the SafeBase is also here

# Goals:
# No information leak. - done? (see bugs)
# Possibility to read and write in some directories - pending (AliasOpen etc)
# Permission managment - done, but no interface yet
# Support of interpreters starting with "-" - partial. interpInit and inteprConfigure support them, interpCreate does not

# Documentation:

# New param -mount takes a list of lists with 2 or 3 arguments.
# If the nested list has 3 args, the first element is the mountpoint, the 2nd the real path, and the last are the list of permissions.
# If the 2nd argument is empty, no mountpoint is created, but the permissions are set.
# length 2: 1st mountpoint, 2nd real path

# -home:
# sets the home directory. default: /


# Known Bugs:

# Leak: [info script] returns the original path - use interp eval instead. NOTE: [catch], store old value, \32 - EOF -- reported, fixed
# Other solution: overwrite [info script], it converts the orig name to the virtual. Note: the same directory can be mounted under several points. Do we have to track this? -- done

# Leak: [info nameofexecutable] shoud return "" in a safe interp. Maybe this is a tcl bug. (A interp created with [interp create -safe] shoud be safe by default) -- reported, fixed
# Solution: overwrite [::tcl::info::nameofexecutable] -- done in safe interp

# Missing: tcl_platform has not all the data, some modules (e.g. msgcat) relies on them. Docs? Bug (in msgcat)? -- reported, msgcat fixed


# ToDo:
# Fix Bugs
# AliasOpen -- if access is not given, it defaults to "r". This is not documented.
# SafeFS managment
# add param -home
# add param -mount
# merge pathinfo into state var

# We require at least Tcl 8.5 - this means no Tcl 8.6 features.
package require Tcl 8.5

apply {{} {
	# we load the orig safe, so auto_load don't overwrite any of my procs
	set orig_safe [file normalize [file join [info library] safe.tcl]]
	set script [file normalize [info script]]
	if {$orig_safe ne $script} {
		# tclLog "load old script $orig_safe - $script"
		# uplevel #0 [list source $orig_safe]
	}
}}

namespace eval ::safe {

	namespace export interpCreate interpInit interpConfigure interpDelete interpAddToAccessPath interpFindInAccessPath setLogCmd

	variable Log

	# API - Please note that safeTk (::safe::loadTk) use some of the internal procedures from SafeTcl

	# public

	proc interpCreate {args} {
		# Init defaults
		set slave {}
		set nestedok 0
		set acc_path {}
		set staticsok 1
		set deleteHook {}
		set home /
		set mount {}
		
		# Parse args
		if {[lindex $args 0] ne "" && ![string match -* [lindex $args 0]]} {set args [lassign $args slave]}
		ParseArgs $args
		
		if {$slave eq ""} {
			set slave [interp create -safe]
		} else {
			interp create -safe $slave
		}
		
		InitInterp $slave $acc_path $staticsok $nestedok $deleteHook $home $mount
		return $slave
	}
	
	proc interpInit {slave args} {
		# Init defaults
		set nestedok 0
		set acc_path {}
		set staticsok 1
		set deleteHook {}
	
		if {![interp exist $slave]} {return -code error "could not find interpreter \"$slave\""}
		if {![interp issafe $slave]} {return -code error "interpreter \"$slave\" is not safe"}
		ParseArgs $args
		InitInterp $slave $acc_path $staticsok $nestedok $deleteHook
		return $slave
	}
	
	proc interpConfigure {slave args} {
		variable S$slave
		upvar 0 S$slave state
		if {![info exist state]} {return -code error "\"$slave\" is not an interpreter managed by [namespace current]::"}
		# we don't link $acc_path because we have to call auto_reset if it changes
		set acc_path {$invalid$}
		upvar 0 state(staticsok) staticsok
		upvar 0 state(nestedok) nestedok
		upvar 0 state(cleanupHook) deleteHook
		upvar 0 state(home) home
		set d [list -accessPath $state(access_path) -statics $staticsok -nested $nestedok -deleteHook $deleteHook -home $home]
		switch [llength $args] {
			0 {
				return $d
			}
			1 {
				# ok, we have to check the args...
				set arg [lindex $args 0]
				if {$arg ni [dict keys $d]} {return -code error "unknown option \"$arg\""}
				return [list $arg [dict get $d $arg]]
			}
			default {
				# 2 or more args...
				ParseArgs $args
				if {$acc_path ne {$invalid$}} {
					# ok, now we have a problem
					# ignore, retry, abort?
				}
			}
		}
	}
	
	proc interpDelete slave {
		variable S$slave
		upvar 0 S$slave state
		variable P$slave
		upvar 0 S$slave pathinfo
		if {[info exists state(cleanupHook)]} {
			set cl $state(cleanupHook)
			if {[llength $cl] > 0} {
				set state(cleanupHook) {}
				# Ok, safeTk likes it to throw an exception
				if {catch {{*}$cl $slave} res} {
					Log $slave "Delete hook error ($err)"
				}
			}
		}
		if {[info exists state]} {unset state}
		if {[info exists pathinfo]} {unset pathinfo}
		if {[interp exists $slave]} {interp delete $slave}
	}
	
	proc interpFindInAccessPath {slave dir2} {
		# Ok, we use now $state(access_path) and $state(access_path_slave)
		variable S$slave
		upvar 0 S$slave state
		set dir [file split [file normalize $dir2]]
		Log $slave "findInAccessPath $dir" DEBUG
		for {set tail {}; set p $dir} {[llength $p] > 0} {set tail [concat [lrange $p end end] $tail]; set p [lrange $p 0 end-1]} {
			Log $slave "findInAccessPath: check $p, tail: $tail, join: [file join {*}$p]" DEBUG
			if {[file join {*}$p] in $state(access_path)} {
				set index [lsearch -exact $state(access_path) [file join {*}$p]]
				Log $slave "$p found: index $index" DEBUG
				return /[join [concat [lindex $state(virt_path) $index] $tail] /]
			}
		}
		return -code error "$dir2 not found in access path $state(access_path)"
	}
	
	# Add a path to auto_path
	proc interpAddToAccessPath {slave dir2} {
		variable S$slave
		upvar 0 S$slave state
		set dir [file split [file normalize $dir2]]
		for {set tail {}; set p $dir} {[llength $p] > 0} {set tail [concat [lrange $p end end] $tail]; set p [lrange $p 0 end-1]} {
			if {[file join {*}$p] in $state(access_path)} {
				set index [lsearch -exact $state(access_path) [file join {*}$p]]
				set vpath /[join [concat [lindex $state(virt_path) $index] $tail] /]
				if {$vpath ni $state(auto_path)} {
					lappend state(auto_path) $vpath
					interp eval $slave [list set ::auto_path $state(auto_path)]
					# No reset required, index will rebuild if something is missing...
				}
				return $vpath
			}
		}
		# Ok, not found - add a new lib path (/tcl/lib$i)
		set new_mountpoint [list tcl lib[incr state(lib_cnt)]]
		lappend state(access_path) [file normalize $dir2]
		lappend state(virt_path) $new_mountpoint
		lappend state(auto_path) /[join $new_mountpoint /]
		variable P$slave
		upvar 0 P$slave pathinfo
		dict set pathinfo {*}$new_mountpoint /mount [file normalize $dir2]
		dict set pathinfo {*}$new_mountpoint /access {list source load}
		return /[join $new_mountpoint /]
	}
	
	proc setLogCmd args {
		variable Log
		switch [llength $args] {
			0 {
				if {[info exists Log] && [llength $Log] == 2 && [lindex $Log 0] eq "[namespace current]::OldLog"} {
					return [lindex $Log 1]
				} else {
					return {}
				}
			}
			1 {
				if {[lindex $args 0] eq ""} {
					set Log {}
				} else {
					set Log [list [namespace current]::OldLog [lindex $args 0]]
				}
			}
			default {set Log [list [namespace current]::OldLog $args]}
		}
	}
	
	# semi-private (used by safeTk.tcl)

	proc Log {slave msg {type ERROR}} {
		variable Log
		if {[info exists Log] && [llength $Log] > 0}  {
			{*}$Log $slave $msg $type
		}
	}

	# Compat for SafeTk (::safe::loadTk)
	# We don't use any of this procs, but safeTk does.
	
	
	# We don't need to add all the subdirs
	proc AddSubDirs {pathList} {
		return $pathList
	}
	
	# base name for storing all the slave states
    # the array variable name for slave foo is thus "Sfoo"
    # and for sub slave {foo bar} "Sfoo bar" (spaces are handled
    # ok everywhere (or should))
    # We add the S prefix to avoid that a slave interp called "Log"
    # would smash our "Log" variable.
    proc InterpStateName {slave} {
	return "S$slave"
    }
	
	# returns the variable name of the complete path list
    proc PathListName {slave} {
	return "[InterpStateName $slave](access_path)"
    }
    # returns the variable name of the complete path list
    proc VirtualPathListName {slave} {
	return "[InterpStateName $slave](access_path_slave)"
    }
    # returns the variable name of the complete tm path list
    proc TmPathListName {slave} {
	return "[InterpStateName $slave](tm_path_slave)"
    }
    # returns the variable name of the number of items
    proc PathNumberName {slave} {
	return "[InterpStateName $slave](access_path,n)"
    }
    # returns the staticsok flag var name
    proc StaticsOkName {slave} {
	return "[InterpStateName $slave](staticsok)"
    }
    # returns the nestedok flag var name
    proc NestedOkName {slave} {
	return "[InterpStateName $slave](nestedok)"
    }
    # Run some code at the namespace toplevel
    proc Toplevel {args} {
	namespace eval [namespace current] $args
    }
    # set/get values
    proc Set {args} {
	Toplevel set {*}$args
    }
    # lappend on toplevel vars
    proc Lappend {args} {
	Toplevel lappend {*}$args
    }
    # unset a var/token (currently just an global level eval)
    proc Unset {args} {
	Toplevel unset {*}$args
    }
    # test existance 
    proc Exists {varname} {
	Toplevel info exists $varname
    }
    # short cut for access path getting
    proc GetAccessPath {slave} {
	Set [PathListName $slave]
    }
    # short cut for statics ok flag getting
    proc StaticsOk {slave} {
	Set [StaticsOkName $slave]
    }
    # short cut for getting the multiples interps sub loading ok flag
    proc NestedOk {slave} {
	Set [NestedOkName $slave]
    }
    # interp deletion storing hook name
    proc DeleteHookName {slave} {
	return [InterpStateName $slave](cleanupHook)
    }
	# --- End Compat ---
	
	
	# Ok, internal
	
	# We have rewritten Log, so we can filter depending on the type, but for compat... a wraper
	proc OldLog {cmd slave msg type} {
		{*}$cmd "$type for slave $slave : $msg"
	}
	
	proc ParseArgs {arg} {
		upvar nestedok nestedok
		upvar acc_path acc_path
		upvar staticsok staticsok
		upvar deleteHook deleteHook
		upvar home home
		upvar mount mount
		
		set args $arg
		while {[llength $args] > 0} {
			set args [lassign $args arg]
			switch -exact -- $arg {
				-accessPath {
					set args [lassign $args acc_path]
					# not a list -> fail
					if {![string is list $acc_path]} {
						return -code error -level 2 "bad value \"$acc_path\" for -accessPath (type list)"
					}
				}
				-noStatics {
					set staticsok 0
				}
				-statics {
					set args [lassign $args staticsok]
					if {![string is boolean $staticsok]} {
						return -code error -level 2 "bad value \"$staticsok\" for -statics (type boolean)"
					}
				}
				-nestedLoadOk {
					set nestedok 1
				}
				-nested {
					set args [lassign $args nestedok]
					if {![string is boolean $nestedok]} {
						return -code error -level 2 "bad value \"staticsok\" for -nested (type boolean)"
					}
				}
				-deleteHook {
					set args [lassign $args deleteHook]
				}
				-home {
					set args [lassign $args home]
				}
				-mount {
					set args [lassign $args mount]
				}
				default {
					return -code error -level 2 "unknown option \"$arg\""
				}
			}
		}
	}
	
	proc getRealPath {ip path} {
		set path [normalizeFile $ip $path]
		variable P$ip
		upvar 0 P$ip pathinfo
		# Element 0 is emty
		set cpath [lrange [split $path "/"] 1 end]
		for {set p $cpath; set rest {}} {[llength $p] != 0} {set rest [linsert $rest 0 [lindex $p end]]; set p [lrange $p 0 end-1]} {
			if {[dict exist $pathinfo {*}$p "/mount"]} {
				# Mount point found
				set realPath [join [concat [list [dict get $pathinfo {*}$p "/mount"]] $rest] /]
				return $realPath
			}
		}
		# Later "Permission denied" - but for tests
		error "$path not mounted"
	}
	
	proc normalizeFile {ip path} {
		# variable S$ip
		# upvar 0 S$ip state
		# cpath - Current Path
		set cpath [getPWD $ip]
		if {$cpath eq "/"} {
			set cpath {{}}
		} else {
			set cpath [split $cpath /]
		}
		if {[string index $path 0] == "/"} {
			# Absolute path
			set cpath {{}}
		}
		if {[string index $path 0] == "~"} {
			# Ignore any user
			set cpath [split [getHome $ip] /]
			set path [join [lrange [split $path /] 1 end] /]
		}
		foreach npath [split $path /] {
			switch -exact -- $npath {
				{}		-
				. 		{}
				.. 		{
							set cpath [lrange $cpath 0 end-1]
							# Root is root - /../ = /
							if {[llength $cpath] == 0} {set cpath {{}}}
						}
				default	{
					lappend cpath $npath
				}
			}
		}
		if {[llength $cpath] == 1} {return /}
		return [join $cpath /]
	}
	
	proc getPWD {ip} {
		variable S$ip
		return [set S${ip}(pwd)]
	}
	
	proc getHome {ip} {
		variable S$ip
		return [set S${ip}(home)]
	}
	
	proc AliasCd {ip {arg {}}} {
		variable S$ip
		upvar 0 S$ip state
		if {$arg == {}} {set arg $state(home)}
		if {![file::isdirectory $ip $arg]} {error "couldn't change working directory to \"$arg\": no such file or directory"}
		set state(pwd) [normalizeFile $ip $arg]
		return
	}
	
	proc AliasEncoding {interp subcommand args} {
		switch -glob -- $subcommand {
			convertf* {
				# Pass throught
				return [interp invokehidden $interp encoding convertfrom {*}$args]
			}
			convertt* {
				# Pass throught
				return [interp invokehidden $interp encoding convertto {*}$args]
			}
			d* {
				# Todo: Need future handling -- switch to SafeFS
				error "Permission denied"
			}
			n* {
				return [interp invokehidden $interp encoding names]
			}
			s* {
				if {[llength $args] == 0} {
					return [interp invokehidden $interp encoding system]
				} else {
					# A safe interp is not allowed to change the system encoding
					return -code error "Permission denied"
				}
			}
			default {
				return -code error "bad option \"$subcommand\": must be convertfrom, convertto, dirs, names, or system"
			}
		}
		error "No return: encoding $subcommand $args"
	}
	
	
	# TODO: Cleanup
	proc InitInterp {ip acc_path staticsok nestedok deletehook {home /} {mount {}}} {
		namespace upvar [namespace current] S$ip state
		namespace upvar [namespace current] P$ip pathinfo
		
		interp alias $ip cd {} [namespace current]::AliasCd $ip
		interp alias $ip glob {} [namespace current]::AliasGlob $ip
		interp alias $ip pwd {} [namespace current]::getPWD $ip
		interp alias $ip source {} [namespace current]::AliasSource $ip
		interp alias $ip encoding {} [namespace current]::AliasEncoding $ip
		interp alias $ip load {} [namespace current]::AliasLoad $ip
		interp alias $ip open {} [namespace current]::AliasOpen $ip
		
		# Alias some info subcommands
		# TODO: move this subcommands to ::safe::info and rename/hide only here
		foreach {cmd hidden alias} [list ::tcl::info::script InfoScript [namespace current]::AliasInfoScript \
		::tcl::info::frame InfoFrame [namespace current]::AliasInfoFrame] {
			# We have to move the commands to the degfault namespace (::), so we can hide them
			interp eval $ip [list rename $cmd ::[namespace tail $hidden]]
			interp hide $ip [namespace tail $hidden]
			interp alias $ip $cmd {} $alias $ip
		}
		
		foreach ns [namespace children] {
			set targetns ::tcl::[namespace tail $ns]
			# Create empty ns
			interp eval $ip [list namespace eval $targetns {}]
			foreach cmd [info commands ${ns}::*] {
				set targetcmd ${targetns}::[namespace tail $cmd]
				interp alias $ip $targetcmd {} $cmd $ip
				interp eval $ip [list namespace eval $targetns [list namespace export [namespace tail $targetcmd]]]
			}
		}
		
		# interp eval $ip {namespace eval ::tcl::file {namespace ensemble create -command ::file}}
		interp expose $ip file
		
		
		# [info nameofexecutable] is implemented as proc, not as alias
		interp eval $ip [list rename ::tcl::info::nameofexecutable ::InfoNameofexecutable]
		interp hide $ip InfoNameofexecutable
		# We could return a vitrual path like /tcl/bin/tclsh or /tcl/bin/wish -- this prevents that init.tcl add ./lib to the auto_path
		interp eval $ip {proc ::tcl::info::nameofexecutable {} {return {/tcl/bin/wish}}}
		
		# Set up SafeFS
		set vfsInfo [list]
		set libacc [list list source load]
		
		# Set up /tcl/lib
		# TODO: change this, so only $virt_tcl_lib is nessecary (mp at $virt_tcl_lib, not at /tcl/lib)
		set virt_tcl_lib /[join [list tcl lib [file tail [info library]]] /]
		lappend vfsInfo [list /tcl/lib [file normalize [file dirname [info library]]] $libacc]
		
		# add user given directories
		foreach mp $mount {
			switch [llength $mp] {
				3 {
					lassign $mp vp rp ac
					if {$rp ne {}} {set rp [file normalize $rp]}
					lappend vfsInfo [list $vp $rp $ac]
				}
				2 {
					lassign $mp vp rp
					if {$rp ne {}} {set rp [file normalize $rp]}
					lappend vfsInfo [list $vp $rp]
				}
				default {
					return -code error "Wrong format: $mp"
				}
			}
		}
		
		# Build auto_path
		set virt_auto_path [list /tcl/lib $virt_tcl_lib]
		set cnt 0
		# Access path is usualy auto added to the autopath
		foreach pathItem [concat $::auto_path $acc_path] {
			set pathItem [file normalize $pathItem]
			if {[lsearch -exact -index 1 $vfsInfo $pathItem] != -1} {continue}
			set found 0
			foreach mp $vfsInfo {
				lassign $mp vp ap
				if {[string match "${ap}/*" $pathItem]} {
					set npath "${vp}[string range $pathItem [string length $ap] end]"
					if {$npath ni $virt_auto_path} {lappend virt_auto_path $npath}
					set found 1
					break
				}
			}
			if {$found} {continue}
			
			# ok, not on the path so we have to create a new mount like /tcl/lib1 /tcl/lib2 etc
			set new_mountpoint /tcl/lib[incr cnt]
			lappend virt_auto_path $new_mountpoint
			lappend vfsInfo [list $new_mountpoint $pathItem $libacc]
		}
		set state(lib_cnt) $cnt
		set state(tcl_lib) $virt_tcl_lib
		
		# TM path
		set cnt 0
		set tm_path [list]
		foreach pathItem [::tcl::tm::path list] {
			set pathItem [file normalize $pathItem]
			if {[lsearch -exact -index 1 $vfsInfo $pathItem] != -1} {continue}
			set found 0
			foreach mp $vfsInfo {
				lassign $mp vp ap
				if {[string match "${ap}/*" $pathItem]} {
					set npath "${vp}[string range $pathItem [string length $ap] end]"
					if {$npath ni $tm_path} {lappend tm_path $npath}
					set found 1
					break
				}
			 }
			if {$found} {continue}
			
			# ok, not on the path so we have to create a new mount
			set new_mountpoint /tcl/tm[incr cnt]
			lappend tm_path $new_mountpoint
			lappend vfsInfo [list $new_mountpoint $pathItem $libacc]
		}
		set state(tm_path_slave) $tm_path
		
		set state(vfsInfo) $vfsInfo
		# We create a link now. later we change all procs to use the new state(pathinfo)
		namespace eval [namespace current] [list upvar 0 S${ip}(pathinfo) P$ip]
		SyncPaths $ip
		
		set state(pwd) /
		set state(home) $home
		
		set state(nestedok) $nestedok
		set state(access_path_slave) [list]
		set state(cleanupHook) $deletehook
		set state(staticsok) $staticsok
		set state(auto_path) $virt_auto_path
		
		interp eval $ip [list set ::tcl_library $virt_tcl_lib]
		interp eval $ip [list set ::auto_path $virt_auto_path]
		
		# Init the auto loading system
		interp invokehidden $ip -global source [file join [info library] init.tcl]
		interp invokehidden $ip -global source [file join [info library] tm.tcl]
		interp eval $ip ::tcl::tm::path add $tm_path
		
	}
	
	proc SyncPaths interp {
		namespace upvar [namespace current] S$interp state
		namespace upvar [namespace current] P$interp pathinfo
		set pathinfo [dict create /access list]
		foreach mp $state(vfsInfo) {
			lassign $mp vp rp acc
			set vpL [lrange [split $vp /] 1 end]
			if {$rp ne {}} {
				lappend state(virt_path) $vpL
				lappend state(access_path) $rp
				lappend state(access_path_slave) $vp
				dict set pathinfo {*}$vpL /mount $rp
			}
			if {[llength $mp] == 3} {
				dict set pathinfo {*}$vpL /access $acc
			}
		}
	}
	
	
	# We introduce a subnamespace for the file ensemble...
	# this was proc FileAlias {ip subcommand args} so... copy & paste
	namespace eval file {
	
		namespace path [list [namespace parent]]
		
		proc atime {ip f} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			set rf [getRealPath $ip $f]
			# Ok, Maybe I shoud catch this
			return [interp invokehidden $ip tcl:file:atime $rf]
		}
		
		proc copy {ip args} {
			set force 0
			while {1} {
				set args [lassign $args arg]
				switch -exact -- $arg {
					-force {set force 1}
					-- -
					default {break}
				}
			}
			set target [normalizeFile [lindex $args end]]
			set files [list]
			foreach file [lrange $args 0 end-1] {
				set file [normalizeFile $file]
				checkPermission $ip file read $file
				lappend files [getRealPath $ip $file]
			}
			checkPermission $ip file write $target
			if {$force} {
				return [interp invokehidden $ip tcl:file:copy -force -- {*}$files [getRealPath $ip $target]]
			} else {
				return [interp invokehidden $ip tcl:file:copy -- {*}$files [getRealPath $ip $target]]
			}
		}
		
		proc delete {ip args} {
			set force 0
			while {1} {
				set args [lassign $args arg]
				switch -exact -- $arg {
					-force {set force 1}
					-- {break}
					default {set args [concat [list $arg] $args]; break}
				}
			}
			set files [list]
			foreach file $args {
				set file [normalizeFile $ip $file]
				checkPermission $ip file write $file
				lappend files [getRealPath $ip $file]
			}
			if {$force} {
				return [interp invokehidden $ip tcl:file:delete -force -- {*}$files]
			} else {
				return [interp invokehidden $ip tcl:file:delete -- {*}$files]
			}
		}
		
		proc dirname {ip dir} {
			# because [file dirname ~]
			if {[string index $dir 0] eq "~"} {
				return [interp invokehidden $ip tcl:file:dirname [normalizeFile $ip $dir]]
			} else {
				if {[string index $dir end] eq "/"} {set dir [string range $dir 0 end-1]}
				set res [::join [lrange [::split $dir /] 0 end-1] /]
				if {$res eq ""} {return "."}
				return $res
			}
		}
		
		proc executable {ip f} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file list $f
			return [interp invokehidden $ip tcl:file:executable [getRealPath $ip $f]]
		}
		
		proc exists {ip f} {
			set f [normalizeFile $ip $f]
			if {[catch {checkPermission $ip file list [file dirname $f]}]} {
				# Returns 1 if file name exists and the current user has search privileges for the directories leading to it, 0 otherwise.
				return 0
			}
			return [interp invokehidden $ip tcl:file:exists [getRealPath $ip $f]]
		}
		
		proc extension {ip f} {
			# I hope there is no sensitive information leak
			return [interp invokehidden $ip tcl:file:extension $f]
		}
		
		proc isdirectory {ip f} {
			set f [normalizeFile $ip $f]
			if {[catch {checkPermission $ip file list $f}]} {
				return 0
			}
			namespace upvar [namespace parent] P$ip pathinfo
			if {[llength [file split $f]] == 1 || [dict exist $pathinfo {*}[lrange [file split $f] 1 end]]} {
				# Ok, the access system can be abused to set the permissions on a single file. But this is not intended
				return 1
			}
			if {[catch {set rp [getRealPath $ip $f]}]} {return 0}
			return [interp invokehidden $ip tcl:file:isdirectory $rp]
		}
		
		proc isfile {ip f} {
			set f [normalizeFile $ip $f]
			if {[catch {checkPermission $ip file list $f}]} {
				return 0
			}
			return [interp invokehidden $ip tcl:file:isdirectory [getRealPath $ip $f]]
		}
		
		
		proc lstat {ip f var} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			return [interp invokehidden $ip tcl:file:lstat [getRealPath $ip $f] $var]
		}
		
		proc mkdir {ip args} {
			set files [list]
			foreach file $args {
				set f [normalizeFile $ip $file]
				checkPermission $ip file write $f
				lappend files [getRealPath $ip $f]
			}
			return [interp invokehidden $ip tcl:file:mkdir {*}$files]
		}
		
		proc mtime {ip f} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			set rf [getRealPath $ip $f]
			return [interp invokehidden $ip tcl:file:mtime $rf]
		}
		
		
		proc normalize {ip f} {
			return [normalizeFile $ip $f]
		}
		
		proc owned {ip f} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			if {[catch {getRealPath} rp]} {
				return 0
			} else {
				return [interp invokehidden $ip tcl:file:owned $rp]
			}
		}
		
		proc pathtype {ip f} {
			return [expr {[string charat $f 0] == "/" ?"absolute":"relative"}]
		}
		
		proc readable {ip f} {
			set f [normalizeFile $ip $f]
			# file readable is used by package.tcl
			if {([catch {checkPermission $ip file read $f}] && [catch {checkPermission $ip file source $f}]) || [catch {getRealPath $ip $f} rp]} {
				return 0
			}
			return [interp invokehidden $ip tcl:file:readable $rp]
		}
		
		proc rename {ip args} {
			set force 0
			while {1} {
				set args [lassign $args arg]
				switch -exact -- $arg {
					-force {set force 1}
					-- -
					default {break}
				}
			}
			set target [normalizeFile [lindex $args end]]
			set files [list]
			foreach file [lrange $args 0 end-1] {
				set file [normalizeFile $file]
				checkPermission $ip file write $file
				checkPermission $ip file read $file
				lappend files [getRealPath $ip $file]
			}
			checkPermission $ip file write $target
			if {$force} {
				return [interp invokehidden $ip tcl:file:rename -force -- {*}$files [getRealPath $ip $target]]
			} else {
				return [interp invokehidden $ip tcl:file:rename -- {*}$files [getRealPath $ip $target]]
			}
		}
		
		proc rootname {ip f} {
			return [interp invokehidden $ip tcl:file:rootname $f]
		}
		
		proc seperator {ip} {
			# Our viritual filesystem works with / only
			return /
		}
		
		proc size {ip f} {
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			set rf [getRealPath $ip $f]
			return [interp invokehidden $ip tcl:file:size $rf]
		}
		
		proc stat {ip f var} {
			# TODO: emulate stat and lstat for pure virtual directories - use array set
			set f [normalizeFile $ip $f]
			checkPermission $ip file read $f
			return [interp invokehidden $ip tcl:file:stat [getRealPath $ip $f] $var]
		}
		
		proc system {ip path} {
			return SafeFS
		}
		
		proc tail {ip f} {
			# Information leak: [file join [file dirname ~] [file tail ~]] -- home directory
			if {[string length $f] != 0 && [string index $f end] eq "/"} {set f [string range $f 0 end-1]}
			if {[string index $f 0] eq "~"} {set f [normalizeFile $ip $f]}
			# TODO: SafeFs... we have to do this ourself
			return [interp invokehidden $ip tcl:file:tail $f]
		}
		
		proc type {ip f} {
			if {[isdirectory $ip $f]} {
				return "directory"
			}
			set f [normalizeFile $ip $f]
			checkPermission $ip file list $f
			return [interp invokehidden $ip tcl:file:type [getRealPath $ip $f]]
		}
		
		proc volumes {ip} {
			return /
		}
		
		proc writable {ip f} {
			set f [normalizeFile $ip $f]
			if {[catch {checkPermission $ip file write $f}] || [catch {getRealPath $ip $f} rp]} {
				return 0
			}
			return [interp invokehidden $ip tcl:file:writable $rp]
		}
	}
	
	proc AliasGlob {ip args} {
		variable P$ip
		upvar 0 P$ip pathinfo
		# Process args
		set usedir 0
		set dir {}
		set nocomplain 0
		set usepath 0
		set opt {}
		set types {}
		set join 0
		set tails 0
		while {[llength $args] > 0} {
			set args [lassign $args arg]
			switch -glob -- $arg {
				-d* {
					# -path and -directory needs future handling
					if {$usepath} {error "\"-directory\" cannot be used with \"-path\""}
					set args [lassign $args dir]
					set usedir 1
				}
				-j* {set join 1}
				-n* {
					set nocomplain 1
				}
				-p* {
					# Ok, path is hard to handle
					if {$usedir} {error "\"-path\" cannot be used with \"-directory\""}
					set args [lassign $args dir]
					set usepath 1
					# error "unsupported option -path"
				}
				-ta* {set tails 1}
				-ty* {
					set args [lassign $args types]
					lappend opt -types $types
				}
				-- {break}
				-* {error "bad option \"$arg\": must be -directory, -join, -nocomplain, -path, -tails, -types, or --"}
				default {set args [concat [list $arg] $args]; break}
			}
		}
		# Ok, this works like the C-Code of glob
		set prefix {}
		if {$usepath} {
			set last [string last / $dir]
			if {$last == [string length $dir]} {
				set usepath 0
				set usedir 1
			} else {
				if {$last == -1} {
					set prefix $dir
					set $dir {}
				} else {
					set prefix [string range $dir $last end]
					set dir [string range $dir 0 $last-1]
					if {[string first / $dir] == -1} {
						append dir /
					}
				}
				foreach c [split {\[]*?{}}] {
					set prefix [string map [list $c \\$c] $prefix]
				}
			}
		}
		# I don't like gotos
		# goto 4590 :D
		if {[llength $types] > 0} {
			# Ok, I don't think that I need it...
			# But maybe this is wrong...
			# let's see
		}
		# Ok, glob start here...
		set mode [expr {$usedir?1:$usepath?2:0}]
		if {$join} {
			append prefix [join $args /]
			set res [TclGlob $ip $prefix $dir $nocomplain $tails $mode $types]
		} elseif {$usepath} {
			set res {}
			foreach arg $args {
				set str $prefix
				append str $arg
				lappend res {*}[TclGlob $ip $str $dir $nocomplain $tails $mode $types]
			}
		} else {
			set res {}
			foreach arg $args {
				lappend res {*}[TclGlob $ip $arg $dir $nocomplain $tails $mode $types]
			}
		}
		if {!$nocomplain} {
			if {[llength $res] == 0} {
				set err "no files matched glob pattern[expr {$join || [llength $args] == 0?"":"s"}] \""
				if {$join} {
					append err $prefix
				} else {
					append err [join $args " "]
				}
				append err "\""
				error $err
			}
		}
		return $res
	}
	
	proc TclGlob {interp pattern dir nocomplain tails mode types} {
		if {$types eq ""} {set types [list f d]}
		set path [lrange [split [normalizeFile $interp $dir] /] 1 end]
		if {[lindex $path end] eq ""} {set path [lrange $path 0 end-1]}
		if {$dir ne "" && [string index $dir end] ne "/"} {set dir $dir/}
		if {[string index $pattern 0] eq "/"} {		
			set pattern [string range $pattern 1 end]
			if {$dir eq ""} {set dir /}
		}
		set ret [MyGlob $interp $dir $path [lindex [split $pattern /] 0] [lrange [split $pattern /] 1 end] $types]
		if {$tails} {
			set res [list]
			foreach r $ret {
				lappend res [lindex [split $r /] end]
			}
			set ret $res
		}
		return $ret
	}
	
	proc MyGlob {interp prefix path pattern tail types} {
		# TODO: handle special cases "." and ".." (No information disclosure due normalize)
		# TODO: catch glob system call (information disclosure on error e.g. permission denied)
		variable P$interp
		upvar 0 P$interp pathinfo
		if {[llength $tail] == 0} {
			if {$pattern eq ""} {
				# No matching required -- ignore types
				return [list ${prefix}]
			}
			set t $types
		} else {
			set t [list d]
		}
		# [glob "*/*/*/*"] may return in an error if one of the subdirs is not listable...
		checkPermission $interp file list /[join $path /]
		set found [list]
		# Ok, first virtual dir
		if {"d" in $t && ([llength $path] == 0 || [dict exists $pathinfo {*}$path])} {
			# TODO: Optimize
			set p [dict get $pathinfo {*}$path]
			set r [dict filter $p script {k v} {
				return -level 0 [expr {[string match -nocase $pattern $k] && [string index $k 0] ne "/"}]
			}]
			lappend found {*}[dict keys $r]
		}
		set mp {}
		catch {set mp [getRealPath $interp /[join $path /]]}
		if {$mp ne ""} {
			lappend found {*}[glob -nocomplain -directory $mp -tails -types $t $pattern]
		}
		set found [lsort -unique $found]
		set res [list]
		if {[llength $tail] == 0} {
			foreach f $found {lappend res ${prefix}${f}}
		} else {
			foreach f $found {
				lappend res {*}[MyGlob $interp ${prefix}$f/ [concat $path [list $f]] [lindex $tail 0] [lrange $tail 1 end] $types]
			}
		}
		return $res
	}
	
	proc AliasSource {interp args} {
		if {[llength $args] == 3} {
			if {[lindex $args 0] eq "-encoding"} {
				set enc [lindex $args 1]
				set args [lrange $args 2 2]
			} else {
				error "unknown option [lindex $args 0]"
			}
		} elseif {[llength $args] == 1} {
			set enc [encoding system]
		} else {
			error "wrong # args: should be \"source ?-encoding name? fileName\""
		}
		set f [normalizeFile $interp [lindex $args 0]]
		checkPermission $interp file source $f
		set rp [getRealPath $interp $f]
		set code [catch {interp invokehidden $interp source -encoding $enc $rp} res opt]
		if {$code} {
			if {[file tail $rp] ni {tclIndex}} {
				# Log "Source error opt: $opt" DEBUG
			}
			set res [string map [list $rp [lindex $args 0]] $res]
			# TODO: create a nice stack trace (remove my stack items)
			# It seems that the last 4 lines are from this proc
			# Ok, last problem: if the source returns the error... e.g return -code error Test
			# the stack trace woudn't be nice
			# But this looks like a tcl bug.
			dict for {key val} $opt {dict set opt $key [string map [list $rp [lindex $args 0]] $val]}
			set errorInfo [dict get $opt -errorinfo]
			set errorInfo [lrange [split $errorInfo "\r\n"] 0 end-4]
			if {[llength $errorInfo] == 1} {
				dict unset opt -errorinfo
			} else {
				set errorInfo [string map [list $rp [lindex $args 0]] $errorInfo]
				dict set opt -errorinfo [join $errorInfo \n]
			}
		}
		dict incr opt -level
		Log $interp "Source $rp opt: $opt" DEBUG
		return -options $opt $res
	}
	
	
	# I hate it. Many librarys are written with identical pgk_Init and pgk_SafeInit. They simply don't
	# care why one procedure is called "SafeInit". Or what "Safe" mean. Or what it shoud do.
	# Yeah, their package is safe. It does not cause a segfault.
	proc AliasLoad {interp file {libname {}} {slave {}}} {
		variable S$interp
		upvar 0 S$interp state
		set args [list]
		if {$slave ne "" && !$state(nestedok)} {
			Log $interp "Permission denied for \"$file\" $libname: Nested loading not allowed (for $slave)"
			return -code error "Permission denied"
			set args [linsert $args 0 $slave]
		}
		if {$file eq ""} {
			if {!$state(staticsok)} {
				Log $interp "Permission denied for $libname: static loading not ok"
				return -code error "Permission denied"
			}
			Log $interp "load {} $libname $args" INFO
			return [interp invokehidden $interp load {} $libname {*}$args]
		}
		if {$libname ne ""} {
			set args [linsert $args 0 $libname]
		}
		checkPermission $interp file load [normalizeFile $interp $file]
		set rp [getRealPath $interp $file]
		Log $interp "load $rp $args" INFO
		return [interp invokehidden $interp load $rp {*}$args]
	}
	
	# TODO: nonblocking?
	proc AliasOpen {interp path {mode r} {permission {0666}}} {
		# Check for pipes
		if {[string index $path 0] eq "|"} {
			Log $interp "Denied to open pipe: $path" ERROR
			return -code error "Permission denied"
		}
		set normPath [normalizeFile $interp $path]
		# Allways check read access
		checkPermission $interp file read $normPath
		# Ok, maybe file permission is needed.
		# The first step is to fugure out the form.
		# Is r+b correct? Test: valid
		set cw 0
		# we do not pass NONBLOCK
		set nonblock 0
		switch -- $mode {
			r - rb {}
			r+ - rb+ - r+b -
			w - wb -
			w+ - wb+ - w+b -
			a - ab -
			a+ - ab+ - a+b {set cw 1}
			default {
				# ok, second form
				# NOTE: Opening a file with {RDONLY TRUNC} mode result in an error
				# other (new) modes are blocked. I don't know them, so they coud be dangerous. 
				set old $mode
				set mode [list]
				foreach m $old {
					RDONLY -
					BINARY -
					EXCL -
					APPEND -
					TRUNC -
					NOCTTY {lappend mode $m}
					NONBLOCK {set nonblock 1}
					WRONLY -
					RDWR {set cw 1; lappend mode $m}
					CREAT {
						# We check if this file exist, if not, we have to check write.
						# ([open doesnotexist {RDONLY CREAT}] result in creating doesnotexist)
						if {![[namespace current]::file::exist $interp $path]} {
							set cw 1
						}
						lappend mode $m
					}
					default {
						set cw 1
					}
				}
			}
		}
		if {$cw} {checkPermission $interp file write $normPath}
		# TODO: Shoud we realy pass the permission parameter through?
		set rp [getRealPath $interp $normPath]
		set code [catch {interp invokehidden $interp open $rp $mode $permission} res opt]
		if {$code} {
			set res [string map [list $rp $path] $res]
			dict for {key val} $opt {dict set opt $key [string map [list $rp $path] $val]}
		}
		# TODO: set to nonblocking if defined...
		dict incr opt -level
		return -options $opt $res
	}
	
	proc AliasInfoFrame {interp {level {}}} {
		if {$level eq {}} {
			catch {interp invokehidden $interp InfoFrame} res opt
			dict incr opt -level
			return -options $opt $res
		} else {
			set c [catch {interp invokehidden $interp InfoFrame $level} res opt]
			dict incr opt -level
			Log $interp $res DEBUG
			if {!$c && [dict exist $res file]} {
				if {[catch {interpFindInAccessPath $interp [dict get $res file]} f]} {
					dict unset res file
				} else {
					dict set res file $f
				}
			}
			return -options $opt $res
		}
	}
	
	proc AliasInfoScript {interp {new {$invalid$}}} {
		if {$new eq {$invalid$}} {
			# get
			set val [interp invokehidden $interp InfoScript]
			switch -glob -- $val {
				{} {
					return {}
				}
				\\** {
					Log $interp "Set by slave: $val" DEBUG
					return [string range $val 1 end]
				}
				default {
					if {[catch {interpFindInAccessPath $interp [file normalize $val]} f]} {
						return {}
					} else {
						return $f
					}
				}
			}
		} else {
			interp invokehidden $interp InfoScript *$new
			return $new
		}
	}
	
	# nameofexecutable is a proc
	
	# Currently only type file supported, later maybe socket etc.
	proc checkPermission {ip type args} {
		switch -exact -- $type {
			file {
				set access [lindex $args 0]
				set path [lindex $args 1]
				variable P$ip
				upvar 0 P$ip pathinfo
				# Element 0 is empty
				set cpath [lrange [split $path "/"] 1 end]
				for {set p $cpath} {[llength $p] != 0} {set p [lrange $p 0 end-1]} {
					if {[dict exist $pathinfo {*}$p "/access"]} {
						# Access rule found
						set allowed [dict get $pathinfo {*}$p "/access"]
						if {[lsearch -exact $allowed $access] < 0} {
							Log $ip "Permission denied: $type $args" ERROR
							return -code error "Permission denied"
						}
					}
				}
			}
			default {
				return -code error "Permission denied"
			}
		}
	}
}
