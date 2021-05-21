package require eggdrop 1.9.0

package provide dasbrain::ignorechathistory 1.0

namespace eval ::dasbrain::ignorechathistory {
	if {![info exists batches]} {
		variable batches [dict create]
	}
}

bind RAWT - BATCH ::dasbrain::ignorechathistory::batch
bind RAWT - PRIVMSG ::dasbrain::ignorechathistory::msg
bind RAWT - NOTICE ::dasbrain::ignorechathistory::msg

proc ::dasbrain::ignorechathistory::batch {from keyword text tag} {
	variable batches
	if {[string index $text 0] eq {+}} {
		if {[string equal -nocase [lindex [split $text] 1] {chathistory}]} {
			set batch [string range $text 1 [string first { } $text]-1]
			dict set batches $batch 1
		}
	} else {
		set batch [string range $text 1 end]
		dict unset batches $batch
	}
	return 0
}

proc ::dasbrain::ignorechathistory::msg {from keyword text tag} {
	variable batches
	return [expr {[dict exists $tag batch] && [dict exists $batches [dict get $tag batch]]}]
}
