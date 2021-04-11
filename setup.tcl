::tcl::tm::path add [file normalize [file dirname [info script]]]

apply {{} {
	foreach pkg [package names] {
		if {[string match dasbrain::* $pkg]} {
			package forget $pkg
		}
	}
}}