Just a collection of some of my eggdrop scripts, some older, some newer.

## Useage

`source` the setup.tcl from your config.
Then `require dasbrain::modulename` to load `modulename`

## Modules

* clonecheck - checks for clones. Only really works on UnrealIRCd.
* pubeval - let's owners (or everyone) execute scripts in a "safe" interp.  
  Note: The safe interp was designed with the idea that channel owners can load their own channel managment scripts there.  
  Therefore, commands like `putmode` or `putkick` are available.
* safe.tcl - Drop-in replacement for the safe base.
* interp - Backend for pubeval - uses my own safe base (see above) and adds some eggdrop commands to the interp.
