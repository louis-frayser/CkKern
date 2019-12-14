# CkKern

**ckkern -- Check the kernel installation(Gentoo Linux)**


## OVERVIEW

CkKern is a Gentoo Linux tool to verify that the kernels in /boot are configured in GRUB, that the sources in /usr/src 
are compiled into /boot, that /lib/modules is populated for certain critical modules. 
Also verifies the latest kernel and sources are installed. 

Currently only the package "sys-kernel/gentoo-sources" is supported. And the kernels must be build with "genkernel."
It should be easy to work with other kernels.

This tool helps ensure that the running kernel hasn't been deleted and that critical modules (such as file systems) are available upon reboot.

## STATUS

Major tasks are completed.  Those are the tasks that verify that the currently running and currently configured kernels are configured for reboot.

See the TODO list at the end of the main source file, "ckkern.rkt."

![Screenshot](doc/Screenshot%20at%202019-12-13%2005-33-41.png)\
CkKern running in DrRacket. A binary executable can be crated from the {Racket} menu.

