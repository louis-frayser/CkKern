#! /bin/bash
#### Rebuild external modules as needed for all kernels in /usr/src
#### See Also  ckkern(8)
### Versions
### Usage:
### 1. sh module-rebuld.sh >cmds
### 2. cat cmds
### 3. Copy and paste, or run cmds (sh cmds)
echo(){ command echo "$@" ;} 1>&2
 out(){ command echo "$@" ;}

Kernels=$(eselect kernel list | {
	      read _header
	      while read key version rest;
	      do command echo $version;
	      done ;})
### Modules
declare -A MDict=(
    [loop]="sys-fs/loop-aes"
    [zfs]="sys-fs/zfs-kmod"
    [vboxdrv]="app-emulation/virtualbox-modules"
)

### Loop throughh kernel versions
### For each version and missing module package
### 1. find, or build and keep, the missing modules
### 2. Install the missing modules manually,avoiding
###    uninstall of any current modules for other
###    curnel versions
find_module(){
    local found found_some=0 lost_some=0
    local kprefix=${2#linux-}
    local mdir
    for mdir in /lib/modules/${kprefix%-*}*
    do    found=$(find $mdir -name "$1.ko")
	  if [ -z "$found" ]
	  then echo "  Missing "$1.ko" for $mdir"
	       let lost_some+=1
	  else let found_some+=1
	  fi
    done
    if [ $lost_some -eq 0 ]
    then true
    elif [ $found_some -eq 0 ]
    then false
    else return 2
    fi
}

typeset -A Missing
for kernel in $Kernels
do  echo Kernel: $kernel
    for module in ${!MDict[@]}
    do pkg=${MDict[$module]}
       find_module $module $kernel
       case $? in 
	   0) echo "  ok:           for  $pkg in $kernel";;
           2) echo "  partial fail: $pkg in $kernel";;
	   *) echo "  fail:         $pkg for $kernel"
	      Missing[$kernel]+=" $pkg";;
       esac
    done
done
echo
echo "Result"
for kver in ${!Missing[@]}
do echo $kver
   for pkg in ${Missing[$kver]}
   do echo "  missing $pkg"
   done
   echo
done

Default=$(eselect kernel list | { 
	      read _header
	      while read key version rest;
	      do if [ -n "$rest" ]; then command echo "$version"; fi
	      done ;})


for kver in ${!Missing[@]}
do out
   out "eselect kernel set $kver"
   eselect kernel set $kver
   out "cd ${PACKAGES:=/var/cache/binpkgs}"
   for pnp in ${Missing[$kver]}
   do P=${pnp#*/}
      PN=${pnp%/*}
      eb=$(equery which $pnp)
      rhs=${eb##*/}
      PV=${rhs%.ebuild}
      pkg=$PN/$kver/$PV.tbz2

      if [ -e $pkg ] ; then echo found $pkg
      else
	  out ""
	  out "# Must build $pkg"
	  out "emerge -1Bv --nodeps $PN/$P"
	  out "mkdir -p $PN/$kver"
	  out "cp $PN/$PV.tbz2 $PN/$kver/"
      fi
      out "tar xvf $PN/$kver/$PV.tbz2 -C / ./lib/modules"
   done
done

out eselect kernel set "$Default"
