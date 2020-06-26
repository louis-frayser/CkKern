#! /bin/bash
. /etc/conf.d/modules
Modules=($modules)
while read basename
  do
  case $basename in 
	  linux-*) KSrcs+=( $basename);;
	  (zfs*|spl*|loop*) MSrcs+=( ${basename%%-*});;
  esac
done < <(ls /usr/src)

#echo kernels src: ${KSrcs[@]}
#echo -e "modules src: ${MSrcs[@]}\n    using: conf.d/modules:  ${Modules[@]}"

for s in ${KSrcs[@]}; do
	KVers+=(${s#linux-})
done

#for v in ${KVers[@]} ; do echo ver:$v; done
ckextra(){
	declare -A unique
	### Check for  old and other odd kernels  (/boot and /usr/src)
	while read paren rest; do 
		if [[ "$paren" =~ \(.*\)   &&  "$rest" != ~* ]]
			then BlessedGS+=(${paren:1: -1})
		fi
	done < <(eix -n gentoo-sources)

	## Identify Kernels in /boot
	for kern in /boot/kernel-genkernel*
	  do k="${kern##*/}"
	  kver0="${k#kernel-genkernel-x86_64-}"
	  kver=${kver0%.old}
	  unique[$kver]=t
	done
	  GKVers=(${!unique[@]})
	echo 
	echo "Checking for extraneous(not blessed) kernels..."
	for vv in "${GKVers[@]}"; do
		rhs="${vv#*-gentoo}"
		rhs1=${rhs%-lf*}
		v=${vv%-gentoo*}${rhs1}
		unset match
		for bv in ${BlessedGS[@]}; do
			[ $v = $bv ] && { match=true; break ;}
		done
		if ! ${match:=false}; then echo -e "\tW - Extraneous kernel:" /boot/kern*$vv*; fi
	done
	echo -e "\nChecking for extraneouse, unblessed, souces*\n\tFIXME! not implimented yet." 1>&2
}
ckweird(){
### Check for extraneous modules, those with no kernel
	echo; echo "Checking /lib/modues for matching /boot/kernels..."
	for m in /lib/modules/[0-9]*; do 
		test -d $m || continue
		mver="${m##*/}"
		for b in /boot/kernel-*${mver}
		  do { if test -f $b 
				  then : echo "${m##*/}:  ${b##*/}: ok."
				  break
			  else   echo -e "\tW - No kernels for modules: $m"
				  break
			  fi  ;}
		done
	done
}
ckinst(){
	### Check for  uncompiled kernels
	echo -e "\nVerifying that all ${#KSrcs[@]} local kernel sources  are installed in /boot..." 1>&2
	for kver in ${KSrcs[@]}
	  do
	  # echo -en "src: $kver..."
	  if  ls /boot/kernel*"${kver#*-}"* >&/dev/null
		  then : echo -e "\tok"
	  else echo -e "\tW - kernel $kver not installed!" 1>&2
	  fi
	done
}

ckmods(){
	### Check for missing modules for kernels in /boot
	echo -e "\nVerify modules installed for each kernel..."
	for kver  in ${GKVers[@]}
	  do  #echo; echo $kver... 1>&2
	  mdir=/lib/modules/$kver 
	  if ! [ -d $mdir ] ; then  echo "e - missing  $mdir!" 
	  else
		  unset Missing
		  for m in ${Modules[@]}; do 
			  f=$(find $mdir -name "$m.ko"); 
			  test -n "$f" || Missing+=("$m")
		  done
		  if test "${#Missing[@]}" -gt 0 
			  then echo -e "\a\tE - Kernel $kver missing modules ${Missing[@]}!" 1>&2;
			  false
		  fi
	  fi
	  
	done
}
ckinst
ckextra
ckweird
ckmods
status=$?
echo
echo lxc-checkconfig...
CONFIG=/usr/src/linux/.config /usr/bin/lxc-checkconfig | egrep  'dis|miss'
#exit $status
exec /export/home/frayser/src/Projects/sys-kernel/ckkern/ckkern
