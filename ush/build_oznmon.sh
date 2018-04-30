#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

target=${1:-cray}
export dir_root=${2:-$pwd}
clean=${3:-"YES"}
ver=${4:-2.0.0}

if [ $target = wcoss ]; then
    . /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray -o $target = wcoss_c ]; then
    . $MODULESHOME/init/sh
elif [ $target = theia ]; then
    . /apps/lmod/lmod/init/sh
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$dir_root/util/Ozone_Monitor/nwprod/oznmon_shared.v$ver/modulefiles/$target
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi
[ -d $dir_root/exec ] || mkdir -p $dir_root/exec

module purge
if [ $target = wcoss -o $target = cray ]; then
    module load $dir_modules/OznMonBuild
else
    source $dir_modules/OznMonBuild
fi
module list

dlist="oznmon_horiz.fd oznmon_time.fd"

for dir in $dlist; do

    cd $dir_root/util/Ozone_Monitor/nwprod/oznmon_shared.v$ver/sorc/$dir
    make -f makefile clean
    make -f makefile
    make -f makefile install
    if [ $clean = YES ]; then
        make -f makefile clean
    fi

done

exit 0