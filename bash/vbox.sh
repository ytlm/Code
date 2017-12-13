#!/usr/bin/env bash

log=/tmp/vbox.log

function err() {
    echo "-----------------------------------------------"
    echo "Error : "
    cat $log
    echo "-----------------------------------------------"
    exit 1
}

function run() {
    echo -n "Running \"$@\" ... "
    eval $@ > $log 2>&1
    if [[ $? = 0 ]] ; then
        echo " OK"
    else
        echo " FAILED"
        err
    fi
}

vmnames=`VBoxManage list vms | awk -F "\"" '{print $2}'`

for name in $vmnames ; do
    run "VBoxManage snapshot $name take \"Snapshot $(date +%Y%m%d-%H%M%S)\""
    vmSnapShots=`VBoxManage showvminfo $name | grep "Name: Snapshot" | awk '{print $5}' | awk -F ')' '{print $1}'`
    vmsArr=(${vmSnapShots// /}) # turn into an array
    nums=${#vmsArr[@]}          # get an array length
    for ((i = 0; i < $nums - 1; i++)); do
        run "VBoxManage snapshot $name delete ${vmsArr[i]}"
    done
done

#
# crontab
#
# 0 16 * * 5 sh vbox.sh
#
# every friday at four o'clock for backup
#
#


