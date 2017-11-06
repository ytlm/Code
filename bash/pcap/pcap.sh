#!/usr/bin/env bash

LOG=pcap.log

err()
{
    echo "-----------------------------------------------"
    echo "Error :"
    echo "---8<------------------------------------------"
    cat $LOG
    echo "------------------------------------------>8---"
    exit 1
}

run()
{
    echo -n "Running \"$@\" ... "
    eval $@ > $LOG 2>&1
    if [ $? = 0 ] ; then
        echo " OK"
    else
        echo " FAILED"
        err
    fi
}

FILENAME=result

cat $FILENAME | while read LINE
do
    run "curl -s -O $LINE"
    sleep 1
done

# sh pcap.sh 'the content will save the current dir'

