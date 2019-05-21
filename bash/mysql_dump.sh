#!/usr/bin/env bash

DB_HOST="127.0.0.1"
DB_PORT="3306"

DB_USER="root"
DB_PWD="123456"

BACKUP_DIR="/tmp/mysqlbackup"

SKIP_DB="information_schema"
# SKIP_DB="db1 db2"


GZIP="$(which gzip) --fast"
RMF="$(which rm) -f"
MV=$(which mv)
CHMOD="$(which chmod) 0400"

MySql="$(which mysql) -h $DB_HOST -P $DB_PORT -u $DB_USER -p$DB_PWD"
MySqlDump="$(which mysqldump) --single-transaction --routines --quick -h $DB_HOST -P $DB_PORT -u $DB_USER -p$DB_PWD"

DATABASES=$($MySql -Bse "SHOW DATABASES;")

[ ! -d $BACKUP_DIR ] && mkdir -p $BACKUP_DIR

for dbname in $DATABASES; do

    skipdb=0

    if [ "$SKIP_DB" != "" ]; then
        for skb in $SKIP_DB; do
            [ "$skb" == "$dbname" ] && skipdb=1 && break
        done
    fi

    if [ "$skipdb" == "0" ]; then
        dbFileName="${BACKUP_DIR}/${dbname}.sql.gz"

        [ -f $dbFileName ] && $MV "$dbFileName" "$dbFileName.old"

        $MySqlDump -B $dbname | $GZIP > "$dbFileName" && $RMF "${dbFileName}.old"

        $CHMOD $dbFileName
    fi
done
