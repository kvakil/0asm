#!/bin/bash
sector_no=$(($3 * 18))
dd if=/dev/zero of="$2" count=1 bs=512 seek="$sector_no" conv=notrunc
dd if="$1"      of="$2" count=1 bs=512 seek="$sector_no" conv=notrunc
