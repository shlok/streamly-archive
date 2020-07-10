#!/usr/bin/env bash

#-------------------------------------------------------------------------------
# Utility functions.

cmd_fail() {
    if [[ $? != 0 ]]; then
        echo "failed."
        exit 1
    fi
}

cmd_result() {
    cmd_fail
    echo "success."
}

remove_if_exists() {
    local path="$1"

    local type="item"
    if [[ -f $path ]]; then
        type="file"
    elif [[ -d $path ]]; then
        type="directory"
    fi

    if [[ -e $path ]]; then
        echo -n "Removing existing $type ${path##*/}... "
        rm -r "$path"
        cmd_result
    fi
}

remove_test_dir() {
    local removal_dir="$1"
    if [[ -d $removal_dir ]]; then
        if [[ $removal_dir == "./tmp"* ]]; then
            rm -rf $removal_dir
        else
            echo "Directory outside of ./tmp not removed. $removal_dir"
            exit 1
        fi
    fi
}

echo_hrule() {
    echo "----------------------------------------"
}

# Appends "{minimum time},{maximum time},{average time}" to the given output file.
time_executable() {
    local warmup_count="$1"
    local avg_count="$2"
    local expected_in_output_1="$3"
    local expected_in_output_2="$4"
    local removal_dir="$5"
    local output_file="$6"
    local executable="$7"
    local arguments="${@:8}"
    if [[ $warmup_count != 0 ]]; then
        echo -n "        Warming up... "
        for j in $(seq 1 $warmup_count); do
            echo -n "$j "
            remove_test_dir $removal_dir
            $executable $arguments > /dev/null
            if [[ $? != 0 ]]; then echo "failed."; exit 1; fi
        done
        echo "done."
    fi
    echo -n "        Timing... "
    times="["
    for j in $(seq 1 $avg_count); do
        echo -n "$j "
        remove_test_dir $removal_dir
        gtime_executable=""
        if [[ $(uname) = *Linux* ]]; then
            gtime_executable="/usr/bin/time"
        elif [[ $(uname) = *Darwin* ]]; then
            gtime_executable="gtime"
        fi
        output=$($gtime_executable -f '_time%e' $executable $arguments 2>&1)
        if [[ $output != *"$expected_in_output_1"* ]]; then
            echo "Error: $expected_in_output_1 not found in output: $output"; exit 1
        fi
        if [[ $output != *"$expected_in_output_2"* ]]; then
            echo "Error: $expected_in_output_2 not found in output: $output"; exit 1
        fi
        seconds=$(echo "$output" | pcregrep -o1 '_time(.+)$')
        times="$times$seconds,"
    done
    echo "done."
    times=${times%,}"]"
    min_time=$(echo "print float(min($times))" | python)
    max_time=$(echo "print float(max($times))" | python)
    avg_time=$(echo "print float(sum($times))/len($times)" | python)
    echo -n "$min_time,$max_time,$avg_time" >> $output_file
}

#-------------------------------------------------------------------------------

if [[ ! -f ./bench-archive.c ]]; then
    echo "Please run script from its own directory."
    exit 1
fi

if [[ $(uname) = *Linux* ]]; then
    export LC_ALL="en_US.UTF-8"
fi

logfile="./tmp/bench.sh.log"
remove_if_exists $logfile

echo -n "Compiling C program... "
mkdir -p ./tmp
gcc -O2 -Wall bench-archive.c -I/usr/local/opt/libarchive/include -L/usr/local/opt/libarchive/lib -larchive -o bench-archive >> $logfile 2>&1
cmd_result

echo -n "Compiling Haskell programs... "
stack build >> $logfile 2>&1
cmd_result

c_executable="./bench-archive"
hs_plain_executable=$(stack exec -- which bench-archive-plain 2> /dev/null)
hs_streamly_executable=$(stack exec -- which bench-archive-streamly 2> /dev/null)

mb=1000000.0

echo_hrule

#-------------------------------------------------------------------------------


read -p "Create archives? (y/N) " answer
if [[ $answer == y ]]; then
    echo "Creating archives using C program (unless they already exist)..."
    tmp_dir="./tmp"
    mkdir -p $tmp_dir
    for i in 5000000,1 10000000,1 20,500000000 40,500000000; do
        IFS=',' read file_count file_size <<< "${i}"
        archive_path="$tmp_dir/${file_count}_${file_size}.tar"
        existing_file_count=$(tar -tf $archive_path 2> /dev/null | wc -l | bc)
        if [[ $existing_file_count = $file_count ]]; then
            echo "    Archive $archive_path already exists."
        else
            echo -n "    Creating database $archive_path... "
            rm -r $archive_path 2> /dev/null
            $c_executable write $archive_path $file_count $file_size
            cmd_result
        fi
    done
fi

echo "Measuring read performance..."

tmp_dir="./tmp"
mkdir -p $tmp_dir
csv_file=$tmp_dir/read.csv
rm -f $csv_file
echo -n "file_count,file_size,c_min,c_max,c_avg," >> $csv_file
echo -n "hs_plain_min,hs_plain_max,hs_plain_avg," >> $csv_file
echo -n "hs_streamly_min,hs_streamly_max,hs_streamly_avg," >> $csv_file
echo -n "hs_plain_min/c_max,hs_plain_max/c_min,hs_plain_avg/c_avg," >> $csv_file
echo -n "hs_streamly_min/c_max,hs_streamly_max/c_min,hs_streamly_avg/c_avg," >> $csv_file
echo -n "hs_streamly_min/hs_plain_max,hs_streamly_max/hs_plain_min,hs_streamly_avg/hs_plain_avg" >> $csv_file
echo "" >> $csv_file
for i in 5000000,1 10000000,1 20,500000000 40,500000000; do
    IFS=',' read file_count file_size <<< "${i}"
    total_file_size=$(($file_count * $file_size))
    archive_path="$tmp_dir/${file_count}_${file_size}.tar"
    echo -n "$file_count,$file_size," >> $csv_file
    echo "    Timing archive $archive_path with C..."
    time_executable 1 5 "Total filesize: $total_file_size" "File count:     $file_count" "" $csv_file $c_executable read $archive_path
    echo -n "," >> $csv_file
    echo "    Timing archive $archive_path with Haskell (plain)..."
    time_executable 1 5 "Total filesize: $total_file_size" "File count:     $file_count" "" $csv_file $hs_plain_executable read $archive_path
    echo -n "," >> $csv_file
    echo "    Timing archive $archive_path with Haskell (streamly)..."
    time_executable 1 5 "Total filesize: $total_file_size" "File count:     $file_count" "" $csv_file $hs_streamly_executable read $archive_path
    echo "" >> $csv_file
    nlines=$(wc -l < $csv_file)
    awk_prog="(NR!=$nlines)"'{print $0} '
    awk_prog=$awk_prog"(NR==$nlines)"'{print $1 "," $2 "," $3 "," $4 "," $5 "," $6 "," $7 "," $8 "," $9 "," $10 "," $11 "," ($6/$4) "," ($7/$3) "," ($8/$5) "," ($9/$4) "," ($10/$3) "," ($11/$5) "," ($9/$7) "," ($10/$6) "," ($11/$8)}'
    awk -F , "$awk_prog" $csv_file > tmp_file; mv tmp_file $csv_file
    echo "    Appended results to $csv_file"
done
