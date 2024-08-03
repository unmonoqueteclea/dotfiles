#!/bin/bash

# Originally from https://github.com/izabera/ps
# I've just added some comments

# An interview question for a position that requires knowledge of
# bash/linux/stuff could be:

# What if you're ssh'd into a machine, you're in your trusty bash
# shell, but unfortunately you cannot spawn any new processes because
# literally all other pids are taken. What do you do?

# And if that's what you're facing, this might be the tool for you!
# Now you can kinda sorta pretend that you have access to a working ps
# aux.


# so initially i was hoping you could get everything from /proc/<pid>/status
# because it's easy to parse (in most cases) but apparently you can't get
# things like the cpu% :(
#while read -ra data; do
#    case ${data[0]} in
#        State:) stat=${data[1]};;
#        Uid:) uid=${data[1]};;
#        Gid:) gid=${data[1]};;
#        VmSize:) vsz=${data[1]};;
#        VmRSS:) rss=${data[1]};;
#    esac
#done < "$dir"/status
# it would have been so easy!!!!
#
# SO INSTEAD LET'S DO IT AAAAAALL __PROPERLY__ (with zero forking)

# you can test that it doesn't fork with
# strace -fe fork,clone,clone3 -o strout ./psaux.bash; cat strout


# users array to map user IDs to usernames
users=()
# reads the /etc/passwd file to populate the users array
read_passwd() {
    local IFS=: fields
    while read -ra fields; do
        # If the username is longer than 8 characters, truncate it and append a '+' sign
        if (( ${#fields[0]} > 8 )); then
            users[fields[2]]=${fields[0]::7}+
        else
            users[fields[2]]=${fields[0]}
        fi
    done < /etc/passwd
}

# setting Internal Field Separator (IFS) to handle spaces, tabs, and newlines
IFS=$' \t\n'
# devices array to map major device numbers to device names
devices=()
# reads /proc/devices to resolve device names to major numbers
resolve_devices() {
    local fields
    while read -ra fields; do
        # Ensure the first field is a number, which indicates a major device number
        if [[ ${fields[0]} == [0-9]* ]]; then
            devices[fields[0]]+=${fields[1]}" "
        fi
    done < /proc/devices
}

# resolves device names from major and minor numbers
ttyname() {
    local major minor device fmt
    (( major = $1 >> 8, minor = $1 & 0xff ))
    # initialize REPLY variable for storing device names
    REPLY=?
    # loop through known devices to find a match
    for device in ${devices[major]}; do
        # Attempt to find the device name based on various formats
        REPLY=$device/$minor
        for fmt in %s/%s %s%s; do
            printf -v REPLY "$fmt" "$device" "$minor"
            [[ -e /dev/$REPLY ]] && return
        done
    done
}

# arrays to hold process information
process_uid=()
process_pid=()
process_cpu=()
process_mem=()
process_vsz=()
process_rss=()
process_tty=()
process_stat=()
process_start=()
process_time=()
process_command=()
widths=()

# adds process information to arrays and calculates column widths for printing
add_process() {
    process_uid[$2]=$1
    process_pid[$2]=$2
    process_cpu[$2]=$3
    process_mem[$2]=$4
    process_vsz[$2]=$5
    process_rss[$2]=$6
    process_tty[$2]=$7
    process_stat[$2]=$8
    process_start[$2]=$9
    process_time[$2]=${10}
    process_command[$2]=${11}

    local i width
    for (( i = 1; i <= $#; i++)) do
        width=${@:i:1} width=${#width}
        (( widths[i-1] = width > widths[i-1] ? width : widths[i-1] ))
    done
}

# retrieves the terminal size using ANSI escape codes
get_term_size() {
    local oldrow oldcol
    if [[ -t 1 ]]; then
        # Query the terminal for its size using ANSI escape codes
        IFS='[;' read -sdR -p $'\e[6n' _ oldrow oldcol
        printf '\e[%s' '?25l' '9999;9999H'
        IFS='[;' read -sdR -p $'\e[6n' _ LINES COLUMNS
        printf '\e[%s' '?25h' "$oldrow;1H"
    else
        COLUMNS=20000 # Default to a large value if not running in a terminal
    fi
}

# prints the process information in a formatted manner
printall() {
    get_term_size

    # format string for aligning output based on column widths
    printf -v fmt '%%-%ds   %%%ds %%%ds %%%ds %%%ds %%%ds %%-%ds %%-%ds %%%ss %%%ss %%-.%ds' "${widths[@]}"

    local i line
    for i in "${process_pid[@]}"; do
        # format and print each line of process information
        printf -v line "$fmt" \
        "${process_uid[i]}" \
        "${process_pid[i]}" \
        "${process_cpu[i]}" \
        "${process_mem[i]}" \
        "${process_vsz[i]}" \
        "${process_rss[i]}" \
        "${process_tty[i]}" \
        "${process_stat[i]}" \
        "${process_start[i]}" \
        "${process_time[i]}" \
        "${process_command[i]}"
        printf "%.${COLUMNS}s\n" "$line"
    done
}

# main function to gather and display process information
almost_ps_aux() {
    read_passwd
    resolve_devices
    read _ memtotal _ < /proc/meminfo
    read boottime _ < /proc/uptime
    boottime=${boottime%%[!0-9]*}

    local REPLY
    local cmdline stat status
    local dir pid cmd_line stat_fields status_fields name user state tty cpu start vsz rss time time_of_day

    local sys_clk_tck=100 # Hardcoded system clock tick value

    add_process USER PID %CPU %MEM VSZ RSS TTY STAT START TIME COMMAND

    # rry to get the current epoch time if not available
    [[ $EPOCHSECONDS ]] || printf -v EPOCHSECONDS '%(%s)T' -1

    # calculate the time of day in seconds since midnight
    printf -v time_of_day '%(10#%H*3600+10#%M*60+10#%S)T'
    time_of_day=$(($time_of_day))

    # iterate through process directories to gather information
    for dir in /proc/[1-9]*; do
        pid=${dir#/proc/}
        # open file descriptors for process information
        [[ $cmdline ]] && exec {cmdline}>&-
        [[ $stat    ]] && exec {stat}>&-
        [[ $status  ]] && exec {status}>&-

        {
        exec {cmdline}< "$dir"/cmdline || continue
        exec {stat}<    "$dir"/stat    || continue
        exec {status}<  "$dir"/status  || continue
        } 2>/dev/null

        cmd_line=()
        while read -rd '' -u "$cmdline"; do
            cmd_line+=("$REPLY")
        done

        read -ru "$status" _ name
        while read -ru "$status" -a status_fields; do
            case ${status_fields[0]} in
                VmLck:) vmlocked=${status_fields[1]} ;;
                Uid:) uid=${status_fields[2]} ;;
            esac
        done
        (( ! ${#cmd_line[@]} )) && cmd_line[0]=[$name]

        read -rd '' -u "$stat"

        stat_fields=(. "$pid" . ${REPLY##*) })
        state=${stat_fields[3]}
        (( vmlocked )) && state+=L
        (( stat_fields[19] >  0              )) && state+=N
        (( stat_fields[19] <  0              )) && state+='<'
        (( stat_fields[6]  == pid            )) && state+=s
        (( stat_fields[20] != 1              )) && state+=l
        (( stat_fields[8]  == stat_fields[5] )) && state+=+

        ttyname "${stat_fields[7]}"; tty=$REPLY
        start=$((boottime-(stat_fields[22] / sys_clk_tck)))
        cpu=$(((stat_fields[14]+stat_fields[15]) * 1000 / sys_clk_tck))
        if (( start )); then
            cpu=$((cpu/start)) cpu=$((${cpu::-1})).${cpu: -1}
        else
            cpu=0.0
        fi

        if (( start >= time_of_day )); then
            printf -v start '%(%b%d)T' "$((EPOCHSECONDS-start))"
        else
            printf -v start '%(%H:%M)T' "$((EPOCHSECONDS-start))"
        fi
        vsz=$((stat_fields[23]/1024))
        rss=$((stat_fields[24] * 4096 / 1024))
        mem=$((rss*1000/memtotal)) mem=$((${mem::-1})).${mem: -1}
        time=$(((stat_fields[14]+stat_fields[15]) / sys_clk_tck))
        printf -v time '%d:%02d' "$((time/60))" "$((time%60))"

        add_process      "${users[uid]-?}"     "$pid"     "$cpu"     "$mem"     "$vsz"     "$rss"     "$tty"       "$state"       "$start"      "$time"          "${cmd_line[*]}"
    done

    # close file descriptors
    [[ $cmdline ]] && exec {cmdline}>&-
    [[ $stat    ]] && exec {stat}>&-
    [[ $status  ]] && exec {status}>&-

    printall
}

# call the main function to execute the script
almost_ps_aux
