#!/usr/bin/env bash
set -Eeuo pipefail

trap cleanup SIGINT SIGTERM ERR EXIT

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

# convert command regex into a list of PIDs
command_to_pid() {
    pgrep -d',' -f "${command}"
}
# run top command in batch mode and remove the summary,
# apply the needed filters
run_top() {
    # command and pid are mutually exclusive filters
    local _pid=$([ "${command}" == "" ] && echo "${pid}" || command_to_pid)
    local _top='top -b -n 1'
    local filter_pid=$([ "${_pid}" == "" ] && echo "" || echo "-p ${_pid}")
    local filter_user=$([ "${user}" == "" ] && echo "" || echo "-u ${user}")
    # run top and remove summary rows
    ${_top} ${filter_pid} ${filter_user} | tail -n +8
}

# obtain the %CPU from top list of process
cpu_pids() {
    echo "${top_output}" | tr -s ' ' | awk -F " " 'BEGIN {sum=0};{ sum += $9 } END { print sum }'
}
# obtain the %MEM from top list of process
mem_pids() {
    echo "${top_output}" | tr -s ' ' | awk -F " " 'BEGIN {sum=0}; { sum += $10 } END { print sum }'
}
# collect stats at the specified period
collect() {
    while [[ ("$time" -lt 0) || ("$SECONDS" -lt "$time") ]];
    do
	timenow=$(date +%T);
	top_output=$(run_top)
	echo "$(date +%T),$(cpu_pids),$(mem_pids)" >> $file
	sleep $period
    done
}

main() {
  file=${script_dir}/${name}
  gnufile=${script_dir}/top-stats.gnuplot
  if test -f "$file"; then
      msg "${RED} File ${file} already exists. ${NOFORMAT}"
      rm -i ${file}
  fi
  if [ ${plot} -eq 1 ];
  then
      touch ${gnufile}
      if test -f "$gnufile"; then rm ${gnufile}; fi
      echo 'set xtics font ", 4"' >> ${gnufile}
      echo 'set ytics font ", 5"' >> ${gnufile}
      echo 'set key font ",5"' >> ${gnufile}
      echo "set datafile separator ','" >> ${gnufile}
      echo "set xdata time # tells gnuplot the x axis is time data" >> ${gnufile}
      echo "set timefmt \"%H:%M:%S\" # specify our time string format" >> ${gnufile}
      echo "set format x \"%H:%M:%S\" # otherwise it will show only MM:SS" >> ${gnufile}
      echo "plot \"${file}\" using 1:2 title 'CPU %' with lines, \\"  >> ${gnufile}
      echo "     \"${file}\" using 1:3 title 'Memory %' with lines "  >> ${gnufile}
  fi
  msg ""
  filetext="${GREEN}${file}${NOFORMAT}"
  msg "Capturing stats from  ${BLUE}top${NOFORMAT} to ${filetext}"
  msg "Press C-c to stop..."
  msg ""
  collect
}

report_results() {
    if [[ !(-z "${file:-}")  ]]
    then
	count=$(awk  -F "," '{ count++ } END { print count }' $file)
	avg_cpu=$(awk  -F "," '{ total += $2; count++ } END { print total/count }' $file)
	avg_mem=$(awk  -F "," '{ total += $3; count++ } END { print total/count }' $file)
	max_cpu=$(cut -d, -f 2 $file | sort -g -r | head -n 1)
	max_mem=$(cut -d, -f 3 $file | sort -g -r | head -n 1)
	min_cpu=$(cut -d, -f 2 $file | sort -g | head -n 1)
	min_mem=$(cut -d, -f 3 $file | sort -g | head -n 1)
	echo "{\"time\": " $SECONDS ", \"count\": " $count ", \"average_cpu\": " $avg_cpu ", \"max_cpu\": " $max_cpu ", \"min_cpu\": " $min_cpu " \"average_mem\": " $avg_mem ", \"max_mem\": " $max_mem ", \"min_mem\": " $min_mem "}"
    fi
    echo ""
}

usage() {
  cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") [-h] [-V] [-q] [-t secs] [-T period] [--pid pid] [--user user] [--command command] [--name name]
Collect CPU and memory % usage values from 'top' and store
the values in a text file with the columns: time,CPU(%),memory(%)

Values of CPU can be greater than % because they are measured for a single core.
Memory percentage doesn't take into account swap.

PID, USER and COMMAND filters are mutually exclusive.

Available options:
-h, --help      Print this help and exit
-V, --version   Show script version
-p, --plot      Plot values after finish collection
--pid           Collect stats for a specific PID (or comma-separated list of pids)
--user          Collect stats for a specific user
--command       Collect stats for the specific command regex
--name          Output file name (default "top-stats.csv")
-q, --quiet     Don't show status messages
-t, --time      Total collection seconds (default: infinite)
-T, --period    Collection period in seconds (default: 1)
EOF
  exit
}

version() {
    echo "$(basename "${BASH_SOURCE[0]}") 0.1.0"
    exit
}

# like of a finally block for the script
cleanup() {
  trap - SIGINT SIGTERM ERR EXIT
  if [ "$plot" -eq 1 ];
  then
      $(gnuplot -p ${gnufile})
  fi
  report_results
}

setup_colors() {
  if [[ -t 2 ]] && [[ -z "${NO_COLOR-}" ]] && [[ "${TERM-}" != "dumb" ]]; then
    NOFORMAT='\033[0m' RED='\033[0;31m' GREEN='\033[0;32m' ORANGE='\033[0;33m' BLUE='\033[0;34m' PURPLE='\033[0;35m' CYAN='\033[0;36m' YELLOW='\033[1;33m'
  else
    NOFORMAT='' RED='' GREEN='' ORANGE='' BLUE='' PURPLE='' CYAN='' YELLOW=''
  fi
}

# Messages printed with msg() are sent to stderr stream and support special sequences
# In short: stdout is for output, stderr is for messaging.
msg() {
  echo >&2 -e "${1-}"
}

die() {
  local msg=$1
  local code=${2-1} # default exit status 1
  msg "${RED}${msg}${NOFORMAT}"
  exit "$code"
}

parse_params() {
  plot=0
  quiet=0
  time=-1
  period=1
  pid=""
  user=""
  command=""
  name="top-stats.csv"

  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -v | --verbose) set -x ;;
    -V | --version) version ;;
    --no-color) NO_COLOR=1 ;;
    -p | --plot) plot=1 ;;
    -q | --quiet) quiet=1;;
    -T | --period)
	period="${2-}"
	shift ;;
    -t | --time)
	time="${2-}"
	shift ;;
    --pid)
	pid="${2-}"
	shift ;;
    --user)
	user="${2-}"
	shift ;;
     --command)
	command="${2-}"
	shift ;;
     --name)
	name="${2-}"
	shift ;;
    -?*) die "Unknown option: $1" ;;
    *) break ;;
    esac
    shift
  done

  args=("$@")
  [[ ${user} != "" ]] && [[ ${command} != "" ]]  && die "Error: user, command and pid filters are mutually exclusive"
  [[ ${user} != "" ]] && [[ ${pid} != "" ]]  && die "Error: user, command and pid filters are mutually exclusive"
  [[ ${pid}  != "" ]] && [[ ${command} != "" ]]  && die "Error: user, command and pid filters are mutually exclusive"  
  return 0
}

setup_colors
parse_params "$@"

# script logic here
main "$@"
