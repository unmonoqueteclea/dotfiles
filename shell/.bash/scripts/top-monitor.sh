#!/usr/bin/env bash
set -Eeuo pipefail

trap cleanup SIGINT SIGTERM ERR EXIT

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

cpu_general() {
    top -bn1 | grep "%Cpu(s)" | cut -d, -f 1 | tr -d '%Cpu(s): ' | tr -d ' us'
}

cpu_pid() {
    top -bn1 -p $pid | tail -n +8 | tr -s ' ' | cut -d ' ' -f 10
}

mem_general() {
    top -bn1 | grep "MiB Mem" | cut -d, -f 1 | tr -d 'MiB Mem : ' | tr -d ' total'
}

swap_general() {
    top -bn1 | grep "MiB Swap" | cut -d, -f 1 | tr -d 'MiB Swap : ' | tr -d ' total'
}

mem_pid() {
    top -bn1 -p $pid | tail -n +8 | tr -s ' ' | cut -d ' ' -f 11
}

collect() {
    while [[ ("$time" -lt 0) || ("$SECONDS" -lt "$time") ]];
    do
	timenow=$(date +%T);
	if [ ${cpu} -eq 1 ];
	then
	    val=$([ ${pid} -eq -1 ] && cpu_general || cpu_pid)
	    echo "$timenow,$val" >> $file
	else
	    val=$([ ${pid} -eq -1 ] && mem_general || mem_pid)
	    swap=$([ ${pid} -eq -1 ] && swap_general || echo '0')
	    echo "$timenow,$val,$swap" >> $file
	fi
	sleep $period
    done
}

main() {
  file=${script_dir}/top-stats.csv
  gnufile=${script_dir}/top-stats.gnuplot
  if [ ${plot} -eq 1 ];
  then
      touch ${gnufile}
      if test -f "$gnufile"; then rm ${gnufile}; fi
      echo 'set xtics font ", 3"' >> ${gnufile}
      echo 'set ytics font ", 4"' >> ${gnufile}
      echo 'set key font ",4"' >> ${gnufile}
      echo "set datafile separator ','" >> ${gnufile}
      echo "set xdata time # tells gnuplot the x axis is time data" >> ${gnufile}
      echo "set timefmt \"%H:%M:%S\" # specify our time string format" >> ${gnufile}
      echo "set format x \"%H:%M:%S\" # otherwise it will show only MM:SS" >> ${gnufile}
      echo "plot \"${file}\" using 1:2 with lines "  >> ${gnufile}
  fi
  if test -f "$file"; then
      msg "${RED} File ${file} already exists. ${NOFORMAT}"
      msg "${RED} File will be overwritten. ${NOFORMAT}"
      rm ${file} ;
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
	avg=$(awk  -F "," '{ total += $2; count++ } END { print total/count }' $file)
	max_value=$(cut -d, -f 2 $file | sort -g -r | head -n 1)
	min_value=$(cut -d, -f 2 $file | sort -g | head -n 1)
	echo "{\"time\": " $SECONDS ", \"count\": " $count ", \"average\": " $avg ", \"max\": " $max_value ", \"min\": " $min_value "}"
    fi
}

usage() {
  cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") -c|-m [-h] [-V] [-q] [-t secs] [-T period] [-pid pid]
Collect user CPU or free memory from 'top' and store the values in a text file.

Available options:
-h, --help      Print this help and exit
-V, --version   Show script version
-c, --cpu      	Collect user CPU usage values (in %)
-p, --plot      Plot values after finish collection
--pid           Collect stats for a specific PID
-m, --memory  	Collect free memory values (in MB)
-q, --quiet     Don't show status messages
-t, --time      Total collection seconds (default: infinite)
-T, --period    Collection period in seconds (default: 1)
EOF
  exit
}

version() {
    echo "$(basename "${BASH_SOURCE[0]}") 0.0.1"
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
  msg "$msg"
  exit "$code"
}

parse_params() {
  cpu=0
  memory=0
  plot=0
  quiet=0
  time=-1
  pid=-1
  period=1

  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -v | --verbose) set -x ;;
    -V | --version) version ;;
    --no-color) NO_COLOR=1 ;;
    -c | --cpu) cpu=1 ;;
    -m | --memory) memory=1 ;;
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
    -?*) die "Unknown option: $1" ;;
    *) break ;;
    esac
    shift
  done

  args=("$@")
  [[ ${cpu} -eq 1 ]] && [[ ${memory} -eq 1 ]]  && die "Choose only one: cpu or memory"
  [[ ${cpu} -eq 0 ]] && [[ ${memory} -eq 0 ]]  && die "Choose one flag: cpu or memory"
  return 0
}

parse_params "$@"
setup_colors

# script logic here
main "$@"
