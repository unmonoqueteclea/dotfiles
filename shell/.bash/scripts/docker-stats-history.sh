#!/usr/bin/env bash

# Template from https://betterdev.blog/minimal-safe-bash-script-template/

# fail fast. see https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
# -e: exit immediately when a command fails
# -o pipefail: sets the exit code of a pipeline to that of the rightmost command
#               to exit with a non-zero status
# -u: treat unset variables as an error and exit immediately
# -E: needs to be set if we want the ERR trap to be inherited by shell functions
set -Eeuo pipefail

trap cleanup SIGINT SIGTERM ERR EXIT

# script logic
collect() {
    if [ "$cpu" -eq 1 ];
    then
	while [[ ($time -lt 0) || ($SECONDS -lt $time) ]];
	do
	    docker stats --no-stream |
		grep ${container} |
		awk  -v date="$(date +%T)" '{OFS=","};{print date, $3}'|
		sed -e 's/%//g' >> ${file};
	done
    else
	while [[ ($time -lt 0) || ($SECONDS -lt $time) ]];
	do
	    docker stats --no-stream |
		grep ${container} |
		awk -v date="$(date +%T)" '{OFS=","}{ if(index($4, "GiB")) {gsub("GiB","",$4); print date, $4*1000} else {gsub("MiB","",$4); print date,$4}}' >> ${file};
	done
    fi
}

main() {
  # modified from https://www.zakariaamine.com/2019-12-04/monitoring-docker
  container=${args[0]-}
  file=${script_dir}/stats-${container}.csv
  gnufile=${script_dir}/stats-${container}.gnuplot

  if [ "$plot" -eq 1 ];
  then
      # create gnuplot file
      touch ${gnufile}
      if test -f "$gnufile"; then rm ${gnufile}; fi
      echo "set datafile separator ','" >> ${gnufile}
      echo "set xdata time # tells gnuplot the x axis is time data" >> ${gnufile}
      echo "set timefmt \"%H:%M:%S\" # specify our time string format" >> ${gnufile}
      echo "set format x \"%H:%M:%S\" # otherwise it will show only MM:SS" >> ${gnufile}
      echo "plot \"${file}\" using 1:2 with lines "  >> ${gnufile}
  fi
  
  if test -f "$file"; then
      msg "${RED} File ${file} already exists. It will be overwritten ${NOFORMAT}"
      rm ${file} ;
  fi
  msg ""
  filetext="${GREEN}${file}${NOFORMAT}"
  msg "Capturing stats from container ${BLUE}${container}${NOFORMAT} to ${filetext}"
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

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

usage() {
  cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") -c|-m [-h] [-v] [-V] [-q] [-t secs] container_name
Collect docker stats values for a specific container and store them
in a text file.

Available options:
-h, --help      Print this help and exit
-v, --verbose   Print script debug info
-V, --version   Show script version
-c, --cpu      	Collect CPU usage values
-p, --plot      Plot values after finish collection
-m, --memory  	Collect memory usage values
-q, --quiet     Don't show status messages
-t, --time      Total collection seconds (default: infinite)
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
  # script cleanup here
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
    # default values of variables set from params
  cpu=0
  memory=0
  plot=0
  quite=0
  time=-1
  

  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -v | --verbose) set -x ;;
    -V | --version) version ;;
    --no-color) NO_COLOR=1 ;;
    -c | --cpu) cpu=1 ;;
    -m | --memory) memory=1 ;;
    -p | --plot) plot=1 ;;
    -q | --quiet) quiet=1 ;;
    -t | --time)
	time="${2-}"
	shift ;;
    -?*) die "Unknown option: $1" ;;
    *) break ;;
    esac
    shift
  done

  args=("$@")

  [[ ${#args[@]} -eq 0 ]] && msg "Missing script arguments" && usage
  [[ ${cpu} -eq 1 ]] && [[ ${memory} -eq 1 ]]  && die "Choose only one: cpu or memory"
  [[ ${cpu} -eq 0 ]] && [[ ${memory} -eq 0 ]]  && die "Choose one flag: cpu or memory"
  
  return 0
}

parse_params "$@"
setup_colors

# script logic here
main "$@"
