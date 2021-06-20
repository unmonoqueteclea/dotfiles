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
main() {
  msg "${RED}Read parameters:${NOFORMAT}"
  msg "- flag: ${flag}"
  msg "- param: ${param}"
  msg "- arguments: ${args[*]-}"
}

# find script location, so that we can create paths like this:
# cat "$script_dir/my_file"
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

usage() {
  cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") [-h] [-v] [-f] -p param_value arg1 [arg2...]
Script description here.
Available options:
-h, --help      Print this help and exit
-v, --verbose   Print script debug info
-V, --version   Show script version
-f, --flag      Some flag description
-p, --param     Some param description
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
  flag=0
  param=''

  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -v | --verbose) set -x ;;
    -V | --version) version ;;
    --no-color) NO_COLOR=1 ;;
    -f | --flag) flag=1 ;; # example flag
    -p | --param) # example named parameter
      param="${2-}"
      shift
      ;;
    -?*) die "Unknown option: $1" ;;
    *) usage ;;
    esac
    shift
  done

  args=("$@")

  # check required params and arguments
  [[ -z "${param-}" ]] && die "Missing required parameter: param"
  [[ ${#args[@]} -eq 0 ]] && die "Missing script arguments"

  return 0
}

parse_params "$@"
setup_colors

# script logic here
main "$@"
