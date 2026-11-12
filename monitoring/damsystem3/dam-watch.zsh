#!/bin/zsh -f
# Read-only DAM file checks. No network requests unless --send is supplied.
set -u
setopt PIPE_FAIL
export LC_ALL=C

config="${0:A:h}/config.zsh"
send=0
while (( $# )); do
  case "$1" in
    --config) (( $# >= 2 )) || exit 2; config="$2"; shift 2 ;;
    --send) send=1; shift ;;
    --dry-run) send=0; shift ;;
    *) print -u2 -- 'Usage: dam-watch.zsh [--config /absolute/config.zsh] [--dry-run|--send]'; exit 2 ;;
  esac
done
[[ -r "$config" ]] || { print -u2 -- "Configuration not readable: $config"; exit 2; }
typeset -a MONITOR_FILES MONITOR_IDS
COMPUTER_PING_URL=''
RECORDING_PING_URL=''
MAX_DATA_AGE_SECONDS=180
FUTURE_TOLERANCE_SECONDS=60
DAM_TIMEZONE=''
source "$config" || exit 2
[[ -z "$DAM_TIMEZONE" ]] || export TZ="$DAM_TIMEZONE"
[[ "$MAX_DATA_AGE_SECONDS" == <-> && "$FUTURE_TOLERANCE_SECONDS" == <-> ]] || {
  print -u2 -- 'Age limits must be nonnegative integers.'; exit 2
}
(( MAX_DATA_AGE_SECONDS >= 60 )) || { print -u2 -- 'Maximum age must be at least 60 seconds.'; exit 2; }
(( ${#MONITOR_FILES} > 0 && ${#MONITOR_FILES} == ${#MONITOR_IDS} )) || {
  print -u2 -- 'Configure one or more MONITOR_FILES and a matching MONITOR_IDS array.'; exit 2
}
for id in "${MONITOR_IDS[@]}"; do
  [[ "$id" == <-> ]] || { print -u2 -- 'Monitor IDs must be integers.'; exit 2; }
done

valid_url() {
  [[ "$1" =~ '^https://hc-ping[.]com/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$' ]]
}
ping_check() {
  # Do not log URLs: their UUIDs allow someone to send false heartbeats.
  /usr/bin/curl --silent --fail --connect-timeout 5 --max-time 10 \
    --output /dev/null --request POST --data-binary "$2" "$1"
}
network_failed=0
if (( send )); then
  valid_url "$COMPUTER_PING_URL" && valid_url "$RECORDING_PING_URL" || {
    print -u2 -- 'Set both URLs to distinct Healthchecks HTTPS UUID ping URLs.'; exit 2
  }
  [[ "$COMPUTER_PING_URL" != "$RECORDING_PING_URL" ]] || {
    print -u2 -- 'Use two separate Healthchecks checks.'; exit 2
  }
  ping_check "$COMPUTER_PING_URL" 'DAMWatch task is running on the recording Mac.' || {
    print -u2 -- 'Computer heartbeat could not reach Healthchecks.'; network_failed=1
  }
fi

now=$(/bin/date +%s)
typeset -a problems
for (( i=1; i<=${#MONITOR_FILES}; i++ )); do
  file="${MONITOR_FILES[$i]}"
  id="${MONITOR_IDS[$i]}"
  if [[ "$file" != /* || ! -f "$file" || ! -r "$file" ]]; then
    problems+=("Monitor $id: live file missing, inaccessible or not an absolute file path.")
    continue
  fi
  # Look for the latest complete, status-1 count record for this monitor.
  # A zero activity count is valid. Incomplete writes and malformed rows are ignored.
  # At one-minute intervals, 512 lines comfortably exceeds the default age limit.
  record=$(/usr/bin/tail -n 512 -- "$file" | /usr/bin/awk -F '\t' -v monitor="$id" '
    { sub(/\r$/, "") }
    NF == 42 && $4 == "1" && $6 == monitor && ($8 == "Ct" || $8 == "CT" || $8 == "0") {
      good=1
      for (j=11; j<=42; j++) if ($j !~ /^[0-9]+$/) good=0
      if (good) last=$2 " " $3
    }
    END { if (last != "") print last }
  ')
  if (( $? != 0 )) || [[ -z "$record" ]]; then
    problems+=("Monitor $id: no complete valid count record in the recent file tail.")
    continue
  fi
  epoch=$(/bin/date -j -f '%d %b %y %H:%M:%S' "$record" '+%s' 2>/dev/null)
  if (( $? != 0 )) || [[ "$epoch" != <-> ]]; then
    problems+=("Monitor $id: cannot parse recording timestamp.")
    continue
  fi
  age=$(( now - epoch ))
  if (( age < -FUTURE_TOLERANCE_SECONDS )); then
    problems+=("Monitor $id: timestamp is in the future; check clocks and DAM_TIMEZONE.")
  elif (( age > MAX_DATA_AGE_SECONDS )); then
    problems+=("Monitor $id: last valid record is $age seconds old (limit $MAX_DATA_AGE_SECONDS).")
  else
    print -- "Monitor $id: OK; last valid record age $age seconds."
  fi
done

if (( ${#problems} )); then
  message="RECORDING UNHEALTHY: ${(j: :)problems}"
  print -u2 -- "$message"
  if (( send )); then
    ping_check "${RECORDING_PING_URL}/fail" "$message" || {
      print -u2 -- 'Recording failure signal could not reach Healthchecks; missed pings remain the fallback.'
    }
  fi
  exit 1
fi
print -- 'RECORDING HEALTHY: all configured monitors have recent valid records.'
if (( send )); then
  ping_check "$RECORDING_PING_URL" 'All configured monitors have recent valid count records.' || {
    print -u2 -- 'Recording heartbeat could not reach Healthchecks.'; network_failed=1
  }
else
  print -- 'DRY RUN: no network requests sent.'
fi
(( network_failed == 0 )) || exit 3
exit 0
