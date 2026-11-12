#!/bin/zsh -f
# Run manually on the recording Mac after configuration and a successful dry run.
set -eu
source_dir="${0:A:h}"
config_source="${1:-${source_dir}/config.zsh}"
target="$HOME/Library/Application Support/DAMWatch"
logs="$HOME/Library/Logs/DAMWatch"
plist="$HOME/Library/LaunchAgents/org.lab.damwatch.plist"
label='org.lab.damwatch'

[[ -r "$config_source" ]] || { print -u2 -- 'First create and edit config.zsh from config.example.zsh.'; exit 2; }
/bin/zsh -f "$source_dir/dam-watch.zsh" --config "$config_source" --dry-run
# Validate ping URLs without sending anything or printing secrets.
(
  source "$config_source"
  for url in "$COMPUTER_PING_URL" "$RECORDING_PING_URL"; do
    [[ "$url" =~ '^https://hc-ping[.]com/[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$' ]] || exit 2
  done
  [[ "$COMPUTER_PING_URL" != "$RECORDING_PING_URL" ]]
) || { print -u2 -- 'Configure two distinct Healthchecks UUID URLs before installing.'; exit 2; }

/bin/mkdir -p "$target" "$logs" "$HOME/Library/LaunchAgents"
/bin/chmod 700 "$target" "$logs"
/bin/cp "$source_dir/dam-watch.zsh" "$target/dam-watch.zsh"
[[ "${config_source:A}" == "$target/config.zsh" ]] || /bin/cp "$config_source" "$target/config.zsh"
/bin/chmod 600 "$target/config.zsh" "$target/dam-watch.zsh"

tmp_plist=$(/usr/bin/mktemp "$target/agent.XXXXXX")
trap '/bin/rm -f "$tmp_plist"' EXIT
/usr/bin/plutil -create xml1 "$tmp_plist"
/usr/bin/plutil -insert Label -string "$label" "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments -array "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.0 -string '/bin/zsh' "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.1 -string '-f' "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.2 -string "$target/dam-watch.zsh" "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.3 -string '--config' "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.4 -string "$target/config.zsh" "$tmp_plist"
/usr/bin/plutil -insert ProgramArguments.5 -string '--send' "$tmp_plist"
/usr/bin/plutil -insert RunAtLoad -bool YES "$tmp_plist"
/usr/bin/plutil -insert StartInterval -integer 60 "$tmp_plist"
/usr/bin/plutil -insert StandardOutPath -string "$logs/watch.log" "$tmp_plist"
/usr/bin/plutil -insert StandardErrorPath -string "$logs/errors.log" "$tmp_plist"
/usr/bin/plutil -lint "$tmp_plist"
/bin/chmod 600 "$tmp_plist"
/bin/mv "$tmp_plist" "$plist"
/bin/launchctl bootout "gui/$(/usr/bin/id -u)/$label" 2>/dev/null || true
/bin/launchctl bootstrap "gui/$(/usr/bin/id -u)" "$plist"
print -- 'Installed. Checks run at login and every 60 seconds while this user is logged in.'
print -- "Logs: $logs"
print -- 'Confirm both checks are Up in Healthchecks. Complete the failure tests in SETUP.txt.'
