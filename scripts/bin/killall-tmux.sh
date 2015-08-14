tmux list-panes -s -F "#{pane_pid} #{pane_current_command}" | grep -v tmux | awk '{print }' | xargs kill -9
