#! /bin/zsh

PANEL_FIFO=/tmp/panel-fifo
PANEL_HEIGHT=46
PANEL_FONT_FAMILY="CamingoCode:size=9"
#PANEL_FONT_FAMILY="Cantarell:size=9"
#PANEL_FONT_FAMILY="Cantarell:size=9"
ICON_FONT="FontAwesome:size=11"
ICON_FONT2="fontcustom:size=11"
# export PANEL_FIFO PANEL_HEIGHT PANEL_FONT_FAMILY

if [ $(pgrep -cx panel) -gt 1 ] ; then
	printf "%s\n" "The panel is already running." >&2
	exit 1
fi

trap 'trap - TERM; kill 0' INT TERM QUIT EXIT

[ -e "$PANEL_FIFO" ] && rm "$PANEL_FIFO"
mkfifo "$PANEL_FIFO"

bspc config top_padding $PANEL_HEIGHT
bspc control --subscribe > "$PANEL_FIFO" &
xtitle -sf 'T%s' > "$PANEL_FIFO" &

# clock
while true;
do
	echo 'C\uf017' $(date +"%l:%M %p");
	sleep 5;
done > "$PANEL_FIFO" &

# date
while true;
do echo 'D\uf073' $(date +'%b %d')
   sleep 5
done > $PANEL_FIFO &
# battery
while true;
do
	BATT_LEVEL=$(acpi -b | grep -o '...%' | sed 's/%//' | sed 's/ //')
	if [ $BATT_LEVEL -ge 80 ]
	then
		echo 'B\uf116' $BATT_LEVEL%
	fi

	if [ $BATT_LEVEL -ge 30 -a $BATT_LEVEL -lt 80 ]
	then
		echo 'B\uf117' $BATT_LEVEL%
	fi

	if [ $BATT_LEVEL -lt 30 ]
	then
		echo 'B%{T3}\uf115%{T1}' $BATT_LEVEL%
	fi


	sleep 30;
done > "$PANEL_FIFO" &

# alsa volume
while true;
do
	ALSA_VOLUME=$(amixer get Master | grep 'Mono: Playback' | grep -o '...%' | sed 's/\[//' | sed 's/%//' | sed 's/ //')
	ALSA_STATE=$(amixer get Master | grep 'Mono: Playback' | grep -o '\[on]')

	if [ $ALSA_STATE ]
	then
		if [ $ALSA_VOLUME -ge 70 ]
		then
			echo V'\uf028' $ALSA_VOLUME
		fi
		if [ $ALSA_VOLUME -gt 0 -a $ALSA_VOLUME -lt 70 ]
		then
			echo V'\uf027' $ALSA_VOLUME
		fi
		if [ $ALSA_VOLUME -eq 0 ]
		then
			echo V'\uf026' $ALSA_VOLUME
		fi
	else
		echo V'\uf026' $ALSA_VOLUME
	fi
	sleep 0.1
done > $PANEL_FIFO &


# wifi
while true;
do
	WIFI_SSID=$(iw wlp3s0 link | grep 'SSID' | sed 's/SSID: //' | sed 's/\t//')
	WIFI_SIGNAL=$(iw wlp3s0 link | grep 'signal' | sed 's/signal: //' | sed 's/ dBm//' | sed 's/\t//')
	echo L'\uf1eb' $WIFI_SSID '|' $WIFI_SIGNAL
	sleep 10
done > $PANEL_FIFO &

# power
echo P'\uf011' > $PANEL_FIFO &

# music controls
while true;
do
	SONG_NAME=$(mpc | head -n1)
	if [[ -n $(mpc status | grep paused) ]]
	then
		echo "R%{T3}%{A:mpc prev:}\uf19c%{A} %{A:mpc play:}\uf198%{A}  %{A:mpc next:}\uf17c%{A}%{T1} $SONG_NAME"
	else
		echo "R%{T3}%{A:mpc prev:}\uf19c%{A} %{A:mpc pause:}\uf191%{A} %{A:mpc next:}\uf17c%{A}%{T1} $SONG_NAME"
	fi

	sleep 1
done > $PANEL_FIFO &


. panel_colors

cat "$PANEL_FIFO" | panel_bar | lemonbar -g x$PANEL_HEIGHT -f "$PANEL_FONT_FAMILY" -f "$ICON_FONT" -f "$ICON_FONT2" -F "$COLOR_FOREGROUND" -B "$COLOR_BACKGROUND" -u 2 | zsh &

wait
