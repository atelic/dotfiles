set fish_greeting ""

# Paths to your tackle
# set tacklebox_path ~/.tackle ~/.tacklebox

# Theme
#set tacklebox_theme entropy

# Which modules would you like to load? (modules can be found in ~/.tackle/modules/*)
# Custom modules may be added to ~/.tacklebox/modules/
# Example format: set tacklebox_modules virtualfish virtualhooks

# Which plugins would you like to enable? (plugins can be found in ~/.tackle/plugins/*)
# Custom plugins may be added to ~/.tacklebox/plugins/
# Example format: set tacklebox_plugins python extract

# Load Tacklebox configuration
# set tacklebox_path ~/.tackle
# set tacklebox_modules virtualfish virtualhooks 
# set tacklebox_plugins python extract git
# . ~/.tacklebox/tacklebox.fish

## Aliases
alias gst 'git status'
alias pacman 'sudo pacman'
alias ncmpcpp 'sh ~/bin/music.sh'
alias ls 'ls --group-directories-first --color'
alias sl 'ls'
function reload
    source ~/.config/fish/config.fish
end
function sudo!!
    eval sudo $history[1]
end



