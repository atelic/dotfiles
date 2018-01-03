# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
if $(gls &>/dev/null); then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi

if type "exa" > /dev/null; then
  alias ls='exa --group-directories-first $@'
  alias l='exa --group-directories-first -l $@'
fi

alias detect="bash <(curl -s https://blackducksoftware.github.io/hub-detect/hub-detect.sh)"

orggrep() {
  find . -name '*.org' -exec cat {} \; | ag "$@"
}

test_endpoints() {
  ENDPOINTS=("recipes" "recipes?q=decent" "recipes/2KAeAoQiOs6K4OaC0GC64g" "questions" "offers")
  for e in ${ENDPOINTS[@]}; do
    http -v ":5000/api/$e"
  done
}

alias scc='ssh ebarbour@10.100.100.83'
alias myip='ipconfig getifaddr en0'
alias pacman='pacapt'
