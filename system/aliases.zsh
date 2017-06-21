# grc overides for ls
#   Made possible through contributions from generous benefactors like
#   `brew install coreutils`
if $(gls &>/dev/null)
then
  alias ls="gls -F --color"
  alias l="gls -lAh --color"
  alias ll="gls -l --color"
  alias la='gls -A --color'
fi
if type "exa" > /dev/null; then
  alias ls=exa
fi
orggrep() {
  find . -name '*.org' -exec cat {} \; | ag "$@"
}
open() {
  (xdg-open "$@")&
}
