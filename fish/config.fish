alias selfcam='mpv av://v4l2:/dev/video0 --profile=low-latency --untimed'
alias htpasswd='openssl passwd -apr1'

. /opt/asdf-vm/asdf.fish

export TERM=xterm-color

# opam configuration
test -r /home/andi/.opam/opam-init/init.zsh && . /home/andi/.opam/opam-init/init.fish > /dev/null 2> /dev/null || true
