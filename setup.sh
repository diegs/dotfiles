#!/usr/bin/env bash

# vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# golang
go get -u github.com/sourcegraph/go-langserver
go get -u github.com/nsf/gocode
go get -u golang.org/x/tools/cmd/goimports

echo You may want to visit https://github.com/BurntSushi/ripgrep/releases
