# Emacs

Make sure to have the Elpa package repository configured:

    (add-to-list 'package-archives
         '("elpa" . "https://elpa.gnu.org/"))

Install Haskell Mode:

    M-x package-install Ret haskell-mode

Install Haskell for Emacs:

    M-x package-install Ret haskell-emacs

Start GHCI session:

    $ emacs hello.hs
    M-x haskell-session-change

Write a function:

    twice x = x * 2

Evaluate the buffer:

    C-c C-l
