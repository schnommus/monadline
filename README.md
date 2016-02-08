# monadline

Ridiculously lightweight prompt prettifier written in Haskell, Powerline inspired.

## Building/Installation

Clone this git repository to somewhere on your machine (i.e `~/.monadline`)

    git clone https://bitbucket.org/schnommus/monadline ~/.monadline

Install `ghc` (Glasgow Haskell Compiler) & `cabal` (Haskell package manager).
Packages are available in most distributions. On debian:

    sudo apt-get install ghc cabal-install

Now, enter the clone directory and build monadline

    cd ~/.monadline
    cabal build

Put this in your `~/.bashrc`

    #!/bin/bash

    MONADLINE_DIR=~/.monadline

    set_monadline_prompt(){
        PS1="$($MONADLINE_DIR/dist/build/monadline/monadline $? $(jobs -p|wc -l) "$(git branch 2>&1)") "
    }

    PROMPT_COMMAND=set_monadline_prompt

**IMPORTANT:** Change `MONADLINE_DIR` above to point to your monadline clone directory

To see some results:

    source ~/.bashrc

You're done configuring monadline! Next stop...

## Pretty fonts

To get monadline's fonts to display properly, you need powerline fonts.

These can be obtained from [here](https://github.com/powerline/fonts)

Follow the README to get the fonts installed.

Then, select one of these fonts in your terminal emulator.

Prettiness ensues. Have fun!
