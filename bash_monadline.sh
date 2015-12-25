#!/bin/bash

# Make sure to run this in your .bashrc
# Change the directory below to point to your monadline binary

set_bash_prompt(){
    PS1="$(~/haskell/monadline/monadline $? $(jobs -p|wc -l)) "
}

PROMPT_COMMAND=set_bash_prompt
