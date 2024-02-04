#!/usr/bin/env fish
function awsume
    bass source (which awsume) $argv
end

#AWSume alias to source the AWSume script
alias awsume="source (pyenv which awsume.fish)"
