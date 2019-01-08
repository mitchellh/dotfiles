function err {
    tput bold
    tput setaf 1
    echo "$@" 1>&2
    tput sgr0
}

function status {
    tput bold
    tput setaf 4
    echo "$@"
    tput sgr0
}

function success {
    tput bold
    tput setaf 2
    echo "$@"
    tput sgr0
}

function is_devserver {
    [ -f "$HOME/DEV_SERVER" ]
    return $?
}
