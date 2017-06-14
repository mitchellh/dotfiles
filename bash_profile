. ~/.bashrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[[ -s "/Users/mitchellh/.gvm/scripts/gvm" ]] && source "/Users/mitchellh/.gvm/scripts/gvm"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/mitchellh/Downloads/google-cloud-sdk/path.bash.inc' ]; then source '/Users/mitchellh/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/mitchellh/Downloads/google-cloud-sdk/completion.bash.inc' ]; then source '/Users/mitchellh/Downloads/google-cloud-sdk/completion.bash.inc'; fi
