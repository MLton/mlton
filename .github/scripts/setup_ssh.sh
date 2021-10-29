mkdir -m 0700 -p $HOME/.ssh
(umask 0077 ; printenv SSH_KEY > $HOME/.ssh/id_rsa)
(umask 0077 ; printenv SSH_CONFIG > $HOME/.ssh/config)
(umask 0077 ; printenv SSH_KNOWN_HOSTS > $HOME/.ssh/known_hosts)
