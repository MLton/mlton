mkdir -m 0700 -p $HOME/.ssh
(umask 0077 ; printenv SSH_KEY_RSA >  $HOME/.ssh/id_rsa)
if [ ! -s $HOME/.ssh/id_rsa ]; then rm -f $HOME/.ssh/id_rsa; fi
(umask 0077 ; printenv SSH_KEY_ED25519 > $HOME/.ssh/id_ed25519)
if [ ! -s $HOME/.ssh/id_ed25519 ]; then rm -f $HOME/.ssh/id_ed25519; fi
(umask 0077 ; printenv SSH_CONFIG > $HOME/.ssh/config)
(umask 0077 ; printenv SSH_KNOWN_HOSTS > $HOME/.ssh/known_hosts)
