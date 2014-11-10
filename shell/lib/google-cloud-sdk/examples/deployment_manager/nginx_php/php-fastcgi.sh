cat <<EOF >/etc/init.d/php-fcgi
#!/bin/bash

DIR=/var/run/php-fastcgi
PID=\$DIR/php-fastcgi.pid
USER=www-data
RETVAL=0

start() {
  export PHP_FCGI_MAX_REQUESTS=0
  mkdir -p \$DIR
  if [[ -r \$PID ]]; then
    echo "Already running."
    exit 1
  else
    echo "Starting fcgi"
    /usr/bin/spawn-fcgi -s \$DIR/php-fastcgi.socket -P \$PID -C 6 -u \$USER -g \$USER -f /usr/bin/php5-cgi
    RETVAL=\$?
  fi
}

stop() {
  if [[ -r \$PID ]]; then
     killproc -p \$PID
     RETVAL=\$?
  else
    echo "Could not find PID file."
    exit 1
  fi
}

case \$1 in
  start)
    start
  ;;
  stop)
    stop
  ;;
  restart)
    stop
    start
  ;;
  *)
    echo "Usage: \$0 {start|stop|restart}"
    exit 1
  ;;
esac
exit \$RETVAL
EOF
chmod a+rx /etc/init.d/php-fcgi
update-rc.d php-fcgi defaults

