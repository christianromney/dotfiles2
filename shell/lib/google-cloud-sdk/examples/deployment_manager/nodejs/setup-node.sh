DIR=/srv/www
FILE=hello.js
mkdir -p $DIR
cat <<EOF >$DIR/$FILE
var http = require('http');
var server = http.createServer(function (request, response) {
  response.writeHead(200, {"Content-Type": "text/html"});
  response.end("<html><body>Hello World</body></html>");
});

server.listen($PORT);
EOF

chmod a+r $DIR/$FILE

cat <<EOF >/etc/init.d/nodejs
#!/bin/sh

PIDFILE=/var/run/nodejs.pid
NODE="/usr/local/bin/node $DIR/$FILE"

. /lib/init/vars.sh
. /lib/lsb/init-functions

do_start()
{
  start-stop-daemon --start --chuid nobody --quiet --pidfile \$PIDFILE --background --exec \$NODE  || { log_daemon_msg " NodeJS already running."; return 1; }

  start-stop-daemon --start --chuid nobody --quiet --make-pidfile --pidfile \$PIDFILE --background --exec \$NODE || { log_daemon_msg " Failed to start NodeJS."; return 2; }
}

case "\$1" in
  start)
    do_start
    ;;
  *)
    echo "Usage: nodejs start"
    exit 3
    ;;
esac
EOF

chmod a+x /etc/init.d/nodejs

