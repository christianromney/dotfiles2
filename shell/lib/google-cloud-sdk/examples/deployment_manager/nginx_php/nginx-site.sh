if [ "$USE_FPM" == "true" ]; then
  SOCKET_FILE=/var/run/php5-fpm.socket
else
  SOCKET_FILE=/var/run/php-fastcgi/php-fastcgi.socket
fi

cat <<EOF >/etc/nginx/sites-available/default
server {
  server_name example;
  access_log /srv/www/example/logs/access.log;
  error_log /srv/www/example/logs/error.log;
  root /srv/www/example/public_html;
  index index.htm index.html index.php;

  location ~ \.php\$ {
    include fastcgi_params;
    fastcgi_pass unix:$SOCKET_FILE;
  }
}
EOF


