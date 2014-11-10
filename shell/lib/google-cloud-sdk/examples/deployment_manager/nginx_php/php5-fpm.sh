cat <<EOF > /etc/php5/fpm/pool.d/www.conf
[example]
user = www-data
group = www-data
listen = /var/run/php5-fpm.socket
listen.owner = www-data
listen.group = www-data
pm = dynamic
pm.max_children = 10
pm.start_servers = 2
pm.min_spare_servers = 2
pm.max_spare_servers = 6
chdir = /
EOF

