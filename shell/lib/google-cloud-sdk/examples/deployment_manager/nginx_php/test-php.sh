cat <<EOF >/srv/www/example/public_html/test.php
<?php phpinfo(); ?>
EOF
chmod a+r /srv/www/example/public_html/test.php

