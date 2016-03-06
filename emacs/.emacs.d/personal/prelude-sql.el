(require 'sql)

(defun personal/mysql-connect ()
  (interactive)
  (let ((sql-user (getenv "DB_USER"))
        (sql-password (getenv "DB_PASS"))
        (sql-database (getenv "DB_BASE"))
        (sql-server (getenv "DB_HOST"))
        (sql-port 3306))
    (sql-mysql (current-buffer))))

(global-set-key (kbd "<f10>") 'personal/mysql-connect)
