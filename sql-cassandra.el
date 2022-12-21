;;; sql-cassandra.el --- Cassandra support for sql.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Package-Requires: ((emacs "29"))
;; Version: 0.2.2
;; Keywords: sql, cassandra, cql, cqlsh

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'sql)
(require 'seq)

(defcustom sql-cassandra-program "cqlsh"
  "Command to start the Cassandra client."
  :type 'file
  :group 'SQL)

(defcustom sql-cassandra-options
  '("--no-color")
  "List of additional options for `sql-cassandra-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-cassandra-login-params
  `((user :default ,(user-login-name))
    password
    (database :default "system")
    (server :default "localhost")
    (port :default 9042))
  "List of login parameters needed to connect to Cassandra cluster."
  :type 'sql-login-params
  :group 'SQL)


(eval-and-compile
  (defconst sql-cassandra-special-commands
    '("capture" "cls" "copy" "describe" "expand" "login" "serial"
      "source" "unicode" "clear" "consistency" "desc" "exit" "help"
      "paging" "show" "tracing")
    "Special commands recognized by cqlsh.

Refer to:
https://cassandra.apache.org/doc/latest/cassandra/tools/cqlsh.html#special-commands"))

(eval-when-compile
  (defconst sql-cassandra-native-types
    '("ascii" "bigint" "blob" "boolean" "counter" "date" "decimal"
      "double" "duration" "float" "inet" "int" "smallint" "text" "time"
      "timestamp" "timeuuid" "tinyint" "uuid" "varchar" "varint")
    "Cassandra native types.

Refer to
 https://cassandra.apache.org/doc/latest/cassandra/cql/types.html#native-types")

  (defconst sql-cassandra-reserved-types
    '("bitstring" "byte" "complex" "enum" "interval" "macaddr")
    "Cassandra reserved types.

Refer to
 https://cassandra.apache.org/doc/latest/cassandra/cql/appendices.html#appendix-b-cql-reserved-types.")

  (defconst sql-cassandra-keywords
    '("add" "aggregate" "all" "allow" "alter" "and" "apply" "as" "asc"
      "ascii" "authorize" "batch" "begin" "bigint" "blob" "boolean"
      "by" "called" "clustering" "columnfamily" "compact" "contains"
      "count" "counter" "create" "custom" "date" "decimal" "delete"
      "desc" "describe" "distinct" "double" "drop" "entries" "execute"
      "exists" "filtering" "finalfunc" "float" "from" "frozen" "full"
      "function" "functions" "grant" "if" "in" "index" "inet" "infinity"
      "initcond" "input" "insert" "int" "into" "json" "key" "keys"
      "keyspace" "keyspaces" "language" "limit" "list" "login" "map"
      "modify" "nan" "nologin" "norecursive" "nosuperuser" "not" "null"
      "of" "on" "options" "or" "order" "password" "permission" "permissions"
      "primary" "rename" "replace" "returns" "revoke" "role" "roles"
      "schema" "select" "set" "sfunc" "smallint" "static" "storage"
      "stype" "superuser" "table" "text" "time" "timestamp" "timeuuid"
      "tinyint" "to" "token" "trigger" "truncate" "ttl" "tuple" "type"
      "unlogged" "update" "use" "user" "users" "using" "uuid" "values"
      "varchar" "varint" "where" "with" "writetime")
    "Cassandra keywords.

Refer to https://cassandra.apache.org/doc/latest/cassandra/cql/appendices.html#appendix-A"))

(defvar sql-cassandra-font-lock-keywords
  (eval-when-compile
    (list
     `(,(concat "^\\s-*" (regexp-opt sql-cassandra-special-commands) ".*$")
       . font-lock-doc-face)
     (apply #'sql-font-lock-keywords-builder
	    'font-lock-type-face nil sql-cassandra-native-types)
     (apply #'sql-font-lock-keywords-builder
	    'font-lock-type-face nil sql-cassandra-reserved-types)
     (apply #'sql-font-lock-keywords-builder
	    'font-lock-keyword-face nil
	    (seq-remove (lambda (el)
			  (member el sql-cassandra-native-types))
			sql-cassandra-keywords)))))



(defun sql-comint-cassandra (product options &optional buf-name)
  "Create comint buffer and connect to Cassandra cluster."
  (let ((params (append
		 options
		 (unless (string-empty-p sql-user)
		   (list "-u" sql-user))
		 (unless (string-empty-p sql-password)
		   (list "-p" sql-password))
                 (unless (string-empty-p sql-database)
                   (list "-k" sql-database))
                 (unless (string-empty-p sql-server)
                   (list sql-server
			 (number-to-string sql-port))))))
    (sql-comint product params buf-name)
    (add-hook 'sql-login-hook #'sql-cassandra--setup-interactive-mode)))

(defun sql-cassandra--setup-interactive-mode ()
  (remove-hook 'sql-login-hook #'sql-cassandra--setup-interactive-mode)

  (setq comint-process-echoes t)

  ;; Use our product's terminator
  (setq-local sql-send-terminator t)

  ;; cqlsh doesn't have special command line switch, so we have to use
  ;; special command and wait for response
  (goto-char (point-max))
  (sql-send-string "PAGING OFF")
  (let ((attempts 3))
    (while (and (>= (setq attempts (1- attempts)) 0)
		(goto-char (point-max))
		(not (re-search-backward "^disabled query paging" nil t)))
      (sleep-for 0.1))))

;;;###autoload
(defun sql-cassandra (&optional buffer)
  "Run Cassandra client as an inferior process."
  (interactive "P")
  (sql-product-interactive 'cassandra buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))

(sql-add-product
 'cassandra "Cassandra"
 :free-software t
 :font-lock 'sql-cassandra-font-lock-keywords
 :sqli-program 'sql-cassandra-program
 :sqli-options 'sql-cassandra-options
 :sqli-login 'sql-cassandra-login-params
 :sqli-comint-func #'sql-comint-cassandra
 :list-all "describe tables"
 :list-table "describe table %s"
 :prompt-regexp "^\\(?:[[:alnum:]_]+@\\)?cqlsh:[[:alnum:]_]*> "
 :prompt-cont-regexp "^ +\\.\\{3\\} "
 :syntax-alist '(;; map / set / udt literals
		 (?{ . "(") (?} . ")")
		 ;; list literals
		 (?\[ . "(") (?\] . ")")
		 )
 :terminator `(,(concat
		 "\\(^\\s-*"
		 ;; don't send terminator for special commands
		 (regexp-opt sql-cassandra-special-commands)
		 ".*\\|;\\)")
	       . ";")
 )

(provide 'sql-cassandra)
;;; sql-cassandra.el ends here
