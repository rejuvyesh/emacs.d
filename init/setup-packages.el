;;; local packages
;; elpa package-repositories
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(provide 'setup-packages)
;;; setup-packages.el ends here
