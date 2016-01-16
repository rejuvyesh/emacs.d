;;; local packages
;; elpa package-repositories
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  
(package-initialize)

;; Install use-package if not there
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(provide 'setup-packages)
;;; setup-packages.el ends here
