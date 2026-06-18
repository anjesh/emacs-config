;;; my-csv.el --- CSV mode configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :hook (csv-mode . (lambda ()
                      (csv-align-mode 1)
                      (csv-header-line-mode 1))))

(provide 'my-csv)
;;; my-csv.el ends here
