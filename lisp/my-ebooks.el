;;; my-ebooks.el --- CalibreDB and nov configuration -*- lexical-binding: t; -*-

(require 'use-package)

(defun my/nov-view-image (path)
  "View the image at PATH in a new buffer."
  (interactive)
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (image-mode)
      (pop-to-buffer buf))))

(defun my/nov-insert-image (path alt)
  "Insert an image for PATH at point, falling back to ALT.
Images are resized to a smaller dimension and are clickable."
  (let ((type (if (or (and (fboundp 'image-transforms-p) (image-transforms-p))
                      (not (fboundp 'imagemagick-types)))
                  nil
                'imagemagick)))
    (if (not (display-graphic-p))
        (insert alt)
      (seq-let (x1 y1 x2 y2) (window-inside-pixel-edges
                              (get-buffer-window (current-buffer)))
        (let* ((max-width (truncate (* 0.3 (- x2 x1))))
               (max-height (truncate (* 0.3 (- y2 y1))))
               (image
                (ignore-errors
                  (create-image path type nil
                                :ascent 100
                                :max-width max-width
                                :max-height max-height))))
          (if image
              (let ((map (make-sparse-keymap)))
                (define-key map [mouse-1]
                            (lambda ()
                              (interactive)
                              (my/nov-view-image path)))
                (define-key map (kbd "RET")
                            (lambda ()
                              (interactive)
                              (my/nov-view-image path)))
                (insert (propertize " "
                                    'display image
                                    'keymap map
                                    'help-echo "Click to enlarge")))
            (insert alt)))))))

(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" "\\.EPUB\\'")
  :config
  (setq nov-text-width 80)
  (add-hook 'nov-mode-hook #'visual-line-mode)
  (require 'shr)
  (setq shr-inhibit-images nil)
  (setq shr-use-fonts t)
  (setq shr-max-image-proportion 0.8)
  (advice-add 'nov-insert-image :override #'my/nov-insert-image))

(defun my/calibredb-open-with-nov ()
  "Open the current book with nov.el."
  (interactive)
  (let ((file (calibredb-get-file-path (car (calibredb-find-candidate-at-point)) t)))
    (if file
        (progn
          (find-file file)
          (when (string-suffix-p "epub" file t)
            (nov-mode)))
      (message "No file found."))))

(use-package calibredb
  :ensure t
  :defer t
  :config
  (setq calibredb-root-dir "~/Calibre/")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Calibre/")))
  :bind
  ("C-c e" . calibredb)
  (:map calibredb-search-mode-map
        ("RET" . my/calibredb-open-with-nov)
        ("s" . calibredb-search-live-filter)
        ("?" . calibredb-dispatch))
  (:map calibredb-show-mode-map
        ("RET" . my/calibredb-open-with-nov)))

(provide 'my-ebooks)
;;; my-ebooks.el ends here
