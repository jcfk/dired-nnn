;;; dired-nnn.el --- nnn-like file actions for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/jcfk/dired-nnn

;; Package-Requires: ((emacs "29.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; nnn-like file actions for Dired, inspired by dired-hacks/dired-ranger.

;; See https://github.com/jcfk/dired-nnn for more information.

;;; Code:

(require 'dired)
(require 'dired-aux)

(defgroup dired-nnn nil
  "Implementation of nnn-like file actions for Dired."
  :group 'dired-nnn)

(defcustom dired-nnn-mark-new-files nil
  "If non-nil, recently pasted or moved files will be marked."
  :type 'boolean
  :group 'dired-nnn)

(defun dired-nnn--marked-files ()
  "Return list of the absolute paths of all marked files."
  (seq-remove
   (lambda (fpath)
     (member (file-name-nondirectory fpath) '("." "..")))
   (delete-dups
    (mapcan
     (lambda (dired-buffer)
       (let (dgmf)
         (setq dgmf
               (with-current-buffer (cdr dired-buffer)
                 (dired-get-marked-files nil nil nil t nil)))
         (if (or (eq (length dgmf) 1) (eq (car dgmf) t))
             (setq dgmf (cdr dgmf)))
         dgmf))
     dired-buffers))))

(defun dired-nnn--dest-filename-func (dir)
  "Return a function that renames files to under DIR."
  (lambda (file)
    (file-name-concat dir (file-name-nondirectory file))))

;;;###autoload
(defun dired-nnn-toggle-mark ()
  "Toggle selection of the file or directory under the point.

Cannot operate on '.' or '..'."
  (interactive)
  (condition-case nil
      (if (member
           (dired-get-filename nil nil)
           (dired-nnn--marked-files))
          (dired-unmark nil)
        (dired-mark nil))
    (error (message "Cannot operate on '.' or '..'"))))

;;;###autoload
(defun dired-nnn-paste (arg)
  "Paste the selected files to the cwd.

If `dired-nnn-mark-new-files' is non-nil, then recently created files will be
marked.

With prefix ARG, preserve marks on the original files. This overrides
`dired-nnn-mark-new-files'."
  (interactive "P")
  (let ((dir (dired-current-directory))
        (selected-files (dired-nnn--marked-files)))
    (if (not arg)
        (dolist (dired-buffer dired-buffers)
          (with-current-buffer (cdr dired-buffer)
            (dired-unmark-all-files ?*))))
    (dired-create-files #'copy-file "Copy" selected-files
                        (dired-nnn--dest-filename-func dir)
                        (when (and dired-nnn-mark-new-files (not arg))
                          ?*))
    (revert-buffer)))

;;;###autoload
(defun dired-nnn-move ()
  "Move the selected files to the cwd.

If `dired-nnn-mark-new-files' is non-nil, then recently created files will be
marked."
  (interactive)
  (let ((dir (dired-current-directory))
        (selected-files (dired-nnn--marked-files)))
    (dolist (dired-buffer dired-buffers)
      (with-current-buffer (cdr dired-buffer)
        (dired-unmark-all-files ?*)))
    (dired-create-files #'rename-file "Rename" selected-files
                        (dired-nnn--dest-filename-func dir)
                        (when dired-nnn-mark-new-files ?*)))
  (revert-buffer))

(provide 'dired-nnn)

;;; dired-nnn.el ends here
