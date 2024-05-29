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
  :group 'dired-hacks)

;;;###autoload
(defun dired-nnn--marked-files ()
  "Return list of the absolute paths of all marked files."
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
    dired-buffers)))

;;;###autoload
(defun dired-nnn--dest-filename-func (dir)
  "Return a function that renames files to under DIR."
  (lambda (file)
    (file-name-concat dir (file-name-nondirectory file))))

(defun dired-nnn-toggle-mark ()
  "Toggle selection of the file or directory under the point."
  (interactive)
  (if (member
       (dired-get-filename)
       (dired-nnn--marked-files))
      (dired-unmark nil)
    (dired-mark nil)))

;; The case of multiple selected files with the same basename.
(defun dired-nnn-paste ()
  "Paste the selected files to the cwd."
  (interactive)
  (let ((dir (dired-current-directory)))
    (dired-create-files #'copy-file "Copy" (dired-nnn--marked-files)
                        (dired-nnn--dest-filename-func dir)))
  (dolist (dired-buffer dired-buffers)
    (with-current-buffer (cdr dired-buffer)
      (dired-unmark-all-files ?*)))
  (revert-buffer))

;; The case of multiple selected files with the same basename.
(defun dired-nnn-move ()
  "Move the selected files to the cwd."
  (interactive)
  (let ((dir (dired-current-directory)))
    (dired-create-files #'rename-file "Rename" (dired-nnn--marked-files)
                        (dired-nnn--dest-filename-func dir)))
  (dolist (dired-buffer dired-buffers)
    (with-current-buffer (cdr dired-buffer)
      (dired-unmark-all-files ?*)))
  (revert-buffer))

(provide 'dired-nnn)

;;; dired-nnn.el ends here
