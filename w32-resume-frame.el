;;; w32-resume-frame.el --- Resume frame geometry for NTEmacs.

;; Copyright (C) 2014  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Version: 0.1
;; Keywords: frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package supplies frame parameter restoring feature for NTEmacs.

;;; Code:

(defgroup w32-resume-frame nil "Resume frame geometry for NTEmacs."
  :group 'frame)

(defcustom w32-resume-reg-program (executable-find "reg")
  "Path for reg.exe executable."
  :group 'w32-resume-frame)

(defvar w32-resume-emacs-registry-key "HKCU\\Software\\GNU\\Emacs"
  "A registry key for Emacs.")

(defvar w32-resume-storing-status-name "W32_RESUME_FRAME_USING"
  "A value name of package using status on windows registry.")

(defvar w32-resume-registry-value-alist
  '(("Emacs.Geometry"           func   w32-resume--geometry-encoder)
    ("Emacs.ToolBar"            bool   tool-bar-mode)
    ("Emacs.MenuBar"            bool   menu-bar-mode)
    ("Emacs.VerticalScrollBars" bool   scroll-bar-mode)
    ("Emacs.ScrollBarWidth"     [obj]  scroll-bar-width)
    ("Emacs.LineSpacing"        [obj]  line-spacing)
    )
  "Alist of registry value and value encoding method.
An element consists of (VALUE-NAME METHOD PARAM).
VALUE-NAME is a registry value name string.
METHOD is a symbol how to store PARAM value:
func - PARAM as function. A return value will be stored.
bool - PARAM value as boolean.
obj  - PARAM value as object.
If method is vector, PARAM will be handled as frame parameter.")

(defconst w32-resume-xrdb-true-value "on"
  "True value on xrdb.")

(defconst w32-resume-xrdb-false-value "off"
  "False value on xrdb.")

(defun w32-resume--bool-encoder (value)
  "Encode boolean value into xrdb notation."
  (if value
      w32-resume-xrdb-true-value
    w32-resume-xrdb-false-value))

(defun w32-resume--geometry-encoder ()
  "Encode current frame's geometry data into xrdb notation."
  (format "%dx%d+%d+%d"
	  (frame-width) (frame-height)
	  (frame-parameter (selected-frame) 'left)
	  (frame-parameter (selected-frame) 'top)))

(defun w32-resume--add-registry (value-name value-string)
  "Store VALUE-STRING into VALUE-NAME on `w32-resume-emacs-registry-key'
in windows registry."
  (call-process w32-resume-reg-program nil nil nil
		"add" w32-resume-emacs-registry-key
		"/v" value-name
		"/t" "REG_SZ"
		"/d" value-string
		"/f"))

(defun w32-resume--delete-registry (value-name)
  "Delete VALUE-NAME on `w32-resume-emacs-registry-key'
from windows registry."
  (call-process w32-resume-reg-program nil nil nil
		"delete" w32-resume-emacs-registry-key
		"/v" value-name
		"/f"))

(defun w32-resume--query-registry (value-name)
  "Fetch VALUE-NAME value on `w32-resume-emacs-registry-key'
in windows registry."
  (with-temp-buffer
    (when (eq 0 (call-process "reg" nil (current-buffer) nil
			      "query" "hkcu\\software\\gnu\\emacs"
			      "/v" value-name
			      ))
      (goto-char (point-min))
      (save-match-data
	(when (re-search-forward
	       (format "^    %s    REG_SZ    \\(.*\\)$"
		       (regexp-quote value-name)) nil t)
	  (match-string-no-properties 1))))))

(defvar w32-resume-stored-status-cache nil
  "Cache for `w32-resume-stored-p' function result.")

(defun w32-resume-stored-p ()
  "Return non-nil if frame status stored in windows registry."
  (when (eq 1 (or w32-resume-stored-status-cache
		  (setq w32-resume-stored-status-cache
			(if (w32-resume--query-registry w32-resume-storing-status-name)
			    1 0))))
    t))

(defun w32-resume--storing-status-on ()
  "Turn on storing status."
  (w32-resume--add-registry w32-resume-storing-status-name "")
  (setq w32-resume-stored-status-cache nil))

(defun w32-resume--storing-status-off ()
  "Turn off storing status."
  (w32-resume--delete-registry w32-resume-storing-status-name)
  (setq w32-resume-stored-status-cache nil))

(defun w32-resume--store-frame-status ()
  "Store frame status values described by `w32-resume-registry-value-alist'
into windows registry."
  (w32-resume--storing-status-on)
  (dolist (cur w32-resume-registry-value-alist)
    (let ((value-name (car cur))
	  (type (cadr cur))
	  (param (caddr cur))
	  framep)
      (when (vectorp type)
	(setq framep t
	      type (aref type 0)))

      (cond
       ((eq type 'bool)
	(w32-resume--add-registry value-name
				  (w32-resume--bool-encoder
				   (if framep
				       (frame-parameter nil param)
				     (symbol-value param)))))
       ((eq type 'obj)
	(let ((value (if framep
			 (frame-parameter nil param)
		       (symbol-value param))))
	  (if value
	      (w32-resume--add-registry value-name (prin1-to-string value t))
	    (w32-resume--delete-registry value-name))))
       ((eq type 'func)
	(let ((result (ignore-errors (funcall param))))
	  (if result
	      (w32-resume--add-registry value-name result)
	    (w32-resume--delete-registry value-name))))
       
       ))))

(defun w32-resume--clean-frame-status ()
  "Delete frame status values described by `w32-resume-registry-value-alist'
from windows registry."
  (dolist (cur w32-resume-registry-value-alist)
    (let ((value-name (car cur)))
      (w32-resume--delete-registry value-name))))

(provide 'w32-resume-frame)

;;; w32-resume-frame.el ends here
