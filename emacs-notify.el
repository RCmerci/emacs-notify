;;; emacs-notify.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  rcmerci

;; Author: rcmerci <rcmerci@rcmercis-rmbp.local>
;; Keywords: 

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

;; 

;;; Code:

(require 'cl-lib)
(require 'names)


(define-namespace emacs-notify

(defvar //notification-surviving-time 5
  "the notification's surviving time.")
(defvar //bottom-window-lines 1
  "the bottom window's height, in lines.")
(defvar //notification-buffer-name "*emacs-notify*"
  "this buffer displays the current notification message.")
(defvar //notification-detail-buffer-name "*emacs-notify-details*"
  "this buffer displays the details of notification messages.")
(defvar //last-timer-function nil)
(defvar //last-timer nil)
(defvar //bottom-window-status '((:temp . t))
  "status of bottom window.")


(defun //get-notify-buffer-create ()
  "get the notify buffer, if not exists, then create it, 
finally set the local variable `window-size-fixed`."
  (let ((buffer (get-buffer-create //notification-buffer-name)))
    (with-current-buffer buffer
      (setq window-size-fixed t)
      buffer)))

(defun //get-notify-buffer ()
  (get-buffer //notification-buffer-name))

(defun //get-bottom-window-create (height)
  "get the bottom window which display notify buffer, if not exists, then create it with HEIGHT lines,
finally set the window parameter 'no-other-window, see also `set-window-parameter`."
  (let ((existp (get-buffer-window (//get-notify-buffer-create))))
    (if existp
	(progn
	  (window-resize existp (- height (window-height existp)) nil t)
	  existp)
      (let ((window (split-window (frame-root-window) (- height) 'below)))
	(set-window-parameter window 'no-other-window t)
	window))))

(defun //get-bottom-window ()
  (let ((notify-buffer (//get-notify-buffer)))
    (if notify-buffer
	(get-buffer-window notify-buffer)
      nil)))


(defun //bottom-window-notify (content height quit-function)
  (let ((bottom-window (//get-bottom-window-create height)))
    (with-current-buffer (//get-notify-buffer)
      (let ((old-content (buffer-string)))
	(undo-boundary)
	(beginning-of-buffer)
	(insert content)
	(beginning-of-buffer)
	(set-window-buffer bottom-window (current-buffer))
	(set-window-dedicated-p bottom-window t)
	(if quit-function
	    (funcall quit-function))))))

(defun //make-quit-function (temp time quit-action)
  (lambda ()
    (when //last-timer-function
      (funcall //last-timer-function)
      (setq //last-timer-function nil))
    (when //last-timer
      (cancel-timer //last-timer)
      (setq //last-timer nil))
    
    (setq //last-timer-function quit-action)
    (let ((rst `(,(not (null temp)) ,(not (null (cdr (assoc :temp //bottom-window-status)))))))
      (cond
       ((equal rst '(t t))
	(setq //last-timer
	      (run-at-time time nil (lambda ()
				      (when (functionp quit-action)
					(funcall quit-action))
				      (delete-window (//get-bottom-window))))))
       ((equal rst '(t nil))
	(setq //last-timer
	      (run-at-time time nil (lambda ()
				      (with-current-buffer (//get-notify-buffer)
					(when (functionp quit-action)
					  (funcall quit-action))
					(primitive-undo 1 buffer-undo-list))))))
       ((equal rst '(nil t))
	(setf (cdr (assoc :temp //bottom-window-status)) nil))
       ((equal rst '(nil nil)) nil)))))



(defun //get-notify-details-buffer-create ()
  (let ((buffer (get-buffer //notification-detail-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create //notification-detail-buffer-name))
      (//init-notify-details-buffer))
    buffer))

(defun //init-notify-details-buffer-content ()
  (with-current-buffer (get-buffer //notification-detail-buffer-name)
    (insert "* Notifications\n")
    (insert "* Status\n")))

(defun //init-notify-details-buffer ()
  (with-current-buffer (get-buffer //notification-detail-buffer-name)
    (org-mode)
    (//init-notify-details-buffer-content)
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" 'quit-window)
    (local-set-key (kbd "<RET>") 'org-open-at-point)))

(defun //insert-notify-detail (content title type)
  (with-current-buffer (//get-notify-details-buffer-create)
    (setq buffer-read-only nil)
    (case type
      ;; notification
      ('notification
       (beginning-of-buffer)
       (search-forward-regexp "^[[:space:]]*?\\* Notifications\n" nil t))
      
      ;; status
      (t
       (beginning-of-buffer)
       (search-forward-regexp "^[[:space:]]*?\\* Status\n" nil t)))
    
    (let ((start (point)))
      (insert "\n")
      (forward-line -1)
      (insert (format "** %s\n" title))
      (insert (format "/%s/\n" (current-time-string)))
      (insert (format "%s\n" content))
      (indent-region start (point)))
    (setq buffer-read-only t)
    ))

(defun /list-notifications ()
  "switch to the `emacs-notify//notification-detail-buffer-name` buffer window."
  (interactive)
  (display-buffer (//get-notify-details-buffer-create)
		  '((display-buffer-pop-up-window)))
  (select-window (get-buffer-window //notification-detail-buffer-name)))

(defun /delete-notify-window ()
  "close the window which displays notifications on the bottom."
  (interactive)
  (delete-window (//get-bottom-window)))
)

(cl-defun emacs-notify/notify (summary &key title detail (temp t) (time emacs-notify//notification-surviving-time) quit-action)
  "make notifications display on the bottom window, and send details of notifications to notification-details buffer, which can be viewed use `emacs-notify/list-notifications`.
SUMMARY: summary of this notification.
DETAIL: details of this notification.
TITLE: title of this notification.
TEMP: nil if this notification is a status(always display on the bottom window), else t.(default t) 
TIME: how long this notification display on the bottom window, if TEMP is t. (default `emacs-notify//notification-surviving-time`)
QUIT-ACTION: function which is called when this notification disappear on the bottom window. 
==============
\"disappear on the bottom window\" : 
1. out of TIME if TEMP is t
2. another notification occurs, no matter what TEMP is.
"
  (let ((quit-func (emacs-notify//make-quit-function temp time quit-action)))
    (emacs-notify//bottom-window-notify summary emacs-notify//bottom-window-lines quit-func)

    (let ((detail (or detail summary))
    	  (title (or title "."))
    	  type)
      (if temp
    	  (setq type 'notification)
    	(setq type 'status))
      (emacs-notify//insert-notify-detail detail title type))
    ))








(provide 'emacs-notify)
;;; emacs-notify.el ends here
