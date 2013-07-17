;;; gitconfig.el ---
;;
;; Filename: gitconfig.el
;; Description:
;; Author: Samuel Tonini
;; Maintainer: Samuel Tonini
;; Version: 0.0.1
;; URL:
;; Keywords: git, gitconfig

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;   Manual Installation:
;;
;;    (add-to-list 'load-path "~/path/to/gitconfig.el/")
;;    (require 'gitconfig)
;;    (global-gitconfig-mode)
;;
;;   Interesting variables are:
;;
;;       `<var>`
;;
;;            <description>
;;
;;
;;   Major commands are:
;;
;;        M-x <command>
;;
;;            <description>
;;


;;; Code:

(defcustom gitconfig-command "git"
  "The shell command for git"
  :type 'string
  :group 'gitconfig)

(defvar gitconfig-buffer-name "*GITCONFIG*"
  "Name of the gitconfig output buffer.")

(defvar gitconfig--local-config-hash (make-hash-table :test 'equal)
  "hash table with the local git config's.")

(defun gitconfig-current-inside-git-repository-p ()
  ""
  (let ((inside-work-tree (shell-command-to-string
                           (format "%s rev-parse --is-inside-work-tree"
                                   gitconfig-command))))
    (string= (replace-regexp-in-string "\n" "" inside-work-tree nil t) "true")))

(defun gitconfig-path-to-git-repository ()
  ""
  (let ((path-to-git-repo (shell-command-to-string
                           (format "%s rev-parse --show-toplevel"
                                   gitconfig-command))))
    (replace-regexp-in-string "\n" "" path-to-git-repo nil t)))

(defun gitconfig--get-variables (location)
  "Get all variables for the given `location` and return a hash table
   with all varibales in it."
  (unless (gitconfig-current-inside-git-repository-p)
    (error "fatal: Not a git repository (or any of the parent directories): .git"))
  (let ((config-string (shell-command-to-string
                        (format "%s config --%s --list"
                                gitconfig-command location)))
        (variable-hash (make-hash-table :test 'equal)))
    (setq config-string (split-string config-string "\n"))
    (delete "" config-string)
    (mapcar (lambda (x) (puthash (car (split-string x "="))
                                 (car (last (split-string x "=")))
                                 variable-hash)) config-string)
    variable-hash))

(defun gitconfig--get-variable (location name)
  ""
  (let ((value (gethash name (gitconfig--get-variables location))))
    (if (not value)
        (message (format "No %s variable in location: --%s available" name location))
      value)))

(defun gitconfig--get-keys (hash)
  "Return all keys in given `hash`."
  (let (keys)
    (maphash (lambda (key value) (setq keys (cons key keys))) hash)
    keys))

(defun gitconfig-get-local-varibales ()
  ""
  (gitconfig--get-variables "local"))

(defun gitconfig-get-global-varibales ()
  ""
  (gitconfig--get-variables "global"))

(defun gitconfig-get-system-varibales ()
  ""
  (gitconfig--get-variables "system"))

(defun gitconfig-get-local-variable (name)
  ""
  (gitconfig--get-variable "local" name))

(defun gitconfig-get-global-variable (name)
  ""
  (gitconfig--get-variable "global" name))

(defun gitconfig-get-system-variable (name)
  ""
  (gitconfig--get-variable "system" name))

;; gitconfig-get-variables-from-file
;; gitconfig-get-variable-from-file

;; gitconfig-get-section-variables
;; gitconfig-get-section-variable
