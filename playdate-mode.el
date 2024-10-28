;;; playdate-mode.el --- Mode for developing Playdate apps and games  -*- lexical-binding: t; -*-

;; URL: https://github.com/themkat/playdate-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (lsp-mode "8.0.0") (lua-mode "20210802") (projectile "2.8.0") (s "1.13.0") (f "0.20.0"))

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

;; Mode for developing applications and games for the Playdate console.
;; Derives from lua-mode and adds some keywords, lsp rules and so on.

;;; Code:
(require 'lua-mode)
(require 'lsp-mode)
(require 'projectile)
(require 's)
(require 'f)

;; TODO: make it possible to do this automatically
(defcustom playdate-luacats-dir "/Users/marie/Downloads/playdate-luacats"
  "Directory where playdate-luacats is downloaded to."
  :type 'string
  :group 'playdate-mode)

(defcustom playdate-simulator-executable "/Users/marie/Developer/PlaydateSDK/bin/Playdate Simulator.app/Contents/MacOS/Playdate Simulator"
  "Command to use for invoking the simulator application."
  :type 'string
  :group 'playdate-mode)

(defcustom playdate-no-pdxinfo-name-fallback "myawesome-playdate-program"
  "Fallback for pdx filename if user haven't created a pdxinfo file."
  :type 'string
  :group 'playdate-mode)

(defun playdate--closest-main-lua ()
  "Finds the closest main.lua to the file currently being edited."
  (locate-dominating-file default-directory "main.lua"))

(defun playdate--project-root ()
  "Finds the project root, either from finding a main.lua or a projectile project."
  (or (playdate--closest-main-lua)
      (projectile-project-root)))

(defun playdate--get-name-from-pdxinfo ()
  "Returns the name found in the pdxinfo file. nil if not found."
  (let* ((pdxinfo-dir (locate-dominating-file default-directory "pdxinfo"))
         (pdxinfo-file (concat pdxinfo-dir "pdxinfo"))
         (pdxinfo-content (and pdxinfo-dir
                               (f-read-text pdxinfo-file))))
    (if (not (null pdxinfo-content))
        (nth 1 (s-match "name=\\(.*\\)\n" pdxinfo-content)))))

(defun playdate--get-pdxfile-name ()
  "Returns the pdx file name based upon the given candidates."
  (or (playdate--get-name-from-pdxinfo)
      playdate-no-pdxinfo-name-fallback))

(defun playdate-compile-program ()
  "Compiles the Playdate program. Returns t on success"
  (interactive)
  (let ((project-directory (playdate--project-root)))
    (equal (shell-command (concat "pdc " project-directory " " (concat project-directory (playdate--get-pdxfile-name))))
           0)))

(defun playdate--run-simulator ()
  "Helper function for running the simulator after compilation."
  (let ((project-directory (playdate--project-root))
        (pdxfile (playdate--get-pdxfile-name)))
    (message "Filename: %s" (f-expand (concat project-directory
                                              pdxfile)))
    (async-shell-command (concat (shell-quote-argument playdate-simulator-executable)
                                 " "
                                 (f-expand (concat project-directory
                                                   pdxfile
                                                   ".pdx"))))))

(defun playdate-run-program ()
  "Compiles, then runs the program."
  (interactive)
  (if (playdate-compile-program)
      (playdate--run-simulator)
    (error "Could not compile program. Check Shell Output buffer for details!")))

(define-derived-mode playdate-mode
  lua-mode
  "Playdate mode"
  "Mode for developing Playdate games."
  
  ;; Add additional keywords
  (font-lock-add-keywords nil '(("import" . 'font-lock-keyword-face)))

  ;; LSP related settings
  ;; TODO: handle the possibility of users using other lua lsps? Any way to force lua-language-server for this mode?
  (setq-local lsp-enabled-clients '(lua-language-server))
  ;; TODO: handling multiple user library locations? Or are the most important ones searched automatically in the current project?
  (setq-local lsp-lua-workspace-library (vector playdate-luacats-dir))
  (setq-local lsp-lua-runtime-nonstandard-symbol ["+=" "-=" "*=" "/=" "//=" "%=" "<<=" ">>=" "&=" "|=" "^="])
  (setq-local lsp-lua-runtime-special '((import . "require")))
  (lsp))


(provide 'playdate-mode)
;;; playdate-mode.el ends here
