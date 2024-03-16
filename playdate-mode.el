;;; playdate-mode.el --- Mode for developing Playdate apps and games  -*- lexical-binding: t; -*-

;; URL: https://github.com/themkat/playdate-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (lsp-mode "8.0.0") (lua-mode "20210802"))

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

;; TODO: make it possible to do this automatically
(defcustom playdate-luacats-dir "/Users/marie/Downloads/playdate-luacats"
  "Directory where playdate-luacats is downloaded to."
  :type 'string
  :group 'playdate-mode)

(defcustom playdate-simulator-executable "/Users/marie/Developer/PlaydateSDK/bin/Playdate Simulator.app/Contents/MacOS/Playdate Simulator"
  "Command to use for invoking the simulator application."
  :type 'string
  :group 'playdate-mode)

;; TODO: actually support reading the name from the pdxinfo file lol
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

(defun playdate-compile-program ()
  "Compiles the Playdate program."
  (interactive)
  (let ((project-directory (playdate--project-root)))
    (compile (concat "pdc " project-directory " " (concat project-directory playdate-no-pdxinfo-name-fallback)))))

(defun playdate--run-simulator-callback (buffer msg)
  "Helper function for running the simulator after compilation."
  (kill-buffer buffer)
  (let ((project-directory (playdate--project-root)))
    (async-shell-command (concat (shell-quote-argument playdate-simulator-executable)
                           " "
                           (concat project-directory playdate-no-pdxinfo-name-fallback))))
  (delete 'playdate--run-simulator-callback compilation-finish-functions))

(defun playdate-run-program ()
  "Compiles, then runs the program."
  (interactive)
  (add-to-list 'compilation-finish-functions 'playdate--run-simulator-callback)
  (playdate-compile-program))

;; TODO: handle the possibility of users using other lua lsps? Any way to force lua-language-server for this mode?

(define-derived-mode playdate-mode
  lua-mode
  "Playdate mode"
  "Mode for developing Playdate games."
  
  ;; Add additional keywords
  (font-lock-add-keywords nil '(("import" . 'font-lock-keyword-face)))
  
  ;; TODO: handling multiple user library locations? Or are the most important ones searched automatically in the current project?
  (setq-local lsp-lua-workspace-library (vector playdate-luacats-dir))
  (setq-local lsp-lua-runtime-nonstandard-symbol ["+=", "-=", "*=", "/=", "//=", "%=", "<<=", ">>=", "&=", "|=", "^="])
  (setq-local lsp-lua-runtime-special '((import . "require")))
  (lsp))


(provide 'playdate-mode)
;;; playdate-mode.el ends here
