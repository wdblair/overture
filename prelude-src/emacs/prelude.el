;;; prelude.el  -  Major mode for editing prelude source in Emacs

;;; 2011 Rémy Wyss

;;; Auteur : Rémy WYSS
;;; Date   : 1 / 10 / 2011

;;; This file is not part of GNU Emacs

;;; INSTALL :

;;; Put these lines at the end of your .emacs :

; (setq auto-mode-alist (cons '("\\.plu$" . prelude-mode) auto-mode-alist))
; (autoload 'prelude-mode "prelude" "Edition de code prelude" t)

;;; set the  load-path variable :

; (setq load-path
;      (append load-path
;	      '("/home/dir_where_you_put_prelude.el")))

;;; Byte-compile prelude.el to speed-up
;;; the loading of a prelude source file :

; M-x byte-compile-file  -> prelude.el





;;; USAGE :

; \C-c\C-c -> send a node to the preludec compiler.
; \C-c\C-h -> print clocks .
; \C-c\C-d -> print deadlines .
; To close the compilation window :
;        \C-x 0 while in compilation buffer
;   or   \C-x 1 while in source buffer
;
; TAB -> indent current line.



;;; code :


(require 'font-lock)


; version of prelude-mode
(defconst prelude-mode-version "1.0")

;;; Customisable values :

(defvar prelude-ask-node-mode nil
  "*Way to choose the node to compile:
     - t   : prompt the user for the name of the node.
     - nil : compile current node without asking.")


(defvar prelude-compiler-name "preludec"
  "*Name of the prelude compiler to call.")

(defvar prelude-comment-ind-level 2
  "*How many spaces to indent a comment.")


;;; Hooks

(defvar prelude-mode-hook nil
  "functions called when entering prelude Mode.")

;;; Key-map for Lustre-mode

(defvar prelude-mode-map nil
  "Keymap for prelude major mode.")
(if prelude-mode-map
    ()
  (setq prelude-mode-map (make-sparse-keymap))
  (define-key prelude-mode-map ","   'electric-prelude-special-char)
;  (define-key prelude-mode-map ":"   'electric-prelude-special-char)
  (define-key prelude-mode-map "\C-c\C-c" 'prelude-compil)
  (define-key prelude-mode-map "\C-c\C-h" 'prelude-clocks)
  (define-key prelude-mode-map "\C-c\C-d" 'prelude-deadlines)
  (define-key prelude-mode-map "\r"  'electric-prelude-end-of-line)
;;  (define-key prelude-mode-map "\t"  'electric-prelude-tab)
  )


;;; Compilation -------------------------------------------------

(require 'compile)

(defun prelude-compil ()
  "Saves the file and calls the prelude compiler. Prompt for saving if
`compilation-ask-about-save' is non nil"
  (interactive)
  (if (buffer-modified-p)
      (if (or (not compilation-ask-about-save)
	      (y-or-n-p "Save file? "))
	  (save-buffer)))
  (let ((fichier (buffer-file-name))
	(node nil))
    (if prelude-ask-node-mode
	(setq node (prelude-ask-node-name))
      (setq node (prelude-get-current-node)))
    (if (and (not prelude-ask-node-mode) (eq node ""))
	(message "no node to compile.")
      (progn
	(delete-other-windows)
	(split-window-vertically)
	(get-buffer-create "*compil-prelude*")
	(select-window (next-window))
	(switch-to-buffer "*compil-prelude*")
	(start-process "prelude-compilation"
		       "*compil-prelude*"
		       prelude-compiler-name
		       "-node"
		       node
                       fichier)
	(end-of-buffer)
	(select-window (next-window))))))



(defun prelude-clocks ()
  "Saves the file and calls the prelude compiler. Prompt for saving if
`compilation-ask-about-save' is non nil"
  (interactive)
  (if (buffer-modified-p)
      (if (or (not compilation-ask-about-save)
	      (y-or-n-p "Save file? "))
	  (save-buffer)))
  (let ((fichier (buffer-file-name))
	(node nil))
    (if prelude-ask-node-mode
	(setq node (prelude-ask-node-name))
      (setq node (prelude-get-current-node)))
    (if (and (not prelude-ask-node-mode) (eq node ""))
	(message "no node to compile.")
      (progn
	(delete-other-windows)
	(split-window-vertically)
	(get-buffer-create "*compil-prelude*")
	(select-window (next-window))
	(switch-to-buffer "*compil-prelude*")
	(start-process "prelude-compilation"
		       "*compil-prelude*"
		       prelude-compiler-name
		       "-node"
		       node
                       "-print_clocks"
                       fichier)
	(end-of-buffer)
	(select-window (next-window))))))


(defun prelude-deadlines ()
  "Saves the file and calls the prelude compiler. Prompt for saving if
`compilation-ask-about-save' is non nil"
  (interactive)
  (if (buffer-modified-p)
      (if (or (not compilation-ask-about-save)
	      (y-or-n-p "Save file? "))
	  (save-buffer)))
  (let ((fichier (buffer-file-name))
	(node nil))
    (if prelude-ask-node-mode
	(setq node (prelude-ask-node-name))
      (setq node (prelude-get-current-node)))
    (if (and (not prelude-ask-node-mode) (eq node ""))
	(message "no node to compile.")
      (progn
	(delete-other-windows)
	(split-window-vertically)
	(get-buffer-create "*compil-prelude*")
	(select-window (next-window))
	(switch-to-buffer "*compil-prelude*")
	(start-process "prelude-compilation"
		       "*compil-prelude*"
		       prelude-compiler-name
		       "-node"
		       node
                       "-print_deadlines"
                       fichier)
	(end-of-buffer)
	(select-window (next-window))))))
(defun prelude-get-current-node ()
  "Returns the current node.
   Search backward for keyword 'node' and returns the following node.
   Nil means not in a node."
  (interactive)
  (save-excursion
    (let ((res ""))
      (end-of-line)
      (if (re-search-backward "^\\<node\\>" 1 t)
	  (progn
	    (forward-char 4)
	    (skip-chars-forward " \t\n")
	    (while (not (looking-at "\\((\\| \\|$\\)"))
	      (setq res (concat res (char-to-string (char-after (point)))))
	      (forward-char 1))))
      res)))



(defun prelude-ask-node-name ()
  "Ask for the node to compile."
  (interactive)
  (read-from-minibuffer "node to compile :"))


;;; Font-lock -----------------------------------------------------


(defgroup prelude-faces nil
  "Special faces for the prelude mode."
  :group 'prelude)

(defface prelude-font-lock-governing-face
  '((((background light)) (:foreground "blue" :bold t))
    (t (:foreground "orange" :bold t)))
  "Face description for governing/leading keywords."
  :group 'prelude-faces)

(defvar prelude-font-lock-governing-face
  'prelude-font-lock-governing-face)
(defface prelude-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for prelude operators."
  :group 'prelude-faces)
(defvar prelude-font-lock-multistage-face
  'prelude-font-lock-multistage-face)

(defface prelude-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'prelude-faces)
(defvar prelude-font-lock-operator-face
  'prelude-font-lock-operator-face)
(defvar prelude-font-lock-keywords nil
  "Regular expression used by Font-lock mode.")


(setq prelude-font-lock-keywords
      '(("--.*$" . font-lock-comment-face)
	("(\\*\\(.\\|\n\\)*?\\*)" . font-lock-comment-face)
	("node *\\([a-zA-Z0-9_-]*\\) *(" 1 prelude-font-lock-governing-face nil nil)
        ("\\<\\(const\\|sensor\\|imported\\|actuator\\|wcet\\|let\\|node\\|returns\\|req\\|tel\\|type\\|due\\|before\\|rate\\|var\\)\\>" 1 font-lock-keyword-face nil nil)
        ("\\<\\(node\\|let\\|tel\\)\\>[ \t\n]" 1 prelude-font-lock-multistage-face nil nil)
        ("\\<\\(true\\|and\\|fby\\|merge\\|tail\\|when\\|whennot\\|false\\)\\>" . font-lock-reference-face)
        ("\\(\\(/\\|\\*\\)^\\|::\\)" . font-lock-reference-face)
	("\\<\\(bool\\|int\\|real\\)\\(\\^[^ ;,)]+\\)?\\>" 0
         font-lock-variable-name-face nil nil)))



(defun prelude-font-mode ()
  "Initialisation of font-lock for Prelude mode."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(prelude-font-lock-keywords t)))

; font-lock isn't used if in a  console
(if window-system
    (prog2
	(add-hook 'prelude-mode-hook
		  'turn-on-font-lock)
	(add-hook 'prelude-mode-hook
		  'prelude-font-mode)))

;;; indentation code ----------------------------------------------


(defun prelude-indent-decl ()
  "Returns the indentation of a declaration line. "
  (interactive)
  (let ((result 2))
    (save-excursion
      (if (re-search-backward "^\\<\\(const\\|tel\\|type\\|var\\|imported\\|sensor\\|actuator\\)\\>" 0 t)
	  (cond
	   ((looking-at "^\\<tel\\>") (setq result 2))
	   ((looking-at "^\\<\\(const\\|type\\|var\\)\\>[ \t]*$")
	    (setq result 2))
	   ((looking-at
	     "^\\<const\\>[ \t]*.+") (setq result 6))
	   ((looking-at "^\\<type\\>[ \t]*.+") (setq result 5))
	   ((looking-at "^\\<var\\>[ \t]*.+") (setq result 4)))))
    result))


(defun electric-prelude-special-char ()
  "Insert a space after ',' or ':' ."
  (interactive)
  (insert last-command-char)
  (insert " "))

(defun electric-prelude-special-op ()
  "Insert before and after binop"
  (interactive)
  (insert " ")
  (insert last-command-char)
  (insert " "))

(defun electric-prelude-end-of-line ()
  "Insert a newline."
  (interactive)
  (newline))

(defun electric-prelude-tab ()
  "Indent current line ."
  (interactive)
  (let ((mark (make-marker)))
    (set-marker mark (point))
    (beginning-of-line)
    (prelude-indent (prelude-compute-indent))
    (goto-char (marker-position mark))
    (set-marker mark nil)
    )
  (if (looking-at "^")
      (skip-chars-forward " \t")))


(defun prelude-get-beginning-of-line (&optional arg)
  "Returns position of the first non-space char of the current line,
   or line (arg - 1) if optional argument is given."
  (interactive)
  (save-excursion
    (beginning-of-line arg)
    (let ((deb (point)))
      (skip-chars-forward " \t")
      (let ((fin (point)))
        (- fin deb)))))

(defun prelude-get-point-of-line ()
  "Returns position of the first char of the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (point)))

(defun prelude-skip-comments ()
  "set point before the commentary of the current line (if any)."
  (interactive)
  (beginning-of-line)
  (while (not (or (looking-at "$")
		  (looking-at "--")))
    (forward-char 1)))

(defun prelude-line-is-comment (&optional arg)
  "non-nil means line is only a commentary."
  (interactive)
  (save-excursion
    (beginning-of-line arg)
    (skip-chars-forward " \t")
    (looking-at "--")))

(defun prelude-line-is-decl ()
  "non-nil means current line is a declaration. "
  (interactive)
  (save-excursion
    (let ((res nil)
	  (continue t))
      (while continue
	(if (= (point) 1)
	    (setq continue nil))
	(re-search-backward
	 "\\<\\(const\\|let\\|node\\|imported node\\|actuator\\|sensor\\|var\\|type\\|function\\)\\>" 1 t)
	(if (not (prelude-line-is-comment))
	    (setq continue nil)))
      (if (looking-at "\\<\\(const\\|type\\|var\\)\\>")
	  (setq res t))
      (if (looking-at "\\<\\(let\\|node\\|function\\)\\>")
	  (setq res nil))
      res)))


(defun prelude-in-comment ()
  "non-nil means point is inside a comment."
  (interactive)
  (save-excursion
    (re-search-backward "--" (prelude-get-point-of-line) t)))

(defun prelude-skip-commentary-lines ()
  "set point to the beginnig of the first non-commemtary line before
   the current line."
  (interactive)
  (forward-line -1)
  (while (and (prelude-line-is-comment) (> (point) 1))
    (forward-line -1)))

(defun prelude-indent (niveau)
  "Indents current line ."
  (interactive "p")
  (beginning-of-line)
  (delete-char (prelude-get-beginning-of-line))
  (let ((ind niveau))
    (while (> ind 0)
      (insert " ")
      (setq ind (- ind 1)))))


(defun prelude-find-noindent-reg ()
  "non-nil means current line begins with:
   const, function, include, let, var, tel, node, returns, type."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and
     (looking-at

"\\<\\(const\\|imported node\\|sensor\\|actuator\\|let\\|node\\|returns\\|req\\|tel\\|type\\|var\\)\\>")
     (not (prelude-in-comment)))))


(defun prelude-find-unmatching-parent ()
  "Looks for an unmatched parenthese, and returns its position.
   (or nil if there isn't any). "
  (interactive)
  (let ((continue t)
        (result nil)
        (beg nil)
        (count-parent 0))
    (save-excursion
      (beginning-of-line)
      (if (= (point) 1)
	  (setq continue nil))
      (while continue
        (forward-line -1)
        (setq beg (point))
	(end-of-line)
        (while (and (not (looking-at "^")) continue)
          (if (and (looking-at ")") (not (prelude-in-comment)))
              (setq count-parent (- count-parent 1))
            (if (and (looking-at "(") (not (prelude-in-comment)))
                (progn
                  (setq count-parent (+ count-parent 1))
                  (if (= count-parent 1)
		      (progn
			(setq result (- (point) beg))
			(setq continue nil))))))
	  (forward-char -1))
	(skip-chars-forward " \t")
	(if (and (looking-at "\\<const\\|var\\|type\\|node\\|function\\>")
		 (not (prelude-in-comment)))
	    (setq continue nil))
	(beginning-of-line)
	(if (= (point) 1)
	    (setq continue nil))))
    result))


(defun prelude-indent-normal ()
  "non-nil means indent normally."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at "[]a-zA-Z0-9^[()]+")))

(defun prelude-empty-line ()
  "non-nil means line is empty."
  (interactive)
  (save-excursion
    (skip-chars-forward " \t")
    (looking-at "$")))



(defun prelude-compute-indent ()
  "Returns indentation of current line."
  (interactive)
  (cond
   ; if line is comment
   ((prelude-line-is-comment) prelude-comment-ind-level)
   ; if line begins with node,include...
   ((prelude-find-noindent-reg) 0)
   ; if line begins with 'then' or 'else'
   ((prelude-find-then-else-beg) (prelude-indent-then-else-beg))
   ; if previous line ends with 'then' or 'else'
   ((prelude-find-then-else-end) (+ (prelude-indent-then-else-end) 2))
   ; looks for an unmatched parenthese
   ((prelude-find-unmatching-parent) (+ (prelude-find-unmatching-parent) 1))
   ; if line is a declaration
   ((prelude-line-is-decl) (prelude-indent-decl))
   ; if previous line ends with '->'
   ((prelude-find-arrow) (prelude-indent-arrow-equal-bool))
   ; if previous line ends with '='
   ((prelude-find-equal-end) (prelude-indent-arrow-equal-bool))
   ; if previous line ends with a boolean operator
   ((prelude-find-bool-expr-end) (prelude-indent-arrow-equal-bool))
   ; if line is a 'normal line'
   ((prelude-indent-normal) 2)
   ; line is empty
   ((prelude-empty-line) 2)
   ; else ...
   (t 0)))


(defun prelude-find-arrow ()
  "non-nil means previous line ends with '->' ."
  (interactive)
  (save-excursion
    (prelude-skip-commentary-lines)
    (prelude-skip-comments)
    (skip-chars-backward " \t")
    (forward-char -2)
    (and (looking-at "->") (not (prelude-in-comment)))))


(defun prelude-indent-arrow-equal-bool ()
  "Find the level of indentation when previous line ends with '->',
'='
   or a boolean (or, xor, and)."
  (interactive)
  (save-excursion
    (prelude-skip-commentary-lines)
    (+ (prelude-get-beginning-of-line) 2)))


(defun prelude-find-bool-expr-end ()
  "non-nil means last line ends with 'and', 'xor' or 'or'."
  (interactive)
  (let ((result nil))
    (save-excursion
      (prelude-skip-commentary-lines)
      (prelude-skip-comments)
      (skip-chars-backward " \t")
      (forward-char -2)
      (setq result (and (looking-at "\\<or\\>")
			(not (prelude-in-comment))))
      (forward-char -1)
      (or (and (looking-at "\\<\\(and\\|not\\|xor\\)\\>")
	       (not (prelude-in-comment)))

	  result))))


(defun prelude-find-then-else-beg ()
  "non-nil means current line begins with 'then' or 'else' ."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (looking-at "\\<\\(else\\|then\\)\\>")
	 (not (prelude-in-comment)))))


(defun prelude-find-then-else-end ()
  "non-nil means last line ends with 'then' or 'else'."
  (interactive)
  (save-excursion
    (prelude-skip-commentary-lines)
    (prelude-skip-comments)
    (skip-chars-backward " \t")
    (forward-char -4)
    (and (looking-at "\\<\\(else\\|then\\)\\>")
	 (not (prelude-in-comment)))))



(defun prelude-find-equal-end ()
  "non-nil means last line ends with '=' ."
  (interactive)
  (save-excursion
    (prelude-skip-commentary-lines)
    (prelude-skip-comments)
    (skip-chars-backward " \t")
    (forward-char -1)
    (and (looking-at "=")
	 (not (prelude-in-comment)))))



(defun prelude-indent-then-else-beg ()
  "Returns the level of indentation of a line beginning with
   'then' or 'else'."
  (interactive)
  (let ((beg nil)
        (result nil)
        (count-expr 0)
        (continue t))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (and (looking-at "\\<then\\>") (not (prelude-in-comment)))
          (while continue
            (prelude-skip-commentary-lines)
	    (setq beg (point))
	    (prelude-skip-comments)
            (skip-chars-forward " \t")
	    (if (and (looking-at "\\<node\\|function\\>")
		     (not (prelude-in-comment)))
		(setq continue nil))
	    (end-of-line)
            (while (and (not (looking-at "^")) continue)
              (if (and (looking-at "\\<then\\>")
		       (not (prelude-in-comment)))
                  (setq count-expr (- count-expr 1))
                (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			 (not (prelude-in-comment)))
		    (progn
                      (setq count-expr (+ count-expr 1))
                      (if (and (= count-expr 1) continue)
                          (progn
                            (setq continue nil)
                            (setq result (- (point) beg)))))))
              (forward-char -1)))
	(if (looking-at "\\<else\\>")
            (while continue
	      (prelude-skip-commentary-lines)
	      (setq beg (point))
	      (prelude-skip-comments)
	      (skip-chars-forward " \t")
	      (if (and (looking-at "\\<node\\|function\\>")
		       (not (prelude-in-comment)))
		  (setq continue nil))
	      (end-of-line)
	      (while (and (not (looking-at "^")) continue)
		(if (and (looking-at "\\<else\\>")
			 (not (prelude-in-comment)))
		    (setq count-expr (- count-expr 1))
		  (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			   (not (prelude-in-comment)))
		      (progn
			(setq count-expr (+ count-expr 1))
			(if (and (= count-expr 1) continue)
			    (progn
			      (setq continue nil)
			      (setq result (- (point) beg)))))))
		(forward-char -1))))))
    result))


(defun prelude-indent-then-else-end ()
  "Returns the level of indentation of a line ending with 'then' or
'else'."
  (interactive)
  (let ((beg nil)
        (result nil)
        (count-expr 1)
        (continue t))
    (save-excursion
      (prelude-skip-commentary-lines)
      (prelude-skip-comments)
      (skip-chars-backward " \t")
      (forward-char -4)
      (if (and (looking-at "\\<then\\>") (not (prelude-in-comment)))
          (progn
            (forward-line 1)
            (while continue
              (forward-line -1)
              (setq beg (point))
              (skip-chars-forward " \t")
	      (if (and (looking-at "\\<node\\|function\\>")
		       (not (prelude-in-comment)))
		  (setq continue nil))
	      (end-of-line)
              (while (and (not (looking-at "^")) continue)
                (if (and (looking-at "\\<then\\>")
			 (not (prelude-in-comment)))
		    (setq count-expr (- count-expr 1))
                  (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			   (not (prelude-in-comment)))
		      (progn
                        (setq count-expr (+ count-expr 1))
                        (if (and (= count-expr 1) continue)
                            (progn
                              (setq continue nil)
                              (setq result (- (point) beg)))))))
                (forward-char -1))))
        (if (and (looking-at "\\<else\\>") (not (prelude-in-comment)))
            (progn
              (forward-line 1)
              (while continue
		(forward-line -1)
		(setq beg (point))
		(skip-chars-forward " \t")
		(if (and (looking-at "\\<node\\|function\\>")
			 (not (prelude-in-comment)))
		    (setq continue nil))
		(end-of-line)
		(while (and (not (looking-at "^")) continue)
		  (if (and (looking-at "\\<else\\>")
			   (not (prelude-in-comment)))
		      (setq count-expr (- count-expr 1))
		    (if (and (looking-at "\\<\\(if\\|with\\)\\>")
			     (not (prelude-in-comment)))
			(progn
			  (setq count-expr (+ count-expr 1))
			  (if (and (= count-expr 1) continue)
			      (progn
				(setq continue nil)
				(setq result (- (point) beg)))))))
		  (forward-char -1)))))))
    result))

;;; Major-mode

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lus$" . prelude-mode))

;;;###autoload
(defun prelude-mode ()
  "Major mode for editing Prelude files.

   Special Commands :
     C-c C-c : send a node to the prelude compiler
     TAB     : indent current line

   Customisable variables :

     - prelude-comment-ind-level:
         How many spaces to indent a comment. (default: 2)

     - prelude-compiler-name:
         Name of the prelude compiler to call. (default: 'luciole')

     - prelude-ask-node-mode:
         Way to choose the node to compile:
          * t   : prompt the user for the name of the node.
          * nil : compile current node without asking. (default)


   Send bugs report to p6mip467@infop6.jussieu.fr"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'prelude-mode)
  (setq mode-name "Prelude")
  (use-local-map prelude-mode-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'electric-prelude-tab)
  (run-hooks 'prelude-mode-hook))


(setq comment-start "-- ")
(setq comment-end "")

(provide 'prelude)

;;; prelude.el ends here...

