;;; ================================
;;; TRACE SYSTEM (plug & play)
;;; ================================

(setq TRACE:*depth* 0)
(setq TRACE:*traced* '())

(defun TRACE:MakeIndent (depth / result)
  (setq result "")
  (repeat depth
    (setq result (strcat result "  "))
  )
  result
)

(defun TRACE:Indent ()
  (TRACE:MakeIndent TRACE:*depth*)
)

(defun TRACE:Call (name fn args / result)
  (princ (strcat "\n" (TRACE:Indent) "[ENTER] " name))
  (setq TRACE:*depth* (1+ TRACE:*depth*))

  (setq result (apply fn args))

  (setq TRACE:*depth* (1- TRACE:*depth*))
  (princ (strcat "\n" (TRACE:Indent) "[EXIT] " name))

  result
)

;;; ----------------
;;; Rewrite logic
;;; ----------------

(defun TRACE:Rewrite (expr)
  (cond
    ((and (listp expr)
          (eq (car expr) 'defun))
     (TRACE:RewriteDefun expr))

    ((listp expr)
     (mapcar 'TRACE:Rewrite expr))

    (T expr)
  )
)

(defun TRACE:RewriteDefun (expr / name args body)
  (setq name (cadr expr))
  (setq args (caddr expr))
  (setq body (cdddr expr))

  (setq TRACE:*traced* (cons (vl-symbol-name name) TRACE:*traced*))

  (list 'defun name args
    (list 'TRACE:Call
      (vl-symbol-name name)
      (list 'quote
            (list 'lambda args
              (cons 'progn (mapcar 'TRACE:Rewrite body))))
      (list 'list (cons 'list args))))
)

;;; ----------------
;;; File loader
;;; ----------------

(defun TRACE:LoadFile (filepath / f expr count)
  (setq f     (open filepath "r")
        count 0)

  (while (setq expr (read f))
    (eval (TRACE:Rewrite expr))
    (setq count (1+ count))
  )

  (close f)

  (princ (strcat "\n[TRACE] Loaded: " filepath
                 " (" (itoa count) " forms)"))
)

;;; ----------------
;;; Recursive folder loader (internal)
;;; ----------------

(defun TRACE:WalkFolder (folder depth / files subdirs indent)
  (setq indent (TRACE:MakeIndent depth))

  ;; Load all .lsp files in this folder
  (setq files (vl-directory-files folder "*.lsp" 1))
  (foreach file files
    (TRACE:LoadFile (strcat folder "\\" file))
  )

  ;; Find subdirectories, skipping . and ..
  (setq subdirs
    (vl-remove-if
      (function (lambda (d) (member d '("." ".."))))
      (vl-directory-files folder nil -1)
    )
  )

  (foreach dir subdirs
    (princ (strcat "\n" indent "[TRACE] Entering: " dir))
    (TRACE:WalkFolder (strcat folder "\\" dir) (1+ depth))
  )
)

;;; ----------------
;;; Folder loader (public entry point)
;;; ----------------

(defun TRACE:LoadFolder (folder)
  (setq TRACE:*traced* '())

  (princ (strcat "\n[TRACE] Walking tree from: " folder))

  (TRACE:WalkFolder folder 0)

  (princ (strcat "\n[TRACE] "
                 (itoa (length TRACE:*traced*))
                 " function(s) instrumented:"))
  (foreach fn (reverse TRACE:*traced*)
    (princ (strcat "\n  + " fn))
  )

  (princ)
)
