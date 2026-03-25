;;; ================================
;;; TRACE SYSTEM (plug & play)
;;; ================================

(setq TRACE:*depth* 0)

(defun TRACE:Indent ()
  (apply 'strcat (repeat TRACE:*depth* " "))
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
    ;; rewrite defun
    ((and (listp expr)
          (= (car expr) 'defun))
     (TRACE:RewriteDefun expr))

    ;; recurse lists
    ((listp expr)
     (mapcar 'TRACE:Rewrite expr))

    (T expr)
  )
)

(defun TRACE:RewriteDefun (expr / name args body)
  (setq name (cadr expr))
  (setq args (caddr expr))
  (setq body (cdddr expr))

  (list 'defun name args
    (list 'TRACE:Call
      (strcat (symbol-name name))
      (list 'lambda args
        (cons 'progn (mapcar 'TRACE:Rewrite body)))
      (cons 'list args)))
)

;;; ----------------
;;; File loader
;;; ----------------

(defun TRACE:LoadFile (filepath / f expr)
  (setq f (open filepath "r"))

  (while (setq expr (read f))
    (eval (TRACE:Rewrite expr))
  )

  (close f)
)

;;; ----------------
;;; Folder loader
;;; ----------------

(defun TRACE:LoadFolder (folder / files)
  (setq files (vl-directory-files folder "*.lsp" 1))

  (foreach file files
    (TRACE:LoadFile (strcat folder "\\" file))
  )

  (princ (strcat "\n[TRACE] Loaded folder: " folder))
  (princ)
)