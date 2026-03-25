;;; ================================
;;; TRACE SYSTEM (plug & play)
;;; ================================

(setq TRACE:*depth* 0)
(setq TRACE:*traced* '())   ; <-- collector for redefined names

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

  ;; Record that this function was traced
  (setq TRACE:*traced* (cons (vl-symbol-name name) TRACE:*traced*))

  (list 'defun name args
    (list 'TRACE:Call
      (vl-symbol-name name)
      (list 'quote                        ; use quote, not lambda-in-list
            (list 'lambda args
              (cons 'progn (mapcar 'TRACE:Rewrite body))))
      (list 'list (cons 'list args))))    ; pass args at call time
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
;;; Folder loader
;;; ----------------

(defun TRACE:LoadFolder (folder / files)
  (setq TRACE:*traced* '())   ; reset collector before each load pass

  (setq files (vl-directory-files folder "*.lsp" 1))

  (foreach file files
    (TRACE:LoadFile (strcat folder "\\" file))
  )

  ;; Print every function that was traced
  (princ (strcat "\n[TRACE] Loaded folder: " folder))
  (princ (strcat "\n[TRACE] " (itoa (length TRACE:*traced*))
                 " function(s) instrumented:"))
  (foreach fn (reverse TRACE:*traced*)
    (princ (strcat "\n  + " fn))
  )

  (princ)
)
