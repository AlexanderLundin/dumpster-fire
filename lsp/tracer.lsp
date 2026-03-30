;;; ================================
;;; TRACE SYSTEM (plug & play)
;;; ================================

(setq TRACE:*depth* 0)
(setq TRACE:*traced* '())
(setq TRACE:*hook* nil)   ; set to a function symbol by OBS to intercept calls
(setq TRACE:*logfile* "C:\\temp\\trace.log")
(setq TRACE:*logfp* nil)

(defun TRACE:OpenLog ()
 (princ)
 ; (setq TRACE:*logfp* (open TRACE:*logfile* "w"))
)

(defun TRACE:CloseLog ()
 (if TRACE:*logfp*
   (progn
     (close TRACE:*logfp*)
     (setq TRACE:*logfp* nil)
   )
 )
)

(defun TRACE:Log (msg)
 (princ)
 ; (if TRACE:*logfp*
   ; (write-line msg TRACE:*logfp*)
   ; (progn
     ; (TRACE:OpenLog)
     ; (write-line msg TRACE:*logfp*)
   ; )
 ; )
)

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
  (if TRACE:*hook*
    ;; Delegate to hook (e.g. OBS observability layer)
    (apply TRACE:*hook* (list name fn args))
    ;; Default: print enter/exit and invoke the function
    (progn
      (TRACE:Log (strcat "" (TRACE:Indent) "[ENTER] " name))
      (setq TRACE:*depth* (1+ TRACE:*depth*))
      (setq result (apply fn args))
      (setq TRACE:*depth* (1- TRACE:*depth*))
      (TRACE:Log (strcat "" (TRACE:Indent) "[EXIT] " name))
      result
    )
  )
)

;;; ----------------
;;; Arg-list helpers
;;; ----------------

;;; Extract only the formal parameters from a defun arg list
;;; i.e. everything before the / separator.
;;; (a b c / x y z) → (a b c)
;;; (a b c)         → (a b c)
;;; ()              → ()
(defun TRACE:GetParams (args / result)
  (setq result '())
  (while (and args (not (eq (car args) '/)))
    (setq result (cons (car args) result))
    (setq args (cdr args))
  )
  (reverse result)
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

(setq TRACE:*skip-functions*
'(
  myLispFunctionToSkip
 )
)

(defun TRACE:RewriteDefun (expr / name args params body)
  (setq name   (cadr expr))
  
 ;; Skip certain functions
 (if (member name TRACE:*skip-functions*)
   expr
   (progn

     (setq args   (caddr expr))
     (setq params (TRACE:GetParams args))
     (setq body   (cdddr expr))

     (setq TRACE:*traced* (cons (vl-symbol-name name) TRACE:*traced*))

     (list 'defun name args
       (list 'TRACE:Call
         (vl-symbol-name name)
         (list 'quote
               (list 'lambda params
                 (cons 'progn (mapcar 'TRACE:Rewrite body))))
         (cons 'list params)
       )
     )
   )
 )
 
)

;;; ----------------
;;; File loader
;;; ----------------
(defun TRACE:LoadFile (filepath / f expr count line acc depth ch i result in-string)
  (setq f         (open filepath "r")
        count     0
        acc       ""
        depth     0
        in-string nil)

  (while (setq line (read-line f))
    (setq i 1)
    (while (<= i (strlen line))
      (setq ch (substr line i 1))
      (cond
        ;; Inside a string: handle escape sequences (skip next char)
        ((and (= ch "\\") in-string)
         (setq i (1+ i))
        )
        ;; Quote toggles string state
        ((= ch "\"")
         (setq in-string (not in-string))
        )
        ;; Semicolon outside a string starts a comment — skip rest of line
        ((and (= ch ";") (not in-string))
         (setq i (strlen line))
        )
        ;; Count parens only outside strings and comments
        ((and (= ch "(") (not in-string))
         (setq depth (1+ depth))
        )
        ((and (= ch ")") (not in-string))
         (setq depth (1- depth))
        )
      )
      (setq i (1+ i))
    )

    ;; Accumulate the raw line (read will parse the real structure)
    (setq acc (strcat acc "\n" line))

    ;; A complete top-level form when depth returns to 0
    (if (= depth 0)
      (progn
        (setq result
          (vl-catch-all-apply
            (function (lambda ()
              ;; Use the LISP reader to parse the form, then structurally
              ;; check if it's a defun.  This is whitespace-agnostic so
              ;; "(  defun foo ...)" and multi-line signatures all work.
              (setq expr (read acc))
              (if (and expr (listp expr) (eq (car expr) 'DEFUN))
                (progn
                  (eval (TRACE:Rewrite expr))
                  (setq count (1+ count))
                )
              )
            ))
          )
        )
        (if (vl-catch-all-error-p result)
          (TRACE:Log (strcat "[TRACE] Skipping malformed form in: " filepath
                         "\n  Reason: " (vl-catch-all-error-message result)
                         "\n  Form: " acc))
        )
        (setq acc "")
        (setq in-string nil)
      )
    )
  )

  (close f)

  (TRACE:Log (strcat "[TRACE] Loaded: " filepath
                 " (" (itoa count) " forms)"))
)




;;; ----------------
;;; Recursive folder loader (internal)
;;; ----------------

(defun TRACE:WalkFolder (folder depth / files subdirs indent)
  (setq indent (TRACE:MakeIndent depth))

  ;; Load all .lsp files in this folder, skipping tracer/obs infrastructure
  (setq files (vl-directory-files folder "*.lsp" 1))
  (setq files
    (vl-remove-if
      (function (lambda (f)
        (or (= (strcase f) "TRACER.LSP")
            (= (strcase f) "OBS.LSP"))))
      files
    )
  )
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
    (TRACE:Log (strcat "" indent "[TRACE] Entering: " dir))
    (TRACE:WalkFolder (strcat folder "\\" dir) (1+ depth))
  )
)

;;; ----------------
;;; Folder loader (public entry point)
;;; ----------------

(defun TRACE:LoadFolder (folder)
  (setq TRACE:*traced* '())

  (TRACE:Log (strcat "[TRACE] Walking tree from: " folder))

  (TRACE:WalkFolder folder 0)

  (TRACE:Log (strcat "[TRACE] "
                 (itoa (length TRACE:*traced*))
                 " function(s) instrumented:"))
  (foreach fn (reverse TRACE:*traced*)
    (TRACE:Log (strcat "  + " fn))
  )

  (princ)
)

(princ "\ntracer start.")
(TRACE:LoadFolder "C:\\temp\\LispApp")
(princ "\ntracer complete.")