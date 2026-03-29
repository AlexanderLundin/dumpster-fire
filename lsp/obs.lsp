;;; =====================================================================
;;; OBS.LSP — Bolt-On Observability Framework for Tracer
;;; =====================================================================
;;; Wraps the existing TRACE system (tracer.lsp) with:
;;;   - Per-call timing (milliseconds)
;;;   - Error trapping with context
;;;   - Global variable watchpoints (track mutations)
;;;   - Call statistics (count, total time, avg time per function)
;;;   - Separate obs log file for diagnostics
;;;
;;; USAGE:
;;;   1. Load tracer.lsp first (defines TRACE:Call and instruments functions)
;;;   2. Load this file: (load "obs.lsp")
;;;   3. OBS automatically wraps TRACE:Call — no other setup needed
;;;   4. Add watches:  (OBS:Watch "myVar1")
;;;                    (OBS:Watch "myVar1List")
;;;   5. Run your workflow normally
;;;   6. Dump stats:   (OBS:DumpStats)
;;;   7. Cleanup:      (OBS:Unwrap)
;;;
;;; COMPATIBILITY: AutoCAD 2015+ (Visual LISP / VLISP required)
;;; =====================================================================

;;; ----- Configuration ---------------------------------------------------

(setq OBS:*logfile* "C:\\temp\\obs.log")
(setq OBS:*logfp*   nil)
(setq OBS:*enabled* T)

;;; ----- Internal State --------------------------------------------------

(setq OBS:*wrapped* nil)

;; Statistics: assoc list of ( "FuncName" count totalMs )
(setq OBS:*stats* nil)

;; Watchpoints: list of ("VARNAME" . last-known-value)
(setq OBS:*watches* nil)

;; Error log: list of ("FuncName" "ErrorMsg" timestamp)
(setq OBS:*errors* nil)

;;; ----- Logging ---------------------------------------------------------

(defun OBS:OpenLog ()
  (if (null OBS:*logfp*)
    (setq OBS:*logfp* (open OBS:*logfile* "w"))
  )
)

(defun OBS:CloseLog ()
  (if OBS:*logfp*
    (progn
      (close OBS:*logfp*)
      (setq OBS:*logfp* nil)
    )
  )
)

(defun OBS:Log (msg)
  (if (null OBS:*logfp*)
    (OBS:OpenLog)
  )
  (if OBS:*logfp*
    (write-line msg OBS:*logfp*)
  )
)

(defun OBS:Timestamp (/ now)
  ;; Returns a human-readable timestamp string
  ;; Uses Julian date for sub-second timing where available
  (setq now (getvar "DATE"))
  (if now
    (progn
      ;; Convert Julian date fractional day to HH:MM:SS
      (setq now (* (- now (fix now)) 86400.0)) ;; seconds since midnight
      (strcat
        (itoa (fix (/ now 3600.0)))
        ":"
        (if (< (rem (fix (/ now 60.0)) 60) 10) "0" "")
        (itoa (rem (fix (/ now 60.0)) 60))
        ":"
        (if (< (rem (fix now) 60) 10) "0" "")
        (itoa (rem (fix now) 60))
      )
    )
    "??:??:??"
  )
)

;;; ----- Timing ----------------------------------------------------------

(defun OBS:GetTimeMs ()
  ;; Returns milliseconds since midnight (float) using Julian DATE sysvar.
  ;; This is the most portable high-res timer in AutoLISP.
  (* (- (getvar "DATE") (fix (getvar "DATE"))) 86400000.0)
)

;;; ----- Statistics ------------------------------------------------------

(defun OBS:RecordStat (name elapsedMs / entry rest)
  ;; Update the stats assoc list for function `name`
  (setq entry (assoc name OBS:*stats*))
  (if entry
    (progn
      (setq rest (vl-remove entry OBS:*stats*))
      (setq OBS:*stats*
        (cons
          (list name
                (1+ (cadr entry))                    ;; call count
                (+ (caddr entry) elapsedMs))         ;; cumulative ms
          rest
        )
      )
    )
    ;; First call for this function
    (setq OBS:*stats*
      (cons (list name 1 elapsedMs) OBS:*stats*)
    )
  )
)

;;; ----- Watchpoints -----------------------------------------------------

(defun OBS:Watch (varname / sym)
  "Add a global variable to the watchpoint list.
   Usage: (OBS:Watch \"DL-Descr\")"
  (if (not (assoc varname OBS:*watches*))
    (progn
      (setq sym (read varname))
      (setq OBS:*watches*
        (cons
          (cons varname (eval sym))
          OBS:*watches*
        )
      )
      (OBS:Log
        (strcat "[OBS:WATCH] Added watch: "
                varname
                " = "
                (vl-princ-to-string (eval sym))))
    )
  )
  varname
)

(defun OBS:Unwatch (varname)
  "Remove a variable from the watchpoint list."
  (setq OBS:*watches*
    (vl-remove-if
      (function (lambda (w) (equal (car w) varname)))
      OBS:*watches*
    )
  )
  varname
)

(defun OBS:CheckWatches (context / sym curval oldval)
  "Check all watched variables for changes. Log any mutations."
  (foreach w OBS:*watches*
    (setq sym    (read (car w)))
    (setq curval (vl-catch-all-apply 'eval (list sym)))
    (if (vl-catch-all-error-p curval)
      (setq curval "*UNBOUND*")
    )
    (setq oldval (cdr w))
    (if (not (equal curval oldval))
      (progn
        (OBS:Log
          (strcat "[OBS:WATCH] "
                  (car w)
                  " CHANGED in " context
                  " | was: " (vl-princ-to-string oldval)
                  " | now: " (vl-princ-to-string curval)))
        ;; Update stored value
        (setq OBS:*watches*
          (subst
            (cons (car w) curval)
            w
            OBS:*watches*
          )
        )
      )
    )
  )
)

;;; ----- Core: The TRACE:Call Wrapper ------------------------------------

(defun OBS:DoTraceCall (name fn args / result)
  "Execute the original TRACE:Call behavior inline (enter/exit print + apply)."
  (princ (strcat "\n" (TRACE:Indent) "[ENTER] " name))
  (setq TRACE:*depth* (1+ TRACE:*depth*))
  (setq result (apply fn args))
  (setq TRACE:*depth* (1- TRACE:*depth*))
  (princ (strcat "\n" (TRACE:Indent) "[EXIT] " name))
  result
)

(defun OBS:WrappedTraceCall (name fn args / t0 t1 elapsed result err)
  "Hook for TRACE:*hook* — adds timing, error trapping, and watches around
   the normal TRACE:Call enter/exit behavior."

  (if (null OBS:*enabled*)
    ;; Observability disabled — run original behavior directly
    (OBS:DoTraceCall name fn args)

    ;; --- Full observability path ---
    (progn
      ;; Check watches BEFORE the call
      (if OBS:*watches*
        (OBS:CheckWatches (strcat "BEFORE " name))
      )

      ;; Record start time
      (setq t0 (OBS:GetTimeMs))

      ;; Run the original TRACE:Call behavior inside an error trap
      (setq result
        (vl-catch-all-apply
          'OBS:DoTraceCall
          (list name fn args)
        )
      )

      ;; Record end time and elapsed
      (setq t1 (OBS:GetTimeMs))
      (setq elapsed (- t1 t0))

      ;; Handle potential midnight rollover (DATE-based timer)
      (if (< elapsed 0.0)
        (setq elapsed (+ elapsed 86400000.0))
      )

      ;; Check if the call errored
      (if (vl-catch-all-error-p result)
        (progn
          (setq err (vl-catch-all-error-message result))
          (OBS:Log
            (strcat "[OBS:ERROR] "
                    name
                    " threw: " err
                    " | elapsed: " (rtos elapsed 2 1) "ms"
                    " | time: " (OBS:Timestamp)))
          ;; Store in error list
          (setq OBS:*errors*
            (cons (list name err (OBS:Timestamp)) OBS:*errors*)
          )
          ;; Record stat even for failures
          (OBS:RecordStat name elapsed)
          ;; Check watches AFTER error
          (if OBS:*watches*
            (OBS:CheckWatches (strcat "AFTER(ERR) " name))
          )
          ;; Re-signal the error so caller sees it
          (error err)
        )
        ;; --- Success path ---
        (progn
          ;; Log timing for slow calls (>100ms threshold)
          (if (> elapsed 100.0)
            (OBS:Log
              (strcat "[OBS:SLOW] "
                      name
                      " took " (rtos elapsed 2 1) "ms"
                      " | time: " (OBS:Timestamp)))
          )
          ;; Record stat
          (OBS:RecordStat name elapsed)
          ;; Check watches AFTER the call
          (if OBS:*watches*
            (OBS:CheckWatches (strcat "AFTER " name))
          )
          ;; Return the actual result
          result
        )
      )
    )
  )
)

;;; ----- Wrap / Unwrap ---------------------------------------------------

(defun OBS:Wrap ()
  "Install observability hook into TRACE:*hook*. Call AFTER tracer.lsp is loaded."

  ;; Guard: don't double-wrap
  (if OBS:*wrapped*
    (progn
      (princ "\n[OBS] Already wrapped. Call (OBS:Unwrap) first to re-wrap.")
      (princ)
    )
    (progn
      ;; Verify tracer.lsp is loaded (TRACE:*hook* must exist)
      (if (not (boundp 'TRACE:*hook*))
        (progn
          (princ "\n[OBS] ERROR: TRACE:*hook* not defined. Load tracer.lsp first!")
          (princ)
        )
        (progn
          ;; Install our function as the call hook.
          ;; TRACE:Call checks TRACE:*hook* and delegates to it when non-nil.
          ;; No USUBR redefinition needed — zero risk of argument mismatch.
          (setq TRACE:*hook* 'OBS:WrappedTraceCall)
          (setq OBS:*wrapped* T)

          ;; Initialize log
          (OBS:OpenLog)
          (OBS:Log (strcat "[OBS] Observability hook installed | " (OBS:Timestamp)))
          (OBS:Log (strcat "[OBS] Log: " OBS:*logfile*))

          (princ "\n[OBS] Observability hook installed.")
          (princ)
        )
      )
    )
  )
)

(defun OBS:Unwrap ()
  "Remove the observability hook, restoring default TRACE:Call behavior."
  (if OBS:*wrapped*
    (progn
      ;; Simply clear the hook — TRACE:Call reverts to its default behavior
      (setq TRACE:*hook* nil)

      ;; Flush and close log
      (OBS:Log (strcat "[OBS] Hook removed | " (OBS:Timestamp)))
      (OBS:CloseLog)

      (setq OBS:*wrapped* nil)
      (princ "\n[OBS] Unwrapped. Default TRACE:Call behavior restored.")
      (princ)
    )
    (progn
      (princ "\n[OBS] Not currently wrapped.")
      (princ)
    )
  )
)

;;; ----- Reporting -------------------------------------------------------

(defun OBS:DumpStats (/ sorted entry avg)
  "Print call statistics to the OBS log and command line."
  (OBS:Log "")
  (OBS:Log "=== OBS CALL STATISTICS ===")
  (OBS:Log (strcat "Timestamp: " (OBS:Timestamp)))
  (OBS:Log (strcat "Functions tracked: " (itoa (length OBS:*stats*))))
  (OBS:Log "")
  (OBS:Log "FUNCTION                          CALLS    TOTAL(ms)  AVG(ms)")
  (OBS:Log "---------------------------------------------------------------")

  ;; Sort by total time descending
  (setq sorted
    (vl-sort OBS:*stats*
      (function (lambda (a b) (> (caddr a) (caddr b))))
    )
  )

  (foreach entry sorted
    (setq avg (/ (caddr entry) (float (cadr entry))))
    (OBS:Log
      (strcat
        (OBS:PadRight (car entry) 34)
        (OBS:PadLeft (itoa (cadr entry)) 5)
        "    "
        (OBS:PadLeft (rtos (caddr entry) 2 1) 10)
        "  "
        (OBS:PadLeft (rtos avg 2 2) 8)
      )
    )
  )

  (OBS:Log "---------------------------------------------------------------")

  ;; Also print errors summary
  (if OBS:*errors*
    (progn
      (OBS:Log "")
      (OBS:Log (strcat "ERRORS: " (itoa (length OBS:*errors*))))
      (foreach e OBS:*errors*
        (OBS:Log
          (strcat "  " (car e) " | " (cadr e) " | " (caddr e)))
      )
    )
  )

  ;; Print watched variable final states
  (if OBS:*watches*
    (progn
      (OBS:Log "")
      (OBS:Log "WATCHED VARIABLES (final state):")
      (foreach w OBS:*watches*
        (OBS:Log
          (strcat "  " (car w) " = " (vl-princ-to-string (cdr w))))
      )
    )
  )

  (OBS:Log "")
  (OBS:Log "=== END STATISTICS ===")

  ;; Also echo summary to command line
  (princ (strcat "\n[OBS] " (itoa (length OBS:*stats*)) " functions tracked"))
  (princ (strcat "\n[OBS] " (itoa (length OBS:*errors*)) " errors caught"))
  (princ (strcat "\n[OBS] Stats written to: " OBS:*logfile*))
  (princ)
)

(defun OBS:Reset ()
  "Clear all statistics and error records."
  (setq OBS:*stats*  nil)
  (setq OBS:*errors* nil)
  (OBS:Log (strcat "[OBS] Stats reset | " (OBS:Timestamp)))
  (princ "\n[OBS] Statistics cleared.")
  (princ)
)

;;; ----- String Helpers --------------------------------------------------

(defun OBS:PadRight (s width / pad)
  (setq pad (- width (strlen s)))
  (if (> pad 0)
    (progn
      (repeat pad (setq s (strcat s " ")))
      s
    )
    s
  )
)

(defun OBS:PadLeft (s width / pad result)
  (setq pad (- width (strlen s)))
  (if (> pad 0)
    (progn
      (setq result "")
      (repeat pad (setq result (strcat result " ")))
      (strcat result s)
    )
    s
  )
)

;;; ----- AutoCAD Command Interface ---------------------------------------

(defun c:OBS-ON ()
  "Command: Enable observability"
  (setq OBS:*enabled* T)
  (princ "\n[OBS] Enabled.")
  (princ)
)

(defun c:OBS-OFF ()
  "Command: Disable observability (passthrough mode)"
  (setq OBS:*enabled* nil)
  (princ "\n[OBS] Disabled (passthrough).")
  (princ)
)

(defun c:OBS-STATS ()
  "Command: Dump statistics"
  (OBS:DumpStats)
)

(defun c:OBS-WATCH (/ varname)
  "Command: Add a variable watch"
  (setq varname (getstring T "\nVariable name to watch: "))
  (if (/= varname "")
    (OBS:Watch varname)
    (princ "\n[OBS] Cancelled.")
  )
  (princ)
)

(defun c:OBS-RESET ()
  "Command: Reset statistics"
  (OBS:Reset)
)

;;; ----- Auto-Setup on Load ----------------------------------------------

(princ "\n[OBS] Observability framework loaded.")

;; Auto-wrap if tracer.lsp is already loaded (TRACE:*hook* will be defined)
(if (and (boundp 'TRACE:*hook*) (not OBS:*wrapped*))
  (OBS:Wrap)
  (princ "\n[OBS] tracer.lsp not yet loaded. Call (OBS:Wrap) after loading tracer.lsp.")
)

(princ)

;;; =====================================================================
;;; INITIALIZATION CALLS — Plug & Play Startup
;;; =====================================================================
;;; Uncomment ONE of the patterns below to enable observability for your workflow.
;;;
;;; Pattern 1: Load tracer, auto-wrap OBS, watch problem variables
;;; =====================================================================
; (load "C:\\temp\\tracer.lsp")
; (load "C:\\temp\\obs.lsp")
; (OBS:Watch "myVar1")
; (OBS:Watch "myVar1")
; (OBS:Watch "myVar1List")
; (OBS:Watch "myList2")
; (OBS:Watch "myList3")
; (princ "\n[INIT] Observability ready. Variables: myVar1, myVar1List, myList2, myList3")
; 
;;; After your workflow completes, dump stats:
; (OBS:DumpStats)
; (OBS:Unwrap)

;;; =====================================================================
;;; Pattern 2: Quick debug (myVar1 → nil)
;;; =====================================================================
; (load "C:\\temp\\tracer.lsp")
; (load "C:\\temp\\obs.lsp")
; (OBS:Watch "myVar1")
; (princ "\n[INIT] Watching myVar1 for mutations")

;;; =====================================================================
;;; Pattern 3: Silent mode (statistics only, no alerts)
;;; =====================================================================
; (load "C:\\temp\\tracer.lsp")
; (load "C:\\temp\\obs.lsp")
; (setq OBS:*enabled* T)
; (princ "\n[INIT] Observability running silently (stats only)")
; (OBS:DumpStats)  ;; Call this after your workflow

;;; =====================================================================
;;; Manual Commands (can be called from command line anytime):
;;; =====================================================================
;;; OBS-ON          — Enable observability
;;; OBS-OFF         — Disable (passthrough mode)
;;; OBS-WATCH       — Interactively add a variable to watch
;;; OBS-STATS       — Dump current statistics
;;; OBS-RESET       — Clear statistics
;;;
;;; Lisp API:
;;;   (OBS:Watch "VARNAME")        — Add variable
;;;   (OBS:Unwatch "VARNAME")       — Remove variable
;;;   (OBS:DumpStats)              — Print stats to log + console
;;;   (OBS:Reset)                  — Clear all stats
;;;   (OBS:Wrap)                   — Manually wrap TRACE:Call
;;;   (OBS:Unwrap)                 — Manually unwrap TRACE:Call
;;; =====================================================================

;;; End of obs.lsp
