(in-package :world-state-parser)

;; FIXME: dashes and underscores are strictly speaking not "alphabetic"
(defchartype alphabetic '(or (satisfies alpha-char-p) (eql #\-) (eql #\_)))
(defchartype digit '(satisfies digit-char-p))
(defchartype id-start-char '(or (and alphabetic (not (eql #\-))) (eql #\_)))

(defun not-newline (x)
  (char/= #\newline x))
(defchartype anything-but-newline '(satisfies not-newline))

(defprod ws ()
  (+ #\Space))

(defprod optional-ws ()
  (* #\Space))

(defprod predicate-id% ()
  (id-start-char (* (/ alphabetic digit ))))

(defprod predicate-id (s)
  (^ (@ predicate-id% (setq s (intern (string-upcase predicate-id%))))
     s))

(defprod number ()
  (+ digit))

(defprod numeric-id (s)
  (^ (@ number (setq s (intern (format nil "V~a" number))))
     s))

(defprod variable-id (s)
  (^ (@ predicate-id (setq s (intern (format nil "?~a" predicate-id))))
     s))

(defprod id ()
  (^ (/ numeric-id variable-id)))

(defprod argument-list% (l)
  (* ((/ ((* " ") ",") (+ " "))
      (^ (@ id (push id l))
         (reverse l)))))

(defprod argument-list (l)
  ("("
   (^ ((? (@ id (push id l)))
      (@ argument-list% (if (> (length argument-list%) 0)
                          (setq l (append l argument-list%)))))
      (if l
          l
          'empty-list)) ;; need special identifier? otherwise returns last matched
   ")"))

(defprod binary-assertion (pred l)
  (^ ((@ predicate-id (setq pred predicate-id))
      (@ argument-list (setq l argument-list)))
     (if (eq l 'empty-list)
         `(,pred)
         `(,pred ,@l))))

(defprod numerical-assertion (pred args num)
  (^ ((@ predicate-id (setq pred predicate-id))
      (@ argument-list (setq args argument-list))
      "="
      (@ number (setq num number)))
     `(= (,pred ,@args) ,num)))

(defprod assertion ()
  (^ (/ numerical-assertion binary-assertion)))


(defprod assertion-list (l)
  (^ ((@ assertion (push assertion l))
      (* (ws (@ assertion (push assertion l))))
      (* (/ ws)))
     (reverse l)))

(defparser world-state-parser
    ((/ (^ assertion-list))
     (* (/ (ws #\newline)))))

(defprod logical-assertion (a)
  ;; also includes negated assertions
  (^ (/ ("-" (@ assertion (setq a `(not ,assertion))))
        (@ assertion (setq a assertion)))
     a))

(defprod logical-assertion-list (l)
  (^ (+ (optional-ws (@ logical-assertion (push logical-assertion l)) optional-ws))
     (reverse l)))

(defprod probability ()
  (/
   ((? (/ "0" "1"))
    "."
    (* digit))
   "0" "1"))

(defprod no-change ()
  "<no-change>")
(defprod noise ()
  "<noise>")

(defprod rule-outcome (prob oc)
  (^ (optional-ws
      (@ probability (setq prob (read-from-string probability)))
      optional-ws
      (/ (@ logical-assertion-list (setq oc logical-assertion-list))
         (@ no-change (setq oc nil))
         (@ noise (setq oc :noise)))
    optional-ws
    #\newline)
     (list prob oc)))

(defprod rule (action context outcomes)
  (^ ("ACTION:" optional-ws #\newline optional-ws
    (@ assertion (setq action assertion)) optional-ws #\newline
    "CONTEXT:" optional-ws #\newline optional-ws
    (@ (/ "--" logical-assertion-list) (setq context logical-assertion-list))
    optional-ws #\newline
    "OUTCOMES:" optional-ws #\newline
    (+ (@ rule-outcome (push rule-outcome outcomes))))
     (list action context outcomes)))

(defprod rules (r)
    (^ (+ (/ ((@ rule (push rule r))
              (* (optional-ws #\newline)))
             ("#" (* anything-but-newline) #\newline)
             #\newline))
       (reverse r)))

(defparser rules-parser
    ;; returns a list of (action context outcomes)
    (^ rules))
