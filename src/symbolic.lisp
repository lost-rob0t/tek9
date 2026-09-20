(in-package :tek9)

(defparameter +symbolic-database-name+ "symbolic")
(defparameter +symbolic-default-query-candidates+ 4096)
(defparameter +symbolic-default-max-bindings+ 4096)
(defparameter +symbolic-default-max-derived+ 10000)
(defparameter +symbolic-default-max-rounds+ 32)

(define-condition symbolic-conflict (error)
  ((id :initarg :id :reader symbolic-conflict-id)
   (existing :initarg :existing :reader symbolic-conflict-existing)
   (requested :initarg :requested :reader symbolic-conflict-requested))
  (:report
   (lambda (condition stream)
     (format stream "Conflicting symbolic object ~A."
             (symbolic-conflict-id condition)))))

(define-condition symbolic-inference-limit (error)
  ((kind :initarg :kind :reader symbolic-inference-limit-kind)
   (limit :initarg :limit :reader symbolic-inference-limit-value))
  (:report
   (lambda (condition stream)
     (format stream "Symbolic inference exceeded ~A limit ~D."
             (symbolic-inference-limit-kind condition)
             (symbolic-inference-limit-value condition)))))

(defun symbolic-variable-p (value)
  "Return true when VALUE is a Datalog-style variable such as ?X."
  (and (symbolp value)
       (let ((name (symbol-name value)))
         (and (plusp (length name))
              (char= (char name 0) #\?)))))

(defun %symbolic-name (value label)
  (let ((name
          (etypecase value
            (string value)
            (symbol (string-downcase (symbol-name value))))))
    (unless (plusp (length name))
      (error "~A must not be empty." label))
    name))

(defun %symbolic-normalize-constant (value)
  (cond
    ((or (stringp value)
         (numberp value)
         (characterp value)
         (null value))
     value)
    ((symbolp value)
     (if (symbolic-variable-p value)
         value
         (string-downcase (symbol-name value))))
    (t
     (error "Unsupported symbolic constant ~S." value))))

(defun %symbolic-normalize-term (term &key allow-variables)
  (unless (and (listp term) term)
    (error "A symbolic term must be a non-empty proper list."))
  (let ((predicate (%symbolic-name (first term) "predicate"))
        (arguments
          (loop for argument in (rest term)
                collect
                (progn
                  (when (and (symbolic-variable-p argument)
                             (not allow-variables))
                    (error "Ground facts cannot contain variable ~S." argument))
                  (%symbolic-normalize-constant argument)))))
    (cons predicate arguments)))

(defun %symbolic-canonical-string (value)
  (with-standard-io-syntax
    (let ((*print-readably* t)
          (*print-circle* t)
          (*print-pretty* nil))
      (write-to-string value))))

(defun %symbolic-hash (value)
  "Return a deterministic 64-bit FNV-1a digest for VALUE's canonical form."
  (let ((hash #xcbf29ce484222325))
    (loop for character across (%symbolic-canonical-string value)
          do (setf hash
                   (logand #xffffffffffffffff
                           (* #x100000001b3
                              (logxor hash (char-code character))))))
    (format nil "~16,'0X" hash)))

(defun %symbolic-fact-key (term)
  (format nil "fact:~A:~A"
          (%symbolic-hash (first term))
          (%symbolic-hash term)))

(defun %symbolic-fact-prefix (predicate)
  (format nil "fact:~A:" (%symbolic-hash predicate)))

(defun %symbolic-rule-key (rule-id)
  (format nil "rule:~A" (%symbolic-hash rule-id)))

(defun %symbolic-expert-key (expert-id)
  (format nil "expert:~A" (%symbolic-hash expert-id)))

(defun %symbolic-derivation-prefix (fact-id)
  (format nil "derivation:~A:" fact-id))

(defun %symbolic-derivation-key (fact-id rule-id evidence-ids)
  (format nil "~A~A"
          (%symbolic-derivation-prefix fact-id)
          (%symbolic-hash (list rule-id evidence-ids))))

(defun %symbolic-value (database id)
  (fetch* database id :database-name +symbolic-database-name+))

(defun %symbolic-put-immutable (database id value)
  (let ((existing (%symbolic-value database id)))
    (cond
      ((null existing)
       (put* database value
             :id id
             :database-name +symbolic-database-name+)
       (values value :created))
      ((equal existing value)
       (values existing :existing))
      (t
       (error 'symbolic-conflict
              :id id
              :existing existing
              :requested value)))))

(defun symbolic-fact-id (term)
  "Return the deterministic storage identity for ground TERM."
  (%symbolic-fact-key (%symbolic-normalize-term term :allow-variables nil)))

(defun symbolic-assert-fact (database term
                             &key
                               fact-id
                               source-ids
                               metadata)
  "Persist one immutable ground symbolic TERM.

SOURCE-IDS and METADATA are provenance data only; no Lisp form is evaluated.
Returns FACT-ID and either :CREATED or :EXISTING."
  (let* ((term (%symbolic-normalize-term term :allow-variables nil))
         (id (or fact-id (%symbolic-fact-key term)))
         (projection
           (list :symbolic-kind :fact
                 :fact-id id
                 :predicate (first term)
                 :term term
                 :source-ids (copy-list source-ids)
                 :metadata metadata)))
    (multiple-value-bind (_value state)
        (%symbolic-put-immutable database id projection)
      (declare (ignore _value))
      (values id state))))

(defun %symbolic-fact-value-p (value)
  (and (listp value)
       (eq (getf value :symbolic-kind) :fact)
       (stringp (getf value :fact-id))
       (listp (getf value :term))))

(defun %symbolic-rule-value-p (value)
  (and (listp value)
       (eq (getf value :symbolic-kind) :rule)
       (stringp (getf value :rule-id))))

(defun %symbolic-expert-value-p (value)
  (and (listp value)
       (eq (getf value :symbolic-kind) :expert)
       (stringp (getf value :expert-id))))

(defun %symbolic-prefix-values (database prefix &key (limit 0))
  (let ((rows
          (select-primary-range
           database
           prefix
           :end (concatenate 'string prefix "~")
           :database-name +symbolic-database-name+
           :limit limit)))
    (mapcar #'cdr rows)))

(defun %symbolic-prefix-rows (database prefix &key (limit 0))
  (select-primary-range
   database
   prefix
   :end (concatenate 'string prefix "~")
   :database-name +symbolic-database-name+
   :limit limit))

(defun %symbolic-match-term (pattern fact &optional bindings)
  (when (= (length pattern) (length fact))
    (loop with result = (copy-list bindings)
          for expected in pattern
          for actual in fact
          do (cond
               ((symbolic-variable-p expected)
                (let ((binding (assoc expected result :test #'eq)))
                  (if binding
                      (unless (equal (cdr binding) actual)
                        (return-from %symbolic-match-term (values nil nil)))
                      (push (cons expected actual) result))))
               ((not (equal expected actual))
                (return-from %symbolic-match-term (values nil nil))))
          finally (return (values result t)))))

(defun symbolic-query (database pattern
                       &key
                         (limit 64)
                         (max-candidates +symbolic-default-query-candidates+))
  "Match PATTERN against durable symbolic facts.

PATTERN uses ?VARIABLE symbols. Results are plists containing :FACT-ID, :TERM,
and :BINDINGS. The second return value is true when the candidate bound was
hit, so callers never mistake a bounded query for complete inference."
  (unless (and (integerp limit) (plusp limit))
    (error "limit must be a positive integer."))
  (unless (and (integerp max-candidates) (plusp max-candidates))
    (error "max-candidates must be a positive integer."))
  (let* ((pattern (%symbolic-normalize-term pattern :allow-variables t))
         (prefix (%symbolic-fact-prefix (first pattern)))
         (candidate-limit (1+ (max limit max-candidates)))
         (rows (%symbolic-prefix-rows database prefix :limit candidate-limit))
         (candidate-truncated (> (length rows) (max limit max-candidates)))
         (matches nil)
         (result-truncated nil))
    (dolist (row (if candidate-truncated
                     (subseq rows 0 (max limit max-candidates))
                     rows))
      (let ((value (cdr row)))
        (when (and (%symbolic-fact-value-p value)
                   (equal (first pattern) (getf value :predicate)))
          (multiple-value-bind (bindings matched-p)
              (%symbolic-match-term pattern (getf value :term))
            (when matched-p
              (if (>= (length matches) limit)
                  (progn
                    (setf result-truncated t)
                    (return))
                  (push (list :fact-id (getf value :fact-id)
                              :term (copy-tree (getf value :term))
                              :bindings (nreverse bindings))
                        matches)))))))
    (values (nreverse matches)
            (or candidate-truncated result-truncated))))

(defun symbolic-retract-fact (database fact-id)
  "Delete FACT-ID and its stored derivation records atomically per document."
  (let ((deleted
          (delete-document database fact-id
                           :database-name +symbolic-database-name+))
        (prefix (%symbolic-derivation-prefix fact-id)))
    (dolist (row (%symbolic-prefix-rows database prefix))
      (delete-document database (car row)
                       :database-name +symbolic-database-name+))
    deleted))

(defun %symbolic-variables (terms)
  (remove-duplicates
   (loop for term in terms
         append (loop for value in (rest term)
                      when (symbolic-variable-p value)
                        collect value))
   :test #'eq))

(defun %symbolic-rule-predicates (terms)
  (remove-duplicates (mapcar #'first terms) :test #'equal))

(defun %symbolic-normalize-predicates (predicates)
  (sort
   (remove-duplicates
    (mapcar (lambda (predicate)
              (%symbolic-name predicate "predicate"))
            predicates)
    :test #'equal)
   #'string<))

(defun %symbolic-normalize-capabilities (capabilities)
  (sort
   (remove-duplicates
    (mapcar (lambda (capability)
              (%symbolic-name capability "capability"))
            capabilities)
    :test #'equal)
   #'string<))

(defun symbolic-register-expert (database expert-id
                                 &key
                                   (version "1")
                                   description
                                   input-predicates
                                   output-predicates
                                   capabilities
                                   metadata)
  "Register an immutable symbolic expert manifest.

The manifest declares predicate and capability boundaries. Rules registered to
this expert are checked against those declarations."
  (let* ((expert-id (%symbolic-name expert-id "expert-id"))
         (projection
           (list :symbolic-kind :expert
                 :expert-id expert-id
                 :version (%symbolic-name version "expert version")
                 :description description
                 :input-predicates
                 (%symbolic-normalize-predicates input-predicates)
                 :output-predicates
                 (%symbolic-normalize-predicates output-predicates)
                 :capabilities
                 (%symbolic-normalize-capabilities capabilities)
                 :metadata metadata))
         (key (%symbolic-expert-key expert-id)))
    (multiple-value-bind (_value state)
        (%symbolic-put-immutable database key projection)
      (declare (ignore _value))
      (values expert-id state))))

(defun symbolic-expert (database expert-id)
  "Return the symbolic expert manifest named EXPERT-ID, or NIL."
  (let ((value
          (%symbolic-value database
                           (%symbolic-expert-key
                            (%symbolic-name expert-id "expert-id")))))
    (and (%symbolic-expert-value-p value) value)))

(defun symbolic-experts (database &key (limit 256))
  "Return at most LIMIT registered symbolic expert manifests."
  (remove-if-not #'%symbolic-expert-value-p
                 (%symbolic-prefix-values database "expert:" :limit limit)))

(defun %symbolic-rule-safe-p (antecedents consequents)
  (let ((bound (%symbolic-variables antecedents)))
    (every (lambda (variable)
             (member variable bound :test #'eq))
           (%symbolic-variables consequents))))

(defun %symbolic-validate-expert-rule (expert antecedents consequents)
  (let* ((inputs (getf expert :input-predicates))
         (outputs (getf expert :output-predicates))
         (readable (append inputs outputs)))
    (dolist (predicate (%symbolic-rule-predicates antecedents))
      (unless (member predicate readable :test #'equal)
        (error "Expert ~A does not declare input predicate ~A."
               (getf expert :expert-id) predicate)))
    (dolist (predicate (%symbolic-rule-predicates consequents))
      (unless (member predicate outputs :test #'equal)
        (error "Expert ~A does not declare output predicate ~A."
               (getf expert :expert-id) predicate)))))

(defun symbolic-register-rule (database rule-id antecedents consequents
                               &key
                                 expert-id
                                 (priority 0)
                                 metadata)
  "Register one immutable, range-restricted conjunctive rule.

Rules are data. Predicates are ground names; variables are allowed only in
arguments, and every variable in a consequent must be bound by an antecedent."
  (unless (and (integerp priority))
    (error "priority must be an integer."))
  (let* ((rule-id (%symbolic-name rule-id "rule-id"))
         (antecedents
           (mapcar (lambda (term)
                     (%symbolic-normalize-term term :allow-variables t))
                   antecedents))
         (consequents
           (mapcar (lambda (term)
                     (%symbolic-normalize-term term :allow-variables t))
                   consequents))
         (expert-id (and expert-id (%symbolic-name expert-id "expert-id"))))
    (unless antecedents
      (error "A symbolic rule requires at least one antecedent."))
    (unless consequents
      (error "A symbolic rule requires at least one consequent."))
    (unless (%symbolic-rule-safe-p antecedents consequents)
      (error "Symbolic rule ~A is not range-restricted." rule-id))
    (when expert-id
      (let ((expert (symbolic-expert database expert-id)))
        (unless expert
          (error "Unknown symbolic expert ~A." expert-id))
        (%symbolic-validate-expert-rule expert antecedents consequents)))
    (let* ((projection
             (list :symbolic-kind :rule
                   :rule-id rule-id
                   :expert-id expert-id
                   :priority priority
                   :antecedents antecedents
                   :consequents consequents
                   :metadata metadata))
           (key (%symbolic-rule-key rule-id)))
      (multiple-value-bind (_value state)
          (%symbolic-put-immutable database key projection)
        (declare (ignore _value))
        (values rule-id state)))))

(defun symbolic-rule (database rule-id)
  "Return RULE-ID's symbolic rule projection, or NIL."
  (let ((value
          (%symbolic-value database
                           (%symbolic-rule-key
                            (%symbolic-name rule-id "rule-id")))))
    (and (%symbolic-rule-value-p value) value)))

(defun symbolic-rules (database &key expert-id (limit 1024))
  "Return registered rules, optionally restricted to EXPERT-ID."
  (let ((expert-id (and expert-id (%symbolic-name expert-id "expert-id"))))
    (remove-if-not
     (lambda (value)
       (and (%symbolic-rule-value-p value)
            (or (null expert-id)
                (equal expert-id (getf value :expert-id)))))
     (%symbolic-prefix-values database "rule:" :limit limit))))

(defun %symbolic-substitute (term bindings)
  (mapcar
   (lambda (value)
     (if (symbolic-variable-p value)
         (let ((binding (assoc value bindings :test #'eq)))
           (if binding (cdr binding) value))
         value))
   term))

(defun %symbolic-merge-bindings (left right)
  (let ((result (copy-list left)))
    (dolist (binding right (values result t))
      (let ((existing (assoc (car binding) result :test #'eq)))
        (cond
          ((and existing (not (equal (cdr existing) (cdr binding))))
           (return-from %symbolic-merge-bindings (values nil nil)))
          ((null existing)
           (push binding result)))))))

(defun %symbolic-rule-states (database rule
                              &key
                                max-bindings
                                max-candidates)
  (let ((states (list (list :bindings nil :evidence-ids nil))))
    (dolist (antecedent (getf rule :antecedents))
      (let ((next nil))
        (dolist (state states)
          (let* ((bindings (getf state :bindings))
                 (pattern (%symbolic-substitute antecedent bindings)))
            (multiple-value-bind (matches truncated)
                (symbolic-query database pattern
                                :limit max-candidates
                                :max-candidates max-candidates)
              (when truncated
                (error 'symbolic-inference-limit
                       :kind :candidates
                       :limit max-candidates))
              (dolist (match matches)
                (multiple-value-bind (merged compatible-p)
                    (%symbolic-merge-bindings
                     bindings
                     (getf match :bindings))
                  (when compatible-p
                    (push
                     (list :bindings merged
                           :evidence-ids
                           (append (getf state :evidence-ids)
                                   (list (getf match :fact-id))))
                     next)
                    (when (> (length next) max-bindings)
                      (error 'symbolic-inference-limit
                             :kind :bindings
                             :limit max-bindings))))))))
        (setf states (nreverse next))
        (unless states
          (return))))
    states))

(defun %symbolic-record-derivation (database fact-id rule state round)
  (let* ((rule-id (getf rule :rule-id))
         (evidence-ids (copy-list (getf state :evidence-ids)))
         (key (%symbolic-derivation-key fact-id rule-id evidence-ids))
         (projection
           (list :symbolic-kind :derivation
                 :fact-id fact-id
                 :rule-id rule-id
                 :expert-id (getf rule :expert-id)
                 :evidence-ids evidence-ids
                 :round round)))
    (multiple-value-bind (_value status)
        (%symbolic-put-immutable database key projection)
      (declare (ignore _value))
      status)))

(defun %symbolic-sort-rules (rules)
  (sort (copy-list rules)
        (lambda (left right)
          (let ((lp (getf left :priority))
                (rp (getf right :priority)))
            (if (= lp rp)
                (string< (getf left :rule-id)
                         (getf right :rule-id))
                (> lp rp))))))

(defun symbolic-infer (database
                       &key
                         expert-id
                         (max-rounds +symbolic-default-max-rounds+)
                         (max-derived +symbolic-default-max-derived+)
                         (max-bindings +symbolic-default-max-bindings+)
                         (max-candidates +symbolic-default-query-candidates+))
  "Run bounded deterministic forward chaining over durable facts and rules.

Returns a plist with :ROUNDS, :DERIVED, :DERIVATIONS and :SATURATED. Inference
fails explicitly when a safety bound is exceeded instead of silently returning
an incomplete proof."
  (dolist (pair (list (cons :rounds max-rounds)
                      (cons :derived max-derived)
                      (cons :bindings max-bindings)
                      (cons :candidates max-candidates)))
    (unless (and (integerp (cdr pair)) (plusp (cdr pair)))
      (error "~A bound must be a positive integer." (car pair))))
  (when expert-id
    (unless (symbolic-expert database expert-id)
      (error "Unknown symbolic expert ~A." expert-id)))
  (let ((rules (%symbolic-sort-rules
                (symbolic-rules database :expert-id expert-id)))
        (derived 0)
        (derivations 0)
        (rounds 0)
        (saturated nil))
    (loop for round from 1 to max-rounds
          do (setf rounds round)
             (let ((created-this-round 0))
               (dolist (rule rules)
                 (dolist (state
                          (%symbolic-rule-states
                           database rule
                           :max-bindings max-bindings
                           :max-candidates max-candidates))
                   (dolist (consequent (getf rule :consequents))
                     (let ((term
                             (%symbolic-substitute
                              consequent
                              (getf state :bindings))))
                       (when (some #'symbolic-variable-p term)
                         (error "Rule ~A produced an unbound consequent."
                                (getf rule :rule-id)))
                       (multiple-value-bind (fact-id status)
                           (symbolic-assert-fact database term)
                         (when (eq status :created)
                           (incf derived)
                           (incf created-this-round)
                           (when (> derived max-derived)
                             (error 'symbolic-inference-limit
                                    :kind :derived
                                    :limit max-derived)))
                         (when (eq (%symbolic-record-derivation
                                   database fact-id rule state round)
                                   :created)
                           (incf derivations)))))))
               (when (zerop created-this-round)
                 (setf saturated t)
                 (return))))
    (list :rounds rounds
          :derived derived
          :derivations derivations
          :saturated saturated)))

(defun symbolic-run-expert (database expert-id &rest inference-options)
  "Run only the rules owned by EXPERT-ID."
  (apply #'symbolic-infer
         database
         :expert-id expert-id
         inference-options))

(defun symbolic-ask (database pattern
                     &key
                       expert-id
                       (limit 64)
                       (max-rounds +symbolic-default-max-rounds+)
                       (max-derived +symbolic-default-max-derived+)
                       (max-bindings +symbolic-default-max-bindings+)
                       (max-candidates +symbolic-default-query-candidates+))
  "Infer to a fixed point, then answer PATTERN.

Returns ANSWERS, INFERENCE-STATS, and QUERY-TRUNCATED-P."
  (let ((stats
          (symbolic-infer
           database
           :expert-id expert-id
           :max-rounds max-rounds
           :max-derived max-derived
           :max-bindings max-bindings
           :max-candidates max-candidates)))
    (multiple-value-bind (answers truncated)
        (symbolic-query database pattern
                        :limit limit
                        :max-candidates max-candidates)
      (values answers stats truncated))))

(defun symbolic-explain-fact (database fact-id &key (limit 128))
  "Return FACT-ID and its bounded stored derivation provenance."
  (let ((fact (%symbolic-value database fact-id)))
    (unless (%symbolic-fact-value-p fact)
      (error "Unknown symbolic fact ~A." fact-id))
    (list :fact fact
          :derivations
          (remove-if-not
           (lambda (value)
             (and (listp value)
                  (eq (getf value :symbolic-kind) :derivation)
                  (equal fact-id (getf value :fact-id))))
           (%symbolic-prefix-values
            database
            (%symbolic-derivation-prefix fact-id)
            :limit limit)))))
