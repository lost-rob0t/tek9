(in-package :tek9-tests)
(in-suite :tek9-tests)

(test symbolic-facts-and-queries
  (let ((db (setup-db #P"/tmp/test-tek9-symbolic-facts/")))
    (unwind-protect
         (progn
           (multiple-value-bind (id state)
               (symbolic-assert-fact db '(parent "alice" "bob"))
             (is (eq :created state))
             (is (string= id (symbolic-fact-id '(parent "alice" "bob")))))
           (symbolic-assert-fact db '(parent "bob" "carol"))
           (let* ((fact-id (symbolic-fact-id '(parent "alice" "bob")))
                  (explanation (symbolic-explain-fact db fact-id))
                  (assertions (getf explanation :assertions)))
             (is (= 1 (length assertions)))
             (is (equal '("fixture:a")
                        (getf (first assertions) :source-ids))))
           (multiple-value-bind (answers truncated)
               (symbolic-query db '(parent "alice" "bob"))
             (is (null truncated))
             (is (= 1 (length answers)))
             (is (null (getf (first answers) :bindings))))
           (multiple-value-bind (answers truncated)
               (symbolic-query db '(parent "alice" ?who))
             (is (null truncated))
             (is (= 1 (length answers)))
             (is (equal "bob"
                        (cdr (assoc '?who
                                    (getf (first answers) :bindings)
                                    :test #'eq))))))
      (close-database db))))

(test symbolic-forward-inference-and-explanation
  (let ((db (setup-db #P"/tmp/test-tek9-symbolic-infer/")))
    (unwind-protect
         (progn
           (symbolic-register-expert
            db
            "kinship/1"
            :version "1"
            :input-predicates '(parent)
            :output-predicates '(grandparent)
            :capabilities '(deduction explanation))
           (symbolic-register-rule
            db
            "kinship.grandparent/1"
            '((parent ?x ?y)
              (parent ?y ?z))
            '((grandparent ?x ?z))
            :expert-id "kinship/1")
           (symbolic-assert-fact db '(parent "alice" "bob")
                                :source-ids '("fixture:a"))
           (symbolic-assert-fact db '(parent "bob" "carol")
                                :source-ids '("fixture:b"))
           (let ((stats (symbolic-run-expert db "kinship/1")))
             (is (= 1 (getf stats :derived)))
             (is (getf stats :saturated)))
           (multiple-value-bind (answers truncated)
               (symbolic-query db '(grandparent "alice" ?who))
             (is (null truncated))
             (is (= 1 (length answers)))
             (is (equal "carol"
                        (cdr (assoc '?who
                                    (getf (first answers) :bindings)
                                    :test #'eq))))
             (let* ((fact-id (getf (first answers) :fact-id))
                    (explanation (symbolic-explain-fact db fact-id))
                    (derivations (getf explanation :derivations)))
               (is (= 1 (length derivations)))
               (is (equal "kinship.grandparent/1"
                          (getf (first derivations) :rule-id)))
               (is (= 2 (length (getf (first derivations) :evidence-ids))))))
           (let ((again (symbolic-run-expert db "kinship/1")))
             (is (= 0 (getf again :derived)))
             (is (= 0 (getf again :derivations)))
             (is (getf again :saturated))))
      (close-database db))))

(test symbolic-expert-contracts-are-enforced
  (let ((db (setup-db #P"/tmp/test-tek9-symbolic-expert/")))
    (unwind-protect
         (progn
           (symbolic-register-expert
            db
            "reachability/1"
            :input-predicates '(edge)
            :output-predicates '(reachable))
           (signals error
             (symbolic-register-rule
              db
              "bad.output/1"
              '((edge ?x ?y))
              '((undeclared ?x ?y))
              :expert-id "reachability/1"))
           (signals error
             (symbolic-register-rule
              db
              "bad.unsafe/1"
              '((edge ?x ?y))
              '((reachable ?x ?z))
              :expert-id "reachability/1"))
           (multiple-value-bind (expert-id state)
               (symbolic-register-expert
                db
                "reachability/1"
                :input-predicates '(edge)
                :output-predicates '(reachable))
             (is (string= "reachability/1" expert-id))
             (is (eq :existing state))))
      (close-database db))))

(test symbolic-inference-bounds-fail-explicitly
  (let ((db (setup-db #P"/tmp/test-tek9-symbolic-bounds/")))
    (unwind-protect
         (progn
           (symbolic-register-expert
            db
            "copy/1"
            :input-predicates '(seed)
            :output-predicates '(copied))
           (symbolic-register-rule
            db
            "copy.seed/1"
            '((seed ?x))
            '((copied ?x))
            :expert-id "copy/1")
           (symbolic-assert-fact db '(seed "a"))
           (symbolic-assert-fact db '(seed "b"))
           (signals symbolic-inference-limit
             (symbolic-run-expert db "copy/1" :max-bindings 1)))
      (close-database db))))

(test symbolic-facts-persist-across-reopen
  (let* ((path #P"/tmp/test-tek9-symbolic-persistence/")
         (db (setup-db path)))
    (unwind-protect
         (symbolic-assert-fact db '(knows "alice" "bob"))
      (close-database db))
    (let ((reopened
            (open-database
             (new-database "test" :path path))))
      (unwind-protect
           (multiple-value-bind (answers truncated)
               (symbolic-query reopened '(knows "alice" ?who))
             (is (null truncated))
             (is (= 1 (length answers)))
             (is (equal "bob"
                        (cdr (assoc '?who
                                    (getf (first answers) :bindings)
                                    :test #'eq)))))
        (close-database reopened)))))
