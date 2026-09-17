(in-package :tek9-tests)

(in-suite :tek9-tests)

(defun star-git-custody-fixture (path)
  (let* ((repository (setup-star-git-repository path))
         (accepted-at (encode-universal-time 0 0 0 17 9 2026 0))
         (bytes (star-git-test-octets "{\"custody\":true}"))
         (blob-id (star-git:write-blob repository bytes))
         (commit-id
           (star-git:commit-document
            repository
            "doc-custody"
            blob-id
            :tenant "tenant-a"
            :dataset "dataset-a"
            :dtype "host"
            :mutation-kind "create"
            :accepted-at accepted-at
            :format-version 2
            :provenance
            '(:couch-rev "1-custody"
              :source-actor "collector"
              :operation-id "op-custody"
              :trace-id nil
              :refresh-generation 1
              :policy-digest "sha256:policy"
              :content-digest "sha256:content"
              :schema-release "0.9.1"
              :schema-version "0.9.0")))
         (pack-path (merge-pathnames #P"archive/history.sgp.lzma" path)))
    (multiple-value-bind (pack-id metadata)
        (star-git:build-pack repository (list blob-id commit-id) pack-path)
      (values repository bytes blob-id commit-id pack-id metadata pack-path))))

(test star-git-local-pack-eviction-requires-explicit-policy
  (multiple-value-bind
        (repository bytes blob-id commit-id pack-id metadata pack-path)
      (star-git-custody-fixture #P"/tmp/test-tek9-star-git-custody-local/")
    (declare (ignore bytes commit-id pack-id metadata pack-path))
    (unwind-protect
         (progn
           (is (eq :hot (star-git:object-availability repository blob-id)))
           (signals star-git:custody-policy-error
             (star-git:evict-hot-object repository blob-id))
           (is (eq :evicted
                   (star-git:evict-hot-object
                    repository blob-id :minimum-custody :verified-any)))
           (is (null (star-git:read-object repository blob-id)))
           (is (eq :packed-local
                   (star-git:object-availability repository blob-id)))
           (let ((plan (star-git:plan-object-restore repository blob-id)))
             (is (eq :restore-required (getf plan :status)))
             (is (eq :local-pack (getf (getf plan :preferred) :kind))))
           ;; Compact commit metadata/index rows remain hot and queryable.
           (is (= 1 (length (star-git:log-document repository "doc-custody"))))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-remote-eviction-requires-verified-receipt
  (multiple-value-bind
        (repository bytes blob-id commit-id pack-id metadata pack-path)
      (star-git-custody-fixture #P"/tmp/test-tek9-star-git-custody-remote/")
    (declare (ignore bytes commit-id pack-path))
    (unwind-protect
         (progn
           (star-git:register-custody-receipt
            repository
            "receipt-uploaded"
            pack-id
            :state :stored-unverified
            :backend "linode-archive"
            :catalog-locator "archive:pack/custody"
            :byte-length (getf metadata :byte-length)
            :verification :deferred-strong)
           (signals star-git:custody-policy-error
             (star-git:evict-hot-object repository blob-id))

           (star-git:register-custody-receipt
            repository
            "receipt-verified"
            pack-id
            :state :verified
            :backend "linode-archive"
            :catalog-locator "archive:pack/custody"
            :byte-length (getf metadata :byte-length)
            :verified-at 4000000100
            :verification :full-readback)
           (is (= 1
                  (length
                   (star-git:verified-custody-receipts-for-pack
                    repository pack-id))))
           (is (eq :evicted
                   (star-git:evict-hot-object repository blob-id)))
           (is (eq :packed-remote
                   (star-git:object-availability repository blob-id)))
           (let ((plan (star-git:plan-object-restore repository blob-id)))
             (is (eq :remote-pack (getf (getf plan :preferred) :kind))))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-evicted-commit-payload-keeps-history-and-couch-index
  (multiple-value-bind
        (repository bytes blob-id commit-id pack-id metadata pack-path)
      (star-git-custody-fixture #P"/tmp/test-tek9-star-git-custody-commit/")
    (declare (ignore bytes pack-path))
    (unwind-protect
         (progn
           (star-git:register-custody-receipt
            repository
            "receipt-verified"
            pack-id
            :state :verified
            :backend "linode-archive"
            :catalog-locator "archive:pack/commit"
            :byte-length (getf metadata :byte-length)
            :verified-at 4000000200
            :verification :full-readback)
           (is (eq :evicted
                   (star-git:evict-hot-object repository blob-id)))
           (is (eq :evicted
                   (star-git:evict-hot-object repository commit-id)))
           (is (null (star-git:read-object repository commit-id)))
           (is (string=
                commit-id
                (getf (star-git:read-commit repository commit-id) :commit-id)))
           (is (string=
                commit-id
                (getf (star-git:find-commit-by-couch-rev
                       repository
                       "doc-custody"
                       "1-custody"
                       :tenant "tenant-a"
                       :dataset "dataset-a")
                      :commit-id)))
           (is (= 1 (length (star-git:log-document repository "doc-custody"))))
           (is (null (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-remote-custody-survives-local-pack-loss
  (multiple-value-bind
        (repository bytes blob-id commit-id pack-id metadata pack-path)
      (star-git-custody-fixture #P"/tmp/test-tek9-star-git-custody-loss/")
    (declare (ignore bytes commit-id))
    (unwind-protect
         (progn
           (star-git:register-custody-receipt
            repository
            "receipt-verified"
            pack-id
            :state :verified
            :backend "linode-archive"
            :catalog-locator "archive:pack/remote-only"
            :byte-length (getf metadata :byte-length)
            :verified-at 4000000300
            :verification :full-readback)
           (star-git:evict-hot-object repository blob-id)
           (delete-file pack-path)
           (is (eq :packed-remote
                   (star-git:object-availability repository blob-id)))
           (is (some (lambda (problem)
                       (eq (first problem) :missing-local-pack))
                     (star-git:fsck repository))))
      (star-git:close-repository repository))))

(test star-git-custody-receipts-are-immutable-and-length-checked
  (multiple-value-bind
        (repository bytes blob-id commit-id pack-id metadata pack-path)
      (star-git-custody-fixture #P"/tmp/test-tek9-star-git-custody-receipt/")
    (declare (ignore bytes blob-id commit-id pack-path))
    (unwind-protect
         (progn
           (signals star-git:custody-error
             (star-git:register-custody-receipt
              repository
              "bad-length"
              pack-id
              :state :verified
              :backend "linode-archive"
              :catalog-locator "archive:bad"
              :byte-length (1+ (getf metadata :byte-length))
              :verified-at 4000000400
              :verification :full-readback))
           (star-git:register-custody-receipt
            repository
            "immutable"
            pack-id
            :state :stored-unverified
            :backend "linode-archive"
            :catalog-locator "archive:immutable"
            :byte-length (getf metadata :byte-length)
            :verification :deferred-strong)
           (signals star-git:custody-error
             (star-git:register-custody-receipt
              repository
              "immutable"
              pack-id
              :state :verified
              :backend "linode-archive"
              :catalog-locator "archive:immutable"
              :byte-length (getf metadata :byte-length)
              :verified-at 4000000401
              :verification :full-readback)))
      (star-git:close-repository repository))))
