(in-package :star-git)

(defparameter +custody-db+ "star-git/custody")
(defparameter +custody-by-pack-index+ "star-git/custody-by-pack")
(defparameter +custody-receipt-version+ 1)
(defparameter +custody-states+
  '(:uploaded :stored-unverified :verifying :verified :failed-integrity))
(defparameter +custody-verification-methods+
  '(:none :provider-checksum :full-readback :deferred-strong))

(define-condition custody-error (error)
  ((reason :initarg :reason :reader custody-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "star-git custody error: ~A"
             (custody-error-reason condition)))))

(define-condition custody-policy-error (custody-error)
  ((object-id :initarg :object-id :reader custody-policy-error-object-id)
   (minimum-custody :initarg :minimum-custody
                    :reader custody-policy-error-minimum-custody)))

(defun %custody-error (control &rest arguments)
  (error 'custody-error :reason (apply #'format nil control arguments)))

(defun %custody-policy-error (object-id minimum-custody control &rest arguments)
  (error 'custody-policy-error
         :object-id object-id
         :minimum-custody minimum-custody
         :reason (apply #'format nil control arguments)))

(defun %custody-db (repository)
  (database-db (repository-database repository)
               +custody-db+
               :key-encoding :utf-8
               :value-encoding :octets))

(defun %custody-pack-index-value (document)
  (getf (doc-value document) :pack-id))

(defun %ensure-custody-index (repository)
  (let ((database (repository-database repository)))
    (or (tek9:secondary-index-by-name database +custody-by-pack-index+)
        (register-index database
                        +custody-by-pack-index+
                        #'%custody-pack-index-value
                        :database-name +custody-db+))))

(defun %normalize-custody-state (state)
  (unless (member state +custody-states+ :test #'eq)
    (%custody-error "unsupported custody state ~S" state))
  state)

(defun %normalize-verification-method (method)
  (unless (member method +custody-verification-methods+ :test #'eq)
    (%custody-error "unsupported verification method ~S" method))
  method)

(defun %require-pack-metadata (repository pack-id)
  (or (read-pack-metadata repository pack-id)
      (%custody-error "unknown star-git pack ~S" pack-id)))

(defun %normalize-custody-receipt (repository
                                   receipt-id
                                   pack-id
                                   state
                                   backend
                                   catalog-locator
                                   byte-length
                                   verified-at
                                   verification)
  (check-type receipt-id string)
  (check-type pack-id string)
  (check-type backend string)
  (check-type catalog-locator string)
  (check-type byte-length (integer 0 *))
  (when verified-at
    (check-type verified-at integer))
  (let* ((pack (%require-pack-metadata repository pack-id))
         (state (%normalize-custody-state state))
         (verification (%normalize-verification-method verification))
         (expected-length (getf pack :byte-length)))
    (unless (= byte-length expected-length)
      (%custody-error
       "receipt ~S length ~D does not match pack ~S length ~D"
       receipt-id byte-length pack-id expected-length))
    (when (eq state :verified)
      (unless verified-at
        (%custody-error "verified receipt ~S requires VERIFIED-AT" receipt-id))
      (when (eq verification :none)
        (%custody-error "verified receipt ~S requires a verification method"
                        receipt-id)))
    (list :receipt-version +custody-receipt-version+
          :receipt-id receipt-id
          :pack-id pack-id
          :state state
          :backend backend
          :catalog-locator catalog-locator
          :byte-length byte-length
          :verified-at verified-at
          :verification verification)))

(defun register-custody-receipt (repository receipt-id pack-id
                                 &key
                                   (state :stored-unverified)
                                   (backend "")
                                   (catalog-locator "")
                                   byte-length
                                   verified-at
                                   (verification :none))
  "Persist one immutable provider/archive custody receipt.

This is a trusted embedded storage port. It does not contact a provider and it
must not be projected as an unauthenticated public API. Reusing RECEIPT-ID with
different content is rejected rather than overwritten."
  (let* ((pack (%require-pack-metadata repository pack-id))
         (byte-length (or byte-length (getf pack :byte-length)))
         (receipt (%normalize-custody-receipt
                   repository
                   receipt-id
                   pack-id
                   state
                   backend
                   catalog-locator
                   byte-length
                   verified-at
                   verification))
         (database (repository-database repository)))
    (%custody-db repository)
    (%ensure-custody-index repository)
    (with-write-transaction (database :database-names (list +custody-db+))
      (let ((existing
              (fetch* database receipt-id :database-name +custody-db+)))
        (cond
          ((null existing)
           (put* database receipt :id receipt-id :database-name +custody-db+))
          ((not (equal existing receipt))
           (%custody-error
            "receipt id ~S already exists with different immutable content"
            receipt-id))))
    receipt))

(defun read-custody-receipt (repository receipt-id)
  "Return one custody receipt by id, or NIL."
  (%custody-db repository)
  (fetch* (repository-database repository)
          receipt-id
          :database-name +custody-db+))

(defun custody-receipts-for-pack (repository pack-id)
  "Return all known custody receipts for PACK-ID."
  (%ensure-custody-index repository)
  (mapcar #'doc-value
          (index-fetch (repository-database repository)
                       +custody-by-pack-index+
                       pack-id)))

(defun verified-custody-receipts-for-pack (repository pack-id)
  "Return receipts proving verified remote/provider custody for PACK-ID."
  (remove-if-not
   (lambda (receipt)
     (eq (getf receipt :state) :verified))
   (custody-receipts-for-pack repository pack-id)))

(defun %object-pack-ids (repository object-id)
  (remove-duplicates
   (loop for locator in (pack-locations repository object-id)
         for pack-id = (getf locator :pack-id)
         when (stringp pack-id)
           collect pack-id)
   :test #'string=))

(defun %local-pack-locations (repository object-id)
  (loop for pack-id in (%object-pack-ids repository object-id)
        for metadata = (read-pack-metadata repository pack-id)
        when metadata
          append
          (loop for path in (getf metadata :local-paths)
                for pathname = (and path (probe-file path))
                when pathname
                  collect
                  (list :kind :local-pack
                        :pack-id pack-id
                        :path (namestring pathname)
                        :verified-at (getf metadata :verified-at)
                        :byte-length (getf metadata :byte-length)))))

(defun %remote-pack-locations (repository object-id)
  (loop for pack-id in (%object-pack-ids repository object-id)
        append
        (loop for receipt in (verified-custody-receipts-for-pack
                              repository pack-id)
              collect
              (list :kind :remote-pack
                    :pack-id pack-id
                    :receipt-id (getf receipt :receipt-id)
                    :backend (getf receipt :backend)
                    :catalog-locator (getf receipt :catalog-locator)
                    :verified-at (getf receipt :verified-at)
                    :verification (getf receipt :verification)
                    :byte-length (getf receipt :byte-length)))))

(defun object-locations (repository object-id)
  "Return known hot/local-pack/verified-remote locations for OBJECT-ID.

No remote I/O is performed. Local pack existence is checked with PROBE-FILE;
remote locations are derived only from persisted verified custody receipts."
  (append
   (when (read-object repository object-id)
     (list (list :kind :hot :object-id object-id)))
   (%local-pack-locations repository object-id)
   (%remote-pack-locations repository object-id)))

(defun object-availability (repository object-id)
  "Return current best placement class for OBJECT-ID.

The precedence describes immediate placement, not durability quality: :HOT wins
when bytes are still in LMDB; otherwise verified remote wins over local pack."
  (let ((locations (object-locations repository object-id)))
    (cond
      ((find :hot locations :key (lambda (item) (getf item :kind))) :hot)
      ((find :remote-pack locations :key (lambda (item) (getf item :kind)))
       :packed-remote)
      ((find :local-pack locations :key (lambda (item) (getf item :kind)))
       :packed-local)
      (t :unavailable))))

(defun plan-object-restore (repository object-id)
  "Return a side-effect-free restore plan for OBJECT-ID.

Remote locations are preferred for canonical cold restore; local pack paths are
used as a fallback. The caller owns any provider/network effect."
  (let ((availability (object-availability repository object-id))
        (locations (object-locations repository object-id)))
    (case availability
      (:hot
       (list :object-id object-id :status :already-hot :locations locations))
      (:packed-remote
       (list :object-id object-id
             :status :restore-required
             :preferred
             (find :remote-pack locations
                   :key (lambda (item) (getf item :kind)))
             :locations locations))
      (:packed-local
       (list :object-id object-id
             :status :restore-required
             :preferred
             (find :local-pack locations
                   :key (lambda (item) (getf item :kind)))
             :locations locations))
      (otherwise
       (list :object-id object-id :status :unavailable :locations nil)))))

(defun %custody-satisfies-p (locations minimum-custody)
  (ecase minimum-custody
    (:verified-remote
     (not (null
           (find :remote-pack locations
                 :key (lambda (item) (getf item :kind))))))
    (:verified-any
     (or (find :remote-pack locations
               :key (lambda (item) (getf item :kind)))
         (find :local-pack locations
               :key (lambda (item) (getf item :kind)))))))

(defun evict-hot-object (repository object-id
                         &key
                           (minimum-custody :verified-remote)
                           pinned-p)
  "Remove only OBJECT-ID's hot immutable payload after verified custody.

Commit/history metadata, refs, indexes, pack indexes and custody receipts remain
hot. The default requires a verified remote receipt. :VERIFIED-ANY is an
explicit cache-compaction policy that also permits an intact verified local
pack. PINNED-P is a trusted caller policy decision and blocks eviction."
  (when pinned-p
    (%custody-policy-error object-id minimum-custody
                           "object is pinned by hot-retention policy"))
  (let ((hot (read-object repository object-id)))
    (unless hot
      (return-from evict-hot-object :already-cold))
    (let ((locations (object-locations repository object-id)))
      (unless (%custody-satisfies-p locations minimum-custody)
        (%custody-policy-error
         object-id
         minimum-custody
         "no location satisfies minimum custody ~S"
         minimum-custody))
      (unless (tek9:delete-document
               (repository-database repository)
               object-id
               :database-name +objects-db+)
        (%custody-error "failed to delete hot object ~S" object-id))
      :evicted)))

(defun %known-pack-metadata (repository)
  (let ((database (repository-database repository))
        (rows nil))
    (map-database
     database
     :database-name +packs-db+
     :map-fn
     (lambda (pack-id document)
       (push (cons pack-id (doc-value document)) rows)))
    (nreverse rows)))

(defun %cold-aware-object-present-p (repository object-id)
  (not (eq (object-availability repository object-id) :unavailable)))

(defun %valid-ref-target-p (repository target-id)
  (or (read-commit repository target-id)
      (%cold-aware-object-present-p repository target-id)))

(defun %fsck-hot-objects (repository problems)
  (map-database
   (repository-database repository)
   :database-name +objects-db+
   :map-fn
   (lambda (id document)
     (let* ((value (doc-value document))
            (type (getf value :type))
            (bytes (getf value :bytes)))
       (cond
         ((or (not (stringp type))
              (not (%octet-vector-p bytes)))
          (push (list :invalid-object id) problems))
         ((not (string= id (%typed-object-id type bytes)))
          (push (list :hash-mismatch id) problems))))))
  problems)

(defun %fsck-commits (repository problems)
  (map-database
   (repository-database repository)
   :database-name +commits-db+
   :map-fn
   (lambda (id document)
     (let* ((record (doc-value document))
            (blob-id (getf record :blob-id)))
       (unless (%cold-aware-object-present-p repository id)
         (push (list :missing-commit-object id) problems))
       (unless (and blob-id (%cold-aware-object-present-p repository blob-id))
         (push (list :missing-blob id blob-id) problems))
       (dolist (parent (getf record :parents))
         (unless (read-commit repository parent)
           (push (list :missing-parent id parent) problems))))))
  problems)

(defun %fsck-refs (repository problems)
  (let ((database (repository-database repository)))
    (with-read-transaction (database)
      (lmdb:do-db (ref-name target-id (%refs-db repository))
        (unless (%valid-ref-target-p repository target-id)
          (push (list :dangling-ref ref-name target-id) problems)))))
  problems)

(defun %fsck-pack-metadata (repository problems deep)
  (dolist (entry (%known-pack-metadata repository))
    (let ((pack-id (car entry))
          (metadata (cdr entry)))
      (dolist (path (getf metadata :local-paths))
        (let ((pathname (and path (probe-file path))))
          (cond
            ((null pathname)
             (push (list :missing-local-pack pack-id path) problems))
            ((and deep
                  (not (string= pack-id (pack-id-for-path pathname))))
             (push (list :local-pack-hash-mismatch pack-id path) problems)))))))
  problems)

(defun %fsck-custody (repository problems)
  (%custody-db repository)
  (map-database
   (repository-database repository)
   :database-name +custody-db+
   :map-fn
   (lambda (receipt-id document)
     (let* ((receipt (doc-value document))
            (pack-id (getf receipt :pack-id))
            (pack (and pack-id (read-pack-metadata repository pack-id))))
       (cond
         ((null pack)
          (push (list :custody-missing-pack receipt-id pack-id) problems))
         ((/= (getf receipt :byte-length) (getf pack :byte-length))
          (push (list :custody-length-mismatch receipt-id pack-id) problems)))
       (when (and (eq (getf receipt :state) :verified)
                  (or (null (getf receipt :verified-at))
                      (eq (getf receipt :verification) :none)))
         (push (list :invalid-verified-custody receipt-id pack-id) problems)))))
  problems)

(defun fsck (repository &key deep)
  "Check hot and cold star-git integrity without performing remote I/O.

By default FSCK verifies hot object hashes, history/ref reachability, pack index
metadata, local pack presence and custody metadata. With DEEP true it also
rehashes each present local pack and compares its typed star-git pack id.
Verified remote receipts make evicted object payloads available for reachability,
but remote bytes are never downloaded by this embedded check."
  (%custody-db repository)
  (%ensure-custody-index repository)
  (let ((problems nil))
    (setf problems (%fsck-hot-objects repository problems))
    (setf problems (%fsck-commits repository problems))
    (setf problems (%fsck-refs repository problems))
    (dolist (problem (%fsck-packs repository))
      (push problem problems))
    (setf problems (%fsck-pack-metadata repository problems deep))
    (setf problems (%fsck-custody repository problems))
    (nreverse problems)))
