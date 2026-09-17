(in-package :star-git)

(cffi:define-foreign-library liblzma
  (:unix (:or "liblzma.so.5" "liblzma.so"))
  (t (:default "liblzma")))

(cffi:use-foreign-library liblzma)

(defconstant +lzma-ok+ 0)
(defconstant +lzma-check-crc64+ 4)
(defconstant +lzma-preset+ 6)

(defparameter *lzma-memory-limit* (* 512 1024 1024)
  "Maximum decoder memory accepted for one archived star-git object.")

(define-condition lzma-codec-error (error)
  ((phase :initarg :phase :reader lzma-codec-error-phase)
   (code :initarg :code :reader lzma-codec-error-code))
  (:report
   (lambda (condition stream)
     (format stream "liblzma ~A failed with return code ~D."
             (lzma-codec-error-phase condition)
             (lzma-codec-error-code condition)))))

(cffi:defcfun ("lzma_stream_buffer_bound" %lzma-stream-buffer-bound) :size
  (uncompressed-size :size))

(cffi:defcfun ("lzma_easy_buffer_encode" %lzma-easy-buffer-encode) :int
  (preset :uint32)
  (check :int)
  (allocator :pointer)
  (input :pointer)
  (input-size :size)
  (output :pointer)
  (output-pos :pointer)
  (output-size :size))

(cffi:defcfun ("lzma_stream_buffer_decode" %lzma-stream-buffer-decode) :int
  (memlimit :pointer)
  (flags :uint32)
  (allocator :pointer)
  (input :pointer)
  (input-pos :pointer)
  (input-size :size)
  (output :pointer)
  (output-pos :pointer)
  (output-size :size))

(defun %nonempty-octet-buffer (octets)
  (if (zerop (length octets))
      (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)
      octets))

(defun %lzma-compress (octets)
  "Encode OCTETS as one deterministic XZ-framed LZMA2 stream.

The pack format fixes preset 6, CRC64, one thread and no timestamps. liblzma's
buffer API is single-threaded and emits no wall-clock metadata, so equal input
under this format version produces equal compressed bytes."
  (let* ((octets (%ensure-octets octets))
         (input (%nonempty-octet-buffer octets))
         (bound (%lzma-stream-buffer-bound (length octets)))
         (output (make-array bound :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (input-pointer input)
      (cffi:with-pointer-to-vector-data (output-pointer output)
        (cffi:with-foreign-object (output-position :size)
          (setf (cffi:mem-ref output-position :size) 0)
          (let ((code
                  (%lzma-easy-buffer-encode
                   +lzma-preset+
                   +lzma-check-crc64+
                   (cffi:null-pointer)
                   input-pointer
                   (length octets)
                   output-pointer
                   output-position
                   bound)))
            (unless (= code +lzma-ok+)
              (error 'lzma-codec-error :phase :encode :code code))
            (subseq output 0 (cffi:mem-ref output-position :size))))))))

(defun %lzma-decompress (compressed expected-size)
  "Decode one star-git LZMA stream and require exactly EXPECTED-SIZE bytes."
  (check-type expected-size (integer 0 *))
  (let* ((compressed (%ensure-octets compressed))
         (input (%nonempty-octet-buffer compressed))
         (output-size (max 1 expected-size))
         (output (make-array output-size :element-type '(unsigned-byte 8))))
    (cffi:with-pointer-to-vector-data (input-pointer input)
      (cffi:with-pointer-to-vector-data (output-pointer output)
        (cffi:with-foreign-object (memory-limit :uint64)
          (cffi:with-foreign-object (input-position :size)
            (cffi:with-foreign-object (output-position :size)
              (setf (cffi:mem-ref memory-limit :uint64) *lzma-memory-limit*
                    (cffi:mem-ref input-position :size) 0
                    (cffi:mem-ref output-position :size) 0)
              (let ((code
                      (%lzma-stream-buffer-decode
                       memory-limit
                       0
                       (cffi:null-pointer)
                       input-pointer
                       input-position
                       (length compressed)
                       output-pointer
                       output-position
                       expected-size)))
                (unless (= code +lzma-ok+)
                  (error 'lzma-codec-error :phase :decode :code code))
                (unless (= (cffi:mem-ref input-position :size)
                           (length compressed))
                  (error 'lzma-codec-error :phase :trailing-input :code code))
                (unless (= (cffi:mem-ref output-position :size) expected-size)
                  (error 'lzma-codec-error :phase :size-mismatch :code code))
                (if (zerop expected-size)
                    (make-array 0 :element-type '(unsigned-byte 8))
                    (subseq output 0 expected-size))))))))))
