(require 'eieio)

(defclass projectile-cached-value-template ()
  ((name :initarg :name :documentation
         "Cached value name")
   (getter :initarg :getter :documentation
           "Lambda returning (value timeout) tuple")))

(defclass projectile-static-value ()
  ((value :initarg :value)))

(cl-defmethod cached-value-expire ((this projectile-static-value))
  nil)
(cl-defmethod cached-value-set ((this projectile-static-value) value)
  (oset this value value))
(cl-defmethod cached-value-p ((this projectile-static-value))
  t)
(cl-defmethod cached-value-get ((this projectile-static-value))
  (oref this value))

(defclass projectile-cached-value ()
  ((template :initarg :template :documentation
             "Attribute template")
   (parent :initarg :parent :documentation
           "Parent object")
   (expiration :initform 0 :documentation
         "When the value expires")
   (timeout :initform -1)
   (has_value :initform nil)
   (value :documentation
          "Cached value")))

(cl-defmethod cached-value-expire ((this projectile-cached-value))
  (oset this expiration 0))

(cl-defmethod cached-value-set ((this projectile-cached-value) value timeout)
  (let* ((tpl (oref this template))
         (now (float-time (current-time)))
         (exp (+ now timeout)))
    (oset this timeout timeout)
    (oset this has_value t)
    (oset this value value)
    (oset this expiration exp)))

(cl-defmethod cached-value-p ((this projectile-cached-value))
  (let* ((tpl (oref this template))
         (tout (oref this timeout))
         (exp (oref this expiration))
         (has_val (oref this has_value))
         (now (float-time (current-time))))
    (and has_val (or (< tout 0)
                     (> exp now)))))

(cl-defmethod cached-value-get ((this projectile-cached-value))
  (unless (cached-value-p this)
    (let* ((tpl (oref this template))
           (parent (oref this parent))
           (getter (oref tpl getter))
           (tuple (funcall getter tpl parent))
           (value (car tuple))
           (tout (cadr tuple)))
      (cached-value-set this value tout)))
  (oref this value))

(defclass projectile-file-cache ()
  ((project :initarg :project :documentation
            "Link back to the enclosing project")
   (attrs :initarg :attrs :initform (make-hash-table))
   (templates :allocation :class :initform (make-hash-table))))

(defmethod add-template :STATIC ((class projectile-file-cache) name func)
  (let ((tpls (oref-default class templates))
        (tpl (make-instance projectile-cached-value-template
                            :name name
                            :getter func)))
    (puthash name tpl tpls)
    tpl))

(cl-defmethod cached-value-init ((this projectile-file-cache) name)
  (let* ((attrs (oref this attrs))
         (attr (gethash name attrs)))
    (unless attr
      (let* ((tpls (oref-default (object-class-fast this) templates))
             (tpl (gethash name tpls)))
        (setq attr (make-instance projectile-cached-value
                                  :template tpl
                                  :parent this))
        (puthash name attr attrs)))
    attr))

(cl-defmethod cached-value-get ((this projectile-file-cache) name)
  (let ((attr (cached-value-init this name)))
    (cached-value-get attr)))

(cl-defmethod cached-value-p ((this projectile-file-cache) name)
  (let ((attr (cached-value-init this name)))
    (cached-value-p attr)))

(cl-defmethod cached-value-set ((this projectile-file-cache) name value)
  (let ((attr (cached-value-init this name)))
    (cached-value-set attr value)))

(cl-defmethod cached-value-expire ((this projectile-file-cache) name)
  (let ((attr (cached-value-init this name)))
    (cached-value-expire attr)))

(cl-defmethod cached-value-set-static ((this projectile-file-cache) name value)
  (let* ((attrs (oref this attrs))
         (attr (gethash name attrs)))
    (unless attr
      (setq attr (make-instance projectile-static-value))
      (puthash name attr attrs))
    (cached-value-set attr value)))

(defclass projectile-project-cache ()
  ((root :initarg :root)
   (internal-files :initarg :files :initform (make-hash-table :test 'equal))
   (internal-files-list :initform nil)))

(cl-defmethod ppc-add-file ((this projectile-project-cache) file)
  (let ((files (oref this internal-files))
        (fc (make-instance projectile-file-cache :project this)))
    (cached-value-set-static fc :relative file)
    (puthash file fc files)
    (oset this internal-files-list nil)
    fc))

(cl-defmethod ppc-has-file ((this projectile-project-cache) file)
  (let ((files (oref this internal-files)))
    (gethash file files nil)))

(cl-defmethod ppc-rm-file ((this projectile-project-cache) file)
  (let ((files (oref this internal-files)))
    (remhash file files)
    (oset this internal-files-list nil)))

(cl-defmethod ppc-get-files ((this projectile-project-cache) &optional attr)
  (let ((files-list (oref this internal-files-list)))
    (unless files-list
      (let ((files (oref this internal-files))
            (collected))
        (maphash (lambda (k v) (push v collected)) files)
        (setq files-list (cl-sort collected (lambda (a b)
                                              (string-lessp
                                               (cached-value-get a :absolute)
                                               (cached-value-get b :absolute)))))
        (oset this internal-files-list files-list)))
    (if attr
        (cl-mapcar (lambda (x) (cached-value-get x attr)) files-list)
      files-list)))

(cl-defmethod ppc-get-file ((this projectile-project-cache) file &optional attr)
  (let* ((files (oref this internal-files))
         (file (gethash file files)))
    (if file (if attr (cached-value-get file attr) file))))

(add-template projectile-file-cache
              :absolute
              (lambda (tpl file)
                (let* ((proj (oref file project))
                       (root (oref proj root))
                       (rel_file (cached-value-get file :relative))
                       (abs_file (expand-file-name rel_file root)))
                  (list abs_file -1))))
(add-template projectile-file-cache
              :is_remote
              (lambda (tpl file)
                (let* ((abs (cached-value-get file :absolute))
                       (remote (file-remote-p abs)))
                  (list (if remote t nil) -1))))
(add-template projectile-file-cache
              :file_exists
              (lambda (tpl file)
                (let* ((remote (cached-value-get file :is_remote))
                       (abs (cached-value-get file :absolute))
                       (tout (if remote
                                 (or projectile-file-exists-remote-cache-expire -1)
                               (or projectile-file-exists-local-cache-expire -1)))
                  (list (file-exists-p abs) tout)))))

(provide 'projectile-cache)
