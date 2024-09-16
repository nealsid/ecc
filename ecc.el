;; Some Emacs helper functions to interact with CMake's compile_commands.json file.

(define-error 'compilation-commands-missing "No compilation commands json file in build root")

(setq projects '())

(cl-defun make-project (&key project-name
                             project-short-name
                             source-root
                             build-root
                             compile-commands-command)
  (let* ((json-path (concat build-root "/compile_commands.json"))
         (compile-commands (make-compile-commands-json json-path)))
    `(
      (project-source . ,source-root)
      (project-build-root . ,build-root)
      (project-compile-command-json . ,compile-commands)
      (project-filename-to-compilation . ,(project-build-compilation-hash compile-commands))
      )
    ))


(defun add-project (source-root build-root)
  (add-to-list 'projects (make-project source-root build-root)))

(defun project-find-file-hook ()
  (let ((filename (buffer-file-name)))
    (seq-filter (lambda (project)
                  (if (string-prefix-p (project-source-root project) filename t)
                      project
                    nil))
                projects)))

(defun make-project (source-root build-root)
  (let ((json-path (concat build-root "/compile_commands.json")))
    (unless (file-exists-p json-path)
      (signal 'compilation-commands-missing nil))
    (let ((compile-commands (make-compile-commands-json json-path)))
      `(
        (project-source . ,source-root)
        (project-build-root . ,build-root)
        (project-compile-cmd-json . compile-commands)
        (project-filename-to-compilation . ,(project-build-compilation-hash compile-commands))
        )
      )))

(defmacro project-accessor (field-name body...)
  `(defun ,(intern (concat "project-" field-name)) (&optional project)
     (if (bound-and-true-p project)
         (project-setting-lookup ,field-name project)
       (project-setting-lookup ,field-name (project-for-current-buffer)))))

(project-accessor "source-root" ())

(defun project-setting-lookup (setting-name project-settings)
  (cdr (assoc setting-name project-settings)))

;; (defun project-source-root (project-settings)
;;   (cdr (assoc 'project-source project-settings)))

(defun project-build-root (project-settings)
  (cdr (assoc 'project-build-root project-settings)))

(defun project-compile-command-json (project-settings)
  (cdr (assoc 'project-compile-cmd-json project-settings)))

(defun project-compile-command-ht (project-settings)
  (cdr (assoc 'project-filename-to-compilation project-settings)))

(defun project-build-compilation-hash (json)
  (let ((filename-to-compilation-command (make-hash-table :test 'equal :size 5500)))
    (mapc (lambda (x)
            (let ((file-full-path (gethash "file" x))
                  (file-compilation-command (gethash "command" x)))
              (puthash file-full-path file-compilation-command filename-to-compilation-command)))
          json)
    filename-to-compilation-command))

(defun project-get-compilation-command (fn project-settings)
  (gethash fn (project-compile-command-ht project-settings)))

(defun project-for-current-buffer ()
  ;; Eventually look through list of defined projects for the one
  ;; whose source root contains the current buffer.
  (let ((filename (buffer-file-name)))
    (car (seq-filter (lambda (project)
                       (if (string-prefix-p (project-source-root project) filename t)
                           project
                         nil))
                     projects))))

(defun compile-using-project-compilation-command ()
  (interactive)
  (let* ((project (project-for-current-buffer))
         (compiler-command (project-get-compilation-command (buffer-file-name) project))
         (full-compile-command (concat "cd " (project-build-root project) " && " compiler-command)))
    (compile full-compile-command)))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))
