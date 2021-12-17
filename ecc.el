;; Some Emacs helper functions to interact with CMake's compile_commands.json file.

(define-error 'compilation-commands-missing "No compilation commands json file in build root")

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun make-compile-commands-json (pathname)
  (json-parse-string (file-to-string pathname)))

(defun make-project (source-root build-root)
  (let* ((json-path (concat build-root "/compile_commands.json"))
         (compile-commands (make-compile-commands-json json-path)))
    (unless (file-exists-p json-path)
      (signal 'compilation-commands-missing nil))
    `(
      (project-source . ,source-root)
      (project-build-root . ,build-root)
      (project-compile-cmd-json . ,compile-commands)
      (project-filename-to-compilation . ,(project-build-compilation-hash compile-commands))
      )
    ))

(defun project-source-root (project-settings)
  (cdr (assoc 'project-source project-settings)))

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
  llvm-project)

(defun compile-using-project-compilation-command ()
  (interactive)
  (let* ((project (project-for-current-buffer))
         (compiler-command (project-get-compilation-command (buffer-file-name) project))
         (full-compile-command (concat "cd " (project-build-root project) " && " compiler-command)))
    (compile full-compile-command)))
