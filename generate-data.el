(defun output-compute-parity (n)
  (insert (concat "COMPUTE_PARITY(" (int-to-string n) "ULL);\n")))

(defun output-n-compute-parities (n max)
  (dotimes (i n)
    (output-compute-parity (random max))))

(defun datagen-process-file (fpath n)
  "process the file at fullpath FPATH ..."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (goto-char (point-min)) ;; in case buffer is open
    (output-n-compute-parities n (expt 2 60))
    (save-buffer)
    (kill-buffer mybuffer)))
