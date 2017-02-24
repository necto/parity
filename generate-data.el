(defun output-compute-parity (n)
  (insert (concat "COMPUTE_PARITY(" (int-to-string n) "ULL);\n")))

(defun output-n-compute-parities (n max)
  (dotimes (i n)
    (output-compute-parity (random max))))

(defun process-file (fpath)
  "process the file at fullpath FPATH ..."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (goto-char (point-min)) ;; in case buffer is open
    (output-n-compute-parities 1000 (expt 2 60))
    (save-buffer)
    (kill-buffer mybuffer)))

(process-file "data.hi")
