;; -*- lexical-binding: t; -*-


(defvar tanenbaum-template "/home/liate/projects/tanenbaum/lib/problems/template.ml")

;;;#autoload
(defun tanenbaum-start-day (year day)
  "Start a new day of AoC with tanenbaum"
  (interactive "nAoC year: \nnAoC day: ")
  (let ((filename (format "%s/lib/problems/problem_%d_%02d.ml"
                          (project-root (project-current t))
                          year
                          day)))
    (when (file-exists-p filename)
      (error "There's already a file for AoC %d, day %d (%s)" year day filename))
    (copy-file tanenbaum-template filename)
    (find-file filename)
    (goto-char (point-min))
    (search-forward "(* let year = _ *)")
    (replace-match (concat "let year = " (number-to-string year)))
    (search-forward "(* let day = _ *)")
    (replace-match (concat "let day = " (number-to-string day)))))
