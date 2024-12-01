(require 'org)

(defcustom org-skills-max-level 99
  "The customisable maximum level for skills."
  :type 'integer
  :group 'org-skills)

(defun org-skills-exp-for-level (level)
  "Calculate the total EXP required to reach LEVEL."
  (if (<= level 1)
      0
    (floor (* (expt level 3) 0.25))))

(defun org-skills-insert-progress-bar (current-exp next-exp)
 "Generate a progress bar string based on CURRENT-EXP and NEXT-EXP.
The progress bar uses █ for completion and ░ for the remaining portion,
followed by a percentage."
  (let* ((progress (/ (float current-exp) next-exp))
         (percentage (floor (* progress 100)))
         (bar-width 10)
         (completed (floor (* progress bar-width)))
         (remainder (- bar-width completed)))
    (format "%s%s %d%%" 
            (make-string completed ?█)
            (make-string remainder ?░)
            percentage)))

(defun org-skills-update-skill (skill)
  "Update the skill SKILL by adding 1 EXP and refreshing its progress bar in the task properties."
  (let* ((current-level (string-to-number (or (org-entry-get (point) "LEVEL") "1")))
         (current-exp (string-to-number (or (org-entry-get (point) "EXP") "0")))
         (next-exp (string-to-number (or (org-entry-get (point) "NEXT-EXP") 
                                         (number-to-string (org-skills-exp-for-level 2)))))
         (new-exp (+ current-exp 1)))
    (while (and (>= new-exp next-exp)
                (< current-level org-skills-max-level))
      (setq current-level (1+ current-level))
      (setq new-exp (- new-exp next-exp))
      (setq next-exp (org-skills-exp-for-level current-level)))
    
    (when (>= current-level org-skills-max-level)
      (setq current-level org-skills-max-level)
      (setq new-exp 0)
      (setq next-exp (org-skills-exp-for-level current-level)))

    (org-entry-put (point) "LEVEL" (format "%d/%d" current-level org-skills-max-level))
    (org-entry-put (point) "EXP" (number-to-string new-exp))
    (org-entry-put (point) "NEXT-EXP" (number-to-string next-exp))
    (let ((progress-bar (org-skills-insert-progress-bar new-exp next-exp)))
      (org-entry-put (point) "PROGRESS-BAR" progress-bar))))

(defun org-skills-complete-task ()
  "Hook to add 1 EXP to the skill when a task is marked as DONE."
  (let ((skill (org-entry-get (point) "SKILL")))
    (when skill
      (org-skills-update-skill skill))))

(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (string= org-state "DONE")
              (org-skills-complete-task))))
