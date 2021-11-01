;;; -*- lexical-binding: t -*-
(setq lexical-binding t)


(defconst cpupower-cmd
  "sudo cpupower"
  "cpupower command, might need a 'sudo' infront of it")

(defconst cpupower--compatible-versions
  '("5.4")
  "Versions of cpupower which cpupower.el can work with")

(defun cpupower--num-cpus ()
  "Determine how many CPUs are on this system

Done by cat-ing /proc/cpuinfo and counting lines with
\"processor\" in them.

TODO: do this in a less bad way?"
  (let ((cpu-count 0))
    (with-temp-buffer
      (insert-file-contents "/proc/cpuinfo")
      (while (search-forward "processor" nil t)
        (cl-incf cpu-count)))
    cpu-count))

(defun cpupower--get-available-governors ()
  "Get a list of all valid governors for this system."
  (let ((governors-per-cpu))
    (cl-loop for cpu-num in (number-sequence 0 (cpupower--num-cpus))
             for cpu-governors-file = (format "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_available_governors" cpu-num)
             while (file-exists-p cpu-governors-file)
             do (push (split-string
                       (with-temp-buffer
                         (insert-file-contents cpu-governors-file)
                         (buffer-string)))
                      governors-per-cpu))
    (cl-loop with valid-governors = (car governors-per-cpu)
             for other-governor-set in (cdr governors-per-cpu)
             do (setq other-governor-set
                      (cl-intersection valid-governors other-governor-set))
             finally return valid-governors)))

(defun cpupower--run (subcommand)
  "Execute cpupower with SUBCOMMAND string."
  (with-temp-buffer
    (let ((command (format "%s %s" cpupower-cmd subcommand)))
      (message "running: %s" command)
      (shell-command command (current-buffer))
      (buffer-string))))

(defun cpupower-info ()
  (interactive)
  (message (cpupower--run "--cpu all frequency-info")))

(defun cpupower-set-governor (governor)
  (interactive "sGovernor: ")
  (let ((valid-governors (cpupower--get-available-governors)))
    (unless (member governor valid-governors)
      (error "Invalid governor: %s, must be one of %s" governor valid-governors))
    (cpupower--run (format "--cpu all frequency-set -g %s" governor))
    (cpupower-info)))

(defun cpupower--get-version ()
  (let* ((output (cpupower--run "--version"))
         (tokens (split-string output)))
    (when (string-equal (car tokens) "cpupower")
      (cadr tokens))))

(defun cpupower-get-frequencies ()
  (interactive)
  (let* ((output (cpupower--run "--cpu all frequency-info -f"))
         (tokens (split-string output))
         (frequencies))
    (cl-loop with next-token-is-frequency = nil
             for token in tokens
             if next-token-is-frequency
             do (progn
                  (push (string-to-number token) frequencies)
                  (setq next-token-is-frequency nil))
             else
             do (when (string-equal token "frequency:")
                    (setq next-token-is-frequency t))
             finally return frequencies)))

(defun cpupower-helm-set-governor ()
  (interactive)
  (cpupower-set-governor
   (helm :sources (helm-build-sync-source "cpu-governors"
                    :candidates (cpupower--get-available-governors))
         :buffer "*helm set cpu governor*")))

(provide 'cpupower)
