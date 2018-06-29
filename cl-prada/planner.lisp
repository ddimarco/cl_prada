(in-package :cl-prada)

(defun planner-path ()
  (concatenate 'string *prada-path* "/planner"))

(defun write-prada-files (domain reward-list)
  (with-open-file (reward-file "/tmp/reward.dat" :direction :output :if-exists :supersede)
    (write-line "1" reward-file) ;; 1 -> literal list
    (with-slots (world-state) domain
      (with-slots (symbol->num) world-state
        (dolist (lit (convert-symbols->num-ids symbol->num reward-list))
          (format reward-file "~a~%" (delispify-expr lit))))))
  (with-open-file (symbols-file "/tmp/symbols.dat" :direction :output :if-exists :supersede)
    (with-open-file (rules-file "/tmp/rules.dat" :direction :output :if-exists :supersede)
      (with-open-file (state-file "/tmp/state.dat" :direction :output :if-exists :supersede)
        (write-prada-domain domain :symbols-stream symbols-file
                            :rules-stream rules-file
                            :state-stream state-file))))
  (with-open-file (cfg-file "/tmp/config" :direction :output :if-exists :supersede)
    (format cfg-file "[general]
randSeed 1234
file_symbols symbols.dat
file_rules rules.dat
file_state state.dat
file_reward reward.dat
// 1 PRADA,  2 A_PRADA  3 UCT,  4 SST
plan_type 1

[SHARED PARAMETERS]
discountFactor 0.95
# shared by SST and UCT
noise_scaling_factor 0.75

[PRADA]
PRADA_horizon 4
PRADA_num_samples 500
PRADA_noise_softener 0.2

[UCT]
UCT_horizon 4
UCT_c 1.0
UCT_numEpisodes 2000

[SST]
SST_horizon 2
SST_branch 3
")))

(defun run-planner (domain-name reward-list)
  (let ((reward-list
         (if (symbolp (car reward-list))
             (list reward-list)
             reward-list)))
    (let ((domain (get-domain domain-name)))
      (write-prada-files domain reward-list)
      (with-output-to-string (asdf::*verbose-out*)
        (uiop:run-program (format nil "cd /tmp; ~a" (planner-path)))))))
