;;; agent-shell-ui-tests.el --- Tests for Agent Shell UI fragments -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'agent-shell-ui)

(ert-deftest agent-shell-ui-collapsed-fragments-enable-invisibility ()
  "Collapsed fragments should enable invisible text in the buffer."
  (with-temp-buffer
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model
      :namespace-id "test"
      :block-id "fold"
      :label-left "status"
      :label-right "title"
      :body "body line 1\nbody line 2")
     :expanded nil)
    (should (equal buffer-invisibility-spec t))
    (goto-char (point-min))
    (let ((match (text-property-search-forward
                  'agent-shell-ui-state nil
                  (lambda (_ state)
                    (equal (map-elt state :qualified-id) "test-fold"))
                  t)))
      (should match)
      (should (map-elt (get-text-property (prop-match-beginning match)
                                          'agent-shell-ui-state)
                       :collapsed))
      (when-let* ((body-range
                   (agent-shell-ui--nearest-range-matching-property
                    :property 'agent-shell-ui-section
                    :value 'body
                    :from (prop-match-beginning match)
                    :to (prop-match-end match))))
        (should (eq (get-text-property (map-elt body-range :start) 'invisible) t))))))

;;; agent-shell-ui-tests.el ends here
