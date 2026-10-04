(in-package :star.app)


(defun display-value (value)
  (if (stringp value) value (com.inuoe.jzon:stringify value)))
