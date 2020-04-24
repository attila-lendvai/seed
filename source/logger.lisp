(in-package :seed)

(locally
    #+sbcl(declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
  (handler-bind
      ((sb-kernel:redefinition-warning #'muffle-warning))
    (hu.dwim.logger:deflogger seed () :runtime-level hu.dwim.logger:+warn+)
    (hu.dwim.logger:deflogger eval (seed))
    (hu.dwim.logger:deflogger reader (seed))
    (hu.dwim.logger:deflogger expander (seed)))
    )
