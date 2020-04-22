(defsystem :seed+hu.dwim.logger
  :defsystem-depends-on (:hu.dwim.logger)
  :depends-on (:seed)
  :components ((:module "source"
                :components ((:file "logger")))))
