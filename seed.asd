(defsystem :seed
  :depends-on (:alexandria
               :anaphora
               :iterate)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (:file "logger-stubs" :depends-on ("utils"))
                             (:file "eval" :depends-on ("utils" "logger-stubs" "package"))
                             (:file "emit-ia32" :depends-on ("eval" "utils" "logger-stubs" "package"))))))

(defsystem :seed/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:seed+hu.dwim.logger
               :hu.dwim.stefil+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "eval" :depends-on ("suite"))
                             (:file "ia32" :depends-on ("suite"))))))
