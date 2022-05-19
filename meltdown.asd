(defsystem "meltdown"
  :author "Nyx <n1x@riseup.net"
  :description "A markdown parser in portable common lisp"
  :license "ISC"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util"
                  :depends-on ("md"))
                 (:file "md")))))
