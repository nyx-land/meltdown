(defsystem "meltdown"
  :author "Nyx <n1x@riseup.net"
  :description "A markdown parser in portable common lisp"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "md")))))
