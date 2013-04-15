(defpackage :douban-reader-asd (:use :asdf :cl))

(in-package :douban-reader-asd)

(defsystem douban-reader
    :name "douban-reader"
    :author "lingjie.fan <van0229@gmail.com>"
    :version "1.0"
    :description ""
    :components ((:file "packages")
                 (:file "douban-reader" :depends-on ("packages")))
    :depends-on (:drakma
                 :cxml
                 :cl-ppcre))
