(defpackage :douban-reader
  (:use :cl
        :drakma
        :cxml
        :cl-ppcre)
  (:export
   get-following
   print-following))
