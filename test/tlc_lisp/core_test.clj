(ns tlc-lisp.core-test
  (:require [clojure.test :refer :all]
            [tlc-lisp.core :refer :all]))

(deftest controlar-aridad-test
  (testing "Controlar aridad devuelve error cuando la cantidad de parametros es menor a la esperada"
    (is (= (controlar-aridad '(a b c) 4) '(*error* "too-few-args"))))
  (testing "Controlar aridad devuelve error cuando la cantidad de parametros es mayor a la esperada"
    (is (= (controlar-aridad '(a b c) 2) '(*error* "too-many-args"))))
  (testing "Controlar aridad devuelve aridad cuando la cantidad de parametros es igual a la esperada"
    (is (= (controlar-aridad '(a b c) 3) '3)))
)
