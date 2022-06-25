(ns tlc-lisp.core-test
  (:require [clojure.test :refer :all]
            [tlc-lisp.core :refer :all]))

(deftest estandarizar-test
  (testing "Estandarizar devuelve nil cuando recibe nil"
    (is (nil? (estandarizar nil))))
  (testing "Estandarizar devuelve nil cuando recibe 'nil"
    (is (nil? (estandarizar 'nil))))
  (testing "Estandarizar devuelve nil cuando recibe 'NIL"
    (is (nil? (estandarizar 'NIL))))
  (testing "Estandarizar devuelve el mismo numero"
    (is (= 4 (estandarizar '4))))
  (testing "Estandarizar devuelve minusucla cuando es no evaluado"
    (is (= 'a (estandarizar 'A))))
  (testing "Estandarizar devuelve igual cuando es un string"
    (is (= '"A" (estandarizar '"A"))))
)

(deftest controlar-aridad-test
  (testing "Controlar aridad devuelve error cuando la cantidad de parametros es menor a la esperada"
    (is (= (controlar-aridad '(a b c) 4) '(*error* "too-few-args"))))
  (testing "Controlar aridad devuelve error cuando la cantidad de parametros es mayor a la esperada"
    (is (= (controlar-aridad '(a b c) 2) '(*error* "too-many-args"))))
  (testing "Controlar aridad devuelve aridad cuando la cantidad de parametros es igual a la esperada"
    (is (= (controlar-aridad '(a b c) 3) '3)))
)

(deftest secuencias-iguales-test
  (testing "Secuencias iguales devuelve false cuando dos secuencias tienen distinto tamaño"
    (is (false? (secuencias-iguales? '("a" "b" "c") '("a" "b")))))
  (testing "Secuencias iguales devuelve false cuando dos secuencias tienen distintos elementos u orden de elementos"
    (is (false? (secuencias-iguales? '("a" "b" "c") '("c" "a" "b")))))
  (testing "Secuencias iguales devuelve true cuando dos secuencias tienen igual elementos y en el mismo orden"
    (is (true? (secuencias-iguales? '("a" "b" "c") '("a" "b" "c")))))
)

(deftest igual-test
  (testing "Igual devuelve true cuando dos numeros son el mismo numero"
    (is (true? (igual? '1 '1))))
  (testing "Igual devuelve false cuando dos numeros son distinto numero"
    (is (false? (igual? '1 '2))))
  (testing "Igual devuelve true cuando ambos elementos son la misma letra (simbolo)"
    (is (true? (igual? 'a 'a))))
  (testing "Igual devuelve true cuando ambos elementos son la misma letra (simbolo) sin distinguir entre mayuscula y minuscula"
    (is (true? (igual? 'a 'A))))
  (testing "Igual devuelve true cuando ambos elementos son secuencias con las mismas letras (simbolo) sin distinguir entre mayuscula y minuscula"
    (is (true? (igual? '(a b c) '(A B C)))))
  (testing "Igual devuelve false cuando ambos elementos son secuencias con distintas letras (simbolo) sin distinguir entre mayuscula y minuscula"
    (is (false? (igual? '(a b c) '(A B D)))))
  (testing "Igual devuelve true cuando ambos elementos es nil"
    (is (true? (igual? nil nil))))
  (testing "Igual devuelve true cuando ambos elementos es nil"
    (is (true? (igual? nil 'nil))))
  (testing "Igual devuelve true cuando ambos elementos es nil"
    (is (true? (igual? nil 'NIL))))
  (testing "Igual devuelve true cuando los elementos son nil y secuencia vacia"
    (is (true? (igual? nil '()))))
  (testing "Igual devuelve true cuando los elementos son nil y secuencia vacia"
    (is (true? (igual? 'NIL '()))))
  (testing "Igual devuelve true cuando ambos elementos son secuencias vacias"
    (is (true? (igual? '() '()))))
  (testing "Igual devuelve false cuando los elementos son secuencias vacia y secuencia con nil"
    (is (false? (igual? '() '(nil)))))
  (testing "Igual devuelve true cuando ambos elementos son la misma letra (string) distinguiendo mayusuclas y minusculas"
    (is (true? (igual? "a" "a"))))
  (testing "Igual devuelve false cuando ambos elementos son la misma letra (string) distinguiendo mayusuclas y minusculas"
    (is (false? (igual? "a" "A"))))
  (testing "Igual devuelve false cuando los elementos son simbolo y letra"
    (is (false? (igual? 'a "a"))))
  (testing "Igual devuelve false cuando los elementos son simbolo y letra"
    (is (false? (igual? 'a "A"))))
)
