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
    (is (= (controlar-aridad '(a b c) 4) '(*error* too-few-args))))
  (testing "Controlar aridad devuelve error cuando la cantidad de parametros es mayor a la esperada"
    (is (= (controlar-aridad '(a b c) 2) '(*error* too-many-args))))
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

(deftest error-test
  (testing "Error? devuelve true cuando es una lista con primer elemento *error*"
    (is (true? (error? '(*error* too-few-args)))))
  (testing "Error? devuelve true cuando es una lista con primer elemento *error*"
    (is (true? (error? (list '*error* 'too-few-args)))))
  (testing "Error? devuelve true cuando es una lista con primer elemento *error*"
    (is (true? (error? (list '*ERROR* 'too-few-args)))))
  (testing "Error? devuelve true cuando es una lista con primer elemento *error*"
    (is (true? (error? (list '*Error* 'too-few-args)))))
  (testing "Error? devuelve true cuando es una lista con primer elemento *error*"
    (is (true? (error? (list '*error*)))))
  (testing "Error? devuelve false cuando es una lista vacia"
    (is (false? (error? ()))))
  (testing "Error? devuelve false cuando es nil"
    (is (false? (error? nil))))
)

(deftest revisar-fnc-test
  (testing "Devuelve la lista cuando es un mensaje de error"
    (is (= '(*error* too-few-args) (revisar-fnc '(*error* too-few-args)))))
  (testing "Devuelve nil cuando no es un mensaje de error"
    (is (nil? (revisar-fnc '(too-few-args)))))
  (testing "Devuelve nil cuando no es un mensaje de error"
    (is (nil? (revisar-fnc '*error*))))
  (testing "Devuelve nil cuando es nil"
    (is (nil? (revisar-fnc nil))))
  (testing "Devuelve nil cuando es lista vacia"
    (is (nil? (revisar-fnc '()))))
)

(deftest revisar-lae-test
  (testing "Devuelve nil cuando es nil"
    (is (nil? (revisar-lae nil))))
  (testing "Devuelve nil cuando es vacio"
    (is (nil? (revisar-lae '()))))
  (testing "Devuelve el primer error encontrado"
    (is (= '(*error* too-few-args) (revisar-lae '(1 (*error* too-few-args) 3)))))
  (testing "Devuelve el primer error encontrado"
    (is (= '(*error* too-many-args) (revisar-lae '((*error* too-many-args) (*error* too-few-args) 3)))))
)

(deftest actualizar-amb-test
  (testing "Actualiza el ambiente con nuevo valor"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4))))
  (testing "Actualiza el ambiente con valor existente"
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4))))
  (testing "Actualiza el ambiente vacio"
    (is (= '(b 7) (actualizar-amb '() 'b 7))))
  (testing "No actualiza ambiente cuando recibe error"
    (is (= '(a 1 b 2 c 3)  (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)))))
)

(deftest buscar-test
  (testing "Buscar una clave en el ambiente la devuelve el valor si existe"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5)))))
  (testing "Busca una clave en el ambiente devuelve error si no existe"
    (is (= '(*error* unbound-symbol f) (buscar 'f '(a 1 b 2 c 3 d 4 e 5)))))
)

(deftest fnc-append-test
  (testing "Fusionar dos listas devuelve error si se pasan menos de dos listas"
    (is (= '(*error* too-few-args) (fnc-append '((1 2))))))
  (testing "Fusionar dos listas devuelve error si se pasan mas de dos listas"
    (is (= '(*error* too-many-args) (fnc-append '( (1 2) (3) (4 5) (6 7) )))))
  (testing "Fusionar dos listas devuelve error si se pasan mas de dos listas"
    (is (= '(*error* list expected 3) (fnc-append '( (1 2) 3 )))))
  (testing "Fusionar dos listas devuelve error si se pasa algo que no es lista"
    (is (= '(*error* list expected A) (fnc-append '( (1 2) A )))))
  (testing "Fusionar dos listas devuelve error si se pasa algo que no es lista"
    (is (= '(*error* list expected B) (fnc-append '( B (1 2) )))))
  (testing "Fusionar dos listas no concatena nil y devuelve la lista inicial"
    (is (= '(1 2) (fnc-append '( (1 2) nil)))))
  (testing "Fusionar dos listas no concatena vacio y devuelve la lista inicial"
    (is (= '(1 2) (fnc-append '( () (1 2))))))
  (testing "Fusionar dos listas devuelve nil si el resultado de concatenar es vacio"
    (is (nil? (fnc-append '( nil nil)))))
  (testing "Fusionar dos listas devuelve nil si el resultado de concatenar es vacio"
    (is (nil? (fnc-append '( () ())))))
)

(deftest fnc-env-test
  (testing "Fusion de ambientes devuelve la concatenacion de sus secuencias"
    (is (= '(a 1 b 2 c 3 d 4) (fnc-env () '(a 1 b 2) '(c 3 d 4)))))
  (testing "Fusion de amientes devuelve error si el primer parametro no es vacio"
    (is (= '(*error* too-many-args) (fnc-env '(5) '(a 1 b 2) '(c 3 d 4)))))
)

(deftest fnc-equal-test
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= 't (fnc-equal '(1 1)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= 't (fnc-equal '(A a)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= 't (fnc-equal '("1" "1")))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= 't (fnc-equal '(nil NIL)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (nil? (fnc-equal '(1 2)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (nil? (fnc-equal '(A B)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (nil? (fnc-equal '("1" 1)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= '(*error* too-few-args) (fnc-equal '()))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= '(*error* too-few-args) (fnc-equal '(A)))))
  (testing "Comparar 2 elementos devuelve t si son iguales"
    (is (= '(*error* too-many-args) (fnc-equal '(A a A)))))
)

(deftest fnc-read-test
  (testing "Funcion read lanza error si recibe parametros"
    (is (= '(*error* not-implemented) (fnc-read '((1 2))))))
)

(deftest fnc-terpri-test
  (testing "Funcion terpri lanza error si recibe parametros"
    (is (= '(*error* not-implemented) (fnc-terpri '((1 2))))))
)