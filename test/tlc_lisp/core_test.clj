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

(deftest controlar-aridad-minima-test
  (testing "Controlar aridad minim devuelve error cuando la cantidad de parametros es menor a la esperada"
    (is (= (controlar-aridad-minima '(a b c) 4) '(*error* too-few-args))))
  (testing "Controlar aridad devuelve aridad minima cuando la cantidad de parametros es mayor a la esperada"
    (is (= (controlar-aridad-minima '(a b c) 2) 2)))
  (testing "Controlar aridad devuelve aridad cuando la cantidad de parametros es igual a la esperada"
    (is (= (controlar-aridad-minima '(a b c) 3) 3)))
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

(deftest fnc-add-test
  (testing "Funcion add lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-add '()))))
  (testing "Funcion add lanza error si no recibe 1 parametro"
    (is (= '(*error* too-few-args) (fnc-add '(1)))))
  (testing "Funcion add suma parametros"
    (is (= 12 (fnc-add '(4 4 4)))))
  (testing "Funcion add lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-add '(1 2 3 A)))))
)

(deftest fnc-sub-test
  (testing "Funcion sub lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-sub '()))))
  (testing "Funcion sub invierte signo si no recibe 1 parametro"
    (is (= -1 (fnc-sub '(1)))))
  (testing "Funcion sub resta parametros"
    (is (= -4 (fnc-sub '(1 2 3)))))
  (testing "Funcion sub lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-sub '(1 2 3 A)))))
)

(deftest fnc-lt-test
  (testing "Funcion lt lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-lt '()))))
  (testing "Funcion lt lanza error si no recibe 1 parametro"
    (is (= '(*error* too-few-args) (fnc-lt '(1)))))
  (testing "Funcion lt devuelve nil si el primer parametro es mayor al segundo"
    (is (nil? (fnc-lt '(2 1)))))
  (testing "Funcion lt devuelve t si el primer parametro es menor al segundo"
    (is (= 't (fnc-lt '(1 2)))))
  (testing "Funcion lt lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-lt '(1 A)))))
  (testing "Funcion lt lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-lt '(A 1)))))
  (testing "Funcion lt lanza error si tiene mas de 2"
    (is (= '(*error* too-many-args) (fnc-lt '(1 2 3)))))
)

(deftest fnc-gt-test
  (testing "Funcion gt lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-gt '()))))
  (testing "Funcion gt lanza error si no recibe 1 parametro"
    (is (= '(*error* too-few-args) (fnc-gt '(1)))))
  (testing "Funcion gt devuelve nil si el primer parametro es menor al segundo"
    (is (nil? (fnc-gt '(1 2)))))
  (testing "Funcion gt devuelve t si el primer parametro es mayor al segundo"
    (is (= 't (fnc-gt '(2 1)))))
  (testing "Funcion gt lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-gt '(1 A)))))
  (testing "Funcion gt lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-gt '(A 1)))))
  (testing "Funcion gt lanza error si tiene mas de 2"
    (is (= '(*error* too-many-args) (fnc-gt '(1 2 3)))))
)

(deftest fnc-ge-test
  (testing "Funcion ge lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-ge '()))))
  (testing "Funcion ge lanza error si no recibe 1 parametro"
    (is (= '(*error* too-few-args) (fnc-ge '(1)))))
  (testing "Funcion ge devuelve nil si el primer parametro es menor al segundo"
    (is (nil? (fnc-ge '(1 2)))))
  (testing "Funcion ge devuelve t si el primer parametro es mayor al segundo"
    (is (= 't (fnc-ge '(2 1)))))
  (testing "Funcion ge devuelve t si el primer parametro es igual al segundo"
    (is (= 't (fnc-ge '(1 1)))))
  (testing "Funcion ge lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-ge '(1 A)))))
  (testing "Funcion ge lanza error si 1 parametro no es numero"
    (is (= '(*error* number-expected A) (fnc-ge '(A 1)))))
  (testing "Funcion ge lanza error si tiene mas de 2"
    (is (= '(*error* too-many-args) (fnc-ge '(1 2 3)))))
)

(deftest fnc-reverse-test
  (testing "Funcion reverse lanza error si no recibe parametros"
    (is (= '(*error* too-few-args) (fnc-reverse '()))))
  (testing "Funcion reverse devuelve error si el parametro no es una lista"
    (is (= '(*error* list expected A) (fnc-reverse '(A)))))
  (testing "Funcion reverse devuelve lista al reves"
    (is (= '(1) (fnc-reverse '((1))))))
  (testing "Funcion reverse devuelve lista al reves"
    (is (= '(3 2 1) (fnc-reverse '((1 2 3))))))
  (testing "Funcion reverse lanza error si tiene mas de 1 parametro"
    (is (= '(*error* too-many-args) (fnc-reverse '((1 2 3) (4))))))
)

(deftest evaluar-escalar-test
  (testing "Funcion evaluar escalar devuelve escalar y ambiente global cuando escalar es numero"
    (is (= '(32 (v 1 w 3 x 6)) (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve escalar y ambiente global cuando escalar es string"
    (is (= '("chau" (v 1 w 3 x 6)) (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve valor local y ambiente global cuando escalar es evaluado y existe en local"
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve valor local y ambiente global cuando escalar es evaluado y existe en local (case insensitive)"
    (is (= '("hola" (v 1 w 3 x 6)) (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve valor global y ambiente global cuando escalar es evaluado y existe en global"
    (is (= '(3 (v 1 w 3 x 6)) (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve valor local y ambiente global cuando escalar es evaluado y existe en local y global"
    (is (= '(5 (v 1 w 3 x 6)) (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
  (testing "Funcion evaluar escalar devuelve error y ambiente global cuando escalar es evaluado y no existe ni global ni local"
    (is (= '((*error* unbound-symbol n) (v 1 w 3 x 6)) (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola")))))
)

(deftest evaluar-de-test
  (testing "Funcion evaluar de devuelve una lista con el resultado y un ambiente actualizado con la definicion"
    (is (= '(f (x 1 f (lambda (x)))) (evaluar-de '(de f (x)) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el resultado y un ambiente actualizado con la definicion"
    (is (= '(f (x 1 f (lambda (x) 2))) (evaluar-de '(de f (x) 2) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el resultado y un ambiente actualizado con la definicion"
    (is (= '(f (x 1 f (lambda (x) (+ x 1)))) (evaluar-de '(de f (x) (+ x 1)) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el resultado y un ambiente actualizado con la definicion"
    (is (= '(f (x 1 f (lambda (x y) (+ x y)))) (evaluar-de '(de f (x y) (+ x y)) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el resultado y un ambiente actualizado con la definicion"
    (is (= '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y))) (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de f) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected 2) (x 1)) (evaluar-de '(de f 2 3) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected nil) (x 1)) (evaluar-de '(de (f)) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* list expected x) (x 1)) (evaluar-de '(de 2 x) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* symbol expected 2) (x 1)) (evaluar-de '(de 2 (x)) '(x 1)))))
  (testing "Funcion evaluar de devuelve una lista con el error"
    (is (= '((*error* cannot-set nil) (x 1)) (evaluar-de '(de nil (x) 2) '(x 1)))))
)

(deftest evaluar-if-test
  (testing "Evaluar if"
    (is (= (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(3 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6))))
  )
  (testing "Evaluar if"
    (is (= (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8))))
  )
)

(deftest evaluar-or-test
  (testing "Evaluar or"
    (is (= (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(5 (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '((*error* unbound-symbol r) (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t w 5 x 4 b 8))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
  )
  (testing "Evaluar or"
    (is (= (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
  )
)

(deftest testing-evaluar-setq
  (testing "cuando ejecuto la funcion, el ambiente debería cambiar de la manera esperada"
    (is (= (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 4 m 7))))
    (is (= (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 4 m 7))))
    (is (= (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 7))))
    (is (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(2 (nil nil t t + add w 5 x 2))))
    (is (= (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* symbol expected 7) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t + add w 5 x 7 m 8))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(14 (nil nil t t + add w 5 x 7 m 14))))
    (is (= (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 7))))
    (is (= (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(9 (nil nil t t + add w 5 x 7 y 8 z 9))))
  )
)