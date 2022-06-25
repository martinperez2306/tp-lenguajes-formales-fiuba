(ns tlc-lisp.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  

(require '[clojure.string :refer [blank? ends-with? lower-case]] '[clojure.java.io :refer [reader]])

(defn spy
  ([x] (do (prn x) (prn) x))
  ([msg x] (do (print msg) (print ": ") (prn x) (prn) x))
)

; Funciones principales
(declare repl)
(declare evaluar)
(declare aplicar)

; Funciones secundarias de evaluar
(declare evaluar-de)
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond)
(declare evaluar-eval)
(declare evaluar-exit)
(declare evaluar-load)
(declare evaluar-setq)
(declare evaluar-quote)
(declare evaluar-lambda)
(declare evaluar-escalar)

; Funciones secundarias de aplicar
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva)

; Funciones primitivas
(declare fnc-ge)
(declare fnc-gt)
(declare fnc-lt)
(declare fnc-add)
(declare fnc-env)
(declare fnc-not)
(declare fnc-sub)
(declare fnc-cons)
(declare fnc-list)
(declare fnc-null)
(declare fnc-read)
(declare fnc-rest)
(declare fnc-equal)
(declare fnc-first)
(declare fnc-listp)
(declare fnc-prin3)
(declare fnc-append)
(declare fnc-length)
(declare fnc-terpri)
(declare fnc-reverse)

; Funciones auxiliares
(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)
(declare cargar-arch)
(declare revisar-fnc)
(declare revisar-lae)
(declare actualizar-amb)
(declare controlar-aridad)
(declare aplicar-lambda-simple)
(declare aplicar-lambda-multiple)
(declare evaluar-clausulas-en-cond)
(declare evaluar-secuencia-en-cond)

;Funciones auxilares declaradas por el alumno
(declare estandarizar)
(declare secuencias-iguales?)


; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua. El resultado es una lista con un valor y un ambiente. 
; Si la 2da. posicion del resultado es nil, devuelve true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
  "Inicia el REPL de TLC-LISP."
  ([]
   (println "Interprete de TLC-LISP en Clojure")
   (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2022")
   (println "Inspirado en:")
   (println "  TLC-LISP Version 1.51 for the IBM Personal Computer")
   (println "  Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
   (repl '(add add append append cond cond cons cons de de env env equal equal
               eval eval exit exit first first ge ge gt gt if if lambda lambda
               length length list list listp listp load load lt lt nil nil
               not not null null or or prin3 prin3 quote quote read read
               rest rest reverse reverse setq setq sub sub t t terpri terpri
               + add - sub)))
  ([amb]
   (print ">>> ") (flush)
   (try
     (let [res (evaluar (read) amb nil)]  ; READ, EVAL
       (if (nil? (second res))
           true
           (do (imprimir (first res))     ; PRINT
               (repl (second res)))))     ; LOOP
   (catch Exception e
     (println) (print "*error* ")
     (println (get (Throwable->map e) :cause))
     (repl amb)))))


(defn evaluar
  "Evalua una expresion 'expre' en los ambientes global y local. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb-global amb-local]
  (if (or (igual? expre nil)
          (and (seq? expre)
               (or (empty? expre) (error? expre)))) ; si 'expre' es nil, () o error, devolverla intacta
      (list expre amb-global)                       ; de lo contrario, evaluarla
      (cond
        (not (seq? expre))             (evaluar-escalar expre amb-global amb-local)

        (igual? (first expre) 'cond)   (evaluar-cond expre amb-global amb-local)
        (igual? (first expre) 'de)     (evaluar-de expre amb-global)

         ;
         ;
         ;
         ; Si la expresion no es la aplicacion de una funcion (es una forma especial, una macro...) debe ser evaluada aqui
         ; por una funcion de Clojure especifica debido a que puede ser necesario evitar la evaluacion de los argumentos
         ;
         ;
         ;

        :else (let [res-eval-1 (evaluar (first expre) amb-global amb-local),
				                res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x) amb-local)] (cons (second res-eval-3) (concat (next x) (list (first res-eval-3)))))) (cons (list (second res-eval-1)) (next expre)))]
				               (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2) amb-local)))))


; Evalua una macro COND. Siempre devuelve una lista con un resultado y un ambiente.
(defn evaluar-cond [expre amb-global amb-local]
  "Evalua una forma 'cond' en TLC-LISP."
   (evaluar-clausulas-en-cond (next expre) amb-global amb-local))


(defn evaluar-clausulas-en-cond [expre amb-global amb-local]
  "Une 'evaluar-cond' con 'evaluar-secuencia-en-cond'."
  (if (nil? expre)
      (list nil amb-global)
     	(let [res-eval (evaluar (ffirst expre) amb-global amb-local)]
           (cond
             (error? (first res-eval)) res-eval
             (not (igual? (first res-eval) nil)) (evaluar-secuencia-en-cond (nfirst expre) (second res-eval) amb-local)
	            :else (recur (next expre) (second res-eval) amb-local)))))


; Evalua (con evaluar) secuencialmente las sublistas de una lista y devuelve el valor de la ultima evaluacion.
; Si alguna evaluacion devuelve un error, sera la ultima que se evalue. 
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
	(if (nil? (next lis))
	    (evaluar (first lis) amb-global amb-local)
	    (let [res-eval (evaluar (first lis) amb-global amb-local)]
	         (if (error? (first res-eval))
   		         res-eval
  	           (recur (next lis) (second res-eval) amb-local)))))


(defn evaluar-eval
  "Evalua una forma 'eval' en TLC-LISP."
  [expre amb-global amb-local]
  (let [ari (controlar-aridad (next expre) 1)]
		     (cond
		       (seq? ari) ari
         (and (seq? (second expre)) (igual? (first (second expre)) 'quote)) (evaluar (second (second expre)) amb-global amb-local)
         :else (evaluar (second expre) amb-global amb-local))))


(defn evaluar-exit
  "Sale del interprete de TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list nil nil)
    :else (list (list '*error* 'too-many-args) amb-global)))


(defn evaluar-lambda
  "Evalua una forma 'lambda' en TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
    (and (not (igual? (second expre) nil)) (not (seq? (second expre)))) 
      (list (list '*error* 'list 'expected (second expre)) amb-global)
    :else (list expre amb-global)))


(defn evaluar-load
  "Evalua una forma 'load' en TLC-LISP. Carga en el ambiente un archivo 'expre' con código en TLC-LISP."
  [expre amb-global amb-local]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
				(> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
			 :else (list \space (cargar-arch amb-global amb-local (second expre)))))


(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (error? nomb)
	         (do (imprimir nomb) amb-global) 
          (let [nm (clojure.string/lower-case (str nomb)),
                nom (if (and (> (count nm) 4) (clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
                ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                           (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
							                                                      (cargar-arch (second res) nil in res))
	                                                       (catch Exception e (imprimir nil) amb-global))))
			  	              (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
  		           ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (second res) nil in res))
    (catch Exception e (imprimir (first res)) amb-global)))
)


(defn evaluar-quote
  "Evalua una forma 'quote' de TLC-LISP."
  [expre amb-global _]
  (if (igual? (second expre) nil)
    (list nil amb-global)
    (list (second expre) amb-global)))


(defn aplicar
  "Aplica a la lista de argumentos 'lae' la función 'fnc' en los ambientes dados."
  ([fnc lae amb-global amb-local]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb-global amb-local))
  ([resu1 resu2 fnc lae amb-global amb-local]
   (cond
     (error? resu1) (list resu1 amb-global)
     (error? resu2) (list resu2 amb-global)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb-global amb-local) amb-global)
     :else (aplicar-lambda fnc lae amb-global amb-local))))


(defn aplicar-lambda
  "Aplica la forma lambda 'fnc' a la lista de argumentos 'lae'."
  [fnc lae amb-global amb-local]
  (cond
    (< (count lae) (count (second fnc))) (list '(*error* too-few-args) amb-global)
    (> (count lae) (count (second fnc))) (list '(*error* too-many-args) amb-global)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb-global amb-local)
    :else (aplicar-lambda-multiple fnc lae amb-global amb-local)))


(defn aplicar-lambda-simple
  "Evalua una forma lambda 'fnc' con un cuerpo simple."
  [fnc lae amb-global amb-local]
  (evaluar (first (nnext fnc)) amb-global (concat (reduce concat (map list (second fnc) lae)) amb-local)))


(defn aplicar-lambda-multiple
  "Evalua una forma lambda 'fnc' cuyo cuerpo contiene varias expresiones."
  [fnc lae amb-global amb-local]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb-global amb-local))  ; Nuevo ambiente global
           amb-local))


(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una 'lae' (lista de argumentos evaluados)."
  [fnc lae amb-global amb-local]
  (cond
    (igual? fnc 'add)     (fnc-add lae)

    ; Las funciones primitivas reciben argumentos y retornan un valor (son puras)

    :else (list '*error* 'non-applicable-type fnc)))


(defn fnc-cons
  "Devuelve la inserción de un elem en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad lae 2)]
			 (cond
			   (seq? ari) ari
		    (or (seq? (second lae)) (igual? (second lae) nil)) (cons (first lae) (second lae))
			   :else (list '*error* 'not-implemented))))


(defn fnc-first
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (ffirst lae))))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (or (seq? (first lae)) (igual? (first lae) nil)) (count (first lae))
      :else (list '*error* 'arg-wrong-type (first lae)))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1) nil lae))


(defn fnc-listp
  "Devuelve 't' si un elemento es una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (seq? (first lae)) 't
      :else nil)))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
			   (igual? (first lae) nil) 't
					 :else nil)))


(defn fnc-null
  "Devuelve 't' si un elemento es 'nil' en TLC-Lisp."
  [lae]
  (fnc-not lae))


(defn fnc-prin3
  "Imprime un elemento y lo devuelve."
  [lae]
  (cond
    (< (count lae) 1) (list '*error* 'too-few-args)
				(> (count lae) 1) (list '*error* 'not-implemented)
				(not (seq? (first lae))) (do (print (first lae)) (flush) (first lae))
				:else (do (print (map #(if (igual? % nil) nil %) (first lae))) (flush) (first lae))))


(defn fnc-rest
  "Devuelve una lista sin su 1ra. posición."
  [lae]
  (let [ari (controlar-aridad lae 1)]
				(cond
				  (seq? ari) ari
						(igual? (first lae) nil) nil
						(not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
						:else (nfirst lae))))


(defn imprimir
   "Imprime, con un salto de linea al final, lo recibido devolviendo 
    el mismo valor. Tambien muestra los errores."
   ([elem]
	    (cond
	      (not (seq? elem)) (if (igual? elem \space)
	                            (do (flush) elem)
	                            (do (prn (if (igual? elem nil) nil elem)) (flush) elem))
       (error? elem) (imprimir elem elem)
       :else (do (prn (map #(if (igual? % nil) nil %) elem)) (flush) elem)))
   ([lis orig]
      (if (nil? lis)
	         (do (prn) (flush) orig)
		        (do (pr (first lis)) (print " ") (imprimir (next lis) orig)))))


; FUNCIONES QUE DEBEN SER IMPLEMENTADAS PARA COMPLETAR EL INTERPRETE DE TLC-LISP (ADEMAS DE COMPLETAR 'EVALUAR' Y 'APLICAR-FUNCION-PRIMITIVA'):

(defn estandarizar [expresion]
  "Evalua una expresion y devuelve en funcion del tipo de dato"
  (cond ;En caso de ser nil, 'nil o 'NIL devuelve nil. En caso de ser un string lo devuelve en minuscula. En otro caso lo devuelve intacto.
    (or (nil? expresion) (= "nil" (lower-case expresion))) nil
    (symbol? expresion) (symbol (lower-case expresion))
    :else expresion
  )
)

; user=> (controlar-aridad '(a b c) 3)
; 3
; user=> (controlar-aridad '(a b c) 2)
; (*error* too-many-args)
; user=> (controlar-aridad '(a b c) 4)
; (*error* too-few-args)
(defn controlar-aridad [sec arity]
  "Si la longitud de una lista dada es la esperada, devuelve esa longitud.
   Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
  (cond 
    (= (count sec) arity) arity
    (> (count sec) arity) (list '*error* '"too-many-args")
    (< (count sec) arity) (list '*error* '"too-few-args")
  )
)

; user=> (igual? 1 1)
; true
; user=> (igual? 1 2)
; false
; user=> (igual? 'a 'a)
; true
; user=> (igual? 'A 'A)
; true
; user=> (igual? 'a 'A)
; true
; user=> (igual? 'A 'a)
; true
; user=> (igual? 'a 'b)
; false
; user=> (igual? '(a b c) '(A B C))
; true
; user=> (igual? '(a b c) '(A B D))
; false
; user=> (igual? nil nil)
; true
; user=> (igual? nil 'NIL)
; true
; user=> (igual? 'NIL nil)
; true
; user=> (igual? 'NIL 'NIL)
; true
; user=> (igual? nil ())
; true
; user=> (igual? 'NIL ())
; true
; user=> (igual? () ())
; true
; user=> (igual? () '(nil))
; false
; user=> (igual? "a" "a")
; true
; user=> (igual? "a" "A")
; false
; user=> (igual? 'a "a")
; false
; user=> (igual? 'a "A")
; false
(defn igual? [e1 e2]
  "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
  (if (seq? e1)
      (and ; Si e1 es una secuencia se evalua si el segundo elemento es una secuencia tambien y si contienen los mismos elementos;
        (seq? e2) 
        (secuencias-iguales? e1 e2)
      ) 
      (and ; En otro caso, se evalua si el segundo elemento no es una secuencia y si es el mismo elemento.
        (not (seq? e2))
        (= (estandarizar e1) (estandarizar e2))
      ) 
  )
)

(defn secuencias-iguales? [sec1 sec2]
  "Recibe dos secuencias y verifica la igualdad de sus elementos"
  (and 
    (= (count sec1) (count sec2))
    (every? identity (map-indexed (fn [idx x] (igual? x (nth sec2 idx))) sec1)) 
  )
)