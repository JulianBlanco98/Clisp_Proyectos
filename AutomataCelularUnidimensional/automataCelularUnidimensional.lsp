(defun sublistas (lista-binaria)
  (sublistas-rec lista-binaria (length lista-binaria) 0))

(defun sublistas-rec (lista-binaria longitud i)
  (IF (= i longitud)
    nil
    (cons (list (nth (mod (- i 1) longitud) lista-binaria)
                (nth i lista-binaria)
                (nth (mod (+ i 1) longitud) lista-binaria))
          (sublistas-rec lista-binaria longitud (+ i 1)))))

(defun convertir-a-decimal (bit pos)
  (* bit (expt 2 (- 2 pos))))

(defun binario-decimal (vecino)
  (apply '+ (mapcar 'convertir-a-decimal vecino '(0 1 2))))

	
(defun encontrar-posicion-al-reves (reglas vecino)
  (let ((posicion (binario-decimal vecino)))
    (IF (< posicion 8)
        (nth (- 7 posicion) reglas)
        nil)))

(defun decimal-a-binario-recursivo-2 (numero)
  (if (= numero 0)
      '()
      (append (decimal-a-binario-recursivo-2 (floor (/ numero 2)))
              (list (mod numero 2)))))
(defun relleno-ceros (resultado)
  (append (make-sequence 'list (- 8 (length resultado)) :initial-element 0) resultado))

(defun decimal-a-binario-con-relleno (numero)
  (let ((resultado (decimal-a-binario-recursivo-2 numero)))
    (relleno-ceros resultado)))



(defun automata-celular (lista1 lista2 veces)
  (IF (not (integerp veces)) ; Ver si veces no es un número entero
      (error "El tercer argumento debe ser un número entero")
      (IF (and (listp lista2) (= (length lista2) 8)) ; Si lista2 es una lista de 8 elementos binarios
          (simular-automata lista1 lista2 veces)
          (IF (and (integerp lista2) (<= lista2 255) (>= lista2 0)) ; Si lista2 es un número entre 0 y 255
              (simular-automata lista1 (decimal-a-binario-con-relleno lista2) veces)
              (error "El segundo argumento debe ser una lista de 8 elementos binarios o un número entre 0 y 255")))))


(defun simular-automata (lista1 lista2 veces)
  (IF (= veces 0)
      (progn
        (format t " ~a~%" (subst '_ '0 (subst '@ '1 lista1)))
        lista1)
      (progn
        (format t " ~a~%" (subst '_ '0 (subst '@ '1 lista1)))
        (simular-automata (mapcar #'(lambda (sublista)
                                                (encontrar-posicion-al-reves lista2 sublista))
                                            (sublistas lista1))
                                    lista2
                                    (- veces 1)))))

