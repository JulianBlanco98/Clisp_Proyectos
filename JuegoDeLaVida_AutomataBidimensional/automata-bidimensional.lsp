(defun transformar-matriz (matriz)
  (let ((ultima-fila (car (last matriz)))
        (primera-fila (car matriz)))
    (let ((matriz-transformada (append (list ultima-fila) matriz (list primera-fila))))
      (mapcar 'sublistas matriz-transformada))))

(defun sublistas (lista-binaria)
  (sublistas-rec lista-binaria (length lista-binaria) 0))

(defun sublistas-rec (lista-binaria longitud i)
  (if (= i longitud)
    nil
    (cons (list (nth (mod (- i 1) longitud) lista-binaria)
                (nth i lista-binaria)
                (nth (mod (+ i 1) longitud) lista-binaria))
          (sublistas-rec lista-binaria longitud (+ i 1)))))

(defun obtener-elemento (matriz-transformada fila columna)
  (nth columna (nth fila matriz-transformada)))

(defun generar-indices (indice max-indice)
  (if (= indice max-indice)
      nil
      (cons indice (generar-indices (+ indice 1) max-indice))))

(defun cuadricula (matriz-transformada fila columna)
  (append (obtener-elemento matriz-transformada (1- fila) columna)
          (obtener-elemento matriz-transformada fila columna)
          (obtener-elemento matriz-transformada (1+ fila) columna)))

(defun calcular-resultado (matriz-transformada indices-filas indices-columnas)
  (mapcar (lambda (fila)
            (mapcar (lambda (columna)
                      (cuadricula matriz-transformada fila columna))
                    indices-columnas))
          indices-filas))

(defun reglas (lista)
  (let ((numero (nth 4 lista))
        (resultado (apply '+ (append (reverse (nthcdr 5 (reverse lista)))
                                     (nthcdr 5 lista)))))
    (if (or (= resultado 0) (= resultado 1))
        0
        (if (and (= resultado 2) (= numero 1))
            1
            (if (and (= resultado 2) (= numero 0))
                0
                (if (= resultado 3)
                    1
                    (if (> resultado 3)
                        0)))))))

(defun sacar-vecinos (matriz)
  (let ((matriz-transformada (transformar-matriz matriz)))
  (let ((filas (length matriz-transformada))
        (columnas (length (first matriz-transformada))))
  (let ((indices-filas (generar-indices 1 (1- filas)))
        (indices-columnas (generar-indices 0 columnas)))
  (let ((resultado (calcular-resultado matriz-transformada indices-filas indices-columnas)))
      (mapcar (lambda (fila) (mapcar 'reglas fila)) resultado))))))        
          

(defun simular-juego-de-la-vida (estado-inicial numero-estados)
  (if (= numero-estados 0)
      'FIN
      (progn
        (print (subst '_ '0 (subst '@ '1 estado-inicial)))
        (sleep 1)
        (simular-juego-de-la-vida (sacar-vecinos estado-inicial) (1- numero-estados)))))





