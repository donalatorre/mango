# Mango

Mango es un lenguaje puramente funcional, fuertemente tipado, con datos inmutables, y con evaluación perezosa. Su sintaxis está principalmente basada en lisp, por lo que no hay ambigüedad léxica. Sin embargo, muchas de sus funcionalidades, como el tipado, los datos, y las clases están inspiradas en Haskell.

## Cómo correrlo
Prerequisitos: Tener haskell instalado (cabal, stack, ghc). Haga click [aquí](https://www.haskell.org/platform/) para saber cómo.
1. Clone o descargue este repositorio.
2. Corra `cabal install` para instalar las dependencias necesarias.
3. Cree un archivo (llamémosle `main.mgo`), ahí ponga el código en Mango que desee compilar.
4. Corra `stack run -- "main.mgo"`.

**NOTA**: El proyecto no está completo. Por ahora sólo está hecha la parte del lexer. Por lo tanto, el resultado de correr el compilador es un AST con los elementos del programa.
**UPDATE**: El proyecto ya tiene:
- Soportada una función main.
- Soportado pattern matching básico.
- Recursividad básica.
- Ejecución básica con operaciones aritméticas.

## Cómo codificar en Mango

Antes de empezar, como todo buen tutorial de un lenguaje de programación, mostramos un Hello World hecho con Mango.

```
(print "Hello World!")
```

Imprime

```
Hello World!
```

Es tan simple como hacer una llamada a la función print y pasar como argumento lo que se imprimirá. A lo largo de Mango, las llamadas siempre se realizan con el formato anterior. Se abre paréntesis, se escribe el nombre de la función a llamar, se incluyen los argumentos separados por un espacio, y se cierra paréntesis. Dicho formato fue inspirado en lisp.

Mango soporta los siguientes tipos de dato primitivos: Int, Double, String, Char, Bool. Para declarar constantes, se utiliza la función let. Nótese que es para declarar constantes, no variables. En Mango, toda la información es inmutable. Por ello, cuando se declara algo con el estatuto let, el valor de esa declaración no se puede volver a modificar o reasignar.

```
(let nombre "Adan")
(let edad 22)
(let estatura 1.70)
(let tipoDeSangre 'A')
(let soyMayor (>= edad 18))
(let bebida
  (if soyMayor "Cerveza" "Café")
)


(print "Hola! Me llamo " nombre ", tengo " edad " años")
(print "Mido " estatura ", y mi tipo de sangre es " tipoDeSangre)
(print "Si quieres, vamos a tomar " bebida ", yo la compro!")
```

Imprime

```
Hola! Me llamo Adan, tengo 22 años.
Mido 1.70, y mi tipo de sangre es A.
Si quieres, vamos a tomar Cerveza, yo la compro!
```

Nótese que la función print puede recibir múltiples argumentos, y como resultado, los imprime en consola concatenados.

Mango es un lenguaje fuertemente tipado. En el ejemplo anterior, no se especificó ningún tipo explícitamente, porque Mango los infirió. Sin embargo, es posible hacerlo, con la función ::.

```
(:: nombre String)
(:: edad Int)
(:: estatura Double)
(:: tipoDeSangre Char)
(:: soyMayor Bool)
(:: bebida String)

(let nombre "Adan")
(let edad 22)
(let estatura 1.70)
(let tipoDeSangre 'A')
(let soyMayor (>= edad 18))
(let bebida
  (if soyMayor "Cerveza" "Café")
)

(print "Hola! Me llamo " nombre ", tengo " edad " años")
(print "Mido " estatura ", y mi tipo de sangre es " tipoDeSangre)
(print "Si quieres, vamos a tomar " bebida ", yo la compro!")
```

Dicho formato de tipado se inspiró en el de Haskell.

En este caso, no es muy necesario especificar los tipos. Pero para programas más complejos, puede resultar muy útil para mantener la claridad.

Mango también soporta listas:

```
(let lista [1, 2, 3])

(print lista)
```
Imprime
```
[1,2,3]
```
Ésta es la diferencia principal con la sintaxis de lisp. Las listas se representan con brackets cuadrados.

Las listas en Mango son homogéneas. Es decir, todos los elementos de una lista deben ser del mismo tipo.

Como todo buen lenguaje funcional, las funciones en Mango son de primera clase. Por lo tanto, la declaración de funciones no es tan diferente que la declaración de constantes:

```
(let inc
  (\ [x] (+ x 1))
)

(print (inc 2))
```
Imprime
```
3
```
Se llama a la función \, después se especifica la lista de parámetros, y finalmente el resultado de evaluar la función. Para llamarla, utilizamos el mismo formato que se ha utilizado para todo lo demás, haciendo referencia al nombre inc.

El tipado de las funciones es muy especial en mango, pues se utiliza el formato de una lista, la cual representa los argumentos de la función (en el mismo orden en el que se declararon) seguidos por el tipo del valor de retorno. Como todas las funciones en Mango deben recibir por lo menos un argumento, su tipado siempre se representa con listas de dos o más elementos. Por lo tanto, no genera ambigüedad con el tipado de las listas.

```
(:: inc [Int, Int])
(let inc
  (\ [x] (+ x 1))
)

(:: lista [Int])
(let lista [1, 2, 3])
```

Las funciones en Mango pueden ser recursivas y soportan pattern matching.

```
(:: factorial [Int, Int])
(let factorial
  (\ [0] 1)
  (\ [n] (* n (factorial (- n 1))))
)
```

Mango también tiene tipos polimórficos, que funcionan muy similar a los de Haskell.

```
(:: length [[a], Int])
(let length
  (\ [[]] 0)
  (\ [(Cons _ resto)] (+ 1 (length resto)))
)
```

Es decir, la función anterior acepta listas de cualquier tipo, ya que la longitud de una lista no depende de eso.

Asimismo, hay funciones de orden superior:

```
(:: inc [Int, Int])
(let inc
  (\ [x] (+ x 1))
)

(:: lista [Int])
(let lista [1, 2, 3])

(print (map lista (inc)))
```

Imprime

```
[2,3,4]
```
La evaluación es perezosa, como en Haskell. Es decir, sólo se evalúa lo necesario.

```
(:: func [Int])
(let func
  (\ [x]
    (if (> x 5)
      (/ x 0)
      (/ x 2)
    )
  )
)

(print (func 4))
```
Imprime
```
2
```
En el programa anterior, func recibe un entero. Si éste es mayor que 5, regresa una división entre cero (error). Sin embargo, como la función se llama con el valor de 4 (no es mayor a 5), la división entre 0 no se evalúa, y no hay error.

También se puede crear tipos de dato nuevos. Se deben especificar el nombre del tipo y sus posibles valores.

```
(data Color
  Azul Rojo Verde Amarillo
)

(:: colorFav Color)
(let colorFav Azul)

(print colorFav)
```
Imprime

```
Azul
```
Los posibles valores también pueden ser constructores.

```
(data Figura
  (Circulo Double)
  (Rectangulo Double Double)
)

(:: miCirculo Figura)
(let miCirculo (Circulo 4.0))

(:: miRectangulo Figura)
(let miRectangulo (Rectangulo 3.4 5.5))
```

Asimismo, pueden haber constructores de tipos.

```
(data (Figura a)
  (Circulo a)
  (Rectangulo a a)
)

(:: circuloEntero (Figura Int))
(let circuloEntero (Circulo 4))

(:: circuloReal (Figura Double))
(let circuloReal (Circulo 5.5))
```
Asi es la sintaxis de datos personalizados en Mango. Para conocer la lógica con mayor profundidad, investigue los datos personalizados de Haskell, pues siguen las mismas ideas.

Por ultimo, tambien se soportan limitaciones a los tipos, llamadas clases.

```
(data Punto
  (Punto Double Double)
)

(instance (Num Punto)
  (let +
    (\ [(Punto x1 y1), (Punto x2 y2)]
      (Punto (+ x1 x2) (+ y1 y2))
    )
  )
  (let -
    (\ [(Punto x1 y1), (Punto x2 y2)]
      (Punto (- x1 x2) (- y1 y2))
    )
  )
)

(let a (Punto 1.0 2.0))
(let b (Punto 9.0 10.0))

(print (+ a b))
(print (- a b))
```
Imprime

```
Punto 10.0 12.0
Punto -8.0 -8.0
```
Mango soporta la clase Num, que representa Campos de álgebra abstracta. Los tipos más usuales que soportan Num son Int y Double. Pero en el programa anterior, creamos una instancia para Puntos en el plano cartesiano. NOTA: dicha instancia está incompleta, falta implemntar multiplicación y división. El objetivo del ejemplo era describir la sintaxis de las instancias en Mango.

También se pueden crear clases personalizadas:

```
(class (Eq a)
  (:: == [a, a, Bool])
)


(data Punto
  (Punto Double Double)
)

(instance (Eq Punto)
  (let == [(Punto x1 y1), (Punto x2 y2)]
    (&& (== x1 x2) (== y1 y2))
  )
)   

(let a (Punto 1.0 2.0))
(let b (Punto 9.0 10.0))

(print (== a b))
(print (== a a))
```

Imprime

```
False
True
```
La logica de clases e instancias es la misma que la de Haskell.


### Avance del Proyecto

Este proyecto tiene como objetivo desarrollar el compilador de Mango de tal forma que todas las instrucciones anteriormente mencionadas, puedan ser ejecutadas correcta y eficientemente. Hay varios milestones en el plan de desarrollo. Actualmente se han cubierto los dos primeros los cuales son el Scanner-Parser y la semántica básica. A continuación se explica brevemente en que consisten estos milestones.

El Scanner-Parser es la primera fase de cualquier compilador. El programa es capaz de leer cualquier secuencia de instrucciones permitidas en Mango. Además se reconocen los tokens y se crean las estructuras permitidas por el lenguaje. En caso de encontrar alguna inconsistencia, el compilador es capaz de identificarla e informar al programador acerca de ella. Cuando el programa es aceptado, se genera una estructura con los tokens reconocidos.

El último avance agregado al compilador, es la capacidad de crear una tabla de variables en la cual se guardan los nombres de las variables o datos declarados, junto con su aridad. Esto forma la semántica básica del lenguaje de tal forma que se reconoce cuando los tipos no son primitivos ni creados por el programador. En próximas entregas se planea modificar esta parte del proyecto a como sea necesario para optimizar la información en la tabla de variables.
