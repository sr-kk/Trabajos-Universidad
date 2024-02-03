# Descripción de los datos

Los datos propuestos para el análisis corresponden a un programa de nivelación desarrollado por una universidad pública chilena. Este programa ofrece tutorías a los alumnos que ingresaron a la universidad. Las tutorías se centran principalmente, pero no exclusivamente, en los estudiantes que ingresaron a través del acceso inclusivo (lo que es denotado por VAI en el conjunto de datos proporcionado).

Los alumnos que participan en el programa de tutorías se denominan alumnos BAP. En la base de datos, si BAP = 1, el alumno es un alumno BAP; si BAP = 0, no lo es.

El programa de tutorías está organizado por semestres. Al final del primer semestre, cada estudiante puede decidir si continúa con el programa o no. Si se liberan las cuotas, otros estudiantes pueden participar del programa de tutorías.

En la base de datos se registra el número de tutorías a las que ha asistido cada alumno. Para el Programa Oficial de Nivelación, se considera que un alumno ha sido intervenido si asiste al menos a 10 tutorías cada semestre. Menos de 10 tutorías significa que el alumno sólo ha sido intervenido.

Consideraremos una variable de respuesta: el promedio acumulado al final de cada semestre.

Además se cuenta con los puntajes PSU en Lenguaje y Matemáticas, y el percentil de las notas obtenidas al final de la enseñanza media.

La base de datos contiene las siguientes variables:

* **PTJE_RANKING**: Puntaje correspondiente al percentil de las notas obtenidas al final de la enseñanza media.

* **PTJE_LYC**: Puntaje PSU Lenguaje.

* **PTJE_MAT**: Puntaje PSU Matemáticas.

* **VAI**: Variable binaria: VAI = 1 significa que un estudiante fue admitido en la universidad por medio de un programa de acceso inclusivo. Es altamente probable que su puntaje en Lenjuaje y Matemáticas sea más bien "bajo". VAI = 0 significa que un estudiante entró a la universidad a través del sistema nacional de admisión, el cual usa los puntajes PSU de Matemáticas y Lenguaje, así como el percentil de notas de la enseñanza secundaria.

* **BAP**: Variable binaria: BAP = 1 significa que un estudiante participa del programa de tutorías durante el primer semestre; BAP = 0 si no. Durante el segundo semestre, un alumno participa en el programa de tutorías si el número de tutorías es al menos igual a 1; si falta la variable correspondiente, el alumno no participó en el programa durante el segundo semestre.

* **Tut_20161**: Número de tutorías durante el primer semestre. Si es dato faltante, significa que el alumno no participó.

* **Tut_20162**: Número de tutorías durante el segundo semestre. Si es dato faltante, significa que el alumno no participó.

* **Prom_20161**: Nota promedio al final del primer semestre.

* **Prom_20162**: Nota promedio al final del segundo semestre.


# Preguntas de evaluación de impacto

1. Sea $Z_k$ = 1 si un alumno asistió a k tutorías durante el primer semestre, y $Z_k$ = 0 si un alumno asistió a menos de k tutorías durante el primer semestre. $Z_k$ podría denominarse intensidad del tratamiento. Para los alumnos BAP, ¿en qué valor de k se observa un mayor impacto de las tutorías sobre la nota promedio al final del primer semestre?

2. Al inicio del segundo semestre los estudiantes pueden elegir continuar o no en el programa de tutoría. Evalúe cuál es la mejor decisión: continuar o no en el programa. Para esta pregunta, tenga en cuenta no sólo a los estudiantes BAP en el segundo semestre, sino también a los estudiantes VAI.