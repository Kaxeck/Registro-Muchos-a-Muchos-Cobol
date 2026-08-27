      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTE-EMPRESAS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPRESAS ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empresas\Empre_UM.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRES-TEMP ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empresas\Empre_Temp.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRES-ORDS ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empresas\Empre_Ords.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT EMPLEADOS ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empleados\Empl_UM.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT EMP-TEMP ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empleados\Empl_Temp.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT EMP-ORDS ASSIGN TO DISK
           "C:\Users\keibarra\Documents\Tarea 8\Empleados\Empl_Ords.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT REPORTE ASSIGN TO "REPORTE-EMPRESAS.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT LOG-ERRORES ASSIGN TO "LOG.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADOS.
       01 REG-EMPLEADOS.
           05 RFC-EMPLEADO PIC X(13).
           05 RFC-EMPRESA PIC X(12).
           05 NOMBRE PIC X(20).
           05 APATERNO PIC X(20).
           05 AMATERNO PIC X(20).

       SD EMP-TEMP.
       01 REG-EMPLEADOS-TEMP.
           05 TMP-RFC-EMPLEADO PIC X(13).
           05 TMP-RFC-EMPRESA PIC X(12).
           05 TMP-NOMBRE PIC X(20).
           05 TMP-APATERNO PIC X(20).
           05 TMP-AMATERNO PIC X(20).

       FD EMP-ORDS.
       01 REG-EMPLEADOS-ORDS.
           05 ORD-RFC-EMPLEADO PIC X(13).
           05 ORD-RFC-EMPRESA PIC X(12).
           05 ORD-NOMBRE PIC X(20).
           05 ORD-APATERNO PIC X(20).
           05 ORD-AMATERNO PIC X(20).

       FD EMPRESAS.
       01 REG-EMPRESA.
           05 EMPR-RFC-EMPRESA PIC X(12).
           05 EMPR-RFC-EMPLEADO PIC X(13).
           05 NOMB-EMPRESA PIC X(20).
           05 FECHA-ALTA PIC X(09).
           05 SALARIO PIC 9(06)V99.

       SD EMPRES-TEMP.
       01 REG-EMPRESA-TEMP.
           05 TMP-EMPR-RFC-EMPRESA PIC X(12).
           05 TMP-EMPR-RFC-EMPLEADO PIC X(13).
           05 TMP-NOMB-EMPRESA PIC X(20).
           05 TMP-FECHA-ALTA PIC X(09).
           05 TMP-SALARIO PIC 9(06)V99.

       FD EMPRES-ORDS.
       01 REG-EMPRESA-ORDS.
           05 ORD-EMPR-RFC-EMPRESA PIC X(12).
           05 ORD-EMPR-RFC-EMPLEADO PIC X(13).
           05 ORD-NOMB-EMPRESA PIC X(20).
           05 ORD-FECHA-ALTA PIC X(09).
           05 ORD-SALARIO PIC 9(06)V99.

       FD REPORTE.
       01 REG-REPORTE.
           05 REP-EMPRESA PIC X(20).
           05 FILLER PIC X VALUE SPACE.
           05 REP-RFC-EMPRESA PIC X(12).
           05 FILLER PIC X VALUE SPACE.
           05 REP-RFC-EMPLEADO PIC X(13).
           05 FILLER PIC X VALUE SPACE.
           05 REP-NOMBRE-COMPLETO PIC X(40).
           05 FILLER PIC X VALUE SPACE.
           05 REP-FECHA PIC X(09).
           05 FILLER PIC X VALUE SPACE.
           05 REP-SALARIO PIC ZZ,ZZZ,ZZ9.99.

       FD LOG-ERRORES.
       01 REG-LOG.
           05 LOG-MENSAJE PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-CONTROL.
           05 FIN-EMPLEADOS PIC X VALUE "N".
           05 FIN-EMPRESAS PIC X VALUE "N".
       01 WS-LLAVE-EMPLEADO.
           05 WS-EMP-RFC-EMPRESA PIC X(12).
           05 WS-EMP-RFC-EMPLEADO PIC X(13).
       01 WS-LLAVE-EMPRESA.
           05 WS-EMPR-RFC-EMPRESA PIC X(12).
           05 WS-EMPR-RFC-EMPLEADO PIC X(13).
       01 WS-CONTADORES.
           05 WS-EMPLEADOS-LEIDOS PIC 9(05) VALUE ZERO.
           05 WS-EMPRESAS-LEIDAS PIC 9(05) VALUE ZERO.
           05 WS-PROCESADOS PIC 9(05) VALUE ZERO.
       01 WS-CABECERA.
           05 FILLER PIC X(21) VALUE "EMPRESA".
           05 FILLER PIC X(13) VALUE "RFC EMPRESA".
           05 FILLER PIC X(14) VALUE "RFC EMPLEADO".
           05 FILLER PIC X(41) VALUE "NOMBRE COMPLETO".
           05 FILLER PIC X(12) VALUE "FECHA ALTA".
           05 FILLER PIC X(10) VALUE "SALARIO".
       01 WS-LINEA-CABECERA.
           05 FILLER PIC X(108) VALUE ALL "-".
       01 WS-LINEA-ESTADISTICA.
           05 FILLER PIC X(20) VALUE ALL "-".
       01 MASCARAS.
           05 WS-MAS-EMPLEADOS-LEIDOS PIC ZZ,ZZ9.
           05 WS-MAS-EMPRESAS-LEIDAS PIC ZZ,ZZ9.
           05 WS-MAS-PROCESADOS PIC ZZ,ZZ9.
      *AQUI ENCONTRÉ EL ERROR, LO REMOVÍ Y YA QUEDÓ FUNCIONAL
       77 WS-NOMBRE-COMPLETO PIC X(40).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIALIZAR.
            PERFORM 200-PROCESAR-ARCHIVOS.
            PERFORM 400-FINALIZAR.
            STOP RUN.

       100-INICIALIZAR.
           SORT EMP-TEMP
               ON ASCENDING KEY
                   TMP-RFC-EMPRESA
                   TMP-RFC-EMPLEADO
           USING EMPLEADOS GIVING EMP-ORDS.

           SORT EMPRES-TEMP
               ON ASCENDING KEY
                   TMP-EMPR-RFC-EMPRESA
                   TMP-EMPR-RFC-EMPLEADO
           USING EMPRESAS GIVING EMPRES-ORDS.

           OPEN INPUT EMP-ORDS
                      EMPRES-ORDS
               OUTPUT REPORTE
               LOG-ERRORES.

           PERFORM 130-CABECERA.

           PERFORM 110-LEER-EMPLEADOS.
           PERFORM 120-LEER-EMPRESAS.

       110-LEER-EMPLEADOS.
           READ EMP-ORDS
               AT END
                   MOVE "S" TO FIN-EMPLEADOS
               NOT AT END
                   ADD 1 TO WS-EMPLEADOS-LEIDOS

                   MOVE ORD-RFC-EMPRESA
                   TO WS-EMP-RFC-EMPRESA

                   MOVE ORD-RFC-EMPLEADO
                   TO WS-EMP-RFC-EMPLEADO
           END-READ.

       120-LEER-EMPRESAS.
           READ EMPRES-ORDS
               AT END
                   MOVE "S" TO FIN-EMPRESAS
               NOT AT END
                   ADD 1 TO WS-EMPRESAS-LEIDAS

                   MOVE ORD-EMPR-RFC-EMPRESA
                   TO WS-EMPR-RFC-EMPRESA

                   MOVE ORD-EMPR-RFC-EMPLEADO
                   TO WS-EMPR-RFC-EMPLEADO
           END-READ.

       130-CABECERA.
           DISPLAY WS-LINEA-CABECERA
           DISPLAY WS-CABECERA
           DISPLAY WS-LINEA-CABECERA.

       200-PROCESAR-ARCHIVOS.
           PERFORM UNTIL FIN-EMPLEADOS = "S"
                          OR FIN-EMPRESAS = "S"

               IF WS-LLAVE-EMPLEADO = WS-LLAVE-EMPRESA

                   ADD 1 TO WS-PROCESADOS

                   PERFORM 300-DISPLAY
                   PERFORM 320-ESCRIBIR-REPORTE

                   PERFORM 110-LEER-EMPLEADOS
                   PERFORM 120-LEER-EMPRESAS

                ELSE

                    IF WS-LLAVE-EMPLEADO < WS-LLAVE-EMPRESA
                        PERFORM 330-ESCRIBIR-LOG
                        PERFORM 110-LEER-EMPLEADOS

                    ELSE
                        PERFORM 120-LEER-EMPRESAS

                    END-IF

                END-IF

           END-PERFORM.

       300-DISPLAY.
           PERFORM 310-FORMAR-NOMBRE.
           DISPLAY
               ORD-NOMB-EMPRESA SPACE
               ORD-EMPR-RFC-EMPRESA SPACE
               ORD-RFC-EMPLEADO SPACE
               WS-NOMBRE-COMPLETO SPACE
               ORD-FECHA-ALTA SPACE
               ORD-SALARIO.

       310-FORMAR-NOMBRE.
           MOVE SPACES TO WS-NOMBRE-COMPLETO.

           STRING
               ORD-NOMBRE DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               ORD-APATERNO DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               ORD-AMATERNO DELIMITED BY SPACE
               INTO WS-NOMBRE-COMPLETO
           END-STRING.

       320-ESCRIBIR-REPORTE.
           MOVE ORD-NOMB-EMPRESA TO REP-EMPRESA
           MOVE ORD-EMPR-RFC-EMPRESA TO REP-RFC-EMPRESA
           MOVE ORD-RFC-EMPLEADO TO REP-RFC-EMPLEADO
           MOVE WS-NOMBRE-COMPLETO TO REP-NOMBRE-COMPLETO
           MOVE ORD-FECHA-ALTA TO REP-FECHA
           MOVE ORD-SALARIO TO REP-SALARIO

           WRITE REG-REPORTE.

       330-ESCRIBIR-LOG.
           INITIALIZE REG-LOG
           STRING "ERROR: EL EMPLEADO CON RFC " ORD-RFC-EMPLEADO
                  " NO SE ENCONTRO EN LA EMPRESA " ORD-RFC-EMPRESA
                  DELIMITED BY SIZE INTO LOG-MENSAJE
           END-STRING
           WRITE REG-LOG.

       400-FINALIZAR.
           PERFORM 410-ESTADISTICA.

           CLOSE EMP-ORDS
                 EMPRES-ORDS
                 REPORTE
                 LOG-ERRORES.

       410-ESTADISTICA.
           MOVE WS-EMPLEADOS-LEIDOS TO WS-MAS-EMPLEADOS-LEIDOS
           MOVE WS-EMPRESAS-LEIDAS TO WS-MAS-EMPRESAS-LEIDAS
           MOVE WS-PROCESADOS TO WS-MAS-PROCESADOS

           DISPLAY WS-LINEA-CABECERA
           DISPLAY WS-LINEA-ESTADISTICA.
           DISPLAY "EMPLEADOS LEIDOS: " WS-MAS-EMPLEADOS-LEIDOS
           DISPLAY "EMPRESAS LEIDAS: " WS-MAS-EMPRESAS-LEIDAS.
           DISPLAY "REGISTROS PROCESADOS: " WS-MAS-PROCESADOS
           DISPLAY WS-LINEA-ESTADISTICA.

       END PROGRAM REPORTE-EMPRESAS.
