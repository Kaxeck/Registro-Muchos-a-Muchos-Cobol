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
           "C:\Users\keibarra\Documents\Tarea 8\Empresas\Empre_IND.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EMPRESA-KEY.

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
           05 EMPRESA-KEY.
               10 EMPR-RFC-EMPRESA PIC X(12).
               10 EMPR-RFC-EMPLEADO PIC X(13).
           05 NOMB-EMPRESA PIC X(20).
           05 FECHA-ALTA PIC X(09).
           05 SALARIO PIC 9(06)V99.

       FD REPORTE.
       01  REG-REPORTE-OUT        PIC X(115).


       FD LOG-ERRORES.
       01 REG-LOG.
           05 LOG-MENSAJE PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-CONTROL.
           05 FIN-EMPLEADOS PIC X VALUE "N".
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

       77 WS-NOMBRE-COMPLETO PIC X(40).

       01  WS-DETALLE-REPORTE.
           05 REP-EMPRESA          PIC X(20).
           05 FILLER               PIC X VALUE SPACE.
           05 REP-RFC-EMPRESA      PIC X(12).
           05 FILLER               PIC X VALUE SPACE.
           05 REP-RFC-EMPLEADO     PIC X(13).
           05 FILLER               PIC X VALUE SPACE.
           05 REP-NOMBRE-COMPLETO  PIC X(40).
           05 FILLER               PIC X VALUE SPACE.
           05 REP-FECHA            PIC X(09).
           05 FILLER               PIC X VALUE SPACE.
           05 REP-SALARIO          PIC ZZ,ZZZ,ZZ9.99.

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

           OPEN INPUT EMP-ORDS
                      EMPRES-ORDS
               OUTPUT REPORTE
               LOG-ERRORES.
                      EMPRESAS
               OUTPUT REPORTE.

           PERFORM 130-CABECERA.

           PERFORM 110-LEER-EMPLEADOS.

       110-LEER-EMPLEADOS.
           READ EMP-ORDS
               AT END
                   MOVE "S" TO FIN-EMPLEADOS
               NOT AT END
                   ADD 1 TO WS-EMPLEADOS-LEIDOS
           END-READ.

       130-CABECERA.
           WRITE REG-REPORTE-OUT FROM WS-LINEA-CABECERA.
           WRITE REG-REPORTE-OUT FROM WS-CABECERA.
           WRITE REG-REPORTE-OUT FROM WS-LINEA-CABECERA.

       200-PROCESAR-ARCHIVOS.
           PERFORM UNTIL FIN-EMPLEADOS = "S"

               MOVE ORD-RFC-EMPRESA
                   TO EMPR-RFC-EMPRESA

               MOVE ORD-RFC-EMPLEADO
                   TO EMPR-RFC-EMPLEADO

                    IF WS-LLAVE-EMPLEADO < WS-LLAVE-EMPRESA
                        PERFORM 330-ESCRIBIR-LOG
                        PERFORM 110-LEER-EMPLEADOS
               READ EMPRESAS
                   KEY IS EMPRESA-KEY
                   INVALID KEY
                       CONTINUE
                   NOT INVALID KEY

                        ADD 1 TO WS-EMPRESAS-LEIDAS
                        ADD 1 TO WS-PROCESADOS

                        PERFORM 300-DISPLAY
                        PERFORM 320-ESCRIBIR-REPORTE
               END-READ

               PERFORM 110-LEER-EMPLEADOS

           END-PERFORM.

       300-DISPLAY.
           PERFORM 310-FORMAR-NOMBRE.
           DISPLAY
               NOMB-EMPRESA SPACE
               EMPR-RFC-EMPRESA SPACE
               ORD-RFC-EMPLEADO SPACE
               WS-NOMBRE-COMPLETO SPACE
               FECHA-ALTA SPACE
               SALARIO.

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

      *Aqui aplique un cambio :)
        320-ESCRIBIR-REPORTE.
           MOVE NOMB-EMPRESA       TO REP-EMPRESA
           MOVE EMPR-RFC-EMPRESA   TO REP-RFC-EMPRESA
           MOVE ORD-RFC-EMPLEADO   TO REP-RFC-EMPLEADO
           MOVE WS-NOMBRE-COMPLETO TO REP-NOMBRE-COMPLETO
           MOVE FECHA-ALTA         TO REP-FECHA
           MOVE SALARIO            TO REP-SALARIO

           WRITE REG-REPORTE-OUT FROM WS-DETALLE-REPORTE.

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
                 EMPRESAS
                 REPORTE.

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
