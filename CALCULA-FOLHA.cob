       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULA-FOLHA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Constantes
       78 PERCENTUAL-HORA-EXTRA VALUE 1,50.
       78 PERCENTUAL-COMISSAO   VALUE 0,05.
       78 PERCENTUAL-INSS       VALUE 0,11.

       *> Variaveis internas
       01 WS-VALOR-EXTRA PIC 9(7)V99.
       01 WS-TOTAL-BRUTO PIC 9(7)V99.

       *> Parametros recebidos
       LINKAGE SECTION.
       01 LK-FUNCIONARIO.
          05 LK-NOME          PIC X(40).
          05 LK-SALARIO-BASE  PIC 9(7)V99.
          05 LK-HORAS-EXTRAS  PIC 9(4).
          05 LK-VALOR-HORA    PIC 9(5)V99.
          05 LK-VENDAS        PIC 9(7)V99.

       01 LK-RESULTADO.
          05 LK-SALARIO-BRUTO   PIC 9(7)V99.
          05 LK-SALARIO-LIQUIDO PIC 9(7)V99.
          05 LK-INSS            PIC 9(7)V99.
          05 LK-COMISSAO        PIC 9(7)V99.

       PROCEDURE DIVISION USING LK-FUNCIONARIO
                                LK-RESULTADO.
           DISPLAY "   * CALCULANDO FOLHA..."

           *> 1. Calcular valor das horas extras
           MULTIPLY LK-VALOR-HORA BY PERCENTUAL-HORA-EXTRA
               GIVING WS-VALOR-EXTRA

           MULTIPLY LK-HORAS-EXTRAS BY WS-VALOR-EXTRA
               GIVING WS-VALOR-EXTRA

           *> 2. Calcular salario bruto (base + extras)
           ADD LK-SALARIO-BASE TO WS-VALOR-EXTRA
               GIVING LK-SALARIO-BRUTO

           *> 3. Calcular comissao
           MULTIPLY LK-VENDAS BY PERCENTUAL-COMISSAO
               GIVING LK-COMISSAO

           *> 4. Calcular total bruto
           ADD LK-SALARIO-BRUTO TO LK-COMISSAO
               GIVING WS-TOTAL-BRUTO

           *> 5. Calcular INSS
           MULTIPLY WS-TOTAL-BRUTO BY PERCENTUAL-INSS
               GIVING LK-INSS

           *> 6. Calcular salario liquido
           SUBTRACT LK-INSS FROM WS-TOTAL-BRUTO
               GIVING LK-SALARIO-LIQUIDO

           DISPLAY "   * CALCULO FINALIZADO."
           GOBACK.
