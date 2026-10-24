       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOLHA-PRINCIPAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> DADOS DO FUNCIONARIO
       01 WS-FUNCIONARIO.
          05 WS-NOME          PIC X(40).
          05 WS-SALARIO-BASE  PIC 9(7)V99.
          05 WS-HORAS-EXTRAS  PIC 9(4).
          05 WS-VALOR-HORA    PIC 9(5)V99.
          05 WS-VENDAS        PIC 9(7)V99.

       *> RESULTADOS DO CALCULO
       01 WS-RESULTADO.
          05 WS-SALARIO-BRUTO   PIC 9(7)V99.
          05 WS-SALARIO-LIQUIDO PIC 9(7)V99.
          05 WS-INSS            PIC 9(7)V99.
          05 WS-COMISSAO        PIC 9(7)V99.

       *> MASCARAS PARA EXIBICAO
       01 WS-EXIBE.
          05 WS-NOME-EDIT       PIC X(40).
          05 WS-SALARIO-EDIT    PIC Z.ZZZ.ZZ9,99.
          05 WS-BRUTO-EDIT      PIC Z.ZZZ.ZZ9,99.
          05 WS-LIQUIDO-EDIT    PIC Z.ZZZ.ZZ9,99.
          05 WS-INSS-EDIT       PIC Z.ZZZ.ZZ9,99.
          05 WS-COMISSAO-EDIT   PIC Z.ZZZ.ZZ9,99.

       *> CONTROLE
       01 WS-OPCAO  PIC X(1).
       01 WS-STATUS PIC X(1).

       PROCEDURE DIVISION.
           PERFORM CABECALHO

           PERFORM UNTIL WS-OPCAO = "N"
               PERFORM ENTRADA-DADOS
               PERFORM CALCULAR-FOLHA
               PERFORM EXIBIR-RESULTADO
               PERFORM PERGUNTAR-CONTINUAR
           END-PERFORM

           DISPLAY "========================================"
           DISPLAY "        PROGRAMA ENCERRADO"
           DISPLAY "========================================"
           STOP RUN.

       CABECALHO.
           DISPLAY "========================================"
           DISPLAY "    SISTEMA DE FOLHA DE PAGAMENTO"
           DISPLAY "    VERSAO MODULAR"
           DISPLAY "========================================"
           DISPLAY " ".
           EXIT.

       ENTRADA-DADOS.
           DISPLAY "DADOS DO FUNCIONARIO"
           DISPLAY "----------------------------------------"
           DISPLAY "NOME COMPLETO:"
           ACCEPT WS-NOME
           DISPLAY "SALARIO BASE:"
           ACCEPT WS-SALARIO-BASE
           DISPLAY "VALOR DA HORA TRABALHADA:"
           ACCEPT WS-VALOR-HORA
           DISPLAY "QUANTIDADE DE HORAS EXTRAS:"
           ACCEPT WS-HORAS-EXTRAS
           DISPLAY "TOTAL DE VENDAS NO MES:"
           ACCEPT WS-VENDAS
           DISPLAY " ".
           EXIT.

       CALCULAR-FOLHA.
           DISPLAY "PROCESSANDO FOLHA..."
           MOVE "S" TO WS-STATUS

           CALL "CALCULA-FOLHA"
               USING WS-FUNCIONARIO
                     WS-RESULTADO
               ON EXCEPTION
                   MOVE "E" TO WS-STATUS
                   DISPLAY "ERRO! MODULO DE CALCULO NAO ENCONTRADO!"
               NOT ON EXCEPTION
                   DISPLAY "CALCULO REALIZADO COM SUCESSO!"
           END-CALL

           DISPLAY " ".
           EXIT.

       EXIBIR-RESULTADO.
           IF WS-STATUS = "S"
               MOVE WS-NOME            TO WS-NOME-EDIT
               MOVE WS-SALARIO-BASE    TO WS-SALARIO-EDIT
               MOVE WS-SALARIO-BRUTO   TO WS-BRUTO-EDIT
               MOVE WS-SALARIO-LIQUIDO TO WS-LIQUIDO-EDIT
               MOVE WS-INSS            TO WS-INSS-EDIT
               MOVE WS-COMISSAO        TO WS-COMISSAO-EDIT

               DISPLAY "========================================"
               DISPLAY "         FOLHA DE PAGAMENTO"
               DISPLAY "========================================"
               DISPLAY " "
               DISPLAY "FUNCIONARIO: " WS-NOME-EDIT
               DISPLAY " "
               DISPLAY "SALARIO BASE : R$ " WS-SALARIO-EDIT
               DISPLAY "COMISSAO     : R$ " WS-COMISSAO-EDIT
               DISPLAY "----------------------------------------"
               DISPLAY "SALARIO BRUTO: R$ " WS-BRUTO-EDIT
               DISPLAY " "
               DISPLAY "DESCONTOS"
               DISPLAY "INSS         : R$ " WS-INSS-EDIT
               DISPLAY "----------------------------------------"
               DISPLAY "SALARIO LIQUIDO: R$ " WS-LIQUIDO-EDIT
               DISPLAY " "
               DISPLAY "========================================"
           ELSE
               DISPLAY "ERRO AO CALCULAR FOLHA!"
           END-IF.
           EXIT.

       PERGUNTAR-CONTINUAR.
           DISPLAY " "
           DISPLAY "DESEJA CALCULAR OUTRO FUNCIONARIO? (S/N)"
           ACCEPT WS-OPCAO
           DISPLAY " ".
           EXIT.
