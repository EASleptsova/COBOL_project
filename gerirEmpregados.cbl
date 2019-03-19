      ******************************************************************
      * Author: Ekaterina Sleptsova
      * Date: 11.02.2019
      * Purpose: Projeto final. Gerir os empregados.
      * Tectonics: cobc
      * 
      ****************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Empregados.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT OPTIONAL ficheiro ASSIGN TO "empregados.txt"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS idEmp
               FILE STATUS IS FILESTATUS.
               
               SELECT SORTEDFILE ASSIGN TO DISK
               ACCESS MODE IS SEQUENTIAL.
       
               SELECT TEMPSORT ASSIGN TO DISK
               ACCESS MODE IS SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       FD ficheiro.
       01 RECORD-EMPR.
           88 ENDOFFILE VALUE HIGH-VALUES.
           02 identity PICTURE 9(3).
           02 nome PICTURE x(15).
           02 salario PICTURE 9(4).
           02 genero PICTURE X.
           02 departamento PICTURE 9.
           02 dataDeEntrada PICTURE 9(6).
           
       FD SORTEDFILE
       LABEL RECORDS ARE STANDARD.
       01 RECORD-EMPR-sf.
           88 ENDOFFILE-sf VALUE HIGH-VALUES.
           02 id-sf PICTURE 9(3).
           02 nome-sf PICTURE x(15).
           02 salario-sf PICTURE 9(4).
           02 genero-sf PICTURE X.
           02 departamento-sf PICTURE 9.
           02 dataDeEntrada-sf PICTURE 9(6).
           
       SD TEMPSORT.
           01 RECORD-EMPR-ts.
           88 ENDOFFILE-ts VALUE HIGH-VALUES.
           02 identity-ts PICTURE 9(3).
           02 nome-ts PICTURE x(15).
           02 salario-ts PICTURE 9(4).
           02 genero-ts PICTURE X.
           02 departamento-ts PICTURE 9.
           02 dataDeEntrada-ts PICTURE 9(6).
       
 
       WORKING-STORAGE SECTION.
           01 SEM-VALOR PICTURE X.
           01 FILESTATUS PICTURE XX.
               88 OPERATIONSUCCESSFULL VALUE "00".
               88 RECORDEXISTS VALUE "22".
               88 NORECORDEXISTS VALUE "23".
           01 idEmp PICTURE 9(3). 
           01 nomeEm PICTURE X(15).     
           01 salarioEm PICTURE 9(4).
           01 generoEm PICTURE X.
           01 dataEntrada PICTURE 9(6).
           01 departamentoEm PICTURE 9 value zero.
           01 CONTADOR-EMPREGADOS PICTURE 9(3) VALUE ZEROS.                            
           01 RESPOSTA PICTURE 9. 
           01 erro picture 9 value zero. 
           01 CONDICAO PICTURE 9 VALUE ZERO.
           01 MIN PICTURE 9(4).
           01 MAX PICTURE 9(4).
           01 MEDIO PICTURE 9(4)V9.
           01 MASCARA PICTURE ZZZ9.9.
           01 TOTAL-SALARIOS PICTURE 9(7).
           01 data-invalida PICTURE 9 VALUE ZEROS.      
          
       PROCEDURE DIVISION.
       
       declaratives.
       SupFile section.
           use after error procedure on FICHEIRO.
           display "Erro! Ficheiro inexistente!".
           compute erro = 1.
           display space.
       end declaratives.  
                
       INICIO-PROGRAMA.
       OPEN I-O FICHEIRO.
       PERFORM OBTEM-RESPOSTA UNTIL RESPOSTA = 0. 
       CLOSE FICHEIRO.     
       DISPLAY "OBRIGADO!"
       ACCEPT SEM-VALOR.
       GOBACK. 
       FIM-DO-PROGRAMA. 
       STOP RUN.
       
       OBTEM-RESPOSTA.
           DISPLAY "<1> Inserir um empregado".
           DISPLAY "<2> Apagar um empregado".
           DISPLAY "<3> Atualizar um empregado".
           DISPLAY "<4> Mostrar informacao de um empregado especifico".
           DISPLAY "<5> Mostrar informacao dos todos empregados".
           DISPLAY "<6> Mostrar estatistica dos todos salarios (MAX/MIN/MEDIO). ".
           DISPLAY "<7> Mostrar um departamento especifico.".
           DISPLAY "<0> Sair".
           DISPLAY SPACE.
           DISPLAY "Insira a sua resposta".
           ACCEPT RESPOSTA.
           PERFORM PROCESSA-ESCOLHA.
           
       PROCESSA-ESCOLHA.
           EVALUATE RESPOSTA
               WHEN 1
               PERFORM ENTRADA-EMPREGADO               
               WHEN 2
               PERFORM APAGAR-EMPREGADO
               WHEN 3
               PERFORM ATUALIZAR-EMPREGADO
               WHEN 4
               PERFORM LER-EMPREGADO
               WHEN 5 
               PERFORM MOSTRA
               WHEN 6
               PERFORM MOSTRA-ESTATISTICA-SALARIOS
               WHEN 7
               PERFORM MOSTRA-DEPARTAMENTO
               WHEN 0
               DISPLAY "FIM DO PROGRAMA!".
   
       ENTRADA-EMPREGADO.
           compute data-invalida = 0.
           DISPLAY "Insira o numero de identificacao (3 DIGITOS).".
           ACCEPT idEmp.
               IF idEmp <= 0 or idEmp > 999
                   DISPLAY "O numero de identificacao invalido."
                   COMPUTE data-invalida = 1
               END-IF.  
               
           IF data-invalida = 0      
               DISPLAY "Insira o nome do empregado (15 CARACTERES)."
               ACCEPT nome
           END-IF.   
            
           IF data-invalida = 0     
               DISPLAY "Insira o genero do empregado (letra maiuscula M/F)." 
               ACCEPT genero
                   IF NOT ( genero = 'F' OR genero = 'M')
                       DISPLAY "O genero invalido."
                       COMPUTE data-invalida = 1
                   END-IF
           END-IF. 
           
           IF data-invalida = 0 
               DISPLAY "Insira o salario do empregado (4 DIGITOS, numero inteiro)."
               ACCEPT salario
                  IF salario <= 0 or salario > 9999
                      DISPLAY "O salario invalido."
                      COMPUTE data-invalida = 1
                  END-IF    
           END-IF.
           
           IF data-invalida = 0 
               DISPLAY "Insira o numero de departamento:" 
               DISPLAY "1- Marketing, 2- P&I, 3-Producao."
               ACCEPT departamento
                   IF departamento > 3 OR departamento < 0
                    DISPLAY "O departamento invalido."
                      COMPUTE data-invalida = 1
                   END-IF    
           END-IF.
            
           IF data-invalida = 0 
               DISPLAY "Insira a data de entrada(6 DIGITOS DDMMYY)."
               ACCEPT dataDeEntrada 
                   IF dataDeEntrada  <= 0 or dataDeEntrada > 999999
                   DISPLAY "A data de entrada invalida."
                      COMPUTE data-invalida = 1
                   END-IF  
           END-IF.
           
           IF data-invalida = 0
           move idEmp to identity
               WRITE RECORD-EMPR
                   INVALID KEY
                       IF RECORDEXISTS
                           DISPLAY "INSERT ERROR, RECORD ALREADY EXISTS." 
                       ELSE
                           DISPLAY "UNEXPECTED ERROR. FILE STATUS IS: " FILESTATUS
                       END-IF
                    NOT INVALID KEY
                       display space
                       DISPLAY "Adicionamos um empregado novo."                  
               END-WRITE
           END-IF.
           COMPUTE data-invalida = 0.                                 
           DISPLAY SPACE.
                   
       
       MOSTRA.
       DISPLAY "Ordenar por 1-numero de identificacao, 2-salario, 3-departamento, 4-genero:"
       ACCEPT RESPOSTA
       EVALUATE RESPOSTA
               WHEN 1
               PERFORM LER-FICHEIRO  
               WHEN 2
               PERFORM ORDENAR-SALARIO
               WHEN 3
               PERFORM ORDENAR-DEPARTAMENTO
               WHEN 4
               PERFORM ORDENAR-GENERO     
        DISPLAY SPACE.
          
       LER-FICHEIRO.
       if erro = 0
           MOVE ZEROS TO idEmp
           START FICHEIRO KEY IS GREATER THAN idEmp
               INVALID KEY DISPLAY "UNEXPECTED ERROR ON START."
           END-START 
           READ FICHEIRO NEXT RECORD 
               AT END SET ENDOFFILE TO TRUE
           END-READ
           if erro = 0
               PERFORM UNTIL ENDOFFILE
                   DISPLAY identity SPACE  nome SPACE  genero SPACE departamento SPACE salario SPACE  dataDeEntrada 
                   READ FICHEIRO NEXT RECORD 
                       AT END SET ENDOFFILE TO TRUE
                   END-READ
               END-PERFORM
               DISPLAY SPACE
               
           else
           compute erro = 0
           display space
           end-if
       else
           compute erro = 0
           display space    
       end-if.   
      
       ORDENAR-SALARIO.
       CLOSE FICHEIRO.
       SORT TEMPSORT ON ASCENDING KEY salario-ts
           USING FICHEIRO GIVING SORTEDFILE.
       OPEN INPUT SORTEDFILE.
           PERFORM LER-SORTEDFILE UNTIL CONDICAO = 1.
       DISPLAY SPACE.    
       CLOSE SORTEDFILE.
       OPEN I-O FICHEIRO.
       COMPUTE CONDICAO = 0.
       
       ORDENAR-DEPARTAMENTO.
       
       CLOSE FICHEIRO.
       SORT TEMPSORT ON ASCENDING KEY departamento-ts
           USING FICHEIRO GIVING SORTEDFILE.       
       OPEN INPUT SORTEDFILE.       
           PERFORM LER-SORTEDFILE UNTIL CONDICAO = 1.
       DISPLAY SPACE.       
       CLOSE SORTEDFILE.
       OPEN I-O FICHEIRO.
       COMPUTE CONDICAO = 0.
       
       ORDENAR-GENERO.
       CLOSE FICHEIRO.
       SORT TEMPSORT ON ASCENDING KEY genero-ts
           USING FICHEIRO GIVING SORTEDFILE.
       OPEN INPUT SORTEDFILE.
           PERFORM LER-SORTEDFILE UNTIL CONDICAO = 1.
       DISPLAY SPACE.    
       CLOSE SORTEDFILE.
       OPEN I-O FICHEIRO.
       COMPUTE CONDICAO = 0.
                        
       LER-SORTEDFILE.
       READ SORTEDFILE AT END MOVE 1 TO CONDICAO.
          IF CONDICAO = 0
               PERFORM 
                   DISPLAY SPACE
                   DISPLAY id-sf SPACE  nome-sf " genero:" genero-sf " departamento:" departamento-sf " salario:" salario-sf " data:" dataDeEntrada-sf 
               END-PERFORM
           END-IF.
     
       APAGAR-EMPREGADO.
           DISPLAY "Insira o numero de identificacao (3 digitos).".
           ACCEPT idEmp. 
           DELETE FICHEIRO RECORD
               INVALID KEY
                   IF NORECORDEXISTS
                       DISPLAY "DELETE ERROR, NO RECORD AT "  idEmp
                   ELSE
                       DISPLAY "UNEXPECTED ERROR. FILE STATUS IS " FILESTATUS    
                   END-IF
               NOT INVALID KEY
                   display space
                   DISPLAY "Apagamos com sucesso."
            END-DELETE.           
            DISPLAY SPACE. 
                  
       LER-EMPREGADO.
       
       DISPLAY "Insira o numero de identificacao (3 digitos).".
       ACCEPT idEmp.
       READ FICHEIRO
           INVALID KEY
               IF NORECORDEXISTS
                   DISPLAY "READ ERROR, NO RECORD AT "  idEmp
               ELSE  
                   DISPLAY "UNEXPECTED ERROR. FILE STATUS IS " FILESTATUS    
               END-IF 
       END-READ.
       IF OPERATIONSUCCESSFULL
           display space
           DISPLAY "Id: " identity " nome: " nome  " genero: " genero 
           DISPLAY "departamento: " departamento " salario: " salario " data: "  dataDeEntrada
           display space
       END-IF.        
       DISPLAY SPACE. 
        
       ATUALIZAR-EMPREGADO.
       
       DISPLAY "Insira o numero de identificacao (3 digitos).".
       ACCEPT idEmp.
       READ FICHEIRO
           INVALID KEY
               IF NORECORDEXISTS
                   DISPLAY "READ ERROR, NO RECORD AT "  idEmp
               ELSE  
                   DISPLAY "UNEXPECTED ERROR. FILE STATUS IS " FILESTATUS    
               END-IF 
       END-READ.
       IF OPERATIONSUCCESSFULL
           DISPLAY "Insira o salario novo"
           ACCEPT salario
           DISPLAY "Insira o numero de departamento (1- Marketing, 2- P&I, 3-Producao)"
           ACCEPT departamento
           REWRITE RECORD-EMPR
               INVALID KEY 
                   DISPLAY "UNEXPECTED ERROR. FILE STATUS IS " FILESTATUS 
               NOT INVALID KEY   
                   display space   
                   DISPLAY "Atualizamos com sucesso."
           END-REWRITE
        END-IF.        
        DISPLAY SPACE.
        
       MOSTRA-DEPARTAMENTO.
       if erro = 0
           MOVE ZEROS TO idEmp
           START FICHEIRO KEY IS GREATER THAN idEmp
               INVALID KEY DISPLAY "UNEXPECTED ERROR ON START."
           END-START 
           READ FICHEIRO NEXT RECORD 
               AT END SET ENDOFFILE TO TRUE
           END-READ
           if erro = 0
               DISPLAY "Insira o numero de departamento para mostrar:"
               DISPLAY "1- Marketing, 2- P&I, 3-Producao"
               ACCEPT departamentoEm
               IF ( departamentoEm > 3 OR departamentoEm < 0)
                   DISPLAY SPACE
                   DISPLAY "O numero de departamento invalido."
                   DISPLAY SPACE 
               ELSE                   
                   PERFORM UNTIL ENDOFFILE                
                       IF departamento = departamentoEm
                           DISPLAY SPACE
                           DISPLAY identity space nome  " genero: " genero " departamento: " departamento " salario: " salario " data: "  dataDeEntrada 
                           DISPLAY SPACE 
                       END-IF     
             READ FICHEIRO NEXT RECORD 
                AT END SET ENDOFFILE TO TRUE
             END-READ
                    END-PERFORM    
           else
           compute erro = 0
           display space
           end-if
       else
           compute erro = 0
           display space    
       end-if.   
           
       MOSTRA-ESTATISTICA-SALARIOS.
       PERFORM CONTA-MIN.
       PERFORM CONTA-MAX.
       PERFORM CONTA-MEDIA.
       
       CONTA-MIN.
       if erro = 0
           MOVE ZEROS TO idEmp
           START FICHEIRO KEY IS GREATER THAN idEmp
               INVALID KEY DISPLAY "UNEXPECTED ERROR ON START."
           END-START 
           READ FICHEIRO NEXT RECORD 
               AT END SET ENDOFFILE TO TRUE
           END-READ
           if erro = 0               
               PERFORM             
                   COMPUTE MIN = salario
                       PERFORM UNTIL ENDOFFILE
                           IF salario < MIN
                               COMPUTE MIN = salario                      
                           END-IF                    
                           READ FICHEIRO NEXT RECORD 
                               AT END SET ENDOFFILE TO TRUE
                           END-READ
                       END-PERFORM
                   DISPLAY SPACE    
                   DISPLAY "Salário mínimo: " MIN
                   DISPLAY SPACE
               END-PERFORM
                              
           else
           compute erro = 0
           display space
           end-if
       else
           compute erro = 0
           display space    
       end-if. 
       
       CONTA-MAX.
       if erro = 0
           MOVE ZEROS TO idEmp
           START FICHEIRO KEY IS GREATER THAN idEmp
               INVALID KEY DISPLAY "UNEXPECTED ERROR ON START."
           END-START 
           READ FICHEIRO NEXT RECORD 
               AT END SET ENDOFFILE TO TRUE
           END-READ
           if erro = 0               
               PERFORM             
                   COMPUTE MAX = salario
                       PERFORM UNTIL ENDOFFILE
                           IF salario > MAX
                               COMPUTE MAX = salario                      
                           END-IF                    
                           READ FICHEIRO NEXT RECORD 
                               AT END SET ENDOFFILE TO TRUE
                           END-READ
                       END-PERFORM
                   DISPLAY "Salário máximo: " MAX
                   DISPLAY SPACE
               END-PERFORM
                              
           else
           compute erro = 0
           display space
           end-if
       else
           compute erro = 0
           display space    
       end-if. 
       
       CONTA-MEDIA. 
       if erro = 0
           MOVE ZEROS TO idEmp
           START FICHEIRO KEY IS GREATER THAN idEmp
               INVALID KEY DISPLAY "UNEXPECTED ERROR ON START."
           END-START 
           READ FICHEIRO NEXT RECORD 
               AT END SET ENDOFFILE TO TRUE
           END-READ
           if erro = 0                                
                PERFORM UNTIL ENDOFFILE 
                    COMPUTE CONTADOR-EMPREGADOS = CONTADOR-EMPREGADOS + 1
                    COMPUTE TOTAL-SALARIOS = TOTAL-SALARIOS + salario                                                                
                        READ FICHEIRO NEXT RECORD 
                            AT END SET ENDOFFILE TO TRUE
                        END-READ                        
                 END-PERFORM
                 COMPUTE MEDIO = TOTAL-SALARIOS / CONTADOR-EMPREGADOS
                 MOVE MEDIO TO MASCARA   
                 DISPLAY "Salário médio: " MASCARA
                 DISPLAY SPACE  
                 DISPLAY "Número dos empregados " CONTADOR-EMPREGADOS
                 DISPLAY SPACE              
                 COMPUTE TOTAL-SALARIOS = 0
                COMPUTE CONTADOR-EMPREGADOS = 0                            
           else
           compute erro = 0
           display space
           end-if
       else
           compute erro = 0
           display space    
       end-if.        
                            
       END PROGRAM Empregados.

       
