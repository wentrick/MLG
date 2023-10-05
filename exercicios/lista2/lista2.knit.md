---
title: "Lista 2 MLG"
author: "Davi Wentrick Feijó"
date: "2023-10-02"
output: pdf_document
---



#### Q1. Considere os dados sobre a qualidade do vinho tinto, apresentados no ficheiro `Q01-data.txt`. Ajuste o modelo de regressao linear multipla, e faca uma analise completa desses dados. Que conclusoes voce tira dessa an´alise? (use 5% de significancia durantes as analises)

Com base no conjunto de dados, e possıvel observar que algumas covariaveis estao fortemente correlacionadas. Assim, pede-se:
##### (a) Proponha algum m´etodo para resolver o problema da multicolinearidade no conjunto de dados.
##### (b) Usando algum m´etodo de sele¸c˜ao de vari´aveis, obtenha o modelo final para o conjunto de dados.
##### (c) Apresente a tabela da An´alise de Variˆancia para testar a significˆancia global dos coeficientes do modelo final. Apresente as hip´otese de teste, e conclua.
##### (d) Com base no modelo obtido no item anterior, fa¸ca uma an´alise de res´ıduos e conclua.


#### Q02. Uma equipe de pesquisadores de saude mental deseja comparar tres metodos de tratamento da depressao grave (A, B e C=referencia). Eles tambem gostariam de estudar a relacao entre idade e eficacia do tratamento, bem como a interacao (se houver) entre idade e tratamento. Cada elemento da amostra aleatoria simples de 36 pacientes, foi selecionado aleatoriamente para receber o tratamento A, B ou C. Os dados obtidos podem ser encontrados no ficheiro `Q02-data.txt`. A vari´avel dependente y e a eficacia do tratamento; as variaveis independentes sao: a idade do paciente no aniversario mais proximo, e o tipo de tratamento administrado (use 1% de significancia durantes as analises).
##### (a) Ajuste o modelo de regress˜ao linear e interprete os resultados obtidos.
##### (b) Obtenha a tabela ANOVA para o modelo obtido no item (a) e interprete os resultados.
##### (c) Considere a possibilidade de incluir a interacao entre as varaveis independentes, i.e., assuma que o modelo a ser ajustado tem a seguinte formula¸c˜ao: $yi =  \beta_0 +  \beta_1x_{1i} +  \beta_2x_{Ai} +  \beta_3x_{Bi} +  \beta_4x_{1i}x_{Ai} + \beta_5x_{1i}x_{Bi} + \epsilon_i$ , com a suposicao de que $\epsilon_i ∼ N(0, \sigma2)$. Com base no modelo anterior,
+ (i) Liste todos os possıveis submodelos que podem ser obtidos usando o modelo apresentado anteriormente.
+ (ii) Interprete os coeficientes de regress˜ao associados aos fatores de intera¸c˜ao.
+ (iii) Apresente a tabela anova para testar as seguintes hip´oteses, H0 : β1 = β4 = β5 = 0 contra H1 : ∃βj 6= 0, com j = 1, 4, 5.
+ (iv) Fa¸ca uma an´alise completa dos res´ıduos do modelo.
















