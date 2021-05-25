# Compilador JIT em Haskell
O projeto se trata de uma versão do [tutorial da LLVM](https://llvm.org/docs/tutorial/index.html) para construirmos uma linguagem compilada. Este projeto é retirado diretamente de [Implementing a JIT Compiled Language with Haskell and LLVM](https://www.stephendiehl.com/llvm/#chapter-1-introduction), com alterações onde for necessário. Esse tutorial também é um projeto open-source. Nota-se que, embora seja um tutorial, não deixa de ser um trabalho relativamente complexo e que ajuda a entender como fazer um projeto de maior escala em Haskell, além de que é uma linguagem cada vez mais utilizada para escrever diferentes compiladores. Com isso, o grupo tem a intenção de mostrar como a base aprendida na disciplina pode ser usada diretamente nessa aplicação.

Um dos grandes desafios enfrentados foi a compatibilização dos arquivos com a versão mais recente da LLVM, a LLVM9. O projeto [kaleidoscope-LLVM8](https://github.com/mariari/kaleidoscope-LLVM8) foi base fundamental para isso.

## O que é JIT
A [compilação JIT](https://www.freecodecamp.org/news/just-in-time-compilation-explained/) (*Just In Time Compilation*, ou *Dynamic Compilation*) é uma forma de compilação que traduz o programa em código nativo durante a execução. Isso permite que mais códigos otimizados sejam gerados. É uma compilaçào que é feita durante a execução de um programa, ou seja, em *run time*. Temos como exemplos o JVM, de Java, e o CLR, de C#.
Dessa forma, o código fonte é convertido em uma *assembly-like language structure*, um código intermediário que é então convertido em código de máquina somente quando necessário.

## Linguagem Produzida
A intenção do projeto é criar a linguagem **Kaleidoscope**, uma *toy-language* utilizada também no tutorial original da LLVM. Cabe ressaltar que a LLVM é responsável pelo compilador **clang**, utilizado e até "rivalizado" com o mais tradicional **gcc**. Começamos montando os básicos da linguagem, depois inserindo fluxos de controle (condicionais, loops), operadores definidos por usuários e variáveis mutáveis.

## Instruções
Atualmente, recomenda-se o uso de um terminal ubuntu e o comando stack build para fazer a compilação do projeto.

## Vídeo de apresentação
O vídeo de apresentação pode ser visto clicando [aqui](https://www.youtube.com/watch?v=PR9IxKZmpJ0)
