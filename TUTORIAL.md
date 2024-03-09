# Entendo Algebraic Data Types (ADTs) e Type Classes com Parser Combinators em Haskell

Nos últimos anos, muitas linguagens de programação _mainstream_ vem adotando funcionalidades de linguagens funcionais. Um exemplo disso é o [Rust](https://www.rust-lang.org/) que, dentre os diversos recursos funcionais que possui, adota o conceito de tipos de dados algébricos (ADTs em inglês) e de Type Classes, chamadas de Traits na linguagem. Para quem já conhece um pouco de linguagens funcionais, como Haskell e OCaml, o conceito de ADTs é bem familiar. No entanto, para quem ainda está começando a estudar esse paradigma, ADTs podem parecer um pouco confusos. O objetivo desse tutorial é explicar o que são ADTs criando uma biblioteca de [Parser Combinators](https://en.wikipedia.org/wiki/Parser_combinator) bem básica em Haskell.

Antes de começar o tutorial propriamente dito, é importante entender o que queremos ter ao final deste texto. Da forma mais simples possível, _parsers_ são funções que recebem uma entrada (geralmente uma sequência de caracteres) e os converte em uma estrutura com significado bem definido em certo contexto. O exemplo mais comum disso são as linguagens de programação. Vamos analisar o seguinte código em C:

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

Quando compilamos esse código e o executamos, como nosso computador entende o que precisa ser feito? Ou ainda, como uma sequência de caracteres se transforma em um programa executável? A resposta para essas perguntas é que o código, antes de ser compilado em código de máquina, é transformado em uma estrutura de dados que representa o programa. Essa estrutura de dados é chamada de [**Abstract Syntax Tree** (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree). Uma das etapas desse processo utiliza _parsers_. Vamos entender o seguinte diagrama:

[Processamento de Código Fonte para AST](./images/lang-process.png)

Esse diagrama é uma simplificação do processo. No entanto, ela já é suficiente para termos uma ideia do que está acontecendo por de baixo dos panos. Geralmente, o código (sequência de caracteres) é primeiramente transformado em uma sequência de _tokens_ (agrupamentos de caracteres que possuem sentido léxico entre si). Essa sequência é então transformada em uma árvore de sintaxe abstrata, em um processo que é chamado de _parsing_, no qual sentido semântico é atribuído aos _tokens_. No entanto, nesse tutorial, vamos utilizar uma técnica conhecida como _parser combinators_ para criar um _parser_ que transforma uma sequência de caracteres em uma AST de forma direta.

Ao final, queremos conseguir criar um _parser_ para uma linguagem fictícia bem simples. O seguinte trecho de código é um exemplo do que queremos ser capazes de fazer:

```ts
function main() {
  let x = 10;
  let y = 20;

  if (x > y) {
    print("x é maior que y");
    return;
  }

  print("y é maior que x");
}
```

## O que são Parser Combinators?

Como dito anteriormente, _parsers_ são funções que transformam uma sequência de caracteres em uma árvore de sintaxe abstrata (pelo menos para os fins desse tutorial). Por sua vez, _parser combinators_ são funções que conseguem combinar _parsers_ das mais diversas formas com o objetivo de criar _parsers_ mais complexos.

Assim, vamos começar criando uma estrutura de dados que represente nossos _parsers_ e, em seguida, vamos criar funções que combinam esses _parsers_.

## Iniciando o projeto

Vamos começar criando um novo projeto Haskell. Caso não tenha o ambiente de desenvolvimento Haskell instalado, recomendo seguir o site do [GHCup](https://www.haskell.org/ghcup/) para instalar todas as dependências necessárias. Com o ambiente já configurado, crie um novo projeto com o seguinte comando:

```sh
stack new parser-combinators simple
```

Com o projeto criado, vamos ter a seguinte estrutura de diretórios:

```sh
parser-combinators/
├── src
│   └── Main.hs
├── stack.yaml
├── parser-combinators.cabal 
```

Vamos começar criando um novo módulo para nossas funções de _parser_. Na pasta `src`, crie um novo arquivo chamado `Parsers.hs`. Em seguia, abra o arquivo `parser-combinators.cabal` e adicione o novo módulo na lista de módulos do projeto:

```cabal
-- Other stuff

executable parser-combinators
  hs-source-dirs:      src
  main-is:             Main.hs

  other-modules:       Parsers
  
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
  ghc-options:         -Wall
                       -Wcompat
                       -Widentities
                       -Wincomplete-record-updates
                       -Wincomplete-uni-patterns
                       -Wmissing-export-lists
                       -Wmissing-home-modules
                       -Wpartial-fields
                       -Wredundant-constraints

```

Agora abra o arquivo `Parser.hs` e declare o seguinte módulo:

```hs
module Parsers where
```

Agora estamos prontos para começar a criar nossos _parsers_.

## O tipo `Parser`

Vamos começar criando um tipo que represente todos os nossos _parsers_. Inicialmente, vamos começar implementando uma estrutura mais simples, a qual será incrementada ao longo do tutorial. Queremos que nosso tipo `Parser` seja capaz de representar um _parser_ qualquer, independentemente da estrutura retornada por ele. Para isso, vamos utilizar tipos parametrizados (em algumas linguagens, são também conhecidos como _generics_ ou tipos genéricos). O tipo `Parser` será uma função que recebe uma sequência de caracteres e retorna uma tupla com o resultado do _parser_ e a sequência de caracteres restante. Vamos declarar o tipo `Parser` da seguinte forma:

```hs
newtype Parser a = Parser { runParser :: String -> (a, String) }
```

Aqui, `Parser` é um tipo parametrizado que recebe um tipo qualquer `a`. Além disso, temos também o construtor do tipo `Parser` também chamado de `Parser`. Esse construtor recebe uma função que, dada uma sequência de caracteres, retorna uma tupla com o resultado do _parser_ e a sequência de caracteres restante.

Para aqueles que não estão muito familiarizados com Haskell, isso pode parecer um pouco confuso. No entanto, basicamente, estamos criando um tipo de dados que representa uma função que recebe uma `String` e retorna um par de valores, onde o primeiro valor é do tipo `a` e o segundo valor é uma `String`. Poderíamos escrever uma estrutura parecida em uma linguagem orientada a objetos da seguinte forma (ao longo do tutorial, vamos utilizar algumas comparações com linguagens mais conhecidas para facilitar o entendimento):

```cs
public abstract class Parser<T> {
  public virtual (T, string) RunParser(string input) {}
}
```

## Nosso primeiro _parser_

Agora que podemos representar nossos _parsers_, vamos criar um _parser_ que consegue reconhecer apenas um única caractere, ou seja, dado uma string qualquer, ele consegue reconhecer o primeiro caractere e retornar o restante da string.

```hs
charParser :: Char -> Parser Char
charParser expected = Parser $ \input -> case input of
  (x:xs) -> if x == expected then (x, xs) else ('', input)
  _ -> ('', input)
```

Por enquanto não vamos nos preocupar com erros (mais para frente lidaremos com eles!). O _parser_ `charParser` recebe um caractere esperado e retorna um `Parser` que, dado uma string, se o primeiro caractere for igual ao caractere esperado, retorna o caractere e o restante da string. Caso contrário, retorna uma string vazia e a string original. Veja alguns exemplos de uso:

```hs
runParser (charParser 'h') "hello" -- ('h', "ello")
runParser (charParser 'h') "world" -- ('', "world")
runParser (charParser 'w') "world" -- ('w', "orld")
```
