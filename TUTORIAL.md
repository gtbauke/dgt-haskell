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

## Nosso primeiro parser

Agora que podemos representar nossos _parsers_, vamos criar um _parser_ que consegue reconhecer apenas um única caractere, ou seja, dado uma string qualquer, ele consegue reconhecer o primeiro caractere e retornar o restante da string.

```hs
charParser :: Char -> Parser Char
charParser expected = Parser $ \input -> case input of
  (x:xs) -> if x == expected then (x, xs) else error "Unexpected character"
  _ -> error "Unexpected end of input"
```

Por enquanto não vamos nos preocupar com erros (mais para frente lidaremos com eles!). O _parser_ `charParser` recebe um caractere esperado e retorna um `Parser` que, dado uma string, se o primeiro caractere for igual ao caractere esperado, retorna o caractere e o restante da string. Caso contrário, retorna uma string vazia e a string original. Veja alguns exemplos de uso:

```hs
runParser (charParser 'h') "hello" -- ('h', "ello")
runParser (charParser 'h') "world" -- error "Unexpected character"
runParser (charParser 'w') "world" -- ('w', "orld")
```

## String parser

Agora que podemos reconhecer um único caractere, vamos criar um _parser_ que consegue reconhecer uma sequência inteira de caracteres (uma string). Vamos criar nosso `stringParser` da seguinte forma:

```hs
stringParser :: String -> Parser String
stringParser expected = Parser $ \input -> if expected `isPrefixOf` input then (expected, drop (length expected) input) else error "Unexpected string"
```

Aqui, estamos usando a função `isPrefixOf` (importada de `Data.List`) para verificar se nosso input começa com a string que esperamos. Caso positivo, retornamos a string esperada e o restante do input em uma tupla. Caso negativo, retornamos um erro. Veja alguns exemplos de uso:

```hs
runParser (stringParser "hello") "hello world" -- ("hello", " world")
runParser (stringParser "hello") "world" -- error "Unexpected string"
runParser (stringParser "world") "world" -- ("world", "")
```

## Combinando parsers

Agora já temos _parsers_ suficientes para começarmos a combiná-los. Imagine que queremos criar um _parser_ que reconheça a string "hello" e, em seguia, a string "world". Para isso, precisamos combinar dois `stringParser` de alguma forma, mas como fazemos isso?

Para isso, primeiro precisamos de um _parser_ para a string "hello" e, em seguida, no resultado do primeiro _parser_, aplicamos o segundo, que reconhecerá a string "world". Ou seja, precisamos de uma função que, dado uma lista de _parsers_ (uma lista porque queremos combinar qualquer número de _parsers_ e não apenas dois como no exemplo anterior), retorna um novo _parser_ que aplica sequencialmente cada _parser_ dessa lista e retorna o resultado final. Vamos chamar essa função de `sequenceOf`:

```hs
sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser ([],)
sequenceOf (p : ps) = Parser $ \input ->
  let (x, input') = runParser p input
      (xs, input'') = runParser (sequenceOf ps) input'
   in (x : xs, input'')
```

Note que nessa implementação, estamos utilizando uma extensão da linguagem chamada `TupleSections`. Para habilitá-la, adicione a seguinte linha no início do arquivo `Parsers.hs`:

```hs
{-# LANGUAGE TupleSections #-}
```

Nossa função `sequenceOf` tem dois casos, um para lista vazia e outro para listas não vazias. No primeiro caso, retornamos um _parser_ que, dado uma string, retorna uma tupla com resultado vazio e o próprio input. No segundo caso, aplicamos o primeiro _parser_ da lista e chamamos a função `sequenceOf` recursivamente com o restante da lista no restante da string retornado na primeira aplicação da nossa função. No final, retornamos uma tupla com a lista de resultados e o restante da string. Veja alguns exemplos de uso:

```hs
runParser (sequenceOf [charParser 'h', charParser 'e', charParser 'l', charParser 'l', charParser 'o']) "hello world" -- ("hello", " world")
runParser (sequenceOf [charParser 'h', charParser 'e', charParser 'l', charParser 'l', charParser 'o']) "world" -- error "Unexpected end of input"
runParser (sequenceOf [stringParser "hello", stringParser "world"]) "hello world" -- (["hello", "world"], "")
runParser (sequenceOf [stringParser "hello", stringParser "world"]) "world" -- error "Unexpected string"
```

Agora, vamos criar uma função capaz de aplicar um _parser_ e, caso ele falhe, aplicar outro _parser_, ou seja, dado uma lista de _parsers_, vamos retornar um _parser_ que aplica sequencialmente cada _parser_ da lista e retorna o resultado do primeiro que obteve sucesso. Antes de implementar essa função, precisamos saber quando nossos _parsers_ estão falhando. Até o momento, ignoramos os errors, mas agora eles são importantes para a lógica da nossa função. Para lidar com os erros, vamos alterar nosso tipo `Parser` e vamos introduzir um novo tipo `ParserState` que representa o estado, sucesso ou falha, de qualquer um de nossos _parsers_.

Vamos começar com nosso tipo novo `ParserState`:

```hs
data ParserState a = ParserState {result :: a, rest :: String} | ParserError String
  deriving (Show)
```

Aqui, `ParserState` tem dois possíveis valores, `ParserState` e `ParserError`, que representam, respectivamente, o sucesso e a falha de um _parser_. Para construir esse tipo, estamos utilizando tipos do "tipo soma". Para quem não está familiarizado com esse conceito, basicamente, estamos criando um tipo que pode ser ou um `ParserState` ou um `ParserError`, mas nunca os dois ao mesmo tempo. Na maioria das linguagens _mainstream_ não é possível construir tipos dessa forma, pois essas linguagens são limitadas a tipos do "tipo produto", como os `structs` ou `classes`. Em linguagens funcionais, é muito comum utilizarmos esse tipo de construção, uma vez que ela deixa nosso código muito mais expressivo e compreensível. Mais para frente nesse tutorial, vou explicar o porque dessas classes de tipos terem os nomes "soma" e "produto".

Agora, vamos alterar nosso tipo `Parser` para que ele utilize `ParserState` ao invés de uma tupla:

```hs
newtype Parser a = Parser { runParser :: String -> ParserState a }
```

E agora, precisamos alterar nossas implementação das funções `charParser`, `stringParser` e `sequenceOf`:

```hs
charParser :: Char -> Parser Char
charParser e = Parser $ \case
  (x : xs) -> if x == e then ParserState {result = x, rest = xs} else ParserError "Unexpected character"
  [] -> ParserError "Unexpected end of input"

stringParser :: String -> Parser String
stringParser expected = Parser $ \input ->
  if expected `isPrefixOf` input
    then ParserState {result = expected, rest = drop (length expected) input}
    else ParserError "Unexpected string"

sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> ParserState {result = [], rest = input}
sequenceOf (p : ps) = Parser $ \input -> case runParser p input of
  ParserError e -> ParserError e
  ParserState {result = r, rest = rest'} -> case runParser (sequenceOf ps) rest' of
    ParserError e -> ParserError e
    ParserState {result = rs, rest = rest''} -> ParserState {result = r : rs, rest = rest''}
```

Parece que nossas funções ficaram um pouco mais verbosas (será que há uma forma de reduzirmos essa verbosidade?), mas agora elas são capazes de lidar com erros. Vamos ver como ficaram nossos exemplos de uso:

```hs
runParser (sequenceOf [charParser 'h', charParser 'e', charParser 'l', charParser 'l', charParser 'o']) "hello world" -- ParserState {result = "hello", rest = " world"}
runParser (sequenceOf [charParser 'h', charParser 'e', charParser 'l', charParser 'l', charParser 'o']) "world" -- ParserError "Unexpected end of input"
runParser (sequenceOf [stringParser "hello", stringParser "world"]) "hello world" -- ParserError "Unexpected string"
runParser (sequenceOf [stringParser "hello", stringParser " world"]) "hello world" -- ParserState {result = ["hello"," world"], rest = ""}
```

Com essa nova implementação do nosso tipo `Parser`, podemos criar a função `choice`:

```hs
choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> ParserError "No parsers to choose from"
choice (p : ps) = Parser $ \input -> case runParser p input of
  ParserError _ -> runParser (choice ps) input
  x -> x
```

Se passarmos uma lista vazia para essa função, retornamos um _parser_ que ignora seu input e retorna um erro. Caso contrário, aplicamos o primeiro _parser_ da lista ao input e, caso ele falhe, aplicamos recursivamente a função `choice` ao restante da lista até que um _parser_ tenha sucesso. Veja alguns exemplos de uso:

```hs
runParser (choice [charParser 'h', charParser 'w']) "hello world" -- ParserState {result = 'h', rest = "ello world"}
runParser (choice [charParser 'h', charParser 'w']) "world" -- ParserState {result = 'w', rest = "orld"}
runParser (choice [charParser 'h', charParser 'w']) "ello world" -- ParserError "No parsers to choose from"
```

Nesse momento, podemos combinar `parser` de forma sequencial ou de forma alternativa. Agora, vamos criar mais duas funções que nos permitem aplicar um _parser_ zero ou mais vezes e uma ou mais vezes. Vamos chamar essas funções de `zeroOrMore` e `oneOrMore`, respectivamente:

```hs
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ -> ParserState {result = [], rest = input}
  x -> case runParser (zeroOrMore p) (rest x) of
    ParserError _ -> ParserState {result = [result x], rest = rest x}
    ParserState {result = rs, rest = rest'} -> ParserState {result = result x : rs, rest = rest'}

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ -> ParserError "Expected one or more"
  ParserState {result = r, rest = rest'} -> case runParser (zeroOrMore p) rest' of
    ParserError _ -> ParserState {result = [r], rest = rest'}
    ParserState {result = rs, rest = rest''} -> ParserState {result = r : rs, rest = rest''}
```

A função `zeroOrMore` aplica um _parser_ zero ou mais vezes. Para isso, ela tenta aplicar o _parser_ no input, caso falhe, retorna o estado inicial em uma lista. Se não falhar, aplica recursivamente o _parser_ até que ele falhe e, em seguida, retorna o resultado em uma lista. A função `oneOrMore` é bem parecida, mas ela retorna um erro caso o _parser_ falhe na primeira aplicação. Veja alguns exemplos de uso:

```hs
runParser (zeroOrMore (charParser 'h')) "hello world" -- ParserState {result = "h", rest = "ello world"}
runParser (zeroOrMore (charParser 'h')) "world" -- ParserState {result = "", rest = "world"}
runParser (zeroOrMore (charParser 'h')) "hhhworld" -- ParserState {result = "hhh", rest = "world"}
runParser (oneOrMore (charParser 'h')) "hello world" -- ParserState {result = "h", rest = "ello world"}
runParser (oneOrMore (charParser 'h')) "world" -- ParserError "Expected one or more"
runParser (oneOrMore (charParser 'h')) "hhhworld" -- ParserState {result = "hhh", rest = "world"}
```

## Nossa AST, Tipos de Dados Algébricos e Type Classes

Agora, já temos como combinar nossos _parsers_ de várias formas. Vamos começar a criar um _parser_ para nossa linguagem fictícia, mas antes disso, precisamos pensar em uma forma de representar nossa linguagem como uma árvore de sintaxe abstrata (AST). Vamos criar um novo módulo chamado `Lang` e, em seguida, vamos criar um novo tipo de dados que representa nossa linguagem:
