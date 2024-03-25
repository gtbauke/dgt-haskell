# Entendendo Tipos de Dados Algébricos (ADTs) com Parser Combinators

Em linguagens funcionais, tipos de dados algébricas (ADTs) são a principal forma de representação de estruturas de dados. Por muito tempo, esse conceito ficou restrito a linguagens funcionais, mas com a popularização dos conceitos desse paradigma, muitas linguagens, como por exemplo [Rust](https://www.rust-lang.org/) passaram a adotar ADTs como uma forma de representar suas estruturas de dados.

Sendo assim, é importante entender o que são ADTs e como eles podem ser utilizados para representar as mais diversas estruturas de dados.

## O que faremos?

O objetivo aqui é conseguir representar uma árvore abstrata de sintaxe (AST) para uma linguagem de programação fictícia. Assim, o objetivo é criar uma estrutura de dados que represente uma **árvore** para nossa linguagem.

Para exemplificar melhor essa estrutura, vamos criar um parser para essa linguagem, utilizando a técnica de parser combinators. Assim, criaremos funções responsáveis por criar as diferentes partes da nossa AST e por combinar essas partes para criar nossa árvore final.

## O que são ADTs?

Antes de começarmos a implementar nossa árvore, é importante entender o que de fato são ADTs, uma vez que eles serão a base para a nossa implementação. A ideia principal por trás dos tipos de dados algébricos é que novos tipos podem ser criados a partir da combinação de outros tipos por meio de operações de soma e produto.

Os tipos de dados algébricos possuem esse nome pois eles podem ser representados por meio de operações aritméticas de soma e produto em operandos que representam a quantidade de valores possíveis para cada tipo. Vamos começar analisando os tipos que representam os valores **0** e **1**:

```hs
data Void

data () = ()
```

Aqui, definimos dois tipos, utilizando a palavra-chave `data`. O primeiro tipo, `Void`, não possui valores possíveis, enquanto o segundo tipo, `()`, possui apenas um valor possível, que é `()`.

Aqui, já podemos ver uma grande vantagem que os tipos de dados algébricos nos trazem. Se tivéssemos os seguintes tipos:

```hs
data Void
data Zero

data () = ()
data One = One
```

Como tanto `Void` quanto `Zero` não possuem valores possíveis, eles são equivalentes. O mesmo vale para `()` e `One`. Isso nos permite encontrar uma relação de equivalência entre tipos analisando a quantidade de valores possíveis que podem assumir.

### Combinando tipos - Tipo Produto

Vamos começar falando sobre o tipo produto, uma vez que ele também existe em linguagens *mainstream* como C, C++ e Java.

```hs
data Ponto2D = Ponto Float Float
```

Vamos analisar o caso do tipo `Ponto2D`. Nesse caso, temos um tipo que carrega dois valores do tipo `Float`, ou seja, sempre que formos criar um valor do tipo `Ponto2D`, precisamos fornecer dois valores do tipo `Float`.

Sempre que temos essa situação, onde um tipo é formado pela combinação simultânea de outros tipos, dizemos que temos um tipo produto. Isso porque a quantidade de valores possíveis para o tipo `Ponto2D`, por exemplo, é o produto da quantidade de valores possíveis para cada um dos tipos que o compõem. Nesse caso, como o tipo `Float` possui 2^23 valores possíveis (IEE-754 Single Precision), o tipo `Ponto2D` possui 2^23 * 2^23 = 2^46 valores possíveis.

Podemos construir tipos produtos em linguagens não funcionais também, utilizando `structs` ou `classes`. Por exemplo, em C, poderíamos representar o tipo `Ponto2D` da seguinte forma:

```c
typedef struct {
  float x;
  float y;
} Ponto2D;
```

### Combinando tipos - Tipo Soma

Agora, vamos falar sobre o tipo soma. Vamos analisar como o tipo `Bool` é representado em Haskell:

```hs
data Bool = True | False
```

Nessa definição, criamos um tipo que pode assumir um entre dois valores, mas nunca os dois valores simultaneamente. Esse é um exemplo de um tipo soma, pois ele é formado pela combinação de dois tipos, mas apenas um deles pode ser escolhido. Algebricamente, a quantidade de valores possíveis para um tipo soma é a soma da quantidade de valores possíveis para cada um dos tipos que o compõem.

Para entender melhor essa última parte, vamos analisar o tipo `Maybe`:

```hs
data Maybe a = Nothing | Just a
```

O tipo `Maybe` é um tipo paramétrico que pode guardar um valor do tipo `a` ou não guardar nada. Na definição acima, ele pode assumir dois valores diferentes, `Nothing` e `Just`. No entanto, note que, diferentemente do tipo `Bool`, o tipo `Maybe` tem um valor (`Just`) que pode assumir a mesma quantidade de valores que o tipo `a`. Assim, a quantidade de valores possíveis para o tipo `Maybe` é a soma da quantidade de valores possíveis para `Nothing` e a quantidade de valores possíveis para `Just`.

```hs
data Maybe a = Nothing | Just a = 1 + a
```

Em linguagens não funcionais, expressar tipos soma é possível, mas muito mais complicado, uma vez que essas linguagens não possuem suporte nativo para esse tipo de operação. Em C, por exemplo, poderíamos representar o tipo `Maybe` da seguinte forma:

```c
typedef enum {
  NOTHING,
  JUST
} MaybeTag;

typedef struct {
  MaybeTag tag;
  union {
    void* nothing;
    void* just;
  } value;
} Maybe;
```

Aqui, precisamos utilizar uma *tag* para indicar qual dos valores está sendo armazenado, e uma *union* para armazenar o valor em si. Isso torna a representação de tipos soma muito mais complicada e propensa a erros. Além disso, não temos as mesmas garantias que temos em linguagens funcionais, como Haskell, onde o compilador garante que todos os casos possíveis são tratados.

## Implementando nossa AST

Agora que já temos uma base teórica sobre ADTs, vamos começar a implementar nossa AST. Vamos começar definindo nosso tipo que representará a nossa árvore, e, depois, funções para manipular essa estrutura.

Nossa linguagem fictícia será composta apenas por expressões, ou seja, todas as construções dela retornam um valor. Por simplicidade, vamos inicialmente considerar que nossa linguagem possui apenas números inteiros, números de ponto flutuante, expressões booleanas, operações unárias e binárias e expressões condicionais.

Ou seja, nesse caso temos seis possíveis construções para nossa linguagem e precisamos criar um único tipo que pode assumir qualquer um desses valores. Para isso, vamos utilizar um tipo soma, que chamaremos de `Expression`:

```hs
data Expression = ExprInt Int
                | ExprFloat Float
                | ExprBool Bool
                | ExprUnary UnaryOp Expression
                | ExprBinary Expression BinaryOp Expression
                | ExprIf Expression Expression Expression
                deriving (Show)
```

Além disso, precisamos também definir os tipos `UnaryOp` e `BinaryOp`, que representam quais operações unárias e binárias nossa linguagem suporta:

```hs
data UnaryOp = Negate -- Operação de negação
             | Not    -- Operação de negação lógica
             deriving (Show)

data BinaryOp = Add -- Operação de adição
              | Sub -- Operação de subtração
              | Mul -- Operação de multiplicação
              | Div -- Operação de divisão
              | And -- Operação de conjunção
              | Or  -- Operação de disjunção
              | Eq  -- Operação de igualdade
              | Neq -- Operação de desigualdade
              | Lt  -- Operação de menor que
              | Lte -- Operação de menor ou igual a
              | Gt  -- Operação de maior que
              | Gte -- Operação de maior ou igual a
              deriving (Show)
```

Por que utilizamos tipos soma invés de tipos produto para representar nossa AST? A resposta é simples: em qualquer momento, uma expressão pode assumir apenas um dos valores possíveis, ou seja, nunca teremos uma expressão que é ao mesmo tempo um número inteiro e uma expressão condicional, por exemplo. No entanto, note que também utilizamos de tipos produto para representar as operações unárias, binárias e condicionais. Nesses casos, precisamos guardar mais do que apenas um valor a todos os momentos, e por isso utilizamos tipos produto.

Antes de implementarmos as funções que irão transformar uma string qualquer em uma expressão da nossa linguagem, vamos implementar uma função que nos permita imprimir na tela uma árvore de expressões de forma indentada:

```hs
prettyAST :: Expression -> String
prettyAST = prettyAST' 0
  where
    prettyAST' :: Int -> Expression -> String
    prettyAST' indent (ExprInt n) = replicate indent ' ' ++ show n
    prettyAST' indent (ExprFloat n) = replicate indent ' ' ++ show n
    prettyAST' indent (ExprBool b) = replicate indent ' ' ++ show b
    prettyAST' indent (ExprUnary op e) = replicate indent ' ' ++ show op ++ " " ++ prettyAST' 0 e
    prettyAST' indent (ExprBinary op e1 e2) =
      replicate indent ' '
        ++ prettyAST' 0 e1
        ++ " "
        ++ show op
        ++ " "
        ++ prettyAST' 0 e2
    prettyAST' indent (ExprIf e1 e2 e3) =
      replicate indent ' '
        ++ "if\n"
        ++ prettyAST' (indent + 2) e1
        ++ "\n"
        ++ replicate indent ' '
        ++ "then\n"
        ++ prettyAST' (indent + 2) e2
        ++ "\n"
        ++ replicate indent ' '
        ++ "else\n"
        ++ prettyAST' (indent + 2) e3
```

Essa função apenas transforma uma expressão em uma string, indentando cada nível da árvore para que seja mais fácil visualizar a estrutura da nossa árvore no terminal. Por exemplo:

```hs
expr = ExprIf (ExprBinary Lte (ExprInt 9) (ExprInt 8)) (ExprFloat 9.4) (ExprUnary Negate (ExprInt 9))
putStrLn $ prettyAST expr
```

Esse código imprime a seguinte árvore:

```hs
if
  9 Lte 8
then
  9.4
else
  Negate 9
```

## Implementando o Parser

Agora que já podemos representar nossa árvore de expressões, vamos implementar um parser para nossa linguagem. Antes disso, no entanto, vamos entender a ideia por trás dos parser combinators.

A ideia principal por trás dessa técnica é que podemos criar parsers complexos a partir da combinação de parsers mais simples. Por exemplo, podemos criar um parser para números inteiros e um parser para números de ponto flutuante e, a partir desses dois parsers, criar um para expressões matemáticas, por exemplo. A ideia se parece muito com a ideia por trás dos tipos de dados algébricos, onde podemos criar tipos complexos a partir da combinação de tipos mais simples. No entanto, as operações realizadas são diferentes. Enquanto em ADTs realizamos operações de soma e produto, em parser combinators realizamos operações de sequência e alternância, por exemplo.

### Nosso primeiro parser

Vamos começar implementando um parser muito simples, que é capaz de reconhecer um único caractere. Para isso, vamos criar um novo tipo que represente nosso parser. Esse tipo, chamado de `Parser`, será um tipo que guarda uma função que recebe uma string e retorna o estado de aplicar o parser nessa string de entrada. Vamos chamar esse estado de `ParserState`. Esse estado pode tanto representar um erro quanto o resultado do parser. Nesse caso, vamos representar o resultado com um tipo soma, que assume o valor `ParserError` na falha e o valor `ParserSuccess` no sucesso. Além disso, no caso de sucesso, precisamos guardar o resultado do parser, que será um valor do tipo `a`. Assim, temos que:

```hs
newtype Parser a = Parser {runParser :: String -> ParserState a}

data ParserState a
  = ParserError {_error :: ParserError, _input :: String}
  | ParserSuccess {_result :: a, _rest :: String}
  deriving (Show)
```

Precisamos que o tipo `Parser` seja paramétrico sobre `a` para que possamos repassar essa informação de tipo para o estado do parser. `ParserState` também é paramétrico sobre `a` para que possamos guardar o resultado do parser. Além disso, no caso de erro precisamos guardar uma mensagem de erro, que será um valor do tipo `ParserError`. Vamos definir um novo tipo soma para representar todos os possíveis erros que podem ocorrer durante o parsing:

```hs
data ParserError
  = UnexpectedCharacter Char
  | UnexpectedEndOfInput
  deriving (Show)
```

Agora, podemos criar nosso `charParser`, que recebe um caractere e retorna um parser que reconhece esse caractere:

```hs
charParser :: Char -> Parser Char
charParser expected = Parser $ \case
  (x : xs) ->
    if x == expected
      then ParserSuccess {_result = x, _rest = xs}
      else ParserError {_error = UnexpectedCharacter x, _input = xs}
  [] -> ParserError {_error = UnexpectedEndOfInput, _input = []}
```

Comparamos o primeiro caractere da entrada, com o caractere esperado. Se eles forem iguais, retornamos o caractere e o restante da entrada. Caso contrário, retornamos um erro. Se a entrada estiver vazia, retornamos um erro de fim de entrada.

Podemos testar o funcionamento desse parser:

```hs
charParser 'a' `runParser` "abc" -- ParserSuccess {_result = 'a', _rest = "bc"}
charParser 'a' `runParser` "bcd" -- ParserError {_error = UnexpectedCharacter 'b', _input = "cd"}
```

### Combinando parsers

Agora que já temos um parser, precisamos de formas de combiná-lo para criar parser mais complexos. Imagine que queiramos criar um parser que reconheça a string "abc". Podemos fazer isso combinando três parsers que reconhecem cada caractere individualmente aplicados de forma sequencial. Para isso, vamos criar uma função chamada `sequenceOf`:

```hs
sequenceOf :: [Parser a] -> Parser [a]
sequenceOf [] = Parser $ \input -> ParserSuccess {_result = [], _rest = input}
sequenceOf (p : ps) = Parser $ \input -> case runParser p input of
  ParserError e i -> ParserError e i
  ParserSuccess r rest' -> case runParser (sequenceOf ps) rest' of
    ParserError e i -> ParserError e i
    ParserSuccess rs rest'' -> ParserSuccess (r : rs) rest''
```

Se não passarmos nenhum parser, retornamos um resultado "vazio". Caso contrário, aplicamos o primeiro parser e, se ele for bem sucedido, aplicamos o restante dos parsers. Se algum dos parsers falhar, interrompemos a execução e retornamos o erro.

```hs
runParser (sequenceOf [charParser 'a', charParser 'b', charParser 'c']) "abc" -- ParserSuccess {_result = "abc", _rest = ""}
runParser (sequenceOf [charParser 'a', charParser 'b', charParser 'c']) "bcd" -- ParserError {_error = UnexpectedCharacter 'b', _input = "cd"}
```

Além de combinar parsers de forma sequencial, também precisamos de formas de combinar parsers de forma alternativa, ou seja, invés de retornar um erro se um parser falhar, tentamos o próximo parser até que um deles tenha sucesso ou todos falhem. Para isso, vamos criar uma função chamada `choice`:

```hs
choice :: [Parser a] -> Parser a
choice [] = Parser $ \input -> ParserError {_error = NoParsersToChooseFrom, _input = input}
choice (p : ps) = Parser $ \input -> case runParser p input of
  ParserError _ _ -> runParser (choice ps) input
  x -> x
```

Aqui, se não passarmos nenhum parser, retornamos um erro. Caso contrário, aplicamos o primeiro parser e, se ele falhar, aplicamos o restante dos parsers. Se todos os parsers falharem, retornamos o erro do último parser.

```hs
runParser (choice [charParser 'a', charParser 'b', charParser 'c']) "bcd" -- ParserSuccess {_result = 'b', _rest = "cd"}
runParser (choice [charParser 'a', charParser 'b', charParser 'c']) "def" -- ParserError {_error = NoParsersToChooseFrom, _input = "def"}
```

Além dessas duas operações, vamos precisar de uma forma de aplicar um mesmo parser repetidamente. Para isso, vamos criar duas funções `oneOrMore` e `zeroOrMore`, que aplicam um parser pelo menos uma vez e zero ou mais vezes, respectivamente:

```hs
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ _ -> ParserSuccess {_result = [], _rest = input}
  x -> case runParser (zeroOrMore p) (_rest x) of
    ParserError _ _ -> ParserSuccess {_result = [_result x], _rest = _rest x}
    ParserSuccess rs rest' -> ParserSuccess (_result x : rs) rest'

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \input -> case runParser p input of
  ParserError _ _ -> ParserError {_error = UnexpectedEndOfInput, _input = input}
  ParserSuccess r rest' -> case runParser (zeroOrMore p) rest' of
    ParserError _ _ -> ParserSuccess {_result = [r], _rest = rest'}
    ParserSuccess rs rest'' -> ParserSuccess (r : rs) rest''
```

Ambas as funções seguem lógicas muito parecidas. `zeroOrMore` tenta aplicar o parser e, se ele falhar, retorna uma lista vazia. Caso contrário, tenta aplicar o parser repetidamente até que ele falhe. `oneOrMore` tenta aplicar o parser e, se ele falhar, retorna um erro. Caso contrário, tenta aplicar o parser repetidamente (utilizando a função `zeroOrMore`) até que ele falhe.

### Implementando o parser para nossas expressões

Agora que já temos como combinar parser de formas diferentes, podemos implementar os parsers para nossas expressões. Vamos começar implementando o parser para números inteiros:

```hs
intParser :: Parser Expression
intParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
  ParserError _ _ -> ParserError {_error = FailedToParseInt, _input = input}
  ParserSuccess digits rest -> ParserSuccess {_result = ExprInt (read digits), _rest = rest}
```

A parte principal dessa função é a expressão `oneOrMore (choice (map charParser ['0'..'9']))`, pois é aqui que estamos criando a combinação de todos os parsers que reconhecem caracteres numéricos. Se aplicarmos esse parser em uma string que contém apenas números, ele irá retornar a expressão inteira correspondente. Caso contrário, ele retornará um erro.

```hs
runParser intParser "9849" -- ParserSuccess {_result = ExprInt 9849, _rest = ""}
runParser intParser "98.49" -- ParserSuccess {_result = ExprInt 98, _rest = ".49"}
runParser intParser "no" -- ParserError {_error = FailedToParseInt, _input = "no"}
```

O parser para números de ponto flutuante é bem parecido, mas agora também precisamos lidar com a parte decimal do número:

```hs
floatParser :: Parser Expression
floatParser = Parser $ \input -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) input of
  ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = input}
  ParserSuccess digits rest -> case runParser (charParser '.') rest of
    ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = rest}
    ParserSuccess _ rest' -> case runParser (oneOrMore (choice (map charParser ['0' .. '9']))) rest' of
      ParserError _ _ -> ParserError {_error = FailedToParseFloat, _input = rest'}
      ParserSuccess digits' rest'' -> ParserSuccess {_result = ExprFloat (read (digits ++ "." ++ digits')), _rest = rest''}
```

O código ficou um pouco mais complexo, mas a ideia é a mesma. Primeiro, tentamos reconhecer a parte inteira do número. Se tivermos sucesso, tentamos reconhecer o ponto decimal. Se tivermos sucesso, tentamos reconhecer a parte decimal do número. Se tivermos sucesso, retornamos o número de ponto flutuante correspondente. Caso contrário, retornamos um erro.

```hs
runParser floatParser "89.45" -- ParserSuccess {_result = ExprFloat 89.45, _rest = ""}
runParser floatParser "89" -- ParserError {_error = FailedToParseFloat, _input = ""}
runParser floatParser "89." -- ParserError {_error = FailedToParseFloat, _input = ""}
runParser floatParser "89.4a" -- ParserSuccess {_result = ExprFloat 89.4, _rest = "a"}
```

Para expressões booleanas, precisamos apenas reconhecer as palavras "true" e "false". Para isso, combinaremos, para cada palavra, um `charParser` para cada caractere sequencialmente e, depois, combinaremos os resultados de cada parser de forma alternativa:

```hs
boolParser :: Parser Expression
boolParser = Parser $ \input -> case runParser (choice [trueParser, falseParser]) input of
  ParserError _ _ -> ParserError {_error = FailedToParseBool, _input = input}
  ParserSuccess result rest -> ParserSuccess {_result = ExprBool (result == "true"), _rest = rest}
  where
    trueParser = sequenceOf (map charParser "true")
    falseParser = sequenceOf (map charParser "false")

runParser boolParser "true" -- ParserSuccess {_result = ExprBool True, _rest = ""}
runParser boolParser "false" -- ParserSuccess {_result = ExprBool False, _rest = ""}
runParser boolParser "tru" -- ParserError {_error = FailedToParseBool, _input = "tru"}
```

Agora que temos os parsers mais simples da nossa linguagem feitos, podemos implementar os parsers para as operações unárias, binárias e condicionais. Vamos começar com as operações binárias. Um parser para operação binária, `binaryExpressionParser`, é um parser que reconhece uma expressão, um operador binário e outra expressão de forma sequencial, retornando um erro caso qualquer etapa falhe. Para isso, vamos, primeiro, criar um parser que represente qualquer expressão da nossa linguagem:

```hs
expressionParser :: Parser Expression
expressionParser = choice [floatParser, intParser, boolParser, binaryExpressionParser]
```

Aqui, apenas combinamos de forma alternada todos os parsers que representam expressões da nossa linguagem. Note que a ordem dos parsers na lista é importante, pois o parser irá tentar aplicar os parsers na ordem em que eles aparecem na lista. Agora, podemos implementar o parser para operações binárias:

```hs
binaryExpressionParser :: Parser Expression
binaryExpressionParser = Parser $ \input -> case runParser expressionParser input of
  ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = input}
  ParserSuccess expr1 rest -> case runParser binaryOperator rest of
    ParserError _ _ -> ParserSuccess {_result = expr1, _rest = rest}
    ParserSuccess op rest' -> case runParser expressionParser rest' of
      ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = rest'}
      ParserSuccess expr2 rest'' -> ParserSuccess {_result = ExprBinary (fromString op) expr1 expr2, _rest = rest''}
  where
    stringParser = sequenceOf . map charParser
    binaryOperator = choice (map stringParser ["+", "-", "*", "/", "&&", "||", "==", "!=", "<", "<=", ">", ">="])
```

Esse parser é um pouco mais complexo, mas segue a mesma ideia dos outros. Primeiro tentamos reconhecer uma expressão. Se tivermos sucesso, tentamos reconhecer um operador binário. Se tivermos sucesso, tentamos reconhecer outra expressão. Se tivermos sucesso, retornamos a expressão binária correspondente. Caso contrário, retornamos um erro. Para reconhecer o operador binário, precisamos de mais duas funções `mapParser` e `fromString`. A primeira faz exatamente a mesma coisa que a função `map` do Haskell, mas aplicada a parsers. A segunda é uma função que transforma uma string em um valor do tipo `BinaryOp`:

```hs
mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f p = Parser $ \input -> case runParser p input of
  ParserError e i -> ParserError e i
  ParserSuccess r rest -> ParserSuccess (f r) rest

fromString :: String -> BinaryOp
fromString "+" = Add
fromString "-" = Sub
fromString "*" = Mul
fromString "/" = Div
fromString "&&" = And
fromString "||" = Or
fromString "==" = Eq
fromString "!=" = Neq
fromString "<" = Lt
fromString "<=" = Lte
fromString ">" = Gt
fromString ">=" = Gte
fromString _ = error "Invalid binary operator"
```

Se tentarmos aplicar esse parser em uma string que contém uma expressão binária válida, ele irá retornar um erro. Mas por quê? Nossa lógica está aparentemente correta. O problema é que não estamos levando em consideração os espaços em branco que podem existir entre os tokens da nossa linguagem. Para resolver isso, vamos criar uma função chamada `whitespaceParser` que reconhece um ou mais espaços em branco:

```hs
whitespaceParser :: Parser String
whitespaceParser = zeroOrMore (choice [charParser ' ', charParser '\n', charParser '\t'])
```

A função é bem simples, ela simplesmente aplica o parser de espaço em branco repetidamente até que ele falhe. Agora, podemos modificar o parser de expressões binárias para que ele ignore os espaços em branco entre os tokens:

```hs
binaryExpressionParser :: Parser Expression
binaryExpressionParser = Parser $ \input -> case runParser expressionParser (clean input) of
  ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = input}
  ParserSuccess expr1 rest -> case runParser binaryOperator (clean rest) of
    ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = clean rest}
    ParserSuccess op rest' -> case runParser expressionParser (clean rest') of
      ParserError _ _ -> ParserError {_error = FailedToParseExpression, _input = clean rest'}
      ParserSuccess expr2 rest'' -> ParserSuccess {_result = ExprBinary expr1 (fromString op) expr2, _rest = rest''}
  where
    stringParser = sequenceOf . map charParser
    binaryOperator = choice (map stringParser ["+", "-", "*", "/", "&&", "||", "==", "!=", "<", "<=", ">", ">="])
    clean val = case runParser whitespaceParser val of
      ParserError _ _ -> val
      ParserSuccess _ rest' -> rest'
```

Agora, temos essa função `clean` que recebe uma string e remove todos os espaços em branco precedentes. Usamos essa função antes de aplicar qualquer parser. Se tentarmos aplicar esse parser em uma string que contém uma expressão binária válida, ele irá retornar a expressão correspondente. Caso contrário, retornará um erro.

```hs
runParser binaryExpressionParser "5.67 + 9" -- ParserSuccess {_result = ExprBinary Add (ExprFloat 5.67) (ExprInt 9), _rest = ""}
runParser binaryExpressionParser "8" -- ParserError {_error = FailedToParseExpression, _input = ""}
```

Note que do jeito que implementamos as funções até agora, não conseguimos levar em consideração a precedência de operadores, nem a precedência das próprias construções da linguagem. Faremos isso mais a frente, quando facilitarmos como escrevemos os parsers.

### Deve haver outra forma

Se você prestou bastante atenção nos trechos de código até aqui, percebeu que estamos realizando o mesmo procedimento em vários lugares. Primeiro testamos um parser, se obtivermos sucesso, continuamos nossas operações. Caso obtivermos um erro, retornamos esse erro. Nos parsers mais simples, isso não estava sendo um problema, mas nos mais complexos, como o `binaryExpressionParser` isso se torna um problema. Na nossa implementação, ainda não estamos levando em conta a precedência dos operadores, ou seja, nosso código precisa ser ainda mais complexo, mas escrever parsers dessa forma é muito complicado e propenso a erros.

Felizmente, podemos usar de outra funcionalidade do Haskell para simplificar nossa vida: os `Type Classes`. Esse conceito nos permite definir comportamentos comuns para diferentes tipos de dados. Já usamos ao longo dos nossos exemplos a type class `Show`, que define o comportamento de tipos que podem ser convertidos para strings. No entanto, para nosso caso com os parsers, vamos precisar de type classes mais complexas: `Functor`, `Applicative` e `Monad`.

### Functor

Vamos começar pela mais fácil. A Type Class `Functor` define um comportamento comum para todos os tipos paramétricos que podem ser transformados. Analisando a sua assinatura, temos:

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Aqui, `f` é um tipo paramétrico, `a` e `b` são tipos quaisquer e `fmap` é uma função que transforma um valor do tipo `f a` em um valor do tipo `f b`. No nosso caso, `f` será o tipo `Parser`. Vamos implementar a instância de `Functor` para `Parser` e para `ParserState` (que também é um tipo paramétrico):

```hs
instance Functor Parser where
  fmap = mapParser

instance Functor ParserState where
  fmap _ (ParserError e i) = ParserError e i
  fmap f (ParserSuccess r rest) = ParserSuccess (f r) rest
```

Para o caso do `Parser`, a função `fmap` será a própria `mapParser` que já havíamos implementado antes. Para o caso do `ParserState` temos duas possibilidades: se o parser falhar, retornamos o erro; se o parser tiver sucesso, aplicamos a função `f` no resultado e retornamos o novo resultado.

### Applicative

O type class `Applicative` é muito parecido com o `Functor`, mas agora temos a possibilidade de aplicar funções que estão dentro do mesmo contexto que nossos valores. A assinatura da type class é a seguinte:

```hs
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Aqui, temos que para um tipo `f` ser uma instância de `Applicative`, ele também precisa ser uma instância de `Functor`. A função `pure` transforma um valor do tipo `a` em um valor do tipo `f a`. A função `<*>` aplica uma função que está dentro do contexto `f` em um valor que também está dentro do contexto `f`. Ou seja, a função `pure` tem como objetivo encapsular um valor dentro do contexto f, enquanto a função `<*>` tem como objetivo aplicar uma função que está dentro do contexto `f` em um valor que também está dentro do contexto `f`, retornando algo dentro do mesmo contexto `f`.

Aqui, contexto está sendo usado para se referir aos valores internos a qualquer tipo paramétrico, onde o contexto seria o próprio tipo paramétrico. Vamos implementar a instância de `Applicative` para `Parser` e para `ParserState`:

```hs
instance Applicative ParserState where
  pure x = ParserSuccess x []

  ParserError e i <*> _ = ParserError e i
  ParserSuccess f _ <*> px = fmap f px

instance Applicative Parser where
  pure x = Parser $ \input -> ParserSuccess x input

  pf <*> px = Parser $ \input -> case runParser pf input of
    ParserError e i -> ParserError e i
    ParserSuccess f rest -> runParser (fmap f px) rest
```

Para o `ParserState`, a função `pure` encapsula um valor dentro do contexto, ignorando o resto da entrada. O operador `<*>` tem dois casos. Se o primeiro estado for um erro, retornamos um erro. Caso contrário, aplicamos a função dentro do contexto ao segundo estado passado. Para o `Parser`, a função `pure` encapsula um valor dentro do contexto, mantendo a entrada intacta. O operador `<*>` tem dois casos. Se o primeiro parser falhar, retornamos um erro. Caso contrário, aplicamos a função do primeiro parser ao segundo parser.

Note que aqui, já estamos codificando a lógica sequencial de aplicação dos nossos parsers que discutimos anteriormente. No entanto, ainda falta mais uma type class para simplificar ainda mais a nossa vida.

### Monad

A type class `Monad` é a mais poderosa das três, pois ela nos permite encadear operações de forma sequencial e dependente. A assinatura da type class é a seguinte:

```hs
class (Applicative m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

Para implementar essa type class, precisamos fornecer uma definição para operação de bind `>>=`. A operação de bind é responsável por codificar a lógica do encadeamento de operações, onde cada operação depende do resultado da anterior.

Note que isso é diferente do comportamento do `Applicative`, no qual podemos aplicar operações de forma sequencial também, mas sem depender do resultado da operação anterior. A função `return` é responsável por encapsular um valor dentro do contexto `m`, mas não precisamos fornecer uma implementação. Vamos implementar a instância de `Monad` para `Parser` e para `ParserState`:

```hs
instance Monad ParserState where
  ParserError e i >>= _ = ParserError e i
  ParserSuccess r _ >>= f = f r

instance Monad Parser where
  p >>= f = Parser $ \input -> case runParser p input of
    ParserError e i -> ParserError e i
    ParserSuccess r rest -> runParser (f r) rest
```

As instâncias podem parecer pequenas, mas aqui está a mágica. Veja na implementação de `Monad` para `Parser`, por exemplo. Se o parser `p` falhar, retornamos o erro. Caso contrário, aplicamos a função `f` ao resultado do parser `p`. Isso nos permite escrever parsers complexos de forma muito mais simples. Vamos reescrever nossos parsers com essas novas definições:

```hs
intParser' :: Parser Expression
intParser' = ExprInt <$> fmap (read :: String -> Int) (oneOrMore (choice (map charParser ['0' .. '9'])))

floatParser' :: Parser Expression
floatParser' =
  ExprFloat <$> do
    digits <- oneOrMore (choice (map charParser ['0' .. '9']))
    _ <- charParser '.'
    digits' <- oneOrMore (choice (map charParser ['0' .. '9']))

    return (read (digits ++ "." ++ digits'))
```

Aqui, utilizamos a função `fmap` (`<$>`) para construir uma expressão a partir do resultado encapsulado em um `Parser` que reconhece um número inteiro ou de ponto flutuante. Note que a função `read` é aplicada ao resultado do parser que reconhece os dígitos. Isso é possível porque `Parser` é uma instância de `Monad`, `Applicative` e `Functor`.

Note também, que utilizamos a notação `do` para encadear operações de forma sequencial e dependente. Isso é possível porque `Parser` é uma instância de `Monad`. Essa notação é um açúcar sintático para a operação de bind `>>=`:

```hs
floatParser'' :: Parser Expression
floatParser'' =
  ExprFloat
    <$> ( oneOrMore (choice (map charParser ['0' .. '9']))
            >>= \digits ->
              charParser '.'
                >> ( oneOrMore (choice (map charParser ['0' .. '9']))
                       >>= \digits' -> return (read (digits ++ "." ++ digits'))
                   )
        )
```

Veja como usar a notação `do` é muito mais simples e legível do que usar a notação `>>=`.

### Alternative

Podemos fazer o mesmo para função `boolParser`, mas antes precisamos implementar uma outra instância para `Parser` e `ParserState`, o `Alternative`. A type class `Alternative` codifica a lógica de escolha entre dois valores, ou seja, a lógica de escolha entre dois parsers. A assinatura da type class é a seguinte:

```hs
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

Vamos implementar a instância de `Alternative` para `Parser` e para `ParserState`:

```hs
instance Alternative ParserState where
  empty = ParserError FailedToParseExpression ""

  ParserError _ _ <|> y = y
  x <|> _ = x

instance Alternative Parser where
  empty = Parser $ \input -> ParserError FailedToParseExpression input

  x <|> y = Parser $ \input -> case runParser x input of
    ParserError _ _ -> runParser y input
    x' -> x'

  many x = Parser $ \input -> case runParser x input of
    ParserError _ _ -> ParserSuccess [] input
    ParserSuccess r rest -> case runParser (many x) rest of
      ParserError _ _ -> ParserSuccess [r] rest
      ParserSuccess rs rest' -> ParserSuccess (r : rs) rest'

  some x = Parser $ \input -> case runParser x input of
    ParserError _ _ -> ParserError UnexpectedEndOfInput input
    ParserSuccess r rest -> case runParser (many x) rest of
      ParserError _ _ -> ParserSuccess [r] rest
      ParserSuccess rs rest' -> ParserSuccess (r : rs) rest'
```

Com essa nova definição, podemos implementar o parser para expressões booleanas de forma muito mais simples:

```hs
boolParser' :: Parser Expression
boolParser' = ExprBool <$> fmap (== "true") (sequenceOf (map charParser "true") <|> sequenceOf (map charParser "false"))
```

Além disso, podemos simplificar nossos combinators `sequenceOf`, `choice`, `zeroOrMore` e `oneOrMore`:

```hs
sequenceOf' :: [Parser a] -> Parser [a]
sequenceOf' = sequenceA

choice' :: [Parser a] -> Parser a
choice' = asum

zeroOrMore' :: Parser a -> Parser [a]
zeroOrMore' = many

oneOrMore' :: Parser a -> Parser [a]
oneOrMore' = some
```

Antes nossos combinators eram muito mais complexos, mas agora, com o uso de type classes, conseguimos simplificar bastante nossa implementação, uma vez que a lógica que repetíamos em vários lugares está encapsulada nas próprias definições das type classes.

### Implementando Parser para expressões unárias, binárias e condicionais

Agora que já podemos expressar parsers de forma mais simples, podemos implementar os parsers mais complexos da nossa linguagem: os parsers para expressões unárias e binárias e condicionais. Uma expressão unária é toda expressão da nossa linguagem que possui apenas um operador e um operando. Vamos começar implementando um parser para essa situação:

```hs
unaryParser :: Parser Expression
unaryParser = ExprUnary <$> unaryOperator <*> expressionParser
  where
    unaryOperator = choice' [Negate <$ charParser '-', Not <$ charParser '!']
```

Como comentado, uma expressão unária consiste em um operador seguido de um operando. Na nossa linguagem, o operador pode ser ou o caractere de negação (`-`) ou o caractere de negação lógica (`!`). O operando é qualquer expressão da nossa linguagem. Para implementar esse parser, utilizamos a função `choice'` para escolher entre os dois operadores possíveis e a função `expressionParser` para reconhecer o operando.

Vamos, agora, reimplementar o parser para expressões binárias. No entanto, dessa vez, vamos separar a lógica em casos diferentes, um para cada nível de precedência dos operadores (termos, fatores, condições lógicas e comparações):

```hs
binaryTermParser :: Parser Expression
binaryTermParser =
  ExprBinary <$> expressionParser <*> binaryOperator <*> expressionParser
  where
    binaryOperator = fromString <$> choice' (map stringParser ["+", "-"])
    stringParser = sequenceOf . map charParser

binaryFactorParser :: Parser Expression
binaryFactorParser =
  ExprBinary <$> expressionParser <*> binaryOperator <*> expressionParser
  where
    binaryOperator = fromString <$> choice' (map stringParser ["*", "/"])
    stringParser = sequenceOf . map charParser

binaryLogicParser :: Parser Expression
binaryLogicParser =
  ExprBinary <$> expressionParser <*> binaryOperator <*> expressionParser
  where
    binaryOperator = fromString <$> choice' (map stringParser ["&&", "||"])
    stringParser = sequenceOf . map charParser

binaryComparisonParser :: Parser Expression
binaryComparisonParser =
  ExprBinary <$> expressionParser <*> binaryOperator <*> expressionParser
  where
    binaryOperator = fromString <$> choice' (map stringParser ["==", "!=", "<", "<=", ">", ">="])
    stringParser = sequenceOf . map charParser

binaryParser :: Parser Expression
binaryParser = choice' [binaryTermParser, binaryFactorParser, binaryLogicParser, binaryComparisonParser]
```

Para facilitar nossa vida, vamos alterar a definição do nosso `expressionParser`:

```hs
expressionParser :: Parser Expression
expressionParser = whitespaceParser *> choice [floatParser, intParser, boolParser, unaryParser, binaryParser] <* whitespaceParser
```

Agora, aceitamos qualquer expressão envolta de espaços em branco, os quais ignoramos e retornamos apenas a expressão.
