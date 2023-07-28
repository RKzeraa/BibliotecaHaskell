import Data.List (nub)

type Nome = String

type Telefone = Int

data Pessoa = P Nome Telefone deriving (Show)

instance Eq Pessoa where
  P n1 t1 == P n2 t2 = n1 == n2 && t1 == t2

type Edicao = Int

data Livro = L Nome Edicao deriving (Show)

instance Eq Livro where
  L n1 e1 == L n2 e2 = n1 == n2 && e1 == e2

type Biblioteca = ([Pessoa], [Livro], [(Pessoa, Livro)])

pessoas0 :: [Pessoa]
pessoas0 = [P "Leandro" 12345678, P "Joabe" 45678910, P "Lucas" 96874343, P "Sidney" 93443234]

disponiveis0 :: [Livro]
disponiveis0 = [L "Java" 3, L "Concorrencia" 5]

emprestimos0 :: [(Pessoa, Livro)]
emprestimos0 = [(P "Leandro" 12345678, L "Java" 2), (P "Joabe" 45678910, L "CSP" 3), (P "Lucas" 96874343, L "UML" 4), (P "Lucas" 96874343, L "Haskell" 4), (P "Sidney" 93443234, L "CSP" 3)]

bancoDados0 :: Biblioteca
bancoDados0 = (pessoas0, disponiveis0, emprestimos0)

-- retorna as pessoas que possuem ao menos um empréstimo
pessoasComEmprestimo :: Biblioteca -> [Pessoa]
pessoasComEmprestimo (lp, ll, le) = nub (aux le)
  where
    aux :: [(Pessoa, Livro)] -> [Pessoa]
    aux [] = []
    aux ((p, l) : calda) = p : aux calda

-- retorna o nome de todos os livros (emprestados ou disponíveis)
-- sem repetição
-- pode usar a função nub de Data.List que remove repetição
-- [Entrada] -> livros bancoDados0
-- [Saida] -> ["Java","Concorrencia","CSP","UML","Haskell"]
livros :: Biblioteca -> [Nome]
livros (_, livrosDisponiveis, livrosEmprestados) = nub [nome | L nome _ <- livrosDisponiveis ++ map snd livrosEmprestados]

-- quantidade total (disponível e emprestado) de um livro
-- a contagem considera todas as edições
-- retorna 0 se livro não existe
-- [Entrada] -> qtdExemplares bancoDados0 "CSP"
-- [Saida] -> 2
qtdExemplares :: Biblioteca -> Nome -> Int
qtdExemplares (_, livrosDisponiveis, livrosEmprestados) nome = length [nomeLivro | L nomeLivro _ <- livrosDisponiveis ++ map snd livrosEmprestados, nomeLivro == nome]

-- retorna a quantidade disponível de um livro para
-- empréstimo
-- [Entrada] -> livroDisponivel bancoDados0 "Java"
-- [Saida] -> 1
livroDisponivel :: Biblioteca -> Nome -> Int
livroDisponivel (_, livrosDisponiveis, _) nome = length [nomeLivro | L nomeLivro _ <- livrosDisponiveis, nomeLivro == nome]

-- retorna true se o usuário está apto a pegar um livro emprestado
-- [Entrada] -> usuarioApto bancoDados0 12345678
-- [Saida] -> True
usuarioApto :: Biblioteca -> Telefone -> Bool
usuarioApto (usuarios, _, _) telefone = any (\(P _ tel) -> tel == telefone) usuarios

main :: IO ()
main = do
  putStrLn "Lista de pessoas com emprestimos sem repetição:"
  print (pessoasComEmprestimo bancoDados0)

  putStrLn "Lista de livros sem repetição:"
  print (livros bancoDados0)

  putStrLn "Quantidade total para o livro 'CSP':"
  print (qtdExemplares bancoDados0 "CSP")

  putStrLn "Quantidade de livros 'Java' disponiveis:"
  print (livroDisponivel bancoDados0 "Java")

  putStrLn "O usuário cujo telefone é '12345678' está apto a pegar um livro emprestado?"
  print (usuarioApto bancoDados0 12345678)