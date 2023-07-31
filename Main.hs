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

-- livros que estao com uma pessoa cujo telefone é fornecido
-- lista retornada é vazia quando usuário não possui o livro
-- ou quando o telefone não está cadastrado
-- Retorna os livros emprestados por uma pessoa específica
-- [Entrada] -> livrosPessoa bancoDados0 96874343
-- [Saida] -> [L "UML" 4,L "Haskell" 4]
livrosPessoa :: Biblioteca -> Telefone -> [Livro]
livrosPessoa (_, _, livrosEmprestados) telefone = [livro | (P _ tel, livro) <- livrosEmprestados, tel == telefone]

-- retorna uma lista de pares com o livro no primeiro elemento
-- e a lista de pessoas que estão com o livro emprestado
-- distingue a edição do livro
-- [Entrada] -> emprestimos bancoDados0
-- [Saida] -> [(L "Java" 2,[P "Leandro" 12345678]),(L "CSP" 3,[P "Joabe" 45678910,P "Sidney" 93443234]),(L "UML" 4,[P "Lucas" 96874343]),(L "Haskell" 4,[P "Lucas" 96874343])]
emprestimos :: Biblioteca -> [(Livro, [Pessoa])]
emprestimos (_, _, livrosEmprestados) =
  let livrosUnicos = nub (map snd livrosEmprestados)
   in [(livro, pessoasPorLivro livro) | livro <- livrosUnicos]
  where
    pessoasPorLivro livro = [pessoa | (pessoa, l) <- livrosEmprestados, l == livro]

-- se livro está disponível, e se pessoa está cadastrada,
-- livro sai da lista de disponível e entra na lista de
-- empréstimo
-- caso contrário, a biblioteca é retornada inalterada
-- [Entrada] -> (emprestar bancoDados0 (P "Joabe" 45678910) (L "Java" 3)
-- [Saida] -> ([P "Leandro" 12345678,P "Joabe" 45678910,P "Lucas" 96874343,P "Sidney" 93443234],[L "Concorrencia" 5],[(P "Joabe" 45678910,L "Java" 3),(P "Leandro" 12345678,L "Java" 2),(P "Joabe" 45678910,L "CSP" 3),(P "Lucas" 96874343,L "UML" 4),(P "Lucas" 96874343,L "Haskell" 4),(P "Sidney" 93443234,L "CSP" 3)])
emprestar :: Biblioteca -> Pessoa -> Livro -> Biblioteca
emprestar (pessoas, livrosDisponiveis, livrosEmprestados) pessoa livro
  | pessoa `elem` pessoas = (pessoas, novoLivrosDisponiveis, novoLivrosEmprestados)
  | otherwise = (pessoas, livrosDisponiveis, livrosEmprestados)
  where
    novoLivrosDisponiveis = filter (/= livro) livrosDisponiveis
    novoLivrosEmprestados = (pessoa, livro) : livrosEmprestados

-- se livro está emprestado, retorna a biblioteca com
-- o livro devolvido na lista de livros disponíveis
-- caso contrário, a biblioteca é retornada inalterada
-- [Entrada] -> devolver bancoDados0 12345678 "Java" 3
-- [Saida] -> ([P "Leandro" 12345678,P "Joabe" 45678910,P "Lucas" 96874343,P "Sidney" 93443234],[L "Java" 3,L "Java" 3,L "Concorrencia" 5],[(P "Leandro" 12345678,L "Java" 2),(P "Joabe" 45678910,L "CSP" 3),(P "Lucas" 96874343,L "UML" 4),(P "Lucas" 96874343,L "Haskell" 4),(P "Sidney" 93443234,L "CSP" 3)])
devolver :: Biblioteca -> Telefone -> Nome -> Edicao -> Biblioteca
devolver (pessoas, livrosDisponiveis, livrosEmprestados) telefone nomeLivro edicaoLivro =
  (pessoas, novoLivrosDisponiveis, novoLivrosEmprestados)
  where
    novoLivrosDisponiveis = L nomeLivro edicaoLivro : livrosDisponiveis
    novoLivrosEmprestados = filter (\(P _ telefone, L nome edicao) -> nome /= nomeLivro || edicao /= edicaoLivro) livrosEmprestados

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

  putStrLn "Os livros que estão com usuário cujo telefone é '96874343':"
  print (livrosPessoa bancoDados0 96874343)

  putStrLn "Os livros que estão emprestados:"
  print (emprestimos bancoDados0)

  putStrLn "O usuário 'Joabe' pegou emprestimo do livro 'Java' Ed.'3':"
  print (emprestar bancoDados0 (P "Joabe" 45678910) (L "Java" 3))

  putStrLn "O usuário 'Leandro' devolveu o emprestimo do livro 'Java' Ed.'3':"
  print (devolver bancoDados0 12345678 "Java" 3)