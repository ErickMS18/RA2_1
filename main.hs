import qualified Data.Map as Map
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Control.Monad (foldM)

-------------------------------------------------
-- Dados
-------------------------------------------------

data Item = Item
    { itemID :: String
    , nome :: String
    , quantidade :: Int
    , categoria :: String
    } deriving (Show, Read, Eq)
    
data AcaoLog
    = Add Item
    | Remove String
    | Update Item
    | QueryFail String
    deriving (Show, Read, Eq)

data StatusLog
    = Sucesso String
    | Falha String    
    deriving (Show, Read, Eq)
    
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , acao :: AcaoLog
    , detalhes :: String
    , status :: StatusLog
    } deriving (Show, Read, Eq)
    
data Inventario = Inventario
    { itens :: Map.Map String Item
    } deriving (Show, Read, Eq)
    
    
    
-------------------------------------------------------------------------------------
-- Lógica
-------------------------------------------------------------------------------------

-- Definição de tipo ResultadoOperacao
-- O resultado de uma operação vai retornar o inventário e a entrada no log que descreve a operação

type ResultadoOperacao = (Inventario, LogEntry)


-- Adicionar item: cria um novo item no inventário
-- Recebe o horário, um item, e o inventário; retorna uma string em caso de erro ou ResultadoOperacao em caso de successo
-- Verifica se já existe um item no inventário com o ID fornecido, e retorna um erro caso já exista 
-- Verifica então a quantidade fornecida, retorna um erro se for negativa
-- Se tudo estiver certo, cria um novo mapa contendo o inventário com o novo item, e monta uma entrada log com as informações da ação

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem horario item (Inventario mapa)
    | Map.member (itemID item) mapa =
        Left "ERRO: o item ja existe no inventario"
    | quantidade item < 0 =
        Left "ERRO: a quantidade nao pode ser negativa"
    | otherwise =
        let novoMapa = Map.insert (itemID item) item mapa
            entradaLog = criarLog horario (Add item) ("Item adicionado: " ++ nome item) (Sucesso "Item adicionado com sucesso")
        in Right (Inventario novoMapa, entradaLog)


-- Remover item: remove um item completamente do inventário
-- Recebe horário, item, ID do item e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Verifica se existe um item no inventário com o ID fornecido, retorna erro caso não exista
-- Se tudo estiver certo, cria um novo mapa contendo o inventário sem o item escolhido, e monta uma entrada log com as informações da ação

removeItem :: UTCTime -> String -> Inventario -> Either String ResultadoOperacao
removeItem horario id (Inventario mapa) =
    case Map.lookup id mapa of
        Nothing ->
            Left "ERRO: item nao existe no inventario"
        Just item ->
            let novoMapa = Map.delete id mapa
                entradaLog = criarLog horario (Remove id) ("Item removido: " ++ nome item) (Sucesso "Item removido com sucesso")
            in Right (Inventario novoMapa, entradaLog)
            

-- Atualizar quantidade: muda o estoque do item no inventário
-- Recebe horário, ID do item (string), quantidade (int), e inventário; retorna string em caso de erro e ResultadoOperacao em caso de sucesso
-- Verifica a nova quantidade fornecida, retorna erro caso for negativa 
-- Então, procura um item no inventário com o ID fornecido, retorna erro se não for encontrado
-- Se tudo estiver certo, cria uma versão do item atualizado, um novo mapa contendo o inventário com o novo item, e monta uma entrada log com as informações da ação

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty horario id novaQtd (Inventario mapa)
    | novaQtd < 0 =
        Left "ERRO: quantidade nao pode ser negativa"
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "ERRO: item nao existe no inventario"
            Just item ->
                let itemAtualizado = item { quantidade = novaQtd }
                    novoMapa = Map.insert id itemAtualizado mapa
                    entradaLog = criarLog horario (Update itemAtualizado) ("Quantidade atualizada para " ++ show novaQtd ++ " em: " ++ nome item) (Sucesso "Quantidade atualizada com sucesso")
                in Right (Inventario novoMapa, entradaLog)


-- Vender item: reduz a quantide de um item (para cumprir com os requisitos)
-- Consideramos isso uma operação de atualizar a quantidade, portanto ela puxa updateQty e retorna a ação Update
-- Recebe horário, ID do item, quantidade e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Verifica a quantidade vendida, retorna um erro caso for igual ou menor do que zero
-- Depois, verifica se existe no inventário um item com o ID fornecido, retorna erro caso não for encontrado
-- Caso exista o item, extrai a quantidade atual e subtrai a quantidade a vender, retorna erro caso não houver eestoque o suficiente
-- Se tudo estiver certo, prossegue para updateQty

venderItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
venderItem horario id qtd (Inventario mapa)
    | qtd <= 0 =
        Left "ERRO: a quantidade vendida deve ser positiva."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "ERRO: item não encontrado no inventário."
            Just item ->
                let estoqueAtual = quantidade item
                    novaQtd = estoqueAtual - qtd
                in if novaQtd < 0
                    then Left "ERRO: estoque insuficiente para a venda."
                    else 
                        updateQty horario id novaQtd (Inventario mapa)



-- Função para popular o inventário ao iniciar o programa pela primeira vez.
inventarioInicial :: Inventario
inventarioInicial = Inventario (Map.fromList
    [ ("001", Item "001" "batata_frita" 10 "comida")
    , ("002", Item "002" "yoyo" 25 "brinquedo")
    , ("003", Item "003" "suco_de_goiaba" 5 "bebida")
    , ("004", Item "004" "chocolate_quente" 12 "bebida")
    , ("005", Item "005" "pudim" 8 "comida")
    , ("006", Item "006" "cha_de_camomila" 20 "bebida")
    , ("007", Item "007" "caderno" 15 "papelaria")
    , ("008", Item "008" "caneta_gel" 30 "papelaria")
    , ("009", Item "009" "urso_de_pelucia" 7 "brinquedo")
    , ("010", Item "010" "lapis_de_cor" 18 "papelaria")
    , ("011", Item "011" "bala_de_morango" 50 "comida")
    , ("012", Item "012" "sabonete_de_lavanda" 14 "higiene")
    , ("013", Item "013" "meias" 9 "vestuario")
    , ("014", Item "014" "marshmallow" 22 "comida")
    , ("015", Item "015" "livro_de_contos" 6 "livro")
    ])



-------------------------------------------------------------------------------------
-- Logs e relatórios
-------------------------------------------------------------------------------------

-- Filtra apenas as entradas onde o status é falha.
-- A função percorre todos os LogEntry e mantém somente aqueles
-- onde o status é falha (Falha _). Então são retornados apenas erros registrados no log.

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter (\entradaLog -> 
                        case status entradaLog of 
                            Falha _ -> True; 
                            _ -> False
                    )


-- Aqui são retornadas todas as entradas de log relacionadas a um item específico.
-- é verificado o campo "acao" de cada log,
-- no "Add", o log pertence ao item se itemID i for igual ao idItem, 
-- no "Remove", pertence se o ID removido for igual ao ID buscado,
-- no "Update", pertence se itemID i for igual ao idItem,
-- e no "QueryFail", como os erros não têm ID associado, procura o ID dentro da string `detalhes`,
-- assim, são retornados todas as ações ou erros que mencionam o item.

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem idItem =
    filter (\entrada -> case acao entrada of
        Add i       -> itemID i == idItem
        Remove iID  -> iID == idItem
        Update i    -> itemID i == idItem
        QueryFail _ -> ("ID: " ++ idItem) `isInfixOf` detalhes entrada
        )


-- Aqui é determinado qual item aparece mais vezes nos logs.
-- Para cada LogEntry, um nome é extraído,
-- para o "Add" e o "Update" é usado o nome i, para o "Remove",
-- ele tenta descobrir o nome procurando um Add anterior com o mesmo ID e se não encontrar
-- marca como "<desconhecido>".
-- Depois disso são contadas quantas vezes cada nome aparece usando "Map.fromListWith (+)".
-- Então o item com maior frequência é considerado o item mais movimentado.

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado [] = Nothing
itemMaisMovimentado logs =
    let nomes = [ case acao entradaLog of
    
                    Add item      -> nome item
                    
                    Remove id  ->
                        case [nome item | LogEntry _ (Add item) _ _ <- logs, itemID item == id] of
                            (n:_) -> n  -- retorna o primeiro elemento da lista gerada
                            []    -> "<desconhecido>"
                            
                    Update item   -> nome item
                    
                    _          -> "" | entradaLog <- logs ]

        contagem = Map.fromListWith (+) [(n, 1) | n <- nomes, n /= ""]

    in if Map.null contagem
        then Nothing
        else
            let lista = Map.toList contagem
            
                -- foldr1 reduz a lista a um único elemento usando uma função, começando da direita, sem necessidade de especificar um elemento de início
                -- No caso, seleciona a tupla cujo segundo elemento (indicado por "snd") é maior, até que encontra a tupla com o maior valor
                -- Ao final, guarda a chave (nome do item) e descarta o valor
                -- Como ele começa da direita, caso tenha um empate, ele guardará o item cujo nome vem depois em ordem alfabética (por isso para o inventário inicial sempre será o yoyo)
                (itemMax, _) = foldr1 (\x y -> if snd x > snd y then x else y) lista
            in Just itemMax
            
            
-- Função auxiliar que cria uma entrada de log (para o código main ficar mais limpo)
criarLog :: UTCTime -> AcaoLog -> String -> StatusLog -> LogEntry
criarLog horario acao descricao status =
    LogEntry
        { timestamp = horario
        , acao = acao
        , detalhes = descricao
        , status = status
        }


-- Função auxiliar para imprimir entradas de log de forma mais legível
-- "unwords" serve para juntar strings de uma lista em uma única string
imprimirLogEntry :: LogEntry -> String
imprimirLogEntry (LogEntry timestamp acao detalhes status) =
    unwords
        [ show timestamp
        , "|"
        , imprimirAcao acao
        , "|"
        , detalhes
        , "|"
        , show(status)
        ]

-- Função auxiliar para imprimir ações de forma mais legível
-- Aqui as strings são juntadas por ++ para a formatação ficar melhor, sem espaços a mais
imprimirAcao :: AcaoLog -> String
imprimirAcao (Add item) = "Adição (ID: " ++ itemID item ++ ", nome: " ++ nome item ++ ", quantidade: " ++ show (quantidade item) ++ ", categoria: " ++ categoria item ++ ")"
imprimirAcao (Remove id) = "Remoção (ID: " ++ id ++ ")"
imprimirAcao (Update item) = "Atualização (ID: " ++ itemID item ++ ", nome: " ++ nome item ++ ", quantidade: " ++ show (quantidade item) ++ ", categoria: " ++ categoria item ++ ")"
imprimirAcao (QueryFail msg) = "Erro: " ++ msg



-------------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------------


main :: IO ()
main = do
    putStrLn "=== Sistema de Inventário ==="

    -- Tenta ler o inventário salvo no arquivo, se existir;
    -- caso contrário, inicia com o inventário definido pela função inventarioInicial,
    -- cria um aquivo novo e grava o inventário inicial
    
    conteudoInventario <- catch (readFile "Inventario.dat") 
                                (\(_ :: IOException) -> do
                                
                                    let Inventario mapaInicial = inventarioInicial
                                    let itensIniciais = Map.elems mapaInicial
                                    horario <- getCurrentTime
                                    
                                    -- Insere todos os itens usando addItem para gerar os logs corretamente
                                    -- Usa foldM para ir acumulando itens ao inventario e logs à lista de logs
                                    -- Precisamos usar foldM porque o addItem retorna Either, que é um monad
                                    (inventario, logs) <- foldM
                                        (\(invAtual, logsAtuais) item ->
                                            case addItem horario item invAtual of
                                                Left _ -> return (invAtual, logsAtuais)     -- caso houve um erro, retorna o estado atual
                                                Right (novoInv, entradaLog) ->
                                                    return (novoInv, logsAtuais ++ [entradaLog])   -- retorna o novo inventário e adiciona a entrada nova aos logs
                                        )
                                        (Inventario Map.empty, [])
                                        itensIniciais
                                    
                                    -- Grava o inventario com os itens iniciais ao arquivo
                                    writeFile "Inventario.dat" (show inventario)
                                    
                                    -- Grava os logs no arquivo de logs
                                    mapM_ (\entradaLog -> appendFile "Auditoria.log" (show entradaLog ++ "\n")) logs
                                    
                                    return (show inventario))
                                
    -- conteudoInventario armazena uma string, então precisa ser transformado em um objeto Inventario
    let inventario = read conteudoInventario :: Inventario

    -- Garante que o arquivo de log exista
    -- Caso não encontre o arquivo, cria um novo arquivo vazio
    existe <- doesFileExist "Auditoria.log"
    if not existe then writeFile "Auditoria.log" "" else return ()

    -- Inicia o loop principal
    loop inventario


loop :: Inventario -> IO ()
loop inventario = do
    putStrLn "\n=== MENU ==="
    putStrLn "Escolha uma opção: "
    putStrLn "1 - Adicionar item"
    putStrLn "2 - Remover item"
    putStrLn "3 - Atualizar quantidade"
    putStrLn "4 - Report"
    putStrLn "5 - Listar itens do inventário"
    putStrLn "6 - Vender item"
    putStrLn "0 - Sair"
    
    opcao <- getLine
    case opcao of
    
    
-- No 1, 2 e 3 a quantidade (ou outro campo como ID, nova quantidade) está sendo tratada para aceitar apenas um numeral,
-- evitando que o programa quebre ao usar read em valores inválidos.
-- Uma mensagem de erro é mostrada e o loop é reiniciado
    
        "1" -> do
            putStrLn "\nDigite: id nome quantidade categoria"
            putStrLn "(Atenção: use _ no lugar de espaços e digite números para id e quantidade)"
            entrada <- getLine
            let elementos = words entrada      -- "words" divide a entrada pelos espaços em uma lista de valores
            case elementos of
                (id:nome:qtd:cat:_)     -- verifica se existe pelo menos 4 elementos e os liga a identificadores de cada valor, descartando qualquer valor a mais
                    
                    -- Verifica se a entrada é um valor numérico:
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") qtd -> do
                        horario <- getCurrentTime
                        let item = Item id nome (read qtd) cat
                        case addItem horario item inventario of
                        
                            Left erro -> do
                                let logErro = criarLog horario (QueryFail "Erro ao adicionar item") ("ID: " ++ id ++ ", Nome: " ++ nome) (Falha erro)
                                appendFile "Auditoria.log" (show logErro ++ "\n")
                                putStrLn erro      -- Imprime o erro
                                loop inventario
                            
                            Right (novo, entradaLog) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn "Item adicionado com sucesso"
                                loop novo

                        
                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id nome quantidade categoria"
                    putStrLn "Exemplo: 3 batata_frita 90 comida"
                    loop inventario
                    
        "2" -> do
            putStrLn "\nDigite: id"
            putStrLn "(Atenção: o id deve ser numérico)"
            entrada <- getLine
        
            -- Verifica se a entrada é um valor numérico:
            if all (`elem` "0123456789") entrada then do
                horario <- getCurrentTime
        
                case removeItem horario entrada inventario of
                    Left erro -> do
                        let logErro = criarLog horario (QueryFail "Erro ao remover item") ("ID: " ++ entrada) (Falha erro)
                        appendFile "Auditoria.log" (show logErro ++ "\n")
                        putStrLn erro      -- Imprime o erro
                        loop inventario
        
                    Right (novo, entradaLog) -> do
                        writeFile "Inventario.dat" (show novo)
                        appendFile "Auditoria.log" (show entradaLog ++ "\n")
                        putStrLn "Item removido com sucesso"
                        loop novo
        
            else do
                putStrLn "\nEntrada inválida. O ID deve conter apenas números."
                loop inventario

        "3" -> do
            putStrLn "\nDigite: id novaQuantidade"
            putStrLn "(Atenção: digite nùmeros para id e quantidade)"
            entrada <- getLine
            let elementos = words entrada
            case elementos of
                [id, novaQtd]
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") novaQtd -> do
                        horario <- getCurrentTime
                        let novaQtdNum = read novaQtd
                        
                        case updateQty horario id novaQtdNum inventario of
                            Left erro -> do
                                let logErro = criarLog horario (QueryFail "Erro ao alterar quantidade") ("ID: " ++ id ++ ", quantidade: " ++ novaQtd) (Falha erro)
                                appendFile "Auditoria.log" (show logErro ++ "\n")
                                putStrLn erro      -- Imprime o erro
                                loop inventario
                        
                            Right (novo, entradaLog) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn "Quantidade atualizada com sucesso"
                                loop novo

                        
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id novaQuantidade"
                    putStrLn "Exemplo: 2 10"
                    loop inventario

        "4" -> do
            conteudo <- readFile "Auditoria.log"
            
            -- Divide o conteudo do arquivo log em linhas e converte cada linha de string para LogEntry
            let logs = map read (lines conteudo) :: [LogEntry]

            putStrLn "\n--- Logs de Erro ---"
            -- Imprime cada entrada "entradaLog" na lista "logs" 
            mapM_ (\entradaLog -> putStrLn (imprimirLogEntry entradaLog)) (logsDeErro logs)

            putStrLn "\n--- Item mais movimentado ---"
            case itemMaisMovimentado logs of
                Nothing -> putStrLn "Nenhum item movimentado ainda"
                Just nomeItem -> putStrLn nomeItem

            putStrLn "\nDigite o ID do item para ver o histórico:"
            idItem <- getLine
            let historico = historicoPorItem idItem logs
            if null historico
                then putStrLn "Nenhum histórico encontrado para esse item"
                else do
                    putStrLn ("\n--- Histórico do item " ++ idItem ++ " ---")
                    mapM_ (putStrLn . imprimirLogEntry) historico

            loop inventario



        "5" -> do
            putStrLn "\n--- Itens no Inventário ---"
            if Map.null (itens inventario)
                then putStrLn "Inventário vazio"
                else mapM_ (\(itemID, item) ->
                    putStrLn (itemID ++ ": " ++ nome item ++ " | Quantidade: " ++ show (quantidade item) ++ " | Categoria: " ++ categoria item)
                    ) (Map.toList (itens inventario))
            loop inventario
            
        "6" -> do
            putStrLn "\nDigite: id quantidade_vendida"
            putStrLn "(Atenção: digite números para id e quantidade)"
            entrada <- getLine
            let elementos = words entrada
            case elementos of
                [id, qtd]
                    | all (`elem` "0123456789") id && all (`elem` "0123456789") qtd -> do
                        tempo <- getCurrentTime
                        let qtdNum = read qtd
                        case venderItem tempo id qtdNum inventario of
                            Left erro -> do
                                let entradaLog = LogEntry tempo (QueryFail "Erro ao vender item")
                                                    ("ID: " ++ id)
                                                    (Falha erro)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn erro      -- Imprime o erro
                                loop inventario
                            Right (novo, entradaLog) -> do
                                writeFile "Inventario.dat" (show novo)
                                appendFile "Auditoria.log" (show entradaLog ++ "\n")
                                putStrLn "Venda registrada com sucesso."
                                loop novo
                _ -> do
                    putStrLn "\nEntrada inválida. Use o formato: id quantidade_vendida"
                    putStrLn "Exemplo: 3 5"
                    loop inventario

        "0" -> putStrLn "Encerrando o programa"
        _   -> putStrLn "Opção inválida" >> loop inventario
        
        
