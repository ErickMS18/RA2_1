import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Maybe (isJust)


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

-- Definição de tipo
-- O resultado de uma operação vai retornar o inventário e a entrada no log que descreve a operação
type ResultadoOperacao = (Inventario, LogEntry)

-- Adicionar item
-- Recebe o horário, um item, e o inventário; retorna uma string em caso de erro ou ResultadoOperacao em caso de successo
-- Retorna erro caso um item com o mesmo ID já existe no inventário ou se a quantidade é negativa
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem time item (Inventario mapa)
    | Map.member (itemID item) mapa =
        Left "Erro: item já existe no inventário."
    | quantidade item < 0 =
        Left "Erro: quantidade inicial não pode ser negativa."
    | otherwise =
        let novoMapa = Map.insert (itemID item) item mapa
            entradaLog = LogEntry time
                            (Add item)
                            ("Item adicionado: " ++ nome item)
                            (Sucesso "Operação concluída com sucesso.")
        in Right (Inventario novoMapa, entradaLog)


-- Remover item
-- Recebe horário, item, quantidade (int) e inventário; retorna string em caso de erro ou ResultadoOperacao em caso de sucesso
-- Retorna erro se a quantidade for zero ou negativa, se o item não existe no inventário, ou se a quantidade removida for maior do que o estoque
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time id qtd (Inventario mapa)
    | qtd <= 0 =
        Left "Erro: a quantidade removida deve ser positiva."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "Erro: item não encontrado no inventário."
            Just item
                | quantidade item < qtd ->
                    Left "Erro: estoque insuficiente."
                | otherwise ->
                    let novaQtd = quantidade item - qtd
                        novoMapa =
                            if novaQtd == 0
                                then Map.delete id mapa
                                else Map.insert id (item { quantidade = novaQtd }) mapa
                        logE = LogEntry time
                                        (Remove id)
                                        ("Removidas " ++ show qtd ++ " unidades de " ++ nome item)
                                        (Sucesso "Remoção concluída com sucesso.")
                    in Right (Inventario novoMapa, logE)


-- Atualizar quantidade
-- Recebe horário, ID do item (string), quantidade (int), e inventário; retorna string em caso de erro e ResultadoOperacao em caso de sucesso
-- Retorna erro se quantidade for negativa ou se o item não for encontrado
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty time id novaQtd (Inventario mapa)
    | novaQtd < 0 =
        Left "Erro: quantidade não pode ser negativa."
    | otherwise =
        case Map.lookup id mapa of
            Nothing ->
                Left "Erro: item não encontrado para atualização."
            Just item ->
                let itemAtualizado = item { quantidade = novaQtd }
                    novoMapa = Map.insert id itemAtualizado mapa
                    logE = LogEntry time
                                    (Update itemAtualizado)
                                    ("Quantidade atualizada para " ++ show novaQtd ++ " em " ++ nome item)
                                    (Sucesso "Atualização concluída com sucesso.")
                in Right (Inventario novoMapa, logE)




--------------------------------------------------------------------------------------
-- I/O
--------------------------------------------------------------------------------------

{- 
Teste da serialização (read.show)
Pra testar, tira o trecho abaixo de comentario e roda o código

main :: IO ()
main = do
    let item = Item "10" "Suco de Goiaba" 3 "Consumivel"
    let itemSerializado = show item
    putStrLn ("Serializado: " ++ itemSerializado)

    let itemDesserializado = read itemSerializado :: Item
    putStrLn ("Desserializado: " ++ show itemDesserializado)

    putStrLn ("São iguais? " ++ show (item == itemDesserializado))
-}

main :: IO ()
main = do
    -- Criando um inventário de exemplo
    let inv = Inventario (Map.fromList
            [ ("1", Item "1" "Suco de Goiaba" 5 "Consumível")
            , ("2", Item "2" "Colar de Santo" 1 "Amuleto")
            ])

    -- SALVAR o inventário no disco
    writeFile "inventario.txt" (show inv)
    putStrLn "Inventário salvo no arquivo inventario.txt"

    -- CARREGAR o inventário do disco
    conteudo <- readFile "inventario.txt"
    let invCarregado = read conteudo :: Inventario

    putStrLn "\nInventário carregado do arquivo:"
    print invCarregado
