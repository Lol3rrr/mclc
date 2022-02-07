import System.IO
import System.Environment
import Control.Monad
import Data.Maybe
import Data.List.Split

import qualified Tokenizer
import qualified Parser
import qualified Semantics
import qualified Synthesis

data ExecType = Synth | Test | Check deriving Show;
data Arguments = File String | ExecutionType ExecType deriving Show;

data ExecResult = Checked | Tested | Synthesised deriving Show;

main = do
        raw_args <- getArgs
        let args = parseArguments raw_args
        let file_path = fromJust (getFileArgument args)
        handle <- openFile file_path ReadMode
        contents <- hGetContents handle
        let tokens = Tokenizer.tokenize contents
        let raw_top_level = parseTokens tokens
        let entites = parseEntities (getEntities raw_top_level)
        exec_res <- execute (getExecTypeArgument args) entites
        print exec_res
        hClose handle

parseArguments :: [String] -> [Arguments]
parseArguments [] = []
parseArguments ("-i":file:rest) = (File file) : (parseArguments rest)
parseArguments ("-t":"test":rest) = (ExecutionType Test) : (parseArguments rest)
parseArguments ("-t":"check":rest) = (ExecutionType Check) : (parseArguments rest)
parseArguments ("-t":"synth":rest) = (ExecutionType Synth) : (parseArguments rest)
parseArguments ("-t":other:rest) = error ("Unknown Execution Type: " ++ (show other))
parseArguments other = error ("Unknown Arguments: " ++ (show other))

getFileArgument :: [Arguments] -> Maybe String
getFileArgument [] = Nothing
getFileArgument ((File path):rest) = Just path
getFileArgument (x:rest) = getFileArgument rest

getExecTypeArgument :: [Arguments] -> ExecType
getExecTypeArgument [] = Check
getExecTypeArgument ((ExecutionType ty):rest) = ty
getExecTypeArgument (x:rest) = getExecTypeArgument rest

parseTokens :: [Tokenizer.Token] -> [Parser.TopLevel]
parseTokens tokens = case (Parser.parse tokens) of
  Parser.Ok v -> v
  Parser.Error err err_toks -> error ((show err) ++ (show err_toks))

parseEntities :: [Parser.Entity] -> [Semantics.Entity]
parseEntities raw_entitys = case (Semantics.parseEntities raw_entitys) of
  Semantics.Ok v -> v
  Semantics.Error err -> error (show err)

getEntities :: [Parser.TopLevel] -> [Parser.Entity]
getEntities [] = []
getEntities ((Parser.ParsedEntity e):rest) = e : (getEntities rest)

execute :: ExecType -> [Semantics.Entity] -> IO ExecResult
execute Check _ = return Checked
execute Test _ = error "Testing Circuit"
execute Synth (target:entites)
  = do
      writeFile "file.svg" (show (Synthesis.generateSpaceImage (Synthesis.unwrap synthesis)))
      writeFile "blocks.txt" (foldl1 (++) (map (\block -> (show block) ++ "\n") (Synthesis.blockPlacement (Synthesis.unwrap synthesis))))
      writeFile "command.txt" (foldl1 (\x y -> x ++ "\n" ++ y) (generateCommands (Synthesis.blockPlacement (Synthesis.unwrap synthesis))))
      return Synthesised
  where synthesis = Synthesis.synthesis entites target

generateCommands :: [Synthesis.MinecraftBlock] -> [String]
generateCommands blocks
  = map finalCommand final_commands
  where place_commands = map blockPlaceCommand blocks
        final_command_lists = chunksOf 400 place_commands
        final_commands = map (foldl1 (\x y -> x ++ y)) final_command_lists

finalCommand :: String -> String
finalCommand inner = "summon falling_block ~ ~1 ~ {Time:1,BlockState:{Name:redstone_block},Passengers:[\
  \{id:falling_block,Passengers:[\
  \{id:falling_block,Time:1,BlockState:{Name:activator_rail},Passengers:[\
  \{id:command_block_minecart,Command:'gamerule commandBlockOutput false'}," ++ inner ++ "\
  \{id:command_block_minecart,Command:'setblock ~ ~1 ~ command_block{auto:1,Command:\"fill ~ ~ ~ ~ ~-3 ~ air\"}'},\
  \{id:command_block_minecart,Command:'kill @e[type=command_block_minecart,distance=..1]'}]}]}]}"

blockPlaceCommand :: Synthesis.MinecraftBlock -> String
blockPlaceCommand current
  = "{id:command_block_minecart,Command:'" ++ place_cmd ++ "'},"
  where place_cmd = placeBlockCmd current

placeBlockCmd :: Synthesis.MinecraftBlock -> String
placeBlockCmd (Synthesis.MinecraftBlock (pos, Synthesis.Stone))
  = "setblock " ++ (cmdCordOffsets pos) ++ " stone"
placeBlockCmd (Synthesis.MinecraftBlock (pos, Synthesis.RedstoneDust))
  = "setblock " ++ (cmdCordOffsets pos) ++ " redstone_wire"
placeBlockCmd (Synthesis.MinecraftBlock (pos, Synthesis.RedstoneTorch (Just orient)))
  = "setblock " ++ (cmdCordOffsets pos) ++ " redstone_wall_torch[facing=" ++ (cmdOrientation orient) ++ "]"
placeBlockCmd (Synthesis.MinecraftBlock (pos, Synthesis.RedstoneRepeater orient))
  = "setblock " ++ (cmdCordOffsets pos) ++ " repeater[facing=" ++ (cmdOrientation orient) ++ "]"
placeBlockCmd (Synthesis.MinecraftBlock (pos, Synthesis.RedstoneComparitor orient True))
  = "setblock " ++ (cmdCordOffsets pos) ++ " comparator[facing=" ++ (cmdOrientation orient) ++ ",mode=subtract]"
placeBlockCmd (Synthesis.MinecraftBlock ((x, y, z), ty)) = error (show ty)

cmdCordOffsets :: (Int, Int, Int) -> String
cmdCordOffsets (x, y, z) = "~" ++ (show (-x)) ++ " ~" ++ (show (-z)) ++ " ~" ++ (show (-y))

cmdOrientation :: Synthesis.Orientation -> String
cmdOrientation Synthesis.East = "east"
cmdOrientation Synthesis.West = "west"