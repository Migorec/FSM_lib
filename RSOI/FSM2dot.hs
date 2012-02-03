module Main where

import System.Environment
import System.Console.GetOpt
import Language.Haskell.Exts


data Options = Options {
        optOutput :: String, 
        optInput :: String
    }
    
options :: [OptDescr (Options -> IO Options)]    
options = [
    Option ['o'] ["output"] (ReqArg writeOutput "FILE") "output file"
    ] 
    
writeOutput arg opt = return opt {optOutput = arg} 

main :: IO ()
main = do args <- getArgs
          opts <- case getOpt Permute options args of
                         (act,[path],[])  -> do opts <- foldl (>>=) (return Options {optOutput=path,optInput = path}) act
                                                return opts  
                         (act,path:ps,[]) -> do putStrLn "Можно скомпилировать только один файл. остальные файлы проигнорированы"
                                                opts <- foldl (>>=) (return Options {optOutput=path,optInput=path}) act
                                                return opts
                         (_,_,msgs)      ->  error $ concat msgs
          mainWithOptions opts
          
mainWithOptions :: Options -> IO ()
mainWithOptions opts = do
    let Options { optInput = inPath,
                  optOutput = outPath  } = opts
    prog <- parseFile inPath
    case prog of
            ParseOk modl -> writeFile outPath $ show $ insts modl
            ParseFailed _ _ -> putStrLn "Parse error!"
    

insts :: Module -> [Decl]
insts (Module _ _ _ _ _ _ decls) = filter isInstDecl decls
    where isInstDecl (InstDecl _ _ _ _ _)  = True
          isInstDecl _ = False
    

    