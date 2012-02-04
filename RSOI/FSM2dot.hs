module Main where

import System.Environment
import System.Console.GetOpt
import Language.Haskell.Exts
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Canonical
import Data.Text.Lazy (pack)


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
            ParseOk modl -> writeFile outPath $ show $ edges $ states $ inst modl
            ParseFailed _ _ -> error "Parse error!"

edges :: InstDecl ->  [DotEdge String]
edges (InsDecl (FunBind matches)) = foldr (\el acc -> maybe acc (++acc) $ toEdge el ) [] matches
    where toEdge :: Match -> Maybe [DotEdge String]
          toEdge (Match _ _ (st_patt : msg_patt : _ ) _ rhs _) = do st_name <- name_from_patt st_patt
                                                                    msg_name <- name_from_patt msg_patt
                                                                    newst_name <- name_from_rhs rhs
                                                                    return (map  (\x -> DotEdge {fromNode = st_name,
                                                                                                 toNode = x,
                                                                                                 edgeAttributes = [Label (StrLabel (pack msg_name))]}) newst_name) 
                                                                    
edges _ = error "Unbelievable error!"

name_from_patt :: Pat -> Maybe String
name_from_patt (PApp (UnQual (Ident name)) _) = Just name
name_from_patt (PApp (Qual (ModuleName "RSOI.FSMlib") (Ident name)) _) = Just name
name_from_patt _ = Nothing

name_from_rhs :: Rhs -> Maybe [String]
name_from_rhs (UnGuardedRhs (App (Con (UnQual (Ident "Just"))) (Tuple [Con (UnQual (Ident name)),_]) )) = Just [name]
name_from_rhs (GuardedRhss rhss) = mapM name_from_grhs rhss
    where name_from_grhs (GuardedRhs _ _ (App (Con (UnQual (Ident "Just"))) (Tuple [Con (UnQual (Ident name)),_]) )) = Just name
          name_from_grhs _ = Nothing
name_from_rhs _ = Nothing
            
states :: Decl -> InstDecl
states (InstDecl _ _ _ _ idecls) = case filter isStateFunc idecls of
                                    [a] -> a
                                    _ -> error "That could not be!"
    where isStateFunc (InsDecl (FunBind ((Match _ (Ident "state") _ _ _ _ ): _))) = True  
          isStateFunc _ = False
states _ = error "But how?! O_o"
           
inst :: Module -> Decl
inst (Module _ _ _ _ _ _ decls) = case insts of
                                    [inst] -> inst
                                    [] -> error "There is no FSM instances in this module."
                                    _ -> error "There should be only one FSM instance in the module."
    where insts = filter isInstDecl decls
          isInstDecl (InstDecl _ _ (UnQual (Ident "FSM")) _ _)  = True
          isInstDecl (InstDecl _ _ (Qual (ModuleName "RSOI.FSMlib") (Ident "FSM")) _ _)  = True
          isInstDecl _ = False
    

    