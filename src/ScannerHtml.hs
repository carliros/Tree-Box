module ScannerHtml where
import Data.Char

type Tokens = [Token]

data Token  = Token Tok String NumLin
type NumLin = Int

instance Show Token where
  show (Token t str nl) = show t ++ " " ++ show str ++ " in line " ++ show nl ++ "\n"

data Tok  = TokSemiTag      -- <html, <head, <body, <a, <p <small <big ... </html </head ...
          | TokSymbol       -- = + > : ; { } > !
          | TokValue	    -- "attribute value"
          | TokKeyword      -- important
          | TokString       -- any string or keystring or ident
          | TokError        -- couldn't reconize
                deriving (Eq, Ord)

instance Show Tok where
  show TokSemiTag    = " SemiTag    : "
  show TokSymbol     = " Symbol     : "
  show TokValue	     = " Value      : "
  show TokKeyword    = " Keyword    : "
  show TokString     = " String     : "
  show TokError      = " Error      : "
  
keytags  = [ "html", "head", "body", "table", "tr", "td", "th", "caption", "border", "colspan", "rowspan"
           , "h1", "h2", "h3", "h4", "h5", "h6" 
           , "p", "big", "small", "div"
           , "style" 
           ]
keywords = ["important"]
symbols  = "=*+>:;{}!/<>"
reservedsymbols = ["<", "</"]

scanner src = do file   <- readFile src
                 tokenize keytags keywords reservedsymbols symbols file

tokenize tgs kwd rsb sbc inp = return $ scan tgs kwd rsb sbc inp 1

scan tgs kwd rsb sbc xs n = scan' xs n
  where scan' []         _ = []
        scan' xxs@(x:xs) n
          = if isSpace x 
            then scan' nbs nn
            else Token tok str n : scan' rs n
                where (tok,str,rs) = token x xs
                      (nn,nbs)     = saltarBlancos xxs n
                
        processSemiTagged sym rest = let (tg, ys) = span isAlphaNum rest    -- cuidar que hayga algun espacio
                                     in if tg `elem` tgs
                                        then (TokSemiTag, sym++tg, ys)
                                        else (TokError, "unspected semi tag: " ++ sym ++ tg, ys)

        isSymbolChar x	   = x `elem` sbc
        isNotComillas x = x /= '\"'
        isReservedSymbol str = str `elem` rsb

        token x xs
	        | isSymbolChar x  = let (ys,zs) = span isSymbolChar xs
                                in if isReservedSymbol (x:ys)
                                   then processSemiTagged (x:ys) zs
                                   else (TokSymbol, [x], xs)
	        | x == '\"'       = let (str, y:ys) = span isNotComillas xs
			                    in (TokValue, str, ys)
            | isAlphaNum x    = let (ys,zs) = span isAlphaNum xs
                                    str     = x:ys
                                in if str `elem` kwd
                                   then (TokKeyword, str, zs)
                                   else (TokString , str, zs)
            | otherwise       = (TokError, "simbolo desconocido: " ++ [x], xs)

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | otherwise = (n,xxs)

