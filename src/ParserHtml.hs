{-# LANGUAGE UndecidableInstances#-}

module ParserHtml where

import ScannerHtml
import UU.Parsing
import Data.Char
import Data.List
import NTree

parseHtml file = do 
    tokens <- scanner file
    parseIO pHtml tokens

pHtml =  (\hd bd -> NTree (NTag "html") [hd, bd]) <$ pOpen "html" <*> pHead <*> pBody <* pClose "html"

pHead = (\stl -> NTree (NTag "head") [stl]) <$> pTagged "head" pTagStyle

pTagStyle = NTree NStyle <$> pTagged "style" (pList1 pRuleCss)

pRuleCss = (\lsel pr -> NTree (NRuleCss (map (\sel -> (sel,pr)) lsel)) []) 
           <$> pList1Sep (pSymbol ",") pSelector <* pSymbol "{" 
                                                        <*> pList1Sep (pSymbol ";") pProperty
                                                 <* pSymbol "}"

pSelector = SimpSelector  <$> pSSelector
         <|> CombSelector <$> pSSelector <*> pCombinator <*> pSelector

pSSelector = TypeSelector  <$> pString
          <|> UnivSelector <$  pSymbol "*"

pCombinator = (pSymbol ">" <|> pSymbol "+") `opt` " "

--pProperty = Property Author <$> pString <* pSymbol ":" <*> pString <*> pImportant
pProperty = pDisplay <|> pMargin <|> pPadding <|> pBorderWidth <|> pBorderColor <|> pBorderStyle

pDisplay = Property Author <$> pKeyword "display" <* pSymbol ":" <*> pDisplayValue <*> pImportant
pDisplayValue =  pKeyword "none"
             <|> pKeyword "block"
             <|> pKeyword "inherit"

pMargin = pMarginTop <|> pMarginRight <|> pMarginBottom <|> pMarginLeft
pMarginTop    = Property Author <$> pKeyword "margin-top"    <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pMarginRight  = Property Author <$> pKeyword "margin-right"  <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pMarginBottom = Property Author <$> pKeyword "margin-bottom" <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pMarginLeft   = Property Author <$> pKeyword "margin-left"   <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant

pPadding = pPaddingTop <|> pPaddingRight <|> pPaddingBottom <|> pPaddingLeft
pPaddingTop    = Property Author <$> pKeyword "padding-top"    <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pPaddingRight  = Property Author <$> pKeyword "padding-right"  <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pPaddingBottom = Property Author <$> pKeyword "padding-bottom" <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pPaddingLeft   = Property Author <$> pKeyword "padding-left"   <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant

pBorderWidth = pBorderTopWidth <|> pBorderRightWidth <|> pBorderBottomWidth <|> pBorderLeftWidth
pBorderTopWidth    = Property Author <$> pKeyword "border-top-width"    <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pBorderRightWidth  = Property Author <$> pKeyword "border-right-width"  <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pBorderBottomWidth = Property Author <$> pKeyword "border-bottom-width" <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant
pBorderLeftWidth   = Property Author <$> pKeyword "border-left-width"   <* pSymbol ":" <*> (pNumber <|> pKeyword "inherit") <*> pImportant

pBorderColor = pBorderTopColor <|> pBorderRightColor <|> pBorderBottomColor <|> pBorderLeftColor
pBorderTopColor    = Property Author <$> pKeyword "border-top-color"    <* pSymbol ":" <*> (pColor <|> pKeyword "inherit") <*> pImportant
pBorderRightColor  = Property Author <$> pKeyword "border-right-color"  <* pSymbol ":" <*> (pColor <|> pKeyword "inherit") <*> pImportant
pBorderBottomColor = Property Author <$> pKeyword "border-bottom-color" <* pSymbol ":" <*> (pColor <|> pKeyword "inherit") <*> pImportant
pBorderLeftColor   = Property Author <$> pKeyword "border-left-color"   <* pSymbol ":" <*> (pColor <|> pKeyword "inherit") <*> pImportant

pBorderStyle = pBorderTopStyle <|> pBorderRightStyle <|> pBorderBottomStyle <|> pBorderLeftStyle
pBorderTopStyle    = Property Author <$> pKeyword "border-top-style"    <* pSymbol ":" <*> (pStyle <|> pKeyword "inherit") <*> pImportant
pBorderRightStyle  = Property Author <$> pKeyword "border-right-style"  <* pSymbol ":" <*> (pStyle <|> pKeyword "inherit") <*> pImportant
pBorderBottomStyle = Property Author <$> pKeyword "border-bottom-style" <* pSymbol ":" <*> (pStyle <|> pKeyword "inherit") <*> pImportant
pBorderLeftStyle   = Property Author <$> pKeyword "border-left-style"   <* pSymbol ":" <*> (pStyle <|> pKeyword "inherit") <*> pImportant

pImportant = (True <$ pSymbol "!" <* pKeyword "important") `opt` False

pBody = NTree (NTag "body") <$> pTagged "body" (pList1 pElem)

pElem =  pEHead <|> pParag <|> pBig <|> pSmall <|> pDiv <|> pText

pEHead =  NTree (NTag "h1") <$> pTagged "h1" (pList1 pElem)
      <|> NTree (NTag "h2") <$> pTagged "h2" (pList1 pElem)
      <|> NTree (NTag "h3") <$> pTagged "h3" (pList1 pElem)
      <|> NTree (NTag "h4") <$> pTagged "h4" (pList1 pElem)
      <|> NTree (NTag "h5") <$> pTagged "h5" (pList1 pElem)
      <|> NTree (NTag "h6") <$> pTagged "h6" (pList1 pElem)

pParag = NTree (NTag "p"    ) <$> pTagged "p"     (pList1 pElem)
pBig   = NTree (NTag "big"  ) <$> pTagged "big"   (pList1 pElem)
pSmall = NTree (NTag "small") <$> pTagged "small" (pList1 pElem)
pDiv   = NTree (NTag "div"  ) <$> pTagged "div"   (pList1 pElem)

pText = (\lstr -> NTree (NText (unwords lstr)) []) <$> pList1 pString

pColor = pAny pKeyword ["red", "yellow", "darkgrey", "grey", "white", "green", "blue", "cyan", "magenta", "black"]

pStyle = pAny pKeyword ["hidden", "dotted", "dashed", "solid"]

pTagged tag p = pOpen tag *> p <* pClose tag

pOpen  str = pSemiTag ("<" ++str) *>  pSymbol ">"
pClose str = pSemiTag ("</"++str) <* pSymbol ">"

-- Integracion Scanner - Parser

instance Eq Tok => Eq Token where
  (Token TokString      _  _) == (Token TokString      _  _) = True
  (Token TokNumber      _  _) == (Token TokNumber      _  _) = True
  (Token TokValue       _ _)  == (Token TokValue       _  _) = True
  (Token t1             s1 _) == (Token t2             s2 _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Token tok1 str1 _ <= Token tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token

tSym :: Tok -> String -> Parser Token String
tSym tk str = obtenerVal <$> pSym (Token tk str 0)

obtenerVal (Token _ v _) = v

toInt df str@(x:xs) 
    = if isDigit x
	  then read str :: Int
      else df

--pText       = pString `opt` ""
pString  = tSym TokString ""
pSemiTag = tSym TokSemiTag
pKeyword = tSym TokKeyword
pSymbol  = tSym TokSymbol
pValue   = tSym TokValue ""
pNumber  = tSym TokNumber ""

