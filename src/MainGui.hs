module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import NTree
import FSTree
import ParserHtml
import qualified Data.Map as Map
import Data.Maybe

main :: IO()
main = start gui

gui :: IO()
gui = do -- variable to draw lines
         lines <- variable [ value := [] ]
         -- gui
         f   <- frame [text := "gui"]
         inp <- entry  f [text := "./test/test1.html"]
         pnl <- scrolledWindow f [virtualSize := sz 800 600]
         go  <- button f [text := "Paint"]
         set go [on command := onButtonCommand pnl inp lines]
         set pnl [on paint := onPnlPaint pnl lines]
         --set inp [on enterKey := repaint pnl]
         set f [layout := column 5 [row 5 [hfill $ widget inp, widget go], fill $ widget pnl]]
         return ()

onPnlPaint pnl lines dc rt = do
    listLines <- get lines value
    mapM_ (runCommand) listLines
        where runCommand (Line (x1,y1) (x2,y2)) = line dc (pt x1 y1) (pt x2 y2) []


onButtonCommand pnl inp lines = do
    -- generating the formatting structure to render
    file   <- get inp text
    ast    <- parseHtml file
    let fstree = sem_Root (Root ast)
    let res    = sem_BoxRoot (BoxRoot fstree)

    -- deleting all windows
    windowDestroyChildren pnl

    -- deleting list of lines
    set lines [value := []]

    -- rendering formating structure
    mapM_ (runCommand) res
        where runCommand (Block str x y props)
                                         = do wn <- box str pnl props
                                              windowMove wn (pt x y)
                                              set wn [size := sz 95 50]
                                              repaint pnl
              runCommand (WTam w h)      = do sw@(Size tw th) <- get pnl size
                                              let ns@(Size nw nh) = sizeMax sw (sz w h)
                                              set pnl [virtualSize := ns, scrollRate := sz (nw `div` 100) (nh `div` 100) ]
              runCommand ln              = do listLines <- get lines value
                                              set lines [value := (ln : listLines)]
                                              return ()

-- Css Box
box nm wn props = window wn [on paint := onBoxPaint nm props]

onBoxPaint nm props dc rt@(Rect x y w h) = do
    -- margin points
    let [mt,mr,mb,ml] = map toNumber [ fromMaybe "0" (Map.lookup "margin-top"    props)
                                     , fromMaybe "0" (Map.lookup "margin-right"  props)
                                     , fromMaybe "0" (Map.lookup "margin-bottom" props)
                                     , fromMaybe "0" (Map.lookup "margin-left"   props)]
    let (bx1,by1) = (x+ml+1  ,y+mt+1)
    let (bx2,by2) = (x+w-mr-1,y+mt+1)
    let (bx3,by3) = (x+w-mr-1,y+h-mb-1)
    let (bx4,by4) = (x+ml+1  ,y+h-mb-1)
    
    --border color
    let toColor c = case c of
                        "red"      -> red
                        "yellow"   -> yellow
                        "darkgrey" -> darkgrey
                        "grey"     -> grey
                        "white"    -> white
                        "green"    -> green
                        "blue"     -> blue
                        "cyan"     -> cyan
                        "magenta"  -> magenta
                        _          -> black
    let [bct,bcr,bcb,bcl] = map toColor [ fromMaybe "" (Map.lookup "border-top-color"    props)
                                        , fromMaybe "" (Map.lookup "border-right-color"  props)
                                        , fromMaybe "" (Map.lookup "border-bottom-color" props)
                                        , fromMaybe "" (Map.lookup "border-left-color"   props)]

    -- border style
    let toPenStyle s = case s of
                        "hidden" -> PenTransparent
                        "dotted" -> PenDash DashDot
                        "dashed" -> PenDash DashLong
                        _        -> PenSolid
    let [bst,bsr,bsb,bsl] = map toPenStyle [ fromMaybe "" (Map.lookup "border-top-style"    props)
                                           , fromMaybe "" (Map.lookup "border-right-style"  props)
                                           , fromMaybe "" (Map.lookup "border-bottom-style" props)
                                           , fromMaybe "" (Map.lookup "border-left-style"   props)]

    -- border widths
    let [(bt,dt),(br,dr),(bb,db),(bl,dl)] = map (\s -> let n = toNumber s in (n,n `div` 2)) [ fromMaybe "0" (Map.lookup "border-top-width"    props)
                                                                                            , fromMaybe "0" (Map.lookup "border-right-width"  props)
                                                                                            , fromMaybe "0" (Map.lookup "border-bottom-width" props)
                                                                                            , fromMaybe "0" (Map.lookup "border-left-width"   props)]
    when (bt /= 0) (line dc (pt (bx1+dt) (by1+dt)) (pt (bx2-dt) (by2+dt)) [penWidth := bt, penColor := bct, penKind := bst])
    when (br /= 0) (line dc (pt (bx2-dr) (by2+dr)) (pt (bx3-dr) (by3-dr)) [penWidth := br, penColor := bcr, penKind := bsr])
    when (bb /= 0) (line dc (pt (bx3-db) (by3-db)) (pt (bx4+db) (by4-db)) [penWidth := bb, penColor := bcb, penKind := bsb])
    when (bl /= 0) (line dc (pt (bx4+dl) (by4-dl)) (pt (bx1+dl) (by1+dl)) [penWidth := bl, penColor := bcl, penKind := bsl])
    
    -- padding widths
    let [ppt,ppr,ppb,ppl] = map toNumber [ fromMaybe "0" (Map.lookup "padding-top"    props)
                                         , fromMaybe "0" (Map.lookup "padding-right"  props)
                                         , fromMaybe "0" (Map.lookup "padding-bottom" props)
                                         , fromMaybe "0" (Map.lookup "padding-left"   props)]
    let (cx1,cy1) = (x+ml+bl+ppl    ,y+mt+bt+ppt)
    let (cx2,cy2) = (x+w-mr-br-ppr  ,y+mt+bt+ppt)
    let (cx3,cy3) = (x+w-mr-br-ppr  ,y+h-mb-bb-ppb)
    let (cx4,cy4) = (x+ml+bl+ppl+1  ,y+h-mb-bb-ppb)
    drawRect dc (rect (pt cx1 cy1) (sz (cx2-cx1) (cy4-cy1))) []
    drawText dc nm (pt cx1 cy1) []

toNumber :: String -> Int
toNumber = read

