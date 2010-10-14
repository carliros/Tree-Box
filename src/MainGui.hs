module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import NTree
import FSTree
import ParserHtml
import qualified Data.Map as Map
import Data.Maybe
import CssBox

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

