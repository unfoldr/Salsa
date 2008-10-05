{-# LANGUAGE PatternSignatures, FlexibleContexts #-}
module Main where

import Foreign.Salsa
import Bindings

import Control.Monad
import Data.Array
import Data.Maybe

loadXaml :: Coercible (Obj Object_) a => String -> IO a
loadXaml xamlPath = do
    uri <- new _Uri (xamlPath, _UriKind # _relative_)
    streamInfo <- _Application # _getRemoteStream (uri)
    xamlReader <- new _XamlReader ()
    stream     <- get streamInfo _Stream
    get streamInfo _Stream >>= \s -> xamlReader # _loadAsync s >>= cast

-- | @'neighbors' states cell@ is the number of alive cells adjacent to @cell@ in @states@.
neighbors :: Array (Int,Int) Bool -> (Int,Int) -> Int
neighbors states (row,col) =
    length $ filter (== True) [ states ! cell | cell <- neighborCells ]
    where 
        neighborCells = [ wrap (r,c) | r <- [row-1..row+1],
                                       c <- [col-1..col+1],
                                       wrap (r,c) /= (row,col) ]
        wrap (r,c) = (r `mod` (maxRow+1), c `mod` (maxCol+1))
        (_, (maxRow,maxCol)) = bounds states

step :: Array (Int,Int) (Obj ToggleButton_) -> IO ()
step buttons = do
    buttonStates <- mapM (\b -> get b _IsChecked) (elems buttons)
    let states = listArray (bounds buttons) (map fromJust buttonStates)
    sequence_ $ do
        cell <- indices states
        let state' = case neighbors states cell of
                        2         -> Nothing    -- unchanged
                        3         -> Just True  -- comes to life
                        otherwise -> Just False -- dies
        case state' of
            Nothing   -> mzero
            otherwise -> return $ set (buttons ! cell) [ _IsChecked :== state' ]

main :: IO ()
main = withCLR $ do
    win :: Obj Window_ <- loadXaml "/Conway.xaml"

    -- Build an array of toggle buttons in 'grid'
    grid :: Obj UniformGrid_ <- win # _findName ("grid") >>= cast
    let (rows,cols) = (12,20)
    set grid [_Columns :== fromIntegral cols]
    buttonList <- sequence $ replicate (rows * cols) $ do
        b <- new _ToggleButton ()
        get grid _Children >>=# _add (b)
        return b
    let buttons = listArray ((0,0),(rows-1,cols-1)) buttonList

    timer <- new _DispatcherTimer ()
    set timer [_Tick     :+> delegate _EventHandler (\_ _ -> step buttons),
               _Interval :=> _TimeSpan # _fromSeconds (0.1::Double)]

    runButton :: Obj ToggleButton_ <- win # _findName ("runButton") >>= cast
    set runButton [_Click :+> delegate _RoutedEventHandler
        (\_ _ -> do Just run <- get runButton _IsChecked
                    if run then timer # _start ()
                           else timer # _stop ())]

    clearButton :: Obj Button_ <- win # _findName ("clearButton") >>= cast
    set clearButton [_Click :+> delegate _RoutedEventHandler
        (\_ _ -> mapM_ (flip set [ _IsChecked :== Just False ]) (elems buttons))]
        
    exitButton :: Obj Button_ <- win # _findName ("exitButton") >>= cast
    set exitButton [_Click :+> delegate _RoutedEventHandler (\_ _ -> win # _close_)]
    
    app <- new _Application ()
    app # _run (win)

    return ()

-- vim:set sw=4 ts=4 expandtab:
