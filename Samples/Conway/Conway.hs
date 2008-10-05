{-# LANGUAGE PatternSignatures, FlexibleContexts #-}
module Main where

import Salsa
import Bindings

import Control.Monad
import Data.Array
import Data.Maybe

loadXaml :: Coercible (Obj Object_) a => String -> IO a
loadXaml xamlPath = do
    uri <- new Uri_ (xamlPath, UriKind_ # _Relative_)
    streamInfo <- Application_ # _GetRemoteStream (uri)
    xamlReader <- new XamlReader_ ()
    stream     <- get streamInfo Stream_
    get streamInfo Stream_ >>= \s -> xamlReader # _LoadAsync s >>= cast

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
    buttonStates <- mapM (\b -> get b IsChecked_) (elems buttons)
    let states = listArray (bounds buttons) (map fromJust buttonStates)
    sequence_ $ do
        cell <- indices states
        let state' = case neighbors states cell of
                        2         -> Nothing    -- unchanged
                        3         -> Just True  -- comes to life
                        otherwise -> Just False -- dies
        case state' of
            Nothing   -> mzero
            otherwise -> return $ set (buttons ! cell) [ IsChecked_ :== state' ]

main :: IO ()
main = withCLR $ do
    win :: Obj Window_ <- loadXaml "/Conway.xaml"

    -- Build an array of toggle buttons in 'grid'
    grid :: Obj UniformGrid_ <- win # _FindName ("grid") >>= cast
    let (rows,cols) = (12,20)
    set grid [Columns_ :== fromIntegral cols]
    buttonList <- sequence $ replicate (rows * cols) $ do
        b <- new ToggleButton_ ()
        get grid Children_ >>=# _Add (b)
        return b
    let buttons = listArray ((0,0),(rows-1,cols-1)) buttonList

    timer <- new DispatcherTimer_ ()
    set timer [Tick_     :+> delegate EventHandler_ (\_ _ -> step buttons),
               Interval_ :=> TimeSpan_ # _FromSeconds (0.1::Double)]

    runButton :: Obj ToggleButton_ <- win # _FindName ("runButton") >>= cast
    set runButton [Click_ :+> delegate RoutedEventHandler_ 
        (\_ _ -> do Just run <- get runButton IsChecked_
                    if run then timer # _Start ()
                           else timer # _Stop ())]

    clearButton :: Obj Button_ <- win # _FindName ("clearButton") >>= cast
    set clearButton [Click_ :+> delegate RoutedEventHandler_
        (\_ _ -> mapM_ (flip set [ IsChecked_ :== Just False ]) (elems buttons))]
        
    exitButton :: Obj Button_ <- win # _FindName ("exitButton") >>= cast
    set exitButton [Click_ :+> delegate RoutedEventHandler_ (\_ _ -> win # _Close_)]
    
    app <- new Application_ ()
    app # _Run (win)
    return ()

-- vim:set sw=4 ts=4 expandtab:
