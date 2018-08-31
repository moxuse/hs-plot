:show paths      
:set prompt ""

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import System.FilePath
:load Context

import Control.Concurrent

import Graphics.EasyPlot

import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe
import Graphics.Rendering.Cairo

-- stream
g1 <- streamSmartLazer

-- preview
-- if you instaled gnuplot, can previw befour stream.

-- prev :: [Stroke] -> [IO Bool]
:{
prev st = do
  plot (PNG "./preview.png") [Data2D [Style Lines] [] x | x <- st]
:}

:{
drawMain = forkOS $ do 
  initGUI
  window <- windowNew
  -- windowSetDefaultSize window 1280 200
  -- on window deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  onDestroy window mainQuit
  -- containerAdd window vbox
  done <- newEmptyMVar
  forkIO (mainGUI >> putMVar done ())
  takeMVar done
  -- return ()
-- guiInit :: MVar Bool
-- guiInit = unsafePerformIO (newMVar False)

-- initGUIOnce :: IO ()
-- initGUIOnce = do
  -- init_ <- readMVar guiInit 
  -- when (not init_) $ do
    -- _ <- forkOS $ runInBoundThread $ do
      -- _ <- unsafeInitGUIForThreadedRTS
      -- _ <- swapMVar guiInit True
      -- -- initGUI
      -- -- window <- windowNew
      -- -- windowSetDefaultSize window 1280 200
      -- -- widgetShowAll window
      -- mainGUI --return ()
      -- -- drawMain
    -- return ()
  -- return ()
                                                                                                             
:}

-- drawMain

:set prompt "g-code> "



