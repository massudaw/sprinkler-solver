
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Data.Monoid
import Project
import Hydraulic
import Sprinkler
import Input
import Element
import Domains

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

main = do 
  mapM (displayModel . fmap (grid . initIter 0 1) ) [testGrid,testGrid2]

testGrid2 = ("simple2",) $ fst $ runInput $ mdo
   let dm = 0.08
   r <- node (Reservatorio 10)
   lr <- link [Bomba (0,10) bombaSF ,tubod dm 1 ,joelhoR,tubod dm 1,Turn (1/4) ,joelhoR , tubod dm 1] r t1
   hid <- node (Open 0) >>= tubo dm 1  t1
   lend <- node (Open 0) >>= tubo dm 1  t1
   t1 <- te [lend,hid,lr] dm dm
   return ()
  where
    te c dri dbi = node (Tee (TeeConfig (fst <$> c) (0.1*dbi) dbi dri (100)) Table )
    tubo d l = link [Tubo (Just d) l 100]



testGrid = ("simple",) $ fst $ runInput $ mdo
   let dm = 0.08
   r <- node (Reservatorio 10)
   lr <- link [joelhoR,Bomba (0,10) bombaSF ,tubod dm 1 ]r t1
   t1 <- te [lr,lm1,lb] dm dm
   lb <- tubo dm 1 t1 t2
   t2 <- te [lb,lm2,lf] dm dm
   lf <- node (Open 0) >>= tubo dm 0.1  t2
   let middle = [tubod dm 0.5 , Turn (1/4) ,joelhoR  , tubod dm 1.2 , joelhoL,Turn (-1/4) , tubod dm 0.5]
   og1 <- node (Open 0) -- >>= tubo dm 0.1 tg1
   og2 <- node (Open 0) -- >>= tubo dm 0.1 tg2
   lm1 <- link middle  t1 tg1
   lm2 <- link middle t2 tg2
   let path middle ~((lm1,lm2),(tgn1,tgn2)) = mdo
         gb <- middle  tg2 tg1
         tg1 <- te [og1,gb,lm1] dm dm
         tg2 <- te [lm2,gb,og2] dm dm
         og1 <- tubo dm 1 tg1 tgn1
         og2 <-  tubo dm 1 tg2 tgn2
         return ((og1,og2),(tg1,tg2))
   (_,(tg1,tg2)) <- (path ( tubo dm  1) >~> path ( tubo dm  1)>~> path ( tubo dm  1)) ((lm1,lm2),(og1,og2))
   return ()
  where
    te c dri dbi = node (Tee (TeeConfig (fst <$> c) (0.1*dbi) dbi dri (100)) Table )
    tubo d l = link [Tubo (Just d) l 100]

