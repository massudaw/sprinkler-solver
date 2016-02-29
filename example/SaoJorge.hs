
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Data.Monoid
import Hydraulic
import Project
import Sprinkler
import Input
import Element

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)



gridInput  = [(ph (rteto "mezanino"), pregrid  bombamin )]
      where
        ph = ProjectHeader  "Depósito São Jorge"  "\"GO-060\"" "São Jorge" (Author "Denise Sales Guimarães Massuda" "13.524/ - GO" "Engenheira" "MS Projetos")
        rteto fname = Region "Mezanino"  fname  [] -- [("top","--camera=-57,13,10.67,0,0,0,496 --imgsize=7000,7000"),("ortho","--viewall --projection=o --imgsize=7000,7000")]
        bombamin = Bomba (300,2200) bombaSF
        bombareal = Bomba (930,7200) bombaJohnson2
        dj = 0.25
        dm = 0.20
        dr = 0.15
        db = 0.032
        ldist =  3.66
        dbp = 0.05
        bl = 42.855
        spl = 12.1/ldist
        soff = 4
        bspl = spl/2
        sp = node (Sprinkler (Just (25,16.0))  (Just db) (SPKCoverage spl ldist 6 (SPKGoods 7 1)) (0.16*60*1.7) )
        te c dri dbi = node (Tee (TeeConfig (fst <$> c) (0.1*dbi) dbi dri (100)) Table )
        tubo d l = link [Tubo (Just d) l 100]
        ramal tb (lback ,nnext) = mdo
          vga2 <- node (Open 0)
          lvga2 <- tubo   dm 1.0 tvga2 vga2
          tvga2 <- te [lback ,lvga2,lvga21] dm dm
          lvga21 <- link tb tvga2 nnext
          return (lvga21,tvga2)
        pregrid bomba = fst $ runInput $ mdo
          res <- node (Reservatorio 60)
          tprinc <- link (editDiametro dj <$> principal bomba ) res tehid
          lhid2 <- tubo dj 1 tehid hid2
          hid2 <- node (Open 0 )
          tehid <- te [tprinc,lhid2,mainl] dj dj
          mainl <- link ((editDiametro dj <$> [tubod  62.5  dj,joelhoL,tubod  80.0  dj,joelhoR ] ) <> (editDiametro dm <$> [ tubod 3 dm , joelhoL ,tubod  26.76 dm,Turn (1/4) , joelhoR , tubod 1.5 dm ,joelhoL,Turn (-1/4),joelho])) tehid tvga3
          vga3 <- node (Open 0)
          lvga3 <- tubo  dm 1.0 tvga3 vga3
          tvga3 <- te [mainl,lvga3,lvga32] dm dm
          lvga32 <- link (editDiametro dm <$> [tubod 111.43 dm]) tvga3 tvga2
          vga2 <- node (Open 0)
          lvga2 <- tubo dm 1.0 tvga2 vga2
          tvga2 <- te [lvga32,lvga2,lvga] dm dm

          lvga <- link (editDiametro dm <$> [tubod 0.7 dm  , joelho, tubod 9.35 dm,Turn (1/4), joelhoR , tubod 11 dm ,joelhoL , Turn (-1/4) , Perda  (Just dm) ("Valvula","Governo","")100 ,tubod 0.8 dm, joelhoL , tubod 0.62 dm]) tvga2 tg1
          (lg1,tg1) <- (foldl1 (>~>)[ramal (editDiametro dm <$> [tubod bl dm]) ,ramal (editDiametro dm <$> [tubod 0.65 dm]) ,ramal (editDiametro dm <$> [tubod bl dm]) ,ramal (editDiametro dm <$> [tubod 0.65 dm])]) (lvga ,t1)
          tdr <- node (Open 0)
          ldr <- tubo dr 1 b1 tdr
          (b1,t1) <- vga (lg1,ldr)
          return ()
        vga (lg1 ,ldr) = mdo
          t1 <-  te [lg1,l52,lf] dr dr
          lf <- tubo dr bl t1 b1
          b1 <- te [lf,l5,ldr] dr dr
          let f = foldl1 (>~>) (replicate 22 (uncurry patch) <> [uncurry (spkt ldist soff 3), uncurry (spkt ldist soff 3) , uncurry (spkt ldist soff 3 ), uncurry (spkt 0.1 soff 3 )] )
          or <- node (Open 0)
          ol <- node (Open 0)
          (_,(nil,nij))<- f  ((l5,l52),(or,ol))
          let rm = [tubod 1.1 dr,Turn 0.25 ,joelhoR  ,tubod 1.1 dr, joelhoL,Turn (-0.25) , tubod 1.1 dr ]
          let brm = [tubod 1.1 dr,Turn 0.25 ,joelhoL , tubod 1.1 dr, joelhoR , Turn (-0.25) , tubod 1.1 dr ]
          l5 <- link (editDiametro dr <$> brm )  nil b1
          l52 <- link (editDiametro dr <$> rm ) t1 nij
          return (b1,t1)
        spkt = gridb sp
        patch  = gridb (node $ Open 0)ldist soff 3
        gridb nod dt off num (lir,lil) ~(nir,nil)  = mdo
          nr <- te [lir,ix,cr] dr dbp
          nl <- te [cl,ixl,lil] dr dbp
          ix <- tubo   dbp (fromIntegral off*spl + bspl) nr ni
          ni <- nod
          let f = foldr1 (>=>) ( [spka db ,spka db,spka db])
              spka db oldn  = mdo
                tubo db spl oldn  b
                b <- nod
                return b
          e <- f ni
          ixl <- tubo    dbp (bl - (fromIntegral $ num+off)* spl - bspl) nl   e
          cr<- tubo dr dt nr nir
          cl <- tubo dr dt nl nil
          return ((cr,cl),(nr,nl))


        tubod l d = Tubo (Just d) l 100
        principal bomba = [tubod 0.1 dm ,Turn (-1/4) ,joelhoR,tubod 0.5 dm,tubod 1 dm,joelhoL,Turn (1/4) ,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,Turn (1/4),joelhoR,tubod  1 dm,joelhoL,Turn (-1/4) ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,Turn (-1/4),joelhoR,tubod 1 dm, joelhoL,Turn (1/4) ,tubod  0.2 dm,Turn (1/4),joelhoR, tubod  1  dm,joelhoL , Turn (-1/4),tubod 5 dm ,joelhoR,tubod 1 dm]

          where
            dm = dj
            tubod l d = Tubo (Just d) l 100

main = mapConcurrently solveModel gridInput

