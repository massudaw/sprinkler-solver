
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Data.Monoid
import Project
import Sprinkler
import Input
import Element

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)




gridInput  = [(ph (rteto "teto-grid-D-limite-minimo"), pregrid  bombamin ),(ph (rteto "teto-grid-D"), pregrid  bombareal )]
      where
        ph = ProjectHeader  "Depósito Armazém Johnson & Johnson - Galpão 01"  "\"RODOVIA BR-153, QUADRA CH, JARDIM GUANABARA, GALPÃO 01, GOIÂNIA, GOIÁS\"" "ATLAS LOGÍSTICA LTDA" (Author "Priscila Sathler Garcia" "13.524/ - GO" "Engenheira" "Guava Engenharia")
        rteto fname = Region "Teto - Grid D"  fname  [] -- [("top","--camera=-57,13,10.67,0,0,0,496 --imgsize=7000,7000"),("ortho","--viewall --projection=o --imgsize=7000,7000")]
        bombamin = Bomba (930,7200) bombaJohnson2
        bombareal = Bomba (930,7200) bombaJohnson2
        dj = 0.25
        dm = 0.20
        dr = 0.15
        db = 0.080
        bl = 21.855
        spl = 2.56
        bspl = 0.7
        sp = node (Sprinkler (Just (25,24.0))  (Just db) 14 (0.16*60*1.7) )
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
          hid2 <- node (Open 1900)
          tehid <- te [tprinc,lhid2,mainl] dj dj
          mainl <- link ((editDiametro dj <$> [tubod  62.5  dj,joelhoL,tubod  80.0  dj,joelhoR ] ) <> (editDiametro dm <$> [ tubod 3 dm , joelhoL ,tubod  26.76 dm, joelhoUV (-1/2), tubod 1.5 dm ,joelhoDV 0,joelho])) tehid tvga3
          vga3 <- node (Open 0)
          lvga3 <- tubo  dm 1.0 tvga3 vga3
          tvga3 <- te [mainl,lvga3,lvga32] dm dm
          lvga32 <- link (editDiametro dm <$> [tubod 111.43 dm]) tvga3 tvga2
          vga2 <- node (Open 0)
          lvga2 <- tubo dm 1.0 tvga2 vga2
          tvga2 <- te [lvga32,lvga2,lvga] dm dm
          lvga <- link (editDiametro dm <$> [tubod 0.7 dm  , joelho, tubod 9.35 dm,jd (upC 0) dm , tubod 11 dm ,jd (dowC 0 ) dm , Perda  (Just dm) ("Valvula","Governo","")100 ,tubod 0.8 dm, joelhoL , tubod 0.62 dm]) tvga2 tg1
          (lg1,tg1) <- (foldl1 (>~>)[ramal (editDiametro dm <$> [tubod 21.85 dm]) ,ramal (editDiametro dm <$> [tubod 0.65 dm]) ,ramal (editDiametro dm <$> [tubod 21.85 dm]) ,ramal (editDiametro dm <$> [tubod 0.65 dm])]) (lvga ,t1)
          t1 <-  te [lg1,l52,lf] dr dr
          lf <- tubo dr 21.85  t1 b1
          let rm = [tubod 1.1 dr,jd (upC 0) dr ,tubod 1.1 dr, jd (dowC 0) dr ,tubod 1.1 dr ]
          let brm = [tubod 1.1 dr,jd (dowC 0) dr ,tubod 1.1 dr, jd (upC 0) dr ,tubod 1.1 dr ]
          l5 <- link (editDiametro dr <$> brm )  t5 b1
          l52 <- link (editDiametro dr <$> rm ) t1 t4
          tdr <- node (Open 0)
          ldr <- tubo db 1 tdr b1
          b1 <- te [lf,l5,ldr] dr dr
          t4 <- te [l8,l7,l52] dr db
          l7 <- tubo db bl t4 t5
          l8 <- tubo dr ldist t4 nij
          t5 <- te  [l5,l7,l9] dr db
          l9 <- tubo  dr ldist  t5 nil
          let f = foldl1 (>~>) (replicate 11 (uncurry patch) <> [uncurry (spkt ldist 3 1), uncurry (spkt ldist 2 4) , uncurry (spkt 1.42 2 4 )] )
          ((tr,tl),(nil,nij)) <- f  ((l9,l8),(tor,tol))
          tor <- te [tr,bend,lor] dr dr
          tol <- te [lol,bend,tl] dr dr
          bend <- tubo dr bl tor tol
          lol  <- tubo db 0.1 tol ol
          lor  <- tubo db 0.1 tor or
          or <- node (Open 0)
          ol <- node (Open 0)
          return ()
        spkt dt off num (lir,lil) ~(nir,nil)  = mdo
          nr <- te [lir,ix,cr] dr db
          nl <- te [cl,ixl,lil] dr db
          ix <- tubo   db (fromIntegral off*spl + bspl) nr ni
          ni <- sp
          let f = foldr1 (>=>) ((replicate num ) spka)
              spka oldn  = mdo
                tubo db spl oldn  b
                b <- sp
                return b
          e <- f ni
          ixl <- tubo    db (bl - (fromIntegral $ num+off)* spl - bspl) nl   e
          cr<- tubo dr dt nr nir
          cl <- tubo dr dt nl nil
          return ((cr,cl),(nr,nl))

        patch (ti,tj) ~(i,j) = mdo
          idn2 <- te [idt3,idt1, tj] dr db
          idn1 <- te [ti,idt1,idt2] dr db
          idt1 <- tubo  db bl  idn2 idn1
          idt2 <- tubo  dr ldist i idn1
          idt3 <- tubo  dr ldist  j idn2
          return ((idt2,idt3),(idn1,idn2))
        tubod l d = Tubo (Just d) l 100
        ldist =  3.10
        principal bomba = [tubod 0.1 dm ,joelhoD,tubod 0.5 dm,tubod 1 dm,joelhoU0,tubod 2.726 dm ,joelhoL,tubod  1.0 dm,bomba,tubod  1 dm,joelhoUV (-1/4),tubod  1 dm,joelhoD ,tubod  0.76 dm,joelhoL,tubod  3.72 dm,joelhoD,tubod 1 dm, joelhoU0,tubod  0.2 dm,joelhoU0,joelhoL , tubod  1  dm,joelhoD ,tubod 5 dm ,joelhoR,tubod 1 dm]
          where
            dm = dj
            tubod l d = Tubo (Just d) l 100

main = mapConcurrently solveModel gridInput

