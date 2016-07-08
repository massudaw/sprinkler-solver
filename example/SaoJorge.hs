
{-# LANGUAGE RecursiveDo,TypeFamilies,FlexibleContexts,TupleSections, NoMonomorphismRestriction #-}
module Main where

import Data.Monoid
import Hydraulic
import Project
import Domains
import Sprinkler
import Input
import Element

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)

bifurcate ramal  inp dm (tvga3 ,lvga24) = mdo
    let te c dri dbi = node (Tee (TeeConfig (fst <$> c) (Circular <$> [dri,dbi ,dri]) (RoundTee  (pi/2) (0.1*dbi)   100) )Table )
    lvga32 <- inp  tvga3 tvga2
    lvga2 <- ramal tvga2 -- tubo dm 1.0 tvga2 =<< node (Open 0)
    tvga2 <- te [lvga32,lvga2,lvga24] dm dm
    return (tvga2,lvga32)

gridInput  = (ph , pregrid  bombamin )
      where
        ph = ProjectHeader  "Depósito São Jorge"  "\"GO-060\"" "São Jorge" (Author "Denise Sales Guimarães Massuda" "13.524/ - GO" "Engenheira" "MS Projetos") "SAO-JORGE" []
        bombamin = Bomba (600,90*100/6) bombaSF
        dj = 0.25
        dm = 0.20
        dr = 0.15
        db = 0.032
        ldist =  3.66
        dbp = 0.05
        bl = 45.2020
        spl = 12.1/ldist
        bspl = spl/2
        sp = node (Sprinkler ((25,16.0))  db (SPKCoverage spl ldist 6 (SPKGoods 7 1)) (0.16*60*1.7) )
        te c dri dbi = node (Tee (TeeConfig (fst <$> c) (Circular <$> [dri,dbi ,dri]) (RoundTee  (pi/2) (0.1*dbi)   100) )Table )
        tubo d l = link [Tubo (Circular d) l 100]
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
          mainl <- link ((editDiametro dj <$> [tubod 2.0 dm ,joelhoL  dm , tubod 1.0 dm , joelhoR  dm ] ) <> (editDiametro dm <$> [ tubod 3 dm  ,joelhoL dm ,Turn (1/4) , joelhoR dm , tubod 1.5 dm ,joelhoL dm,Turn (-1/4),joelho dm])) tehid tvga3
          lvga3 <- tubo  dm 1.0 tvga3 =<< node (Open 0)
          tvga3 <- te [mainl,lvga3,lvga32] dm dm
          (tvga2,lvga32) <- bifurcate (\tvga2  -> tubo dm 1.0 tvga2 =<< node (Open 0))  (link (editDiametro dm <$> [tubod bl dm])) dm (tvga3,lvga24)
          lvga24 <- link (editDiametro dm <$> [tubod bl  dm]) tvga2 tvga4
          lvga4 <- tubo dm 1.0 tvga4 =<< node (Open 0)
          tvga4 <- te [lvga24,lvga4,lvga45] dm dm
          lvga45 <- link (editDiametro dm <$> [tubod bl  dm]) tvga4 tvga5
          lvga5 <- tubo dm 1.0 tvga5 =<< node (Open 0)
          tvga5 <- te [lvga45,lvga5,lvga] dm dm

          lvga <- link ( [tubod bl dm  , joelhoR dm, tubod 9.35 dm,Turn (1/4), joelhoR dm , tubod 11 dm ,joelhoL dm , Turn (-1/4) , Perda  $ TabelaPerda (Circular dm )("Valvula","Governo","") 100 ,tubod 0.8 dm, joelhoL dm , tubod 0.62 dm]) tvga5 t1
          tdr <- node (Open 0)
          ldr <- tubo dr 1 b1 tdr
          (b1,t1) <- vga (lvga ,ldr)
          return ()
        vga (lg1 ,ldr) = mdo
          t1 <- te [lg1,l52,lf] dr dr
          lf <- tubo dr bl t1 b1
          b1 <- te [lf,l5,ldr] dr dr
          let f = foldl1 (>~>) (replicate 22 (uncurry patch) <> [uncurry (spkt ldist 12), uncurry (spkt ldist 12) , uncurry (spkt ldist 12 ), uncurry (spkt 0.1 12 )] )
          or <- node (Open 0)
          ol <- node (Open 0)
          (_,(nil,nij))<- f  ((l5,l52),(or,ol))
          let rm = [tubod 1.1 dr,Turn 0.25 ,joelhoR  dr ,tubod 1.1 dr, joelhoL dr ,Turn (-0.25) , tubod 1.1 dr ]
              brm = [tubod 1.1 dr,Turn (-0.25) ,joelhoR  dr ,tubod 1.1 dr, joelhoL dr ,Turn (0.25) , tubod 1.1 dr ]
          l5 <- link (rm )  b1 nil
          l52 <- link (brm ) nij t1
          return (b1,t1)
        spkt = gridb sp
        patch  = gridb sp  ldist 12
        gridb nod dt num (lir,lil) ~(nir,nil)  = mdo
          nr <- te [lir,ix,cr] dr dbp
          nl <- te [cl ,ixl,lil] dr dbp
          ix <- tubo dbp bspl nr ni
          ni <- nod
          let f = foldl1 (>=>)  (replicate (num -1) (spka db))
              spka db oldn  = mdo
                tubo db spl oldn  b
                b <- nod
                return b
          e <- f ni
          ixl <- tubo    dbp (bl - (fromIntegral $ num)* spl - bspl) e nl
          cr<- tubo dr dt nr nir
          cl <- tubo dr dt nl nil
          return ((cr,cl),(nr,nl))


        tubod l d = Tubo (Circular d) l 100
        principal bomba = [tubod 0.1 dm ,Turn (-1/4) ,joelhoR dm,tubod 0.5 dm,tubod 1 dm,joelhoL dm,Turn (1/4) ,tubod 2.726 dm ,joelhoL dm ,tubod  1.0 dm,bomba,tubod  1 dm,Turn (1/4),joelhoR dm ,tubod  1 dm,joelhoL dm ,Turn (-1/4) ,tubod  0.76 dm,joelhoL dm,tubod  3.72 dm,Turn (-1/4),joelhoR dm ,tubod 1 dm, joelhoL dm,Turn (1/4) ,tubod  0.2 dm,Turn (1/4),joelhoR dm , tubod  1  dm,joelhoL dm , Turn (-1/4),tubod 5 dm ,joelhoR dm ,tubod 1 dm]

          where
            dm = dj
            tubod l d = Tubo (Circular d) l 100

main = renderModel  (defAmbient water undefined) (replicate 4 (MiscelaneousStorage OrdinaryII 3.7 ))  [0,1,2,3] gridInput

