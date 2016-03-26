
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


gridInput  = [(ph (rteto "drain"), pregrid  bombamin )]
      where
        ph = ProjectHeader  "Depósito São Jorge"  "\"GO-060\"" "São Jorge" (Author "Denise Sales Guimarães Massuda" "13.524/ - GO" "Engenheira" "MS Projetos")
        rteto fname = Region "Mezanino"  fname  [] -- [("top","--camera=-57,13,10.67,0,0,0,496 --imgsize=7000,7000"),("ortho","--viewall --projection=o --imgsize=7000,7000")]
        bombamin = Bomba (300,2200) bombaSF
        bombareal = Bomba (930,7200) bombaJohnson2
        dj = 0.1
        dm = 0.10
        dr = 0.10
        db = 0.032
        ldist =  3.66
        dbp = 0.05
        spl = 12.1/ldist
        soff = 5
        inp = node (Open (-20))
        bspl = spl/2
        dec = 7.951125628088189e-3
        t05 = Turn (dec)
        ut05 = Turn (-dec)
        te c dri dbi = node (Tee (TeeConfig (fst <$> c)  (0.1*dbi) (pi/2)dbi dri (100)) Table )
        te135  c dri dbi = node (Tee (TeeConfig (fst <$> c)  (0.1*dbi) (pi/4) dbi dri (100)) Table )
        te45  c dri dbi = node (Tee (TeeConfig (fst <$> c)  (0.1*dbi) (3*pi/4) dbi dri (100)) Table )
        tubo d l = link [Tubo (Just d) l 100]
        tubod l d = Tubo (Just d) l 100
        pregrid bomba = fst $ runInput $ mdo
          res <- node (Reservatorio 60)
          tprinc <- link (editDiametro dj <$> principal ) res t4
          let rramal tl tm (tprinc,t3) = mdo
                t4 <- te [tprinc,o5 ,t43] dr dr
                o5 <- link [tubod tl dr] t4 =<< inp
                t43  <- link tm  t4 t3
                return (t43,t4)
          let ramal tl tm (tprinc,t3) = mdo
                t4 <- te [t43,o5 ,tprinc] dr dr
                o5 <- link [tubod tl dr] t4 =<< inp
                t43  <- link tm  t4 t3
                return (t43,t4)
          let ramal45 tl tm (tprinc,t3) = mdo
                t4 <- te45 [t43,o5 ,tprinc] dr dr
                o5 <- link [tubod tl dr] t4 =<< inp
                t43  <- link tm  t4 t3
                return (t43,t4)
          let rramal135 tl tm (tprinc,t3) = mdo
                t4 <- te135 [tprinc,o5 ,t43] dr dr
                o5 <- link [tubod tl dr] t4 =<< inp
                t43  <- link tm  t4 t3
                return (t43,t4)


          let rramal45 tl tm (tprinc,t3) = mdo
                t4 <- te45 [tprinc,o5 ,t43] dr dr
                o5 <- link [tubod tl dr] t4 =<< inp
                t43  <- link tm  t4 t3
                return (t43,t4)

          (t21,t4) <- (
                      rramal135 5.1 [t05,tubod 1.7 dr,t05]
                      >~>ramal45 13.25 [ut05,tubod 1.7 dr,ut05]
                      >~> rramal135 5.1 [t05,tubod 3.85 dr, t05]
                      >~> ramal45 13.25 [ut05,tubod 1.7 dr,ut05]
                      >~> rramal135 5.1 [t05,tubod 3.85 dr, t05]
                      >~> ramal45 13.25 [ut05,tubod 1.7 dr,ut05]
                      >~> rramal135 5.1 [tubod 3.85 dr]
                      >~> rramal135 5.1 [t05,tubod 3.85 dr,t05]
                      >~> ramal 5.1 [tubod 3.85 dr]
                      >~> ramal45 13.25 [ut05,tubod 1.7 dr,ut05]
                      >~> rramal 5.1 [t05,tubod 3.85 dr,t05]
                      >~> ramal45 13.25 [ut05,tubod 0.1 dr,ut05]
                      >~> rramal 5.1 [t05,tubod 5.67 dr,t05]
                      >~> ramal45 13.25 [ut05,tubod 4.88 dr,ut05]
                      >~> rramal45 13.25 [t05,tubod 0.72 dr,jd left45 dr ,tubod 1 dr]
                      >~> ramal 5.1 [ut05,tubod 2.8 dr,ut05]
                      >~> rramal 5.1 [t05,tubod 2.8 dr,t05]
                      >~> ramal 5.1 [ut05,tubod 2.2 dr,ut05]
                      >~> rramal 5.1 [t05,tubod 0.35 dr,jd left45 dr]
                      >~> ramal 5.1 [tubod 2.8 dr]
                      >~> ramal 5.1 [ut05,tubod 2.8 dr,ut05]
                      >~> rramal 3.2 [t05,tubod 2.8 dr,t05]
                      >~> ramal 5[ut05,tubod 2.8 dr,ut05]
                      >~> rramal 3.1 [t05,tubod 3.20 dr , t05,jd right90 dr ,tubod 4.5 dr ]
                      )(tprinc,t1)
          t1 <-  inp
          return ()

        principal = [tubod 0.1 dm ,Turn dec ,joelhoR,tubod 0.5 dm,tubod 1 dm,ut05,joelhoL,ut05]

          where
            dm = dj
            tubod l d = Tubo (Just d) l 100

main = mapConcurrently (solveModel ) gridInput

