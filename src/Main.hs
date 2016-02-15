{-# LANGUAGE OverloadedStrings #-}
import Options.Applicative

import Text.XML.HXT.Core

import System.IO
import System.Environment
import System.Exit


data Measurements = Measurements
    { neckCirc :: Float
    , frontWidth :: Float
    , chestCirc :: Float
    , waistCirc :: Float
    , hipHeight :: Float
    , hipCirc :: Float

    , shoulderWidth :: Float
    , backWidth :: Float
    , backLength :: Float

    , upperArmCirc :: Float
    , sleeveLength :: Float
    , ankleCirc :: Float

    , pathToInput :: String
    }

main :: IO ()
main = execParser ms >>= runWithOptions
  where
    parser = Measurements
        <$> option auto (long "neck_circ")
        <*> option auto (long "front_width")
                     <*> option auto (long "chest_circ")
                     <*> option auto (long "waist_circ")
                     <*> option auto (long "hip_height")
                     <*> option auto (long "hip_circ")

                     <*> option auto (long "shoulder_width")
                     <*> option auto (long "back_width")
                     <*> option auto (long "back_length")

                     <*> option auto (long "upper_arm_circ")
                     <*> option auto (long "sleeve_length")
                     <*> option auto (long "ankle_circ")
                     <*> argument str (metavar "PATH" <>
                                       help "path to the input svg")
    ms = info parser mempty


runWithOptions :: Measurements -> IO ()
runWithOptions ms = do
    [rc] <- runX (application ms)
    if rc >= c_err
        then exitWith (ExitFailure(0-1))
        else exitWith ExitSuccess


application :: Measurements -> IOSArrow b Int
application ms =
    readDocument [] src
    >>>
    processChildren (processDocumentRootElement ms `when` isElem)
    >>>
    writeDocument [] ""
    >>>
    getErrStatus
  where
    src = pathToInput ms


processDocumentRootElement :: Measurements -> IOSArrow XmlTree XmlTree
processDocumentRootElement ms =
    processTopDown (addPath `when` isSVG)
  where
    isSVG = isElem >>> hasName "svg"
    addPath = replaceChildren (getChildren <+> pathElement)
    pathElement = drawPattern []


drawPattern :: ArrowXml a => Pattern -> a XmlTree XmlTree
drawPattern pattern
    = selem "g"
      [ mkelem "path"
        [ sattr "style" "stroke:#000000;stroke-width:2.0;fill:none"
        , sattr "d" points 
        ] []
      ]
  where
    points = "M 0 0 L 100 100" 


type Pattern = [PathElement]

data PathElement
    = MoveTo (Float, Float)
    | LineTo (Float, Float)


data BasicBodies = BasicBodies
    { a :: (Float, Float)
    , b :: (Float, Float)
    , c :: (Float, Float)
    , d :: (Float, Float)
    , e :: (Float, Float)
    , f :: (Float, Float)
    , g :: (Float, Float)
    }
  deriving (Show)

basicBodies :: Measurements -> BasicBodies
basicBodies ms = BasicBodies a b c d e f g
  where
    a = (0, 0)
    b = (fst a + nc/6 + 1, snd a - nc/16)
    c = (fst a + sw/2, snd b + (sw/2 - (fst b - fst a))*sin(sa)/cos(sa))
    d = (0, 0)
    e = (0, 0)
    f = (0, 0)
    g = (0, 0)
    nc = neckCirc ms
    sw = shoulderWidth ms
    sa = 0.07 * pi
