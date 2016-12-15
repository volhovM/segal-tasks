
import           Control.Monad         (forM_, when)
import           Data.Bool             (bool)
import qualified Data.ByteString       as B
import           Data.List             (intercalate)
import           Data.Serialize        (encode)
import qualified Data.Vector.Generic   as VG
import           Data.Vector.Serialize ()
import           System.Directory      (createDirectory, createDirectoryIfMissing,
                                        doesDirectoryExist, doesFileExist)
import           System.Random         (randomRIO)

import           Solve
import           Types

--timeIt :: IO a → IO (Double, a)
--timeIt action = do
--  t1 ← getTime
--  a ← action
--  t2 ← getTime
--  pure (t2 - t1, a)

choice :: [a] → IO a
choice xs = (xs !!) <$> randomRIO (0, length xs - 1)

replicateM :: Applicative m ⇒ Int → m a → m [a]
replicateM 0 _ = pure []
replicateM n a = (:) <$> a <*> replicateM (n-1) a

runtil :: Monad m ⇒ m (Maybe a) → m a
runtil x = x >>= \case { Just a → pure a ; Nothing → runtil x }

showPars :: Bool → String → Pars → String
showPars b id Pars{..} =
  intercalate "\n" $ map (intercalate ", ") $
  [ [ "** " ++ bool "" "! " b ++ "(" ++ id ++ ") " ++ "α=" ++ show α
    , "K=" ++ showE 1 kk
    , "D=" ++ showE 1 dd
    , "E=" ++ showE 1 ee
    ]
  , [ "Δz = " ++ showE 2 dz
    , "Δt = " ++ showE 2 dt
    ]
  , [ "h = " ++ showE 2 h
    , "I = " ++ show ii
    , "N = " ++ show nn
    ]
  , [ "method = " ++ show method
    ]
  ]


main :: IO ()
main = do
  let n = 1000
  let precalc = False

  if precalc then
    forM_ (αVals <×> (kkVals <×> ddVals)) $ \(α,(kk,dd)) → do
      let ps = (α,kk,dd)
      let pars = ppars ps
      let dir = pdir ps

      exists ← doesFileExist (dir ++ "/pars") >>= \case
        True → do
          pure True
          -- exPars ← read <$> readFile (dir ++ "/pars")
          -- pure (wpars pars == exPars)
        False → do
          createDirectoryIfMissing True dir
          pure False

      print ps

      if exists || α /= 1.0 then
        putStrLn "exists"
      else do
        (axxs,atts) ← runMethod pars n (nn pars `div` n)
        --(calcTime, (axxs,atts)) ← timeIt $ runMethod pars0 n k

        let diverged = isNaN (VG.last axxs) || isInfinite (VG.last axxs)

        let spars = showPars diverged (show ps) pars
        putStrLn spars
        appendFile "data/calc.org" (spars ++ "\n\n")

        when (not diverged) $ do
          writeFile (dir ++ "/pars") (show $ wpars pars)
          B.writeFile (dir ++ "/xx") (encode axxs)
          B.writeFile (dir ++ "/tt") (encode atts)
  else do
    (id, dir) ← runtil $ do
      id ← replicateM 3 (choice ['a'..'z'])
      let dir = "./data/" ++ id
      doesDirectoryExist dir >>= \case
        True → pure Nothing
        False → do
          createDirectory dir
          pure (Just (id, dir))

    (axxs,atts) ← runMethod pars0 n (nn pars0 `div` n)
    writeFile (dir ++ "/pars") (show $ wpars pars0)
    B.writeFile (dir ++ "/xx") (encode axxs)
    B.writeFile (dir ++ "/tt") (encode atts)

    let spars = showPars (isNaN $ VG.last axxs) id pars0
    putStrLn spars
    appendFile "data/calc.org" (spars ++ "\n\n")
