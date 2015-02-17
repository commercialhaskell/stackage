-- | Useful 'System.FilePath' wrapper around Shake.

module Development.Shake.FilePath
    (startShake
    ,target
    ,need
    ,want
    ,Target(Target)
    ,unTarget
    ,Rules
    ,Action
    ,CmdOption(..)
    ,Shake.cmd)
    where

import           Control.Monad.IO.Class
import           Development.Shake (Rules,Action,CmdOption(..))
import qualified Development.Shake as Shake
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Prelude hiding (FilePath)
import           System.Environment

-- | A simple opaque wrapper for the "target" abstraction.
newtype Target = Target
    { unTarget :: FilePath
    }

-- | Start Shake with the given data directory.
startShake :: MonadIO m => Int -> FilePath -> Rules () -> m ()
startShake threads dir rules =
    liftIO (withArgs [] $
                Shake.shakeArgs
                          Shake.shakeOptions
                          { Shake.shakeFiles = FP.encodeString dir
                          , Shake.shakeThreads = threads
                          } $
                rules)

-- | Declare a target, returning the target name.
target :: Target -> Action () -> Rules Target
target name act = do
    (FP.encodeString
         (unTarget name)) Shake.*>
        const act
    return name

-- | Need the given dependencies.
need :: [Target] -> Action ()
need xs = Shake.need $
    map (FP.encodeString . unTarget) xs

-- | Need the given dependencies.
want :: [Target] -> Rules ()
want xs = Shake.want
        (map (FP.encodeString . unTarget) xs)
