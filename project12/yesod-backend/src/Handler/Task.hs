{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Task where

import Import
import Yesod.Core.Json as YJ
import Data.Text

getTaskR :: Int -> Handler YJ.Value
getTaskR taskId = do
        let title = Data.Text.append "Sample title #" (Data.Text.pack.show $ taskId)
        return $ object [("task",object [
                                ("title",String title),
                                ("description",String "sample desc")
                                ])
                        ]