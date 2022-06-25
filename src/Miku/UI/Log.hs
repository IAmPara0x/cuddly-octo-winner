{-# LANGUAGE TemplateHaskell #-}

module Miku.UI.Log
  ( Mode(Main)
  , ResourceName
  , UI(UI)
  , modeL
  , currentLogL
  , uiAttrMap
  , drawUI
  , handleEvent
  )
  where


import Brick.Widgets.Border        qualified as B
import Brick.Widgets.Border.Style  qualified as B
import Brick.Widgets.Center        qualified as C

import Graphics.Vty                qualified as V

import Data.Text                   qualified as T

import Brick.AttrMap (attrMap , AttrMap)
import Brick.Main
  ( continue
  , halt
  )

import Brick.Types
  ( BrickEvent(VtyEvent)
  , EventM
  , Next
  , Padding(Pad)
  , Widget
  )

import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , fill
  , hBox
  , hLimit
  , hLimitPercent
  , padAll
  , padLeft
  , padLeftRight
  , padRight
  , padTop
  , padTopBottom
  , raw
  , txt
  , updateAttrMap
  , vBox
  , vLimit
  , vLimitPercent
  , withAttr
  , withBorderStyle
  )

import Brick.Util (fg)


import Control.Lens
  ( (^.)
  , (&)
  , (^?)
  , ix
  , makeLenses
  )

import Relude

import Miku.Types.Parser (showAtom, BluePrint)
import Miku.Types.Log

import Miku.UI.Utils     (emojiWidth1, emojiWidth2,hexColorToRGB)

data Mode  = Main
             deriving stock (Show)

type ResourceName = ()

data UI = UI { _modeL       :: Mode
             , _currentLogL :: Log
             } deriving stock (Show)

makeLenses ''UI

uiAttrMap :: AttrMap
uiAttrMap = attrMap (fg V.red) []

titleBar :: Text -> Widget n
titleBar t = vBox [ padTop (Pad 1) $ C.hCenter $ txt t
                  , B.hBorder
                  ]

drawHeading :: Heading -> Widget n
drawHeading h = vLimitPercent 5 $ vBox
                  [ titleBar $ showAtom @(BluePrint Heading) h
                  ]


drawCurrentTask :: Maybe Task -> Widget n
drawCurrentTask Nothing  = hLimitPercent 60 $ vLimitPercent 50
                          $ withBorderStyle B.unicodeRounded $ B.border
                          $ vBox [ titleBar "Current Task"
                                 , C.center $ txt "You are currently not doing any tasks."
                                 ]

drawCurrentTask (Just task) = hLimitPercent 60 $ vLimitPercent 50
                            $ withBorderStyle B.unicodeRounded $ B.border
                            $ vBox [ titleBar "Current Task"

                                   , padTopBottom 1
                                     $ C.hCenter
                                     $ txt ("### " <> task ^. taskNameL . nameL)

                                    , vLimitPercent 8 $ hBox [ padLeft (Pad 2) $ txt $ "From: "
                                            <> showAtom @(BluePrint Time) (task ^. taskStartL)
                                           , fill ' '
                                           , padRight (Pad 2) $ txt "To: " <+> emojiWidth2 "🕗"
                                           ]

                                   , withBorderStyle B.ascii B.hBorder

                                   , padTopBottom 1 $ padLeft  (Pad 4)
                                     $ txt $ maybe "" (^. descL) (task ^. taskDescL)

                                   , fill ' '
                                   , withBorderStyle B.ascii B.hBorder
                                   
                                   , padTopBottom 1 $ padLeft (Pad 2) $ txt "Tags: Haskell, Miku"

                                   ]

drawGoalStatus :: GoalStatus -> Widget n
drawGoalStatus Done    = padTopBottom 1 $ txt "[" <+> emojiWidth2 "✅" <+> txt "]" 
drawGoalStatus NotDone = padTopBottom 1 $ txt "[" <+> emojiWidth2 "❌" <+> txt "]" 

drawGoal :: Goal -> Widget n
drawGoal goal = hBox [ drawGoalStatus (goal ^. goalStatusL)
                     , padTopBottom 1 $ padLeft (Pad 2) $ txt (goal ^. goalDescL)
                     ]

drawGoals :: [Goal] -> Widget n
drawGoals goals = vLimitPercent 50 $ withBorderStyle B.unicodeRounded $ B.border
            $ vBox $ [ titleBar "Today's Goals"
                     , padTop (Pad 1) $ C.hCenter $ txt "Not Completed"
                     , withBorderStyle B.ascii B.hBorder
                     ] <> map drawGoal (goalsNotDone goals) <>
                     [ fill ' '
                     , withBorderStyle B.unicode B.hBorder
                     , padTop (Pad 1) $ C.hCenter $ txt "Completed"
                     , withBorderStyle B.ascii B.hBorder
                     ] <> map drawGoal (goalsDone goals) <>
                     [ fill ' ' ]
                     


drawUI :: UI -> [Widget ResourceName]
drawUI (UI Main log) = [ vBox [ drawHeading (log ^. logHeadingL)
                              ,   drawCurrentTask (ongoingTask log)
                              <+> drawGoals       (log ^. logGoalsL)
                              ]
                       ]

handleEvent :: UI -> BrickEvent ResourceName n -> EventM ResourceName (Next UI)
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui _                                     = continue ui
