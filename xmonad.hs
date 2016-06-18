import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.Dmenu
import qualified XMonad.StackSet as W
import Control.Monad ( when, void )
import Data.Map ( fromList )
import Data.Maybe ( fromMaybe, mapMaybe )
import System.IO ( hPutStrLn )
import System.Exit ( exitSuccess )
import System.Posix.Process ( executeFile )
import System.Posix.Directory ( changeWorkingDirectory )

-- ~/.xmonad/lib/Topics.hs should export:
-- topics :: [(String, FilePath, Maybe [(String, [String])])]
--   of (name, dir, (cmd, args))
-- topicLayouts, which modifies a layout hook using onWorkspace
import Topics

main = do
       checkTopicConfig myTopics myTopicConfig
       xmproc <- spawnPipe "xmobar"
       xmonad $ ewmh $ withMyUrgencyHook defaultConfig
        { modMask     = mod4Mask
        , borderWidth = 2
        , normalBorderColor  = "#2a2a2a"
        , focusedBorderColor = "#0099ff"
        , terminal    = myTerminal
        , workspaces  = myTopics
        , keys        = myKeys
        , layoutHook  = myLayoutHook
        , manageHook  = myManageHook <+> manageDocks
        , logHook     = myLogHook xmproc
        }

-- -------------------------- Workspaces -------------------- --
myTopics = map (\ (n, _, _) -> n) topics

myTopicConfig = defaultTopicConfig
  { topicDirs          = fromList $ map getTopicDir topics
  , topicActions       = fromList $ mapMaybe getTopicAction topics
  , defaultTopicAction = const $ spawnShell
  }
  where getTopicDir (tn, td, _) = (tn, td)
        getTopicAction (tn, _, (Just tc)) = Just (tn, (mapM_ spawnInCurrent tc))
        getTopicAction _ = Nothing


-- -------------------------- Keys -------------------------- --
myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
    [ ((modm,               xK_Return ), spawnShell)
    , ((mod1Mask,           xK_space ), spawn "gmrun") --"synapse") -- "exe=`dmenu_path | dmenu -b` && eval \"exec $exe\"")
    , ((mod1Mask,           xK_Menu  ), spawn "xfdesktop --menu")
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_z     ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_z    ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_grave ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. mod1Mask,  xK_Tab   ), windows W.swapDown  )
    , ((modm .|. mod1Mask .|. shiftMask, xK_Tab), windows W.swapUp    )
    , ((modm,               xK_u     ), focusUrgent)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_l     ), sendMessage MirrorExpand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_f     ), sendMessage ToggleStruts)

    , ((modm,               xK_space ), workspacePrompt defaultXPConfig (switchTopic myTopicConfig))
    , ((modm .|. shiftMask, xK_space ), workspacePrompt defaultXPConfig $ windows . W.shift)

    , ((modm .|. shiftMask, xK_Escape), confirmActions $ [ ("shutdown", spawn "systemctl poweroff")
                                                         , ("exit", io exitSuccess)
                                                         , ("cancel", return ())
                                                         ])
    , ((modm,               xK_Escape), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    
    , ((modm,               xK_Print ), spawn "scrot")
    , ((modm .|. shiftMask, xK_Print ), spawn "scrot -u")
    , ((modm .|. mod1Mask,  xK_Print ), spawn "shoot")
    , ((modm,               xK_s     ), spawn "xscreensaver-command -lock")
    , ((modm .|. shiftMask, xK_s     ), spawn "systemctl suspend")
    , ((modm .|. mod1Mask,  xK_s     ), spawn "autorandr.py --change")
    , ((modm,               xK_b     ), spawnBrowser)
    , ((modm .|. shiftMask, xK_b     ), spawnFileBrowser)
    , ((modm .|. mod1Mask,  xK_b     ), spawn "btheadset")
    , ((modm,               xK_d     ), spawnEditor)
    , ((modm,               xK_x     ), spawn "galculator")
    , ((modm,               xK_a     ), spawn "notify-send \"$(date)\"")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [{-(W.greedyView, 0),-} (W.shift, shiftMask)]]
    ++
    [((modm, k), switchTopic myTopicConfig i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- ------------------------- Layout ------------------------- --
myLayoutHook = smartBorders $ avoidStruts $ topicLayouts $
               tabbed shrinkText myTabTheme |||
               ResizableTall 1 (3/100) (60/100) [1] |||
               Mirror (ResizableTall 1 (3/100) (60/100) [1]) |||
               ThreeCol 1 (3/100) (1/2)

myTabTheme = Theme { fontName            = "-*-neep-medium-*-*-*-11-*-*-*-*-*-*-*"
                   , activeColor         = "#0099ff"
                   , activeBorderColor   = "#0099ff"
                   , activeTextColor     = "white"
                   , inactiveColor       = "#2a2a2a"
                   , inactiveBorderColor = "#2a2a2a"
                   , inactiveTextColor   = "grey"
                   , urgentColor         = "yellow"
                   , urgentBorderColor   = "yellow"
                   , urgentTextColor     = "red"
                   , decoHeight          = 13
                   , decoWidth           = 200
                   , windowTitleAddons   = []
                   , windowTitleIcons    = []
                   }

-- ------------------- Window management -------------------- --
myManageHook = composeAll . concat $
               [ [ isApp c                 --> doCenterFloat | c <- myFloatApps      ]
               , [ isApp c <&&> title =? t --> doCenterFloat | (c, t) <- myFloatWins ]
               , [ isApp c                 --> doIgnore      | c <- myIgnores        ]
               --, [ title =? "googleearth-bin" --> doIgnore                        ]
               , [ isFullscreen            --> doFullFloat                        ]
               , [ isApp "gnome-mplayer" <&&> title =? "Gnome MPlayer Fullscreen"
                                           --> doFullFloat                        ]
               ]
               where
                 isApp c = className =? c <||> resource =? c
                 myFloatApps = ["Pidgin"
                               ,"skype"
                               ,"xpad"
                               ,"xfrun4"
                               ,"galculator"
                               ,"pinentry"
                               ]
                 myFloatWins = [("dia",        "Dia v0.97.1"            )
                               ,("thunar",     "File Operation Progress")
                               ,("mscore.real","MuseScore Startup"      )
                               ,("gnucash",    "GnuCash"                )
                               ,("gnucash",    "Select a Budget"        )
                               ,("Iceweasel",  "Downloads"              )
                               ,("Iceweasel",  "Password Required"      )
                               ,("Icedove",    "Password Required"      )
                               ,("claws-mail", "Error"                  )
                               ,("drracket",   "DrRacket 6.1"           )
                               ]
                 myIgnores   = ["Do"
                               ,"synapse"
                               ,"xfce4-notifyd"
                               ]

-- --------------------- Logging ---------------------------- --
myLogHook xmproc = do
                   updatePointer (TowardsCentre 0.3 0.3)
                   dynamicLogWithPP $ xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppTitle  = xmobarColor "white" "" -- . shorten 70
                     }

withMyUrgencyHook = withUrgencyHookC
                      BorderUrgencyHook { urgencyBorderColor = "#ff0000" }
                      urgencyConfig { suppressWhen = Focused }

-- --------------------- Helpers ---------------------------- --
myTerminal = "urxvt"
spawnBrowser = spawn' "firefox"
spawnFileBrowser = spawnInCurrent ("thunar", [])
spawnShell = spawnInCurrent (myTerminal, [])
spawnEditor = spawnInCurrent ("gvim", [])

spawnInCurrent :: (String, [String]) -> X ()
spawnInCurrent (cmd, args) = currentTopicDir myTopicConfig >>= (spawnIn cmd args)

spawn' :: String -> X ()
spawn' cmd = void $ xfork $ executeFile cmd True [] Nothing

spawnIn :: String -> [String] -> Dir -> X ()
spawnIn cmd args dir = void $ xfork $ do
                    when (not $ null dir) $ changeWorkingDirectory dir
                    executeFile cmd True args Nothing

confirmActions :: [(String, X ())] -> X ()
confirmActions as = do
                    s <- dmenu (map fst as)
                    fromMaybe (return ()) $ lookup s as

