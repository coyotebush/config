import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.Run ( spawnPipe )
import qualified XMonad.StackSet as W
import Data.Map ( fromList )
import System.IO ( hPutStrLn )
import System.Exit ( exitSuccess )

main = do
       checkTopicConfig myTopics myTopicConfig
       xmproc <- spawnPipe "xmobar"
       xmonad $ ewmh defaultConfig
        { modMask     = mod4Mask
        , borderWidth = 2
        , normalBorderColor  = "#3C3C3C" -- ala Shiki-Colors themes
        , focusedBorderColor = "#A0A0A0"
        , terminal    = "xfterm4"
        , workspaces  = myTopics
        , keys        = myKeys
        , layoutHook  = myLayoutHook
        , manageHook  = myManageHook <+> manageDocks
        , logHook     = do
                        updatePointer (TowardsCentre 0.3 0.3)
                        dynamicLogWithPP $ xmobarPP
                          { ppOutput = hPutStrLn xmproc
                          , ppTitle = xmobarColor "white" "" . shorten 70
                          }
        }

-- -------------------------- Workspaces -------------------- --
wkW = "w"; wkC = "c"
myTopics = ["1", wkW, wkC, "s"
           , "305", "308", "349", "es"
           , "job", "finance", "ta", "presentation", "hack"
           ]

myTopicConfig = defaultTopicConfig
  { topicDirs = fromList
      [ ("305", "School/2012-2013/CPE305/minimax")
      , ("308", "School/2012-2013/CPE308/eclass")
      , ("349", "School/2012-2013/CPE349")
      , ("es",  "School/2012-2013/ES112")
      , ("job", "Documents/resume")
      , ("ta", "School/2012-2013/CPE315-TA")
      ]
  , topicActions = fromList
      [ (wkW, spawnBrowser)
      , (wkC, spawn "icedove")
      , ("1", spawnFileBrowser)
      , ("finance", spawn "private gnucash")
      , ("ta", spawnFileBrowser)
      , ("presentation", return ())
      ]
  , defaultTopicAction = const $ spawnShell
  }


-- -------------------------- Keys -------------------------- --
myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
    [ ((modm,               xK_Return ), spawnShell)
    --, ((modm,               xK_space ), spawn "synapse") -- "exe=`dmenu_path | dmenu -b` && eval \"exec $exe\"")
    , ((mod1Mask,           xK_Menu  ), spawn "xfdesktop --menu")
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_z     ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_z    ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    --, ((modm,               xK_j     ), windows W.focusDown)
    --, ((modm,               xK_k     ), windows W.focusUp  )
    --, ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_grave ), windows W.swapMaster)
    --, ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    --, ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. mod1Mask,  xK_Tab   ), windows W.swapDown  )
    , ((modm .|. mod1Mask .|. shiftMask, xK_Tab), windows W.swapUp    )
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

    , ((modm .|. mod1Mask .|. shiftMask, xK_Escape), io exitSuccess)
    , ((modm .|. mod1Mask,  xK_Escape), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    
    , ((modm,               xK_Print ), spawn "scrot")
    , ((modm .|. shiftMask, xK_Print ), spawn "scrot -u")
    , ((modm,               xK_s     ), spawn "xscreensaver-command --lock")
    , ((modm .|. shiftMask, xK_s     ), spawn "dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend")
    , ((modm .|. mod1Mask,  xK_s     ), spawn "screenlayout")
    , ((modm,               xK_b     ), spawnBrowser)
    , ((modm .|. shiftMask, xK_b     ), spawnFileBrowser)
    , ((modm,               xK_d     ), spawnEditor)
    , ((modm,               xK_x     ), spawn "grpn")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_4])
        , (f, m) <- [{-(W.greedyView, 0),-} (W.shift, shiftMask)]]
    ++
    [((modm, k), switchTopic myTopicConfig i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_4])]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- ------------------------- Layout ------------------------- --
myLayoutHook = smartBorders $ avoidStruts $
               onWorkspace wkW Full $
               onWorkspace wkC (reflectVert $ Mirror $ Tall 2 (1/100) (30/100)) $
               ResizableTall 1 (3/100) (60/100) [1] |||
               Mirror (ResizableTall 1 (3/100) (60/100) [1]) |||
               Full

-- ------------------- Window management -------------------- --
myManageHook = composeAll . concat $
               [ [ isApp c                 --> doFloat     | c <- myFloatApps      ]
               , [ isApp c <&&> title =? t --> doFloat     | (c, t) <- myFloatWins ]
               , [ isApp c                 --> doIgnore    | c <- myIgnores        ]
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
                               ,"gimp"
                               ,"gimp-2.6"
                               ,"xfrun4"
                               ,"grpn"
                               ,"Melody Assistant"
                               ,"Wine"
                               ,"eclass-view-Main"
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
                               ]
                 myIgnores   = ["Do"
                               ,"synapse"
                               ,"xfce4-notifyd"
                               ]

-- --------------------- Helpers ---------------------------- --
spawnBrowser = spawn "firefox"
spawnFileBrowser = spawnInCurrent $ (++) "thunar "
spawnShell = spawnInCurrent $ (++) "xfterm4 "
spawnEditor = spawnInCurrent $ \d -> "gvim -c ':cd " ++ d ++ "'"

spawnInCurrent cmd = currentTopicDir myTopicConfig >>= (spawnIn cmd)
spawnIn :: (Dir -> String) -> Dir -> X ()
spawnIn cmd dir = spawn $ cmd dir

