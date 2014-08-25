-- Imports.
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig


myFadeHook = composeAll [isUnfocused           -->  transparency 0.1
			,		            transparency 0.1
			, className =? "Wine"  --> opaque
			, className =? "PCSXR" --> opaque
			, className =? "Codeblocks" --> opaque
			]

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Pcsxr"          --> doFloat
    , className =? "Mounter2"       --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "File-roller"    --> doFloat ]

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch terminal
    [ ((modm .|. shiftMask, xK_Return  ), spawn "urxvt -e tmux")
    -- launch nicotine
    , ((modm .|. controlMask, xK_n     ), spawn "nicotine")
    -- launch claws-mail
    , ((modm .|. controlMask, xK_e     ), spawn "claws-mail")
    -- launch office
    , ((modm .|. controlMask, xK_v     ), spawn "libreoffice")
    -- launch moc
    , ((modm .|. controlMask, xK_m     ), spawn "urxvt -e mocp")
    -- launch dwb
    , ((modm .|. controlMask, xK_Return), spawn "dwb")
    -- launch ranger
    , ((modm .|. controlMask, xK_z     ), spawn "urxvt -e ranger")
    -- launch weechat
    , ((modm .|. controlMask, xK_b     ), spawn "urxvt -e weechat")
    -- launch dmenu
    , ((modm, xK_d                     ), spawn "dmenu_run -nb \"#221f1f\" -sb \"#570b0b\"")
    --launch wicd-curses
    , ((modm .|. controlMask, xK_w     ), spawn "urxvt -e wicd-curses")
    --launch htop
    , ((modm .|. controlMask, xK_h     ), spawn "urxvt -e glances")
    -- close focused window
    , ((modm .|. shiftMask, xK_c       ), kill)
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space   ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space   ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n       ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab     ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j       ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k       ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m       ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return  ), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j       ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k       ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h       ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l       ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t       ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma   ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period  ), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b       ), sendMessage ToggleStruts)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q       ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_q       ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#7e1a1a" "", ppTitle = xmobarColor "#000000" "", ppLayout = 
xmobarColor 
"#790a0a" "", ppUrgent 
= xmobarColor "#525252" "" . wrap "[" "]" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig {
        modMask = mod4Mask,
	terminal = "urxvt",
	focusFollowsMouse = True,
	borderWidth = 1,
	workspaces = ["Web", "Media", "Term", "Games", "Coding", "Devel"],
	normalBorderColor = "#7e1a1a",
	focusedBorderColor = "#000000",
	keys = myKeys,
	manageHook = myManageHook,
	logHook = fadeWindowsLogHook myFadeHook,
	handleEventHook = fadeWindowsEventHook
 }
