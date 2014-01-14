--- ~/.xmonad/xmonad.hs
--
-- author: nexus7 <minsukim[at]gmx.net> (november/december 2011) :: mito :: nyamcoder     --
-- 
-- * specials like the multiple and double line pretty printers,
--                                        and all internal documentation by me :D         --
-- 
-- * inspired by a config file by --> nnoell <nnoell3[at]gmail.com>                       --
--                                         (http://pastebin.com/f9RUqLBX)                 --
--    who himself credits:
--       " milomouse     -> copied about 60% of his xmonad.hs. Too many thanks to mention --
--         and1          -> config used as my starting grounds                            --
--         count-corrupt -> copied clickable workspaces and clickable layout swithcher "  --
-- * ivy foster --> named scratchpads
-- * (most) icons taken from subtle WM: http://subforge.org/attachments/download/43/icons.xz
--------------------------------------------------------------------------------------------
--
-- validate syntax: xmonad --recompile                                                    --
--
--------------------------------------------------------------------------------------------
--
-- 		What This Config File Provides
--
--
--
--

-------------------------------------------------------------------------------------------------------------------------------------------
---																	---
---	CONTENTS														---
---																	---
---	   I  Imported Modules		==> additional special macros to load								---
---	  II  Fonts and Colors		==> individual settings for fonts and colors							---
---	 III  Main Functions		==> coordination of all components								---
---	  VI  Statusbars		==> layout for bars displaying extra information						---
---	   V  Workspaces		==> defining virtual desktops (aka workspaces)							---
---	  VI  Tiling Layouts		==> arranging X clients on the workspaces							---
---	 VII  Manage Rules		==> setting rules for certain applications							---
---	VIII  PrettyPrinter		==> setting up how to display layouts, workspaces, and focused window title			---
---	  IX  Tools			==> configuring extra tools									---
---	   X  Themes				==>
---	  XI  Key Bindings		==> creating keyboard shortcuts for certain actions						---
---      XII  Mouse Bindings		==>	"     mouse	  "	 "     "       "						---
---	XIII  TO DO			==> possible more modifications and extra functions						---
---																	---
-------------------------------------------------------------------------------------------------------------------------------------------

---	0  Optional Preloaded Functions

{-# LANGUAGE DeriveDataTypeable		-- 
	, NoMonomorphismRestriction	-- for individual GridSelect layout (minBound/maxBound)
	, TypeSynonymInstances		-- ...
	, MultiParamTypeClasses #-}	-- for transformer TABBED


-------------------------------------------------------------------------------------------------------------------------------------------
---	I  Imported Modules

import			XMonad					-- basic configurations
import			XMonad.Core				-- fullScreenEventHook for proper fullscreen windows (darcs only)

import			XMonad.Layout
import			XMonad.Layout.IM			-- template layout for IM and gimp respectively
--import		XMonad.Layout.Gaps			-- disabled, since there's no bottom bar, and stalonetray keeping the top gap
import			XMonad.Layout.Named			-- custom layout names
import			XMonad.Layout.Tabbed			
import			XMonad.Layout.Circle
import			XMonad.Layout.StackTile			-- http://xmonad.org/xmonad-docs/xmonad-contrib/index.html
import			XMonad.Layout.OneBig
import			XMonad.Layout.Master
import			XMonad.Layout.Reflect			-- mirror actual layout on x-/y-axis
import			XMonad.Layout.Roledex			-- This is a completely pointless layout which acts like Microsoft's Flip 3D

import			XMonad.Layout.Grid			-- grid layout
import			XMonad.Layout.Circle			-- float windows circled
import			XMonad.Layout.Dishes			-- stack extra windows underneath master
import			XMonad.Layout.Mosaic			-- taller, wider, reset layouts??
import			XMonad.Layout.MosaicAlt
import			XMonad.Layout.NoBorders	(noBorders,smartBorders,withBorder) -- options for displaying (full) clients with a frame or without
import			XMonad.Layout.ResizableTile		-- expand & shrink workspaces
import			XMonad.Layout.MultiToggle		-- toggle different layouts 
import			XMonad.Layout.MultiToggle.Instances
import			XMonad.Layout.PerWorkspace (onWorkspace) -- individual workspace layouts
import			XMonad.Layout.Minimize			-- optional minimizing windows & temporal removing from layout
import			XMonad.Layout.LimitWindows		-- limiting the number of windows to be shown

import			XMonad.StackSet	(RationalRect (..), currentTag)
import  qualified	XMonad.StackSet as W			-- manageHook rules

import			XMonad.Hooks.DynamicLog			-- statusbar outputting status information to an external status bar program such as xmobar or dzen
import			XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,manageDocks) -- dock & tray management
import			XMonad.Hooks.ManageHelpers		-- doCenterFloat, isFullscreen, doFullfloat, doSideFloat, doIgnore
import			XMonad.Hooks.UrgencyHook		-- window alert bells --> http://braincrater.wordpress.com/2009/03/14/pimp-your-xmonad-4-urgency-hooks/
import			XMonad.Hooks.EwmhDesktops		-- fullScreenEventHook for proper fullscreen windows ??
import			XMonad.Hooks.SetWMName			-- setWMname
--import			XMonad.Hooks.FadeInactive (setOpacity)	-- here needed for scratchpads

import			XMonad.Prompt				-- xmonad's own command prompt
import			XMonad.Prompt.Shell
import			XMonad.Prompt.XMonad

import			XMonad.Util.Cursor			-- 
import			XMonad.Util.Run (spawnPipe)
import			XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import			XMonad.Util.NamedScratchpad		-- configure individual scratchpads

import			XMonad.Actions.CycleWS (nextWS, prevWS, toggleWS, toggleOrView)	-- cycling through workspaces; toggle last ws
import			XMonad.Actions.GridSelect		-- tool for navigating through all clients
import			XMonad.Actions.FloatKeys		-- enable floating a client by key press
import  qualified 	XMonad.Actions.FlexibleResize as Flex
import			XMonad.Actions.PerWorkspaceKeys		-- per ws key binding

import			Data.Monoid				-- myEventHook = 
import			Data.List				-- for clickable workspaces
import	qualified	Data.Map as M				-- `M.fromList'

import			Graphics.X11.ExtraTypes.XF86

import			System.Exit				-- exitWith
import			System.IO (Handle, hPutStrLn)

-- http://xmonad.org/xmonad-docs/xmonad-contrib/

-------------------------------------------------------------------------------------------------------------------------------------------
---	II  Fonts and Colors

myFont			= "xft:OCR A:size=10"
dzenFont		= "xft:OCR A:size=10"
myTitleFont		= "UnDotum-14:autohint=true"
myEmptyTitleFont	= "UnDotum-14:italic"	-- for when there is no window
wsFont			= "MarVAlea-20"		-- special workspace font

colorBlack		= "#020202"
colorBlackAlt		= "#1c1c1c"
colorGray		= "#444444"
colorGrayAlt		= "#222222"
colorWhite		= "#dddddd"
colorWhiteAlt		= "#aaaaaa"
colorMagenta		= "#8e82a2"
colorBlue		= "#3399ff"
colorRed		= "#d74b73"
colorGreen		= "#99cc66"
myNormalBorderColor	= colorGrayAlt		-- frame color	    for non-focused windows	
myFocusedBorderColor	= colorBlue		-- 	  		focused window
myLayoutFgColor		= colorBlue		-- foreground color of layout indicator
myLayoutBgColor		= colorGrayAlt		-- background


-------------------------------------------------------------------------------------------------------------------------------------------
---	III  Main Functions (and some Hooks)

main :: IO ()
main  = do
	status  <- spawnPipe myDzenStatus					-- display layouts, workspaces, and focused window title
--	wsBar  <- spawnPipe myWsBar
	wsBar1	<- spawnPipe myWsBar1
	wsBar2	<- spawnPipe myWsBar2
	conky   <- spawnPipe myDzenMusic					--	   music titles
	conky   <- spawnPipe myDzenSystem					--	   system values			
	pipe    <- spawnPipe "xmobar $HOME/.xmonad/xxmobarrc-ko"			--	   weather and date			==> 
	xmonad	$ myUrgencyHook	$ defaultConfig					-- 
		{ terminal		= myTerminal				-- define default vte
		, modMask		= mod4Mask				-- use the "Windows"-keys as standard for key bindings
		, focusFollowsMouse	= True
		, borderWidth		= 1
		, normalBorderColor	= myNormalBorderColor
		, focusedBorderColor	= myFocusedBorderColor
		, layoutHook		= myLayoutHook				-- define a set of layouts			==>
--		, workspaces		= myWorkspaces1 <+> myWorkspaces2				-- define individual workspaces			==>
		, workspaces		= myWorkspaces
		, manageHook		= manageDocks 	<+> myManageHook	-- how to deal with bars and windows		==> 
		, logHook		= myLogHookA status <+> myLogHookB wsBar1 <+> myLogHookB wsBar2
		, handleEventHook	= fullscreenEventHook			-- enable to display fullscreen windows
		, keys			= myKeys				-- definitions for shortcuts (aka keybindings)	==> 
		, mouseBindings		= myMouseBindings			-- definitons for mouse actions			==>
		, startupHook		= myStartupHook				-- other important settings at session start
		}

myStartupHook	= setWMName "LG3D"	>> setDefaultCursor xC_left_ptr		-- make desktop recognizable for java apps & set X cursor

myLogHookA h	= dynamicLogWithPP	$ myDzenPP	{ ppOutput = hPutStrLn h }	-- pass desktop action logs to the PP w/ dzen2	==> 
myLogHookB h	= dynamicLogWithPP	$ wsPP		{ ppOutput = hPutStrLn h }
--myLogHookB h	= dynamicLogWithPP	$ ws1PP		{ ppOutput = hPutStrLn h }	-- wsPP
--myLogHookC h	= dynamicLogWithPP	$ ws2PP		{ ppOutput = hPutStrLn h }

-- xmonad myloghook
-- http://www.reddit.com/r/xmonad/comments/ksfbe/having_trouble_configuring_dzen_with_xmonad/
-- http://ench.info/2010/02/current-xmonad-setup
-- http://pbrisbin.com/posts/xmonad_statusbars
--
-- myLogHook :: Handle -> X ()
-- myLogHook h = dynamicLogWithPP $ defaultPP
--      { ... }
--
-- http://www.mntnoe.com/wp-content/uploads/2010/05/xmonad.hs.html  ===  mehrere logs

-- xmonad myworkspaces myloghook

myUrgencyHook	= withUrgencyHook dzenUrgencyHook				-- special settings for urgent events (say apps gaining
	{ args	= ["-fn", dzenFont, "-bg", colorGrayAlt, "-fg", colorGreen] }	-- attention)

--myTerminal	= "XDG_CONFIG_HOME=$HOME/.config/Terminal terminal"	-- doesn't help resetting config for myTerminal	==> IX(d)
myTerminal      = "urxvtc"


-------------------------------------------------------------------------------------------------------------------------------------------
---	IV  Statusbars

---     dzen-bar for the PrettyPrinter with individual layout (top left)		==> section xx
myDzenStatus    = "dzen2 -w '920' -ta 'l'" ++ myDzenStyle
myDzenStyle     = " -h '34' -fg '#3399ff' -bg '#222222'"

---     two xmobar instances for the workspaces

myWsBar2        = "xmobar $HOME/.xmonad/xwsBar2rc"
myWsBar1        = "sleep 4 && xmobar $HOME/.xmonad/xwsBar1rc"	--	"sleep 2 && xmobar $HOME/.xmonad/xwsBar1rc"
-- myWsBar2        = "xmobar $HOME/.xmonad/xwsBar2rc"



---     dzen-bar using conky to display music (top right, left bottom bar)   
myDzenMusic     = "conky -c $HOME/.xmonad/musicrc | dzen2 -x '920' -y '16' -w '475' -ta 'l' -bg '#222222' -fn 'UnDotum-10' " -- +30

-- (1) --> leds, die aus sind, auf #444444 setzen!
-- (2) windowtitle von urxvt wie den von terminal anzeigen lassen!

---     dzen-bar using conky to display system values (top right, right bottom bar)
myDzenSystem    = "conky -c $HOME/.xmonad/systemrc | dzen2 -x '1385' -y '16' -w '365' -ta 'r' -bg '#222222' -fn 'UnDotum-10' "

---     xmobar for date and weather (top right, right upper bar)
---	--> look for its own config file in ... xxx


{-	Notes
	-----
	    (i)	dzen2
		the system logs actually use conky ...
	   (ii)	xmobar config files don't allow comments, so ...
		 *	XMonad logs need the StdinReader-command and its template as well as a pre-defined xmobarPP, 	==> section xx
		**	while other need other commands for using specific xmobar modules (like 

or running own shell scripts
		***	Here, the very configs for the workspace bars are pretty much the same, except for their absolute positions and
			alignments: The top bar is aligned to the left (default), while the bottom is to the right (note the explicit usage of
			the seperator "}{"). Since the widths of both bars are exactly half of that from the string of numbers representing the
			workspace names, they appear as its first and second halves -- so the trick is done by simply using the same xmobarPP
			twice. Tip: with placing the %StdinReader%-template between the brackets, one can (theoretically) get a triple lined output for
			e.g. 9, 12 or 15 workspaces. ;)
			lowerstart false
	  (iii)	dzen vs xmobar
			xmobar: no icons, fonts only; 
			Unicode covers dice symbols with one to six eyes only (U+   -- U+   );
			here the special MarVAlea-font by Martin Vogel covers dice numbers from 0 to 9, keeping the
			workspace area smart and compact.

	References
	----------
			arch wikis
			dzen2 readme
-}
-------------------------------------------------------------------------------------------------------------------------------------------
---	V  Workspaces

myWorkspaces = map show $ [1..9] ++ [0]						-- represent ws names as numbers from 1 to 9, and 0 respectively

-- where 1 is for ...

-------------------------------------------------------------------------------------------------------------------------------------------
---	VI  Layout Declarations

myLayoutHook	= id
--	$ gaps [(U,16), (D,0), (L,0), (R,0)]	-- weglassen, da sonst stalonetray floatet
	$ avoidStruts			-- 
	$ minimize
	$ mkToggle (single TABBED)	-- enable 
	$ mkToggle (single MIRROR)
	$ mkToggle (single REFLECTX)
	$ mkToggle (single REFLECTY)
	$ onWorkspace "1" sysLayouts	--   special ws "1" layouts; alternatively "(myWorkspaces !! 1)" if renamed
	$ onWorkspace "4" gfxLayouts	--		"4"					     4
	$ onWorkspace "5" dtpLayouts	--		"5" 					     5
	$ onWorkspace "6" docLayouts	--		"6"					     6
	$ onWorkspace "7" wwwLayouts	--		"7" 					     7
	$ onWorkspace "8" comLayouts	--		"8" 					     8
	$ onWorkspace "9" mdiLayouts	--		"9"					     9
	$ onWorkspace "0" vmfLayouts	--		"0"					     0
	$ allLayouts			-- ... other defined layouts else.
	where				-- creating own layout collections...
		allLayouts	= myMirr ||| myTile ||| myObig ||| myMosA ||| myTabM
		sysLayouts	= myLogs
		wwwLayouts	= myTabs ||| myTabM ||| myStck
		dtpLayouts	= myTabM ||| myTile
		docLayouts	= myFull ||| myDocs
		gfxLayouts	= myGimp
		comLayouts	= myChat
		mdiLayouts	= myFull ||| myPrat ||| myCirc
		vmfLayouts	= myFull ||| myTabs
					-- ... with definitions (incl. "named"-feature)
		myTile	= named "T"	(smartBorders (ResizableTall 1 0.03 0.5 []))
		myMirr	= named "MT"	(smartBorders (Mirror myTile))
		myMosA	= named "M"	(smartBorders (MosaicAlt M.empty))
		myObig	= named "O"	(smartBorders (OneBig 0.75 0.65))
		myDocs	= named "D"	(Roledex)
		myCirc	= named "C"	(Circle)
		myFull	= named	"F"	(smartBorders (Full))
		myTabs	= named "TS"	(smartBorders (tabbed shrinkText myTabTheme))
		myTabM	= named "TM"	(smartBorders (mastered 0.01 0.4	$ (tabbed shrinkText myTabTheme)))
		myStck	= named "ST"	(smartBorders (StackTile 1 (3/100) (1/2)))
--	orig:	myGimp	= named "G"	(withIM (0.15) (Role "gimp-toolbox")	$ reflectHoriz $ withIM (0.15) (Role "gimp-dock") (myTabs))
-- vgl. https://bbs.archlinux.org/viewtopic.php?pid=1060444#p1060444
-- note: no special Gimp configs necessary anymore since Gimp 2.8 in single window mode
		myGimp  = named "G"     (withIM (0.15) (Role "toolbox")    $ reflectHoriz $ withIM (0.15) (Role "dock") (myTabs))
		myChat	= named "CH"	(withIM (0.20) (Title "Buddy List")	$ Mirror myTile)
		myPrat	= named "P"	(limitWindows 3 $ Dishes nmaster ratio)
						where
							nmaster	= 1		-- default number of windows in the master pane
							ratio	= 1/3		-- default proportion of screen occupied by other panes
		myLogs  = named "L"     (withIM procmeterSize procmeter (allLayouts))
						where
							procmeterSize	= 0.094	-- size of screen procmeter will occupy  
							procmeter	= ClassName "ProcMeter3"      -- match procmeter

{-	References
	----------
		  (i)	http://www.haskell.org/haskellwiki/Xmonad/General_xmonad.hs_config_tips#Gimp
		 (ii)	http://pbrisbin.com/posts/xmonads_im_layout
		(iii)	http://kitenet.net/~joey/blog/entry/xmonad_layouts_for_netbooks/
-}

--onWorkspace "2:im" (named "IM" (reflectHoriz $ withIM (1%8) (Title "Buddy List") (reflectHoriz $ dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs)))) 
-------------------------------------------------------------------------------------------------------------------------------------------
---	VII  Manage Hook		==> what to do with certain apps; use again (myWorkspaces !! n) if renamed

myManageHook :: ManageHook
myManageHook = (composeAll 	. concat $		-- definitions for special actions, groups of apps, and certain window client properties ...
	[ [resource	=? r	--> doIgnore		| r <- myIgnoreR]	-- apps & desktop elements not to be treated as clients by resource
	, [className	=? c	--> doIgnore		| c <- myIgnoreC]	-- 							by class
	, [className	=? c	--> doShift "1"		| c <- mySysS	]	-- move mySysS  windows  to ws  "1" by classname
	, [className    =? c    --> doShiftAndGo "2"         | c <- myMngS	]	--	myMngS			"2"
	, [className    =? c    --> doShift "3"         | c <- myDicS	]	-- 	myDicS			"3"
        , [className    =? c    --> doShiftAndGo "4"	| c <- myGfxS	]	--	myGfxS			"4"
	, [className    =? c    --> doShift "5"         | c <- myDtpS	]	--	myDtpS			"5"
	, [className	=? c	--> doShift "6"		| c <- myDocS	]	-- 	myDocS			"6"	
	, [className	=? c	--> doShiftAndGo "7"	| c <- myWebS	]	--	myWebS			"7"
	, [className	=? c	--> doShift "8"		| c <- myChatS	]	--	myChatS			"8"
        , [className    =? c    --> doShiftAndGo "9"	| c <- myPlrS	]	--	myPlrS			"9"		and follow focus
	, [className	=? c	--> doShiftAndGo "0"	| c <- myGameS	]	--	myGameS			"0"
	, [className	=? c	--> doCenterFloat	| c <- myFloatCC]	-- float centered   by classname 
--	, stringProperty "blah blah" =? "wallpaper1.jpg" --> doShift "web"
--	, [name		=? n	--> doShift "2"		| n <- myMngWall]
	, [stringProperty "WM_NAME" =? "2wall" --> doShift "2"]
	, [name		=? n	--> doCenterFloat	| n <- myFloatCN]	--		    by name
	, [name		=? n	--> doSideFloat CE	| n <- myFloatCE]	--	 east
	, [name		=? n	--> doSideFloat NE	| n <- myFloatNE]	--	 north-east
--	, className =? "Gimp"	--> doFloat

        , [name		=? n	--> doMaster		| n <- myMasterS]	-- set as master
	, [className	=? c	--> doF W.focusDown	| c <- myFocusDC]	-- don't focus at launch by classname
	, [isFullscreen		--> doF W.focusDown	<+> doFullFloat]
	]) <+> namedScratchpadManageHook myScratchpads	--									==> IXd
	where						-- ... and its contents
		doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
		doMaster	= doF W.shiftMaster
		role		= stringProperty "WM_WINDOW_ROLE"
		name		= stringProperty "WM_NAME"
		myIgnoreR	= ["desktop","desktop_window","stalonetray"]
		myIgnoreC	= ["Rainlendar","XEyes"]
		mySysS		= ["ProcMeter3","Main.py"]			-- "Main.py" is pkgbrowser, alt. see title =? "PkgBrowser"; pcurses zenman (! zsh)
		myMngS		= ["Thunar","tdfsb","Tlmgr","Moto4lin"]		-- dropbox *sql cms-div; fz full
		myDicS		= ["Stardict","org-omegat-Main","Midori","Gtranslator","Poedit"]	-- bitext2tmx gok transifex
		myDtpS		= ["libreoffice-writer","libreoffice-calc","libreoffice-impress"
					,"Evince","Scribus","Texmaker","Yudit","Apvlv","Zim","Frescobaldi","Musescore"]	-- unoconv abiword? siag dudenbib tustep vim/tex emacs/auctex  cxoffice
		myDocS		= ["Acroread","Calibre","Lucidor","Qcomicbook","ComicMaster"]		-- zathura pandoc wordpress 
		myWebS		= ["Firefox","Uzbl-tabbed"]	-- wordpress TinyMCE flatpress apache atl.
		myGfxS		= ["Gimp","gimp","Mirage","libreoffice-draw","fontforge","Xsane","Mtpaint","TuxPaint.TuxPaint"] -- vym inkscape
		myChatS		= ["Pidgin","Xchat","Filezilla"]
		myPlrS		= ["Vlc","Streamtuner2","K3b","Easytag","Avidemux2_gtk","Praat","Mixxx","Opensong","Isomaster"]	-- vcdimager lyrics-div mythtv
		myGameS		= ["Etracer","Oolite","scummvm","Ccgo","Colorcode","Celestia","Stellarium","Starplot"]	-- freeorion playonlinux; attn oolite: needs bash & gnustep
		myFloatCC	= ["Gimp","gimp","MPlayer", "File-roller", "zsnes", "Exo-helper-1", "Gksu", "PSX", "Nvidia-settings","Display"
					,"XFontSel","XClock","Warnung"]
		myFloatCN	= ["color-exchange","xmessage","gxmessage","Firefox-Einstellungen","Preferences","Search Engines","File Properties"
					,"Einstellungen","Gtranslator-Einstellungen anpassen","Neues Dokument","ColorCode"]
		myFloatCE	= ["Einstellungen Werkzeugleiste"]
		myFloatNE	= ["Event Tester","Rainlendar","pyChrom"]
		myFocusDC	= ["Event Tester","Notify-osd"]
		myMasterS	= ["Praat Objects","Vorschau Perfection1640:002"]  -- declare which X client from a multi window app becomes master when started
-- https://bbs.archlinux.org/viewtopic.php?id=132319  -->
		myMngWall	= ["feh [1 of 1] - steel-bg.jpg"]



-- zsh-profile --> path für scribus hinzu!
-- mysql postgresql libreoffice-impress
-- cli: units talkfilters
-- https://wiki.archlinux.org/index.php/Scientific_Applications
-- https://wiki.archlinux.org/index.php/Pacman_GUI_Frontends#pkgnotify.sh
-- coding ws
-- games ws
-- ws for database tools/cms/social media : typo3 *sql wordpress atl.
-- finances ws for economic stock charts or personal business stuff, eg gnucash hibiscus buddi tickvue
-- (natural) sciences

-- unoconv: http://dag.wieers.com/home-made/unoconv/
-- http://www.artofsolving.com/opensource/jodconverter
{-	References
	---------- 
	http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
-}


-------------------------------------------------------------------------------------------------------------------------------------------
---	VIII  PrettyPrinters	==> how to display ppLayout, myWorkspaces, and ppTitle using individual styles

myDzenPP        = dzenPP							-- invoke a dzen pretty printer
	{ ppSort		= fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)	-- hide "NSP" from workspace list
	, ppOrder		= \(ws:l:t:_)	-> [l,t]			-- change order of PP elements to layout--workspaces--title
	, ppExtras		= []						-- no additional stuff like logging tools
	, ppSep			= "          "	-- specify a PP element seperator; here used for providing a wide gap left for the workspaces
	, ppTitle		= dzenColor colorBlue 	colorGrayAlt	. wrapClickTitle	.	-- show focused window's title
		(\x -> if null x	-- If there are actually no clients here, display the following ...
			then "^fn(" ++ myEmptyTitleFont ++ ")" ++ "^fg(" ++ "#dddddd" ++ ")" ++ "  준비!"  -- (Get) ready!/At your service!
					-- ... and the focused windows' title with a maximum length of 90 chars and trailing dots otherwise:
			else "^fn(" ++ myTitleFont ++ ")" ++ "  " ++ shorten 90 x ++ "  "
		) . dzenEscape
	, ppLayout         	= dzenColor colorBlue	colorGrayAlt	. wrapClickLayout	. 
               (\x -> case x of
			"Minimize T"			-> " ^i($HOME/.icons/xdzen/tall20n.xbm) "	++ " "	-- "ReTall"
			"Minimize O"			-> " ^i($HOME/.icons/xdzen/onebig20n.xbm) "	++ " "	-- "OneBig"
			"Minimize TS"			-> " ^i($HOME/.icons/xdzen/tab1-20n.xbm) "	++ " "  -- "Tabbed"
			"Minimize TM"			-> " ^i($HOME/.icons/xdzen/tab2-20n.xbm) "	++ " "  -- "Master"
			"Minimize M"			-> " ^i($HOME/.icons/xdzen/mosaic20n.xbm) "	++ " "  -- "Mosaic"
			"Minimize MT"			-> " ^i($HOME/.icons/xdzen/mtall20n.xbm) "	++ " "	-- "Mirror"
			"Minimize G"			-> " ^i($HOME/.icons/xdzen/gimp20n.xbm) "	++ " "	-- "Gimp" -- "Mosaic"
			"Minimize CH"			-> " ^i($HOME/.icons/xdzen/balloon20nr.xbm) "	++ " "	-- "Chat" -- "Mirror"
			"Minimize F"			-> " ^i($HOME/.icons/xdzen/full2-20n.xbm) "	++ " "	-- "Full"
			"Minimize C"			-> " ^i($HOME/.icons/xdzen/circle20n.xbm) "	++ " "  -- "Circle"
			"Minimize D"			-> " ^i($HOME/.icons/xdzen/index28n.xbm) "	++ " "  -- "Roledex"
			"Minimize ST"			-> " ^i($HOME/.icons/xdzen/stack3n.xbm) "	++ " "	-- "Stack"
			"Minimize L"			-> " ^i($HOME/.icons/xdzen/meter20n.xbm) "	++ " "	-- "Syslogs"
			"Minimize P"			-> " ^i($HOME/.icons/xdzen/gnuplot20.xbm) " ++ " "  -- "Praat"
			"Minimize ReflectX L"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/meter20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX T"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/tall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX O"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/onebig20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX TS"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/tab1-20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX TM"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/tab2-20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX M"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/mosaic20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX MT"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/mtall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX G"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/gimp20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectX CH"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/balloon20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")X^fg()" ++ "        "
			"Minimize ReflectY T"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/tall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY O"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/onebig20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY MT"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/mtall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY TM"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/tab2-20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY M"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/mosaic20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY MT"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/mtall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY G"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/gimp20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectY CH"		-> "^fg(" ++ colorGreen ++ ")" ++ " ^i($HOME/.icons/xdzen/balloon20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")Y^fg()" ++ "        "
			"Minimize ReflectX ReflectY T"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/tall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY O"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/onebig20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY TS"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/tab1-20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY TM"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/tab2-20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY M"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/mosaic20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY MT"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/mtall20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY G"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/gimp20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			"Minimize ReflectX ReflectY CH"	-> "^fg(" ++ colorGreen ++ ")" ++ "^i($HOME/.icons/xdzen/balloon20n.xbm)" ++ "^fn(" ++ myTitleFont ++ ")XY^fg()" ++ "        "
			_				-> "^fg(" ++ colorRed ++ ")" ++ " ^i($HOME/.icons/xdzen/flag20n.xbm) ^fg()" ++ " "
		)
	}
	where
		-- enable layout and title clickability:
		wrapClickLayout content	= "^ca(1,xdotool key super+space)" ++ content ++ "^ca()"
		wrapClickTitle content	= "^ca(1,xdotool key super+j)" ++ content ++ "^ca()"

wsPP = xmobarPP	{ ppOrder		= \(ws:l:t:_)	-> [ws]				-- invoke an xmobar pretty printer template used for easyly
		, ppCurrent		= xmobarColor	colorBlue	colorGrayAlt	-- displaying the double line workspace indicator only,
		, ppUrgent		= xmobarColor	colorGreen	colorGrayAlt	-- including its different colors for the actual states
		, ppVisible		= xmobarColor	colorGray	colorGrayAlt
		, ppHidden		= xmobarColor	colorWhite	colorGrayAlt
		, ppHiddenNoWindows	= xmobarColor	colorGray	colorGrayAlt
		, ppWsSep		= ""
		, ppSep			= ""
		}
	where
		currentWsIndex w	= case (elemIndex w myWorkspaces) of
			Nothing		-> "1"
			Just n		-> show (n+1)

-- noch mehr xpms bei siag!

{-	Notes
	-----
			see sections III, IV, V

	References
	----------



	haven't experienced urgent cases yet
-}

-------------------------------------------------------------------------------------------------------------------------------------------
---	IX  Extra Tools


--- (a)	Enable Tabbing (independent from tiling layouts)
data TABBED	= TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k	= k (named "TS" (smartBorders (tabbedAlways shrinkText myTabTheme))) (\_ -> x)

myTabTheme	= defaultTheme
	{ fontName		= myFont
	, inactiveBorderColor	= colorWhiteAlt
	, inactiveColor		= colorGrayAlt
	, inactiveTextColor	= colorWhiteAlt
	, activeBorderColor	= colorBlue
	, activeColor		= colorGrayAlt
	, activeTextColor	= colorBlue
	, urgentBorderColor	= colorGreen
	, urgentTextColor	= colorGreen
	, decoHeight		= 18
	}

--- (b) Xmonad Prompt (for commands and launching programs)
myXPConfig	= defaultXPConfig
	{ font			= myFont
	, bgColor		= colorGrayAlt
	, fgColor		= colorBlue
	, bgHLight		= colorBlue
	, fgHLight		= colorWhite
	, borderColor		= colorBlue
	, promptBorderWidth	= 1
	, height		= 34
	, position		= Top
	, historySize		= 100
	, historyFilter		= deleteConsecutive
	, autoComplete		= Nothing
	}

--- (c) GridSelect (easy window navigation)
myColorizer 	= colorRangeFromClassName
	(0x88,0x88,0x88)	-- background color:	lowest  inactive client
	(0x44,0x44,0x44)	-- 	 "		highest inactive
	(0x22,0x22,0x22)	-- 	 "		     active 	 client
	(0xdd,0xdd,0xdd)	-- foreground color:	     inactive
	(0x33,0x99,0xff)	-- 	 "		     active

myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight		= 48
	, gs_cellwidth		= 280
	, gs_cellpadding	= 5
	, gs_font		= myFont
	}

--- (d) Scratchpads (indivual floating terminals)

myScratchpads :: [NamedScratchpad] 	-- ++ reset
myScratchpads =
	[ NS "term"	"urxvt -title Scratchpad"	(title =? "Scratchpad")					-- see (i)
			(customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))	-- width for gap from left x gap from top x client width x height of screen
        , NS "xmas"     "urxvt -title '*** Xmaspad :: 메리 크리스마스 ^^ ***'  -xrm 'URxvt.perl-ext: '"       (title =? "*** Xmaspad :: 메리 크리스마스 ^^ ***")
                        (customFloating $ W.RationalRect (1/6) (2/5) (2/3) (6/21))
	, NS "alsi"	"urxvt -title ':: Alsipad ::' -xrm 'URxvt.perl-ext: '"	(title =? ":: Alsipad ::")
			(customFloating $ W.RationalRect (1/6) (2/5) (2/3) (9.35/21))
	, NS "music"	"urxvt -T ':: Jukebox ::' -depth 32 -fg '#000000' -xrm 'URxvt.perl-ext: ' -pixmap '$HOME/.xmonad/wallpapers/amaterasu.png' -tr -tint '#000033' -sh 10 -e ncmpcpp"
			(title =? ":: Jukebox ::")
			(customFloating	$ W.RationalRect (1/6) (1/6) (2/3) (2/3))
	, NS "notes" 	"urxvt -T ':: 메모 ::' -xrm 'URxvt.perl-ext: ' -e joe"
			(title =? ":: 메모 ::")
			(customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))						-- see (ii)
--	, NS "procs"	"urxvt -T Procs -e htop --tab -T Loads -e ttyload"	-- see (iii)
--	, NS "procs"    "urxvt -T ':: Procs ::' -xrm 'URxvt.session: L|P' -pixmap '$HOME/.xmonad/wallpapers/pizzabytheslice.jpg'"
	, NS "procs"    "urxvt -T ':: Procs ::' -xrm 'URxvt.background: rgba:0000/dddd/dddd/0000' -xrm 'URxvt.session: L|P'"
			(title =? ":: Procs ::")
			(customFloating $ W.RationalRect l t w h)								-- see (iv)
	, NS "loads"	"urxvt -T ':: Loads ::' -e ttyload"
			(title =? ":: Loads ::")
			(customFloating $ W.RationalRect (0.02/8) (0.25/8) (0.99/1) (2/3))
	, NS "tex"	"winefish"			(className =? "Winefish")	-- ändern, sonst alle wf als ns!
			(customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
	]	-- . concat reset
		where	-- alternative way to define size and position:								-- see also (v)
			h = 0.7		-- height, 70% of screen
			w = 0.53	-- width, 53% of screen
			t = 0.15	-- position beginning 25% of screen height from top
			l = 0.2		--		      30%	    width	left

-- stardict ranger pcmanfm

{-	Notes
	-----
	  (i)	For (most) special characters, other sets like CJK, or spaces as the window's title use "-T '...'". Also since there's no font substitution for any bar, many
		special UTF-8 characters such as ⚒ ⚙ ⚡ ☻ ☯ ♫ and the like can only be displayed in ppTitle with an appropriate (Unicode) myTitlefont-
		set containing them.
	 (ii)	==> IX Key Bindings for an alternative way to launch a NS
??	(iii)	NS not floating when started with tabs (additional --geometry 80x40 or a starting script wouldn't help), so invoke tabs manually
		with < ^C-^S-T >
	 (iv)	alternative positioning
	  (v)	how to set absolute size/position in px?
-}

-- http://www.linux-community.de/Internal/Artikel/Print-Artikel/LinuxUser/2002/05/Jo-s-alternativer-Desktop-ProcMeter/
-- show keys


-------------------------------------------------------------------------------------------------------------------------------------------
---	XI  Key Bindings

myKeys	:: XConfig Layout	-> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ 
	[
--- System exec
	  ((modMask			, xK_o)		, spawn "gksu halt"				) -- power off
	, ((modMask	.|. shiftMask	, xK_o)		, spawn "gksu reboot"				) -- reboot   
--- Xmonad exec
	, ((modMask     .|. shiftMask   , xK_q)         , io	(exitWith ExitSuccess)			) -- quit xmonad   
	, ((modMask                     , xK_q)         , do						  -- see (i)
								spawn "killall xmobar conky"
								restart "xmonad" True			) -- restart xmonad & close bar apps before
--- Tools exec
	, ((modMask			, xK_numbersign), shellPrompt myXPConfig			) -- Xmonad shell prompt
	, ((mod1Mask			, xK_numbersign), xmonadPrompt myXPConfig			) -- Xmonad prompt
	, ((modMask			, xK_g)		, goToSelected	$ myGSConfig myColorizer	) -- GridSelect navigation
        , ((modMask                     , xK_F10)	, spawn "$HOME/.scripts/xkeys"			) -- show list of actual key bindings
	, ((modMask			, xK_F1)	, namedScratchpadAction myScratchpads "term"	) -- scratchpad #1
	, ((0				, xF86XK_Tools)	, namedScratchpadAction myScratchpads "music"	) -- 		#2
	, ((modMask			, xK_F2)	, namedScratchpadAction myScratchpads "notes"	) -- 		#3
	, ((modMask			, xK_F3)	, namedScratchpadAction myScratchpads "procs"	) --		#4
	, ((modMask			, xK_F4)	, namedScratchpadAction myScratchpads "loads"	) --		#5
	, ((modMask			, xK_F9)	, namedScratchpadAction myScratchpads "tex"	) --		#6
	, ((modMask			, xK_F8)	, namedScratchpadAction myScratchpads "xmas"	)
--- Sound control
	, ((0			, xF86XK_AudioMute)	, spawn "amixer set Master toggle"		) -- toggle mute/unmute
	, ((0			, xF86XK_AudioRaiseVolume), spawn "amixer set Master 4%+"		) -- increase volume
	, ((0			, xF86XK_AudioLowerVolume), spawn "amixer set Master 4%-"		) -- decrease volume
        , ((modMask .|. shiftMask , xF86XK_Tools)	, spawn "killall mpd; mpd; terminal -x ncmpcpp"	) -- (re)start mpd & launch audio player
	, ((0			, xF86XK_AudioNext)	, spawn "ncmpcpp next"				) -- next song   
	, ((0			, xF86XK_AudioPrev)	, spawn "ncmpcpp prev"				) -- prev song   
	, ((0			, xF86XK_AudioPlay)	, spawn "ncmpcpp toggle"			) -- toggle play/pause 
	, ((0			, xF86XK_AudioStop)	, spawn "ncmpcpp stop"				) -- stop song   
--- Screen control
	, ((modMask			, xK_s)		, spawn "xscreensaver-command -lock"		) -- lock screen
--	, ((0			, xF86XK_MonBrightnessUp), spawn "	"				) -- increase brightness
--	, ((0			, xF86XK_MonBrightnessDown), spawn "	"				) -- decrease	  "
--- Workspace actions
--	, ((modMask			, xK_b)		, sendMessage ToggleStruts			) -- toggle bars off/on (not in scope)
	, ((modMask			, xK_d)		, spawn "killall -9 dzen2 conky stalonetray"	) -- kill all bars and system tray
	, ((modMask			, xK_comma)	, toggleWS					) -- toggle to the workspace displayed previously
--	, ((mod1Mask			, xK_masculine)	, toggleOrView (myWorkspaces !! 0)		) -- if ws != 0 then move to workspace 0, else mov
	, ((mod1Mask	.|. controlMask	, xK_Left)	, prevWS					) -- Move to previous Workspace
	, ((modMask			, xK_Left)	, prevWS					)
	, ((modMask			, xK_Right)	, nextWS					) -- Move to next Workspace
	, ((mod1Mask	.|. controlMask	, xK_Right)	, nextWS					)
	, ((modMask			, xK_comma)	, sendMessage (IncMasterN 1)			) -- Increment the number of windows in the master
	, ((modMask			, xK_period)	, sendMessage (IncMasterN (-1))			) -- Deincrement the number of windows in the mast
	, ((0				, xK_Print)	, spawn "scrot '$HOME/Desktop/%Y-%m-%d_$wx$h.png'"	) -- screenshot right now & here
{-	, ((0				, xK_F10)	, bindOn					  -- 
								[ ( "0"	, spawn "terminal")		  --
								, ( "8"	, spawn ("xeyes -geometry 55x50+940+0" ++ -- lil cursor finding helper;
								"-fg \\^[#3399ff -center grey95 -outline grey25"))  -- see (ii) & (iii)
								, ( ""	, spawn "gxmessage hello")]	) --
        , ((0                           , xK_F11)       , bindOn                                          -- 
                                                                [ ( "2" , spawn "feh --bg-tile $HOME/.xmonad/wallpapers/Greenstone.bmp")               --
								, ( "3" , spawn "feh --bg-max $HOME/.xmonad/wallpapers/kor-schamanin.jpg")
								, ( "9" , spawn "feh --bg-fill $HOME/.xmonad/wallpapers/JL1171M_StarTrekChair.jpg")
								, ( "" , spawn "feh --bg-fill $HOME/.xmonad/wallpapers/wallpaper-962899.jpg")])
-}
--- Layout actions
	, ((modMask			, xK_space)	, sendMessage NextLayout			) -- Rotate through the available layout algorithm
	, ((modMask 	.|. controlMask	, xK_space)	, setLayout 	$ XMonad.layoutHook conf	) -- Reset the layouts on the current workspace to
	, ((mod1Mask			, xK_F12)	, sendMessage 	$ XMonad.Layout.MultiToggle.Toggle TABBED) -- push layout into tabbed
	, ((modMask 	.|. controlMask	, xK_z)		, sendMessage	$ Toggle MIRROR			) -- Push layout into mirror   
	, ((modMask 	.|. controlMask	, xK_x)		, sendMessage	$ XMonad.Layout.MultiToggle.Toggle REFLECTX) -- Reflect layout by X
	, ((modMask 	.|. controlMask	, xK_y)		, sendMessage	$ XMonad.Layout.MultiToggle.Toggle REFLECTY) -- Reflect layout by Y
--- Clients actions
	, ((modMask			, xK_c)		, kill						) -- close focused window
	, ((mod1Mask			, xK_F4)	, kill						) --	      "
	, ((modMask			, xK_n)		, refresh					) -- resize viewed windows to the correct size
	, ((modMask			, xK_Tab)	, windows W.focusDown				) -- Move focus to the next window
	, ((modMask			, xK_j)		, windows W.focusDown				) --		 "
	, ((mod1Mask			, xK_Tab)	, windows W.focusDown				) --		 "
	, ((modMask			, xK_k)		, windows W.focusUp				) -- focus previous window
	, ((modMask			, xK_a)		, windows W.focusMaster				) --   "     next     "
	, ((modMask 	.|. controlMask	, xK_a)		, windows W.swapMaster				) -- swap focus with master window
	, ((modMask	.|. controlMask	, xK_j)		, windows W.swapDown				) -- 	    "	     next      "
	, ((modMask	.|. controlMask	, xK_k)		, windows W.swapUp				) --	    "        previous  "
	, ((modMask			, xK_h)		, sendMessage Shrink				) -- shrink master
	, ((modMask	.|. controlMask	, xK_Left)	, sendMessage Shrink				) --       "
	, ((modMask			, xK_l)		, sendMessage Expand				) -- expand master
	, ((modMask	.|. controlMask	, xK_Right)	, sendMessage Expand				) --       "
	, ((modMask	.|. controlMask	, xK_h)		, sendMessage MirrorShrink			) -- reflect & shrink master
	, ((modMask	.|. controlMask	, xK_Down)	, sendMessage MirrorShrink			) --           "
	, ((modMask	.|. controlMask	, xK_l)		, sendMessage MirrorExpand			) -- reflect & expand master
	, ((modMask	.|. controlMask	, xK_Up)	, sendMessage MirrorExpand			) --	       "
	, ((mod1Mask	.|. controlMask	, xK_Left)	, withFocused (keysResizeWindow (-30,0) (0,0))	) -- shrink focused float by 50px/horz.
	, ((mod1Mask	.|. controlMask	, xK_Right)	, withFocused (keysResizeWindow (30,0) (0,0))	) -- expand		"
	, ((mod1Mask	.|. controlMask	, xK_Up)	, withFocused (keysResizeWindow (0,-30) (0,0))	) -- shrink focused float by 50px/vert.
	, ((mod1Mask	.|. controlMask	, xK_Down)	, withFocused (keysResizeWindow (0,30) (0,0))	) -- expand		" 
	, ((modMask			, xK_t)		, withFocused 	$ windows	. W.sink	) -- put floated into tiling
	, ((modMask	.|. controlMask	, xK_t)		, rectFloatFocused 				) -- float window
	, ((modMask			, xK_m)		, withFocused minimizeWindow			) -- minimize window	??
	, ((modMask	.|. controlMask	, xK_m)		, sendMessage RestoreNextMinimizedWin		) -- restore minimized 
	, ((modMask	.|. controlMask	, xK_f)		, fullFloatFocused				) -- full screen float
--- Launching apps
	, ((modMask			, xK_Return)	, spawn		$ XMonad.terminal conf		) -- launch standard terminal
        , ((modMask	.|. shiftMask	, xK_v)		, spawn "gvim"					) -- text editor #1
	, ((modMask	.|. shiftMask	, xK_j)         , spawn "terminal -x joe"			) --	   "	 #2
	, ((modMask	.|. shiftMask	, xK_h)         , spawn "winefish"				) --	   "	 #3
	, ((0			, xF86XK_HomePage)	, spawn "thunar"				) -- file manager #1
        , ((modMask	.|. shiftMask	, xK_c)		, spawn "terminal -x mc"			) --	   "	  #2
        , ((modMask	.|. shiftMask	, xK_n)		, spawn "terminal -x ranger"			) --       "      #3
	, ((modMask	.|. shiftMask	, xK_f)		, spawn "LANG=de_DE.utf-8 firefox"		) -- web browser #1
        , ((mod1Mask			, xK_F1)	, spawn "LANG=ko_KR.utf-8 firefox"		) -- web browser #1
	, ((modMask	.|. shiftMask	, xK_m)		, spawn "midori"				) --	  "	 #2
        , ((modMask	.|. shiftMask	, xK_l)		, spawn "terminal -x lynx"			) --	  "	 #3
        , ((modMask	.|. shiftMask	, xK_u)		, spawn "uzbl"					) --	  "	 #4
	, ((0			, xF86XK_Mail)		, spawn "terminal -x mutt"			) -- e-mail client
	, ((modMask	.|. shiftMask	, xK_s)		, spawn "stardict"				) -- dictionary #1
	, ((modMask	.|. shiftMask	, xK_d)         , spawn "terminal -x sdcv"			) -- 	  "	#2
	, ((modMask	.|. shiftMask	, xK_g)		, spawn "gimp"					) -- graphics editor
	, ((modMask	.|. shiftMask	, xK_r)		, spawn "mirage"				) -- graphics viewer
	, ((modMask	.|. shiftMask	, xK_w)		, spawn "soffice -writer"			) -- office program #1
	, ((modMask	.|. shiftMask	, xK_c)		, spawn "soffice -calc"				) --	    "	    #2
	, ((modMask	.|. shiftMask	, xK_i)		, spawn "soffice -impress"			) --	    "	    #3
	, ((modMask	.|. shiftMask	, xK_e)		, spawn "evince"				) -- pdf viewer #1
	, ((modMask	.|. shiftMask	, xK_a)		, spawn "acroread"				) -- 	  "	#2
	, ((modMask	.|. shiftMask	, xK_y)		, spawn "vlc"					) -- media player #1
	, ((modMask	.|. shiftMask	, xK_p)		, spawn "gmrun"					) -- external starter
-- ("rails", spawn "$SHELL -c 'cd $HOME/src/rails && exec emacs'")
--- Helping apps
--	, ((modMask	.|. shiftMask	, xK_period)	, spawn ("xeyes -geometry 55x50+940+0" ++ 	-- lil cursor finding helper;
--                                                                "-fg \\#3399ff -center grey95 -outline grey25"))  -- see (ii) & (iii)                                   ) --
        , ((modMask     .|. shiftMask   , xK_period)     , spawn "xeyes -geometry 55x50+940+0 -fg \\#3399ff -center grey95 -outline grey25")
	, ((modMask	.|. shiftMask	, xK_comma)	, spawn "rainlendar"				) -- calendar widget, see (iv)
-- pcmmanfm
-- vm1 2   wine
	]
	++
	[ ((m 		.|. modMask	, k)		, windows $ f i			) -- Switch to n workspaces and send client to n workspaces
	| (i, k)	<- zip (XMonad.workspaces conf)	([xK_1 .. xK_9]	++ [xK_0])
	, (f, m)	<- [(W.greedyView, 0)		, (W.shift, shiftMask)]		]
	++
	[ ((m		.|. modMask	, key)		, do 
							screenWorkspace sc 	>>= flip whenJust (windows . f)
							spawn "xdotool key xK_F11"	)  -- Switch to nth screens and send client to n screens
	| (key, sc)	<- zip [xK_w, xK_e, xK_r] [0..]
	, (f, m)	<- [(W.view, 0)			, (W.shift, shiftMask)]		]
	where
		fullFloatFocused = withFocused	$ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
		rectFloatFocused = withFocused	$ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ RationalRect 0.05 0.05 0.9 0.9) f

{-      Notes 
        -----
          (i)	a way to represent multiple do-arguments
	 (ii)		 	   multi-line spawn-arguments
	(iii)	a special character like "#" -- what is used for the hex color value for X here for spawning
		($) xeyes -geometry 55x50+940+0 -fg \#3399ff -center grey95 -outline grey25&
		-- must be escaped with a preceeding "\^", see http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
	   ??? 	however, though compiling fine, the blue color is ignored. :P
	 (iv)	how to place a certain app at a certain position (like rainlendar, that's actually ignoring ist own positioning config)?
	 
	x	how to make scratchpads or other apps toggle on/off instead of explicitely killing them?
-}
-------------------------------------------------------------------------------------------------------------------------------------------
---	XII  Mouse Bindings

myMouseBindings (XConfig {XMonad.modMask = modMask})	= M.fromList	$
	[ ((modMask	, button1)	, (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- float window & move by dragging
	, ((modMask	, button2)	, (\w -> focus w >> windows W.shiftMaster))                      -- raise window ontop of stack
	, ((modMask	, button3)	, (\w -> focus w >> Flex.mouseResizeWindow w))                   -- float window & resize by dragging
	, ((modMask	, button4)	, (\_ -> prevWS))                                                -- switch to previous ws
	, ((modMask	, button5)	, (\_ -> nextWS))                                                --     "     next ws
	]

-------------------------------------------------------------------------------------------------------------------------------------------
---	XIII  TO DO

{-	* more key bindings, alternative colors and fonts
	* if desktop is empty, display alternative ws name instead of layout icon
-}


-------------------------------------------------------------------------------------------------------------------------------------------
--- EOF ---
-----------
