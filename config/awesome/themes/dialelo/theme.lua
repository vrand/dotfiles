---------------------------
-- Default awesome theme --
---------------------------

theme_root = "/home/dialelo/.config/awesome/themes/dialelo/"

theme = {}

-- Font
theme.font = "sans 10"

-- Colors
BLACK = "#000000"
WHITE = "#FFFFFF"

LIGHT_GRAY = "#ECECEC"
DARK_GRAY = "#606060"

LIGHT_RED = "#E4001B"

PALE_BLUE = "#D0EAFF"
LIGHT_BLUE = "#A5D1F4"
DARK_BLUE = "#000044"
BLUE = "#6194BC"

theme.bg_normal     = LIGHT_GRAY
theme.bg_focus      = LIGHT_BLUE
theme.bg_urgent     = LIGHT_RED
theme.bg_minimize   = PALE_BLUE

theme.fg_normal     = BLACK
theme.fg_focus      = DARK_BLUE
theme.fg_urgent     = WHITE
theme.fg_minimize   = BLACK

theme.border_width  = "1"
theme.border_normal = BLACK
theme.border_focus  = PALE_BLUE
theme.border_marked = BLUE

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Display the taglist squares
taglist_root = theme_root .. "taglist/"

theme.taglist_squares_sel   = taglist_root .. "squarefw.png"
theme.taglist_squares_unsel = taglist_root .. "squarew.png"

theme.tasklist_floating_icon = taglist_root .. "floatingw.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = theme_root .. "submenu.png"
theme.menu_height = "15"
theme.menu_width  = "100"

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
titlebar_root = theme_root .. "titlebar/"

theme.titlebar_close_button_normal = titlebar_root .. "close_normal.png"
theme.titlebar_close_button_focus  = titlebar_root .. "close_focus.png"

theme.titlebar_ontop_button_normal_inactive = titlebar_root .. "ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = titlebar_root .. "ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = titlebar_root .. "ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = titlebar_root .. "ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = titlebar_root .. "sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = titlebar_root .. "sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = titlebar_root .. "sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = titlebar_root .. "sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = titlebar_root .. "floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = titlebar_root .. "floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = titlebar_root .. "floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = titlebar_root .. "floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = titlebar_root .. "maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = titlebar_root .. "maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = titlebar_root .. "maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = titlebar_root .. "maximized_focus_active.png"

-- You can use your own command to set your wallpaper
theme.wallpaper_cmd = { "/home/dialelo/bin/randombg" }

-- You can use your own layout icons like this:
layout_root = theme_root .. "layouts/"

theme.layout_fairh = layout_root .. "fairhw.png"
theme.layout_fairv = layout_root .. "fairvw.png"
theme.layout_floating  = layout_root .. "floatingw.png"
theme.layout_magnifier = layout_root .. "magnifierw.png"
theme.layout_max = layout_root .. "maxw.png"
theme.layout_fullscreen = layout_root .. "fullscreenw.png"
theme.layout_tilebottom = layout_root .. "tilebottomw.png"
theme.layout_tileleft   = layout_root .. "tileleftw.png"
theme.layout_tile = layout_root .. "tilew.png"
theme.layout_tiletop = layout_root .. "tiletopw.png"
theme.layout_spiral  = layout_root .. "spiralw.png"
theme.layout_dwindle = layout_root .. "dwindlew.png"

theme.awesome_icon = theme_root .. "icons/tux.png"

return theme
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
