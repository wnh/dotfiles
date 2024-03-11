--------------------------------------------------------------------------------
---     Caffeine Replacement
--------------------------------------------------------------------------------
caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
    if state then
        caffeine:setIcon("/Users/wharding/caffeine-on.pdf")
        -- caffeine:setTitle("☕")
    else
        caffeine:setIcon("/Users/wharding/caffeine-off.pdf")
        -- caffeine:setTitle("💤")
    end
end

function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end

--------------------------------------------------------------------------------
---     Pomodoro timer 
--------------------------------------------------------------------------------
hs.loadSpoon("Cherry")


function myLaunchOrFocus(bundleID, location)
-- "com.vscodium"
-- "org.mozilla.firefox"
-- "com.tinyspeck.slackmacgap"
-- "com.install4j.8357-7994-5030-9105.837"
-- app: <userdata 1> -- hs.application: kitty (0x600002bc5af8)
-- "net.kovidgoyal.kitty"
-- app: <userdata 1> -- hs.application: Emacs (0x600002bdec38)
-- "org.gnu.Emacs"
-- app: <userdata 1> -- hs.application: Google Chrome Canary (0x600002bc52f8)
-- "com.google.Chrome.canary"
-- app: <userdata 1> -- hs.application: Android Studio (0x600002bdde78)
-- "com.google.android.studio"
-- app: <userdata 1> -- hs.application: Hammerspoon (0x600002bc02f8)
-- "org.hammerspoon.Hammerspoon"
end

--------------------------------------------------------------------------------
---     Direct app switching
--------------------------------------------------------------------------------

hs.hotkey.bind({"cmd"}, "1", function()
  hs.application.launchOrFocus("/Applications/Firefox.app")
end)

hs.hotkey.bind({"cmd"}, "2", function()
  -- emacs = hs.appfinder.appFromName("emacs")
  emacs = hs.application.find("emacs")
  print("Emacs?:", emacs)
  if emacs ~= nil then
    emacs:setFrontmost()
  else
    hs.application.launchOrFocus("/Users/wharding/.nix-profile/Applications/Emacs.app")
  end
end)

hs.hotkey.bind({"cmd"}, "3", function()
  hs.application.launchOrFocus("/Users/wharding/.nix-profile/Applications/kitty.app")
end)

hs.hotkey.bind({"cmd"}, "4", function()
  app = hs.appfinder.appFromName("VSCodium")
  if app ~= nil then
    app:setFrontmost()
  else
    hs.application.launchOrFocus("/Users/wharding/.nix-profile/Applications/VSCodium.app")
  end
end)

hs.hotkey.bind({"cmd"}, "5", function()
  hs.application.launchOrFocus("/Applications/Studio 3T.app")
end)

hs.hotkey.bind({"cmd"}, "7", function()
  hs.application.launchOrFocus("/Applications/Android Studio.app")
end)


hs.hotkey.bind({"cmd"}, "8", function()
  hs.application.launchOrFocus("/Applications/zoom.us.app")
end)

hs.hotkey.bind({"cmd"}, "9", function()
  hs.application.launchOrFocus("/Applications/Slack.app")
end)

hs.hotkey.bind({"cmd"}, "0", function()
  hs.application.launchOrFocus("/System/Applications/Calendar.app")
end)
--hs.hotkey.bind({"cmd"}, "0", function()
--  -- print(hs.inspect("hi"))
--  hs.reload()
--end)
--
--for _, x in ipairs(hs.window.allWindows()) do
--  local app = x:application()
--  print("app: " .. hs.inspect(app))
--  print(hs.inspect(app:bundleID()))
--  if (app:bundleID() == "org.hammerspoon.Hammerspoon") then
--    x:focus()
--  end
--end
