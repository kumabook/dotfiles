hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
  hs.alert.show("Hello World!")
end)

local mash = {"ctrl", "alt"}

hs.hotkey.bind(mash, "`", function()
  local currentScreen = hs.mouse.getCurrentScreen()
  local allScreens = hs.screen.allScreens()

  local currentIndex = nil
  for i, screen in ipairs(allScreens) do
    if screen:id() == currentScreen:id() then
      currentIndex = i
      break
    end
  end

  local nextIndex = (currentIndex % #allScreens) + 1
  local nextScreen = allScreens[nextIndex]
  local rect = nextScreen:frame()

  hs.mouse.setAbsolutePosition({
    x = rect.x + rect.w / 2,
    y = rect.y + rect.h / 2
  })
end)


hs.hotkey.bind(mash, "Right", function()
  local win = hs.window.focusedWindow()
  if win then
    win:moveToScreen(win:screen():next())
  end
end)

hs.hotkey.bind(mash, "Left", function()
  local win = hs.window.focusedWindow()
  if win then
    win:moveToScreen(win:screen():previous())
  end
end)


hs.hotkey.bind(mash, "up", function()
  local win = hs.window.focusedWindow()
  if win then
    local screenFrame = win:screen():frame()
    local f = win:frame()
    f.y = screenFrame.y
    f.h = screenFrame.h
    win:setFrame(f)
  end
end)

local originalFrames = {}

function rememberWindowFrame(win)
  originalFrames[win:id()] = win:frame()
end

hs.hotkey.bind(mash, "return", function()
  local win = hs.window.focusedWindow()
  if win then
    rememberWindowFrame(win)
    win:moveToUnit(hs.layout.maximized)
  end
end)

hs.hotkey.bind(mash, "delete", function()
  local win = hs.window.focusedWindow()
  if win and originalFrames[win:id()] then
    win:setFrame(originalFrames[win:id()])
  end
end)

hs.hotkey.bind(mash, "down", function()
  local win = hs.window.focusedWindow()
  if not win then return end

  local frame = win:frame()
  local screen = win:screen()
  local screenFrame = screen:frame()

  local halfWidth = screenFrame.w / 2
  local leftX = screenFrame.x
  local rightX = screenFrame.x + halfWidth

  local isLeft = math.abs(frame.x - leftX) < 5 and math.abs(frame.w - halfWidth) < 5
  local isRight = math.abs(frame.x - rightX) < 5 and math.abs(frame.w - halfWidth) < 5

  if isLeft then
    win:setFrame({
      x = rightX,
      y = screenFrame.y,
      w = halfWidth,
      h = screenFrame.h
    })
  elseif isRight then
    local nextScreen = screen:next()
    local nextFrame = nextScreen:frame()

    win:moveToScreen(nextScreen)
    win:setFrame({
      x = nextFrame.x,
      y = nextFrame.y,
      w = nextFrame.w / 2,
      h = nextFrame.h
    })
  else
    local winCenterX = frame.x + frame.w / 2
    local screenMidX = screenFrame.x + halfWidth
    local moveToLeft = winCenterX < screenMidX
    local newX = moveToLeft and leftX or rightX

    win:setFrame({
      x = newX,
      y = screenFrame.y,
      w = halfWidth,
      h = screenFrame.h
    })
  end
end)

hs.hotkey.bind(mash, "C", function()
  local win = hs.window.focusedWindow()
  if not win then return end

  local frame = win:frame()
  local centerX = frame.x + frame.w / 2
  local centerY = frame.y + frame.h / 2

  hs.mouse.setAbsolutePosition({x = centerX, y = centerY})
end)

local clipboardHistoryFile = os.getenv("HOME") .. "/.hammerspoon/clipboard_history.json"
local clipboardFavoritesFile = os.getenv("HOME") .. "/.hammerspoon/clipboard_favorites.json"
local clipboardHistory = {}
local clipboardFavorites = {}
local maxHistorySize = 50

local function loadClipboardFavorites()
  local file = io.open(clipboardFavoritesFile, "r")
  if file then
    local content = file:read("*a")
    file:close()
    if content and content ~= "" then
      local ok, result = pcall(hs.json.decode, content)
      if ok and result then
        clipboardFavorites = result
      end
    end
  end
end

local function saveClipboardFavorites()
  local ok, encoded = pcall(hs.json.encode, clipboardFavorites)
  if ok and encoded then
    local file = io.open(clipboardFavoritesFile, "w")
    if file then
      file:write(encoded)
      file:close()
    end
  end
end

local function loadClipboardHistory()
  local file = io.open(clipboardHistoryFile, "r")
  if file then
    local content = file:read("*a")
    file:close()
    if content and content ~= "" then
      local ok, result = pcall(hs.json.decode, content)
      if ok and result then
        clipboardHistory = result
      end
    end
  end
end

local function saveClipboardHistory()
  local ok, encoded = pcall(hs.json.encode, clipboardHistory)
  if ok and encoded then
    local file = io.open(clipboardHistoryFile, "w")
    if file then
      file:write(encoded)
      file:close()
    end
  end
end

loadClipboardHistory()
loadClipboardFavorites()

local clipboardWatcher = hs.pasteboard.watcher.new(function()
  local ok, err = pcall(function()
    local content = hs.pasteboard.getContents()
    if type(content) ~= "string" or content == "" then return end
    if #content > 10000 then return end
    if content:find("[\0-\8\11\12\14-\31]") then return end
    if clipboardHistory[1] and clipboardHistory[1].text == content then return end

    table.insert(clipboardHistory, 1, {
      text = content,
      time = os.time()
    })
    if #clipboardHistory > maxHistorySize then
      table.remove(clipboardHistory)
    end
    saveClipboardHistory()
  end)
end):start()

local commands = {
  { text = "Reload Config", id = "reload" },
  { text = "Lock Screen", id = "lock" },
  { text = "Sleep", id = "sleep" },
  { text = "Restart Clipboard Watcher", id = "restart_watcher" },
  { text = "Clear Clipboard History", id = "clear_history" },
}

local commandActions = {
  reload = function() hs.reload() end,
  lock = function() hs.caffeinate.lockScreen() end,
  sleep = function() hs.caffeinate.systemSleep() end,
  restart_watcher = function()
    if clipboardWatcher then
      clipboardWatcher:stop()
      clipboardWatcher:start()
      hs.alert.show("Clipboard watcher restarted")
    end
  end,
  clear_history = function()
    clipboardHistory = {}
    saveClipboardHistory()
    hs.alert.show("Clipboard history cleared")
  end,
}

hs.hotkey.bind(mash, "space", function()
  hs.chooser.new(function(choice)
    if choice and commandActions[choice.id] then
      commandActions[choice.id]()
    end
  end):choices(commands):show()
end)

hs.hotkey.bind(mash, "Y", function()
  local choices = {}

  for i, item in ipairs(clipboardFavorites) do
    table.insert(choices, {
      text = item.text,
      subText = "★ Favorite",
      isFavorite = true
    })
  end

  for i, item in ipairs(clipboardHistory) do
    table.insert(choices, {
      text = item.text,
      subText = os.date("%Y/%m/%d %H:%M", item.time),
      isFavorite = false
    })
  end

  local chooser = hs.chooser.new(function(choice)
    if not choice then return end

    local mods = hs.eventtap.checkKeyboardModifiers()
    if mods.shift then
      if choice.isFavorite then
        for i, fav in ipairs(clipboardFavorites) do
          if fav.text == choice.text then
            table.remove(clipboardFavorites, i)
            break
          end
        end
        hs.alert.show("Removed from favorites")
      else
        table.insert(clipboardFavorites, 1, { text = choice.text })
        hs.alert.show("Added to favorites")
      end
      saveClipboardFavorites()
    else
      hs.pasteboard.setContents(choice.text)
      hs.eventtap.keyStrokes(choice.text)
    end
  end)

  chooser:choices(choices):show()
end)
