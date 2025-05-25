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
