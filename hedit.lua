local fs = require("filesystem")
local keyboard = require("keyboard")
local shell = require("shell")
local term = require("term") -- TODO use tty and cursor position instead of global area and gpu
local text = require("text")
local unicode = require("unicode")
local computer = require("computer")

if not term.isAvailable() then
  return
end

local gpu = term.gpu()

local timers = {}
local function timerStart(index)
  timers[index] = computer.uptime()
end

local function timerElapsed(index)
  return computer.uptime() - timers[index]
end

-- %tms for the time taken in milliseconds
-- Used for profiling, you can remove this
local function timerDraw(index, x, y, s)
  local timeTakenSeconds = computer.uptime() - timers[index]
  s = s:gsub("%%tms", timeTakenSeconds * 1000)
       :gsub("%%t", timeTakenSeconds) .. (" "):rep(100)

  local fg, fgp = gpu.getForeground()
  local bg, bgp = gpu.getBackground()

  gpu.setForeground(0xFFFFFF)
  gpu.setBackground(0x000000)
  gpu.set(x, y, s)
  gpu.setForeground(fg, fgp)
  gpu.setBackground(bg, bgp)
end

local c_highlight_keyword = 0xF9275D
local c_highlight_func_def_name = 0x99FF40
local c_highlight_constant = 0xCC6DFF
local c_highlight_comment = 0x75715E
local c_highlight_string = 0xFFDB40
local c_highlight_self = 0xFD9720

local max_time_to_determine_if_multiline_comment_in_seconds = 0.05

-- [[ Highlight helper functions ]]

local leadingOrTrailingString = nil

local function noLeadingOrTrailingStringProcess(self, x, y, w, h, lineNr, wholeStr, str, a, b, s)
  local fg, fgp = gpu.getForeground()
  gpu.setForeground(self.c)
  
  local init = 1
  repeat
    local aa, bb = wholeStr:find(self.p, init)
    if aa and str:find(self.p) and not leadingOrTrailingString(self, wholeStr, str, aa, bb, s) then
      gpu.set(x + aa - 1, y, str:sub(aa, bb))
      str = str:gsub(self.p, (" "):rep(bb - aa + 1), 1)
      init = bb + 1
    elseif aa then
      init = bb + 1
    end
  until not aa or not str:find(self.p)

  gpu.setForeground(fg, fgp)

  return str
end

function leadingOrTrailingString(self, wholeStr, str, a, b, s)
  if not a then return false end
  return wholeStr:sub(a - #s, a - 1) == s or wholeStr:sub(b + 1, b + #s) == s
end

local function isolated(self, wholeStr, str, a, b)
  if not a or not b then return false end
  local leading = wholeStr:sub(a - 1, a - 1)
  local trailing = wholeStr:sub(b + 1, b + 1)

  --[[ untested, but should work better
  return (leading:match("[ (=-+,~*%[/{]") or leading == "") and (trailing:match("[ )=-+,~*%]/}]") or trailing == "")
  ]]
  return (leading:match("[ (=-+]") or leading == "") and (trailing:match("[ )=-+]") or trailing == "")
end

local highlights = {
  -- comments
  { p = "%-%-.*", c = c_highlight_comment },
  -- escapes
  { p = "\\[abfnrtv\\\"'%[%]]", c = c_highlight_constant },
  -- strings
  { p = '(["\'])(.*)%1', c = c_highlight_string, process = function(self, x, y, w, h, lineNr, wholeStr, str, a, b)
    local match = str:sub(a, b)
    local delimiter = match:sub(1, 1)

    local segments = { { a = 1 } }

    local i = 2

    while i <= #match do
      local ch = match:sub(i, i)
      if ch == delimiter then
        segments[#segments].b = i
        local ch2 = nil
        repeat
          i = i + 1
          ch2 = match:sub(i, i)
        until ch2 == delimiter or ch2 == ""
        if ch2 == delimiter then
          table.insert(segments, { a = i })
        end
      end

      i = i + 1
    end

    local fg, fgp = gpu.getForeground()
    gpu.setForeground(self.c)

    for _, seg in ipairs(segments) do
      if seg.b then
        local s = match:sub(seg.a, seg.b)
        str = str:gsub(text.escapeMagic(s), (" "):rep(#s), 1)

        -- split so we don't draw spaces, so already (escape stuff for example) highlighted stuff still shows
        local parts = { { a = 1 } }
        i = 2
        while i <= #s do
          local ch = s:sub(i, i)
          if ch == " " then
            parts[#parts].b = i - 1
            repeat
              i = i + 1
            until s:sub(i, i) ~= " "
            if s:sub(i, i) ~= "" then
              table.insert(parts, { a = i })
            end
          end

          i = i + 1
        end

        for _,part in ipairs(parts) do
          local pa, pb = part.a, part.b or #s
          gpu.set(x + a - 1 + seg.a - 1 + pa - 1, y, s:sub(pa, pb))
        end
      end
    end

    gpu.setForeground(fg, fgp)

    return str
  end, specialFinishCondition = function(self, wholeStr, str, a, b)
    return 
  end },
  -- func def name
  { p = "%sfunction (%w[%S]*)%([%w%s,%d]*%)", c = c_highlight_func_def_name, captures = true },
  { p = "^function (%w[%S]*)%([%w%s,%d]*%)", c = c_highlight_func_def_name, captures = true },
  -- constants
  { p = "nil", c = c_highlight_constant },
  { p = "false", c = c_highlight_constant },
  { p = "true", c = c_highlight_constant },
  { p = "%.%.%.", c = c_highlight_constant, process = function(self, x, y, w, h, lineNr, wholeStr, str, a, b)
    return noLeadingOrTrailingStringProcess(self, x, y, w, h, lineNr, wholeStr, str, a, b, ".")
  end, specialFinishCondition = function(self, wholeStr, str, a, b)
    return leadingOrTrailingString(self, wholeStr, str, a, b, ".")
  end },
  -- keywords
  { p = "local", c = c_highlight_keyword },
  { p = "return", c = c_highlight_keyword },
  { p = "if", c = c_highlight_keyword },
  { p = "do", c = c_highlight_keyword },
  { p = "then", c = c_highlight_keyword },
  { p = "end", c = c_highlight_keyword },
  { p = "while", c = c_highlight_keyword },
  { p = "for", c = c_highlight_keyword },
  { p = "repeat", c = c_highlight_keyword },
  { p = "until", c = c_highlight_keyword },
  { p = "function", c = c_highlight_keyword },
  { p = "or", c = c_highlight_keyword },
  { p = "and", c = c_highlight_keyword },
  { p = "in", c = c_highlight_keyword },
  { p = "else", c = c_highlight_keyword },
  { p = "not", c = c_highlight_keyword },
  { p = "elseif", c = c_highlight_keyword },
  { p = "break", c = c_highlight_keyword },
  { p = "%.%.", c = c_highlight_keyword, process = function(self, x, y, w, h, lineNr, wholeStr, str, a, b)
    return noLeadingOrTrailingStringProcess(self, x, y, w, h, lineNr, wholeStr, str, a, b, ".")
  end, specialFinishCondition = function(self, wholeStr, str, a, b)
    return leadingOrTrailingString(self, wholeStr, str, a, b, ".")
  end },
  { p = "==", c = c_highlight_keyword },
  { p = ">=", c = c_highlight_keyword },
  { p = "<=", c = c_highlight_keyword },
  { p = "<", c = c_highlight_keyword },
  { p = ">", c = c_highlight_keyword },
  { p = "=", c = c_highlight_keyword },
  { p = "~=", c = c_highlight_keyword },
  { p = "%*", c = c_highlight_keyword },
  { p = "%+", c = c_highlight_keyword },
  { p = "%-", c = c_highlight_keyword },
  { p = "%^", c = c_highlight_keyword },
  { p = "/", c = c_highlight_keyword },
  { p = "%%", c = c_highlight_keyword },
  -- self
  { p = "self", c = c_highlight_self },
  -- hex color preview
  { p = "0x(%x%x%x%x%x%x)", c = nil, process = function(self, x, y, w, h, lineNr, wholeStr, str, a, b)
    if isolated(self, wholeStr, str, a, b) then
      local hex = tonumber(str:sub(a, b):match(self.p), 16)
      local bg, bgp = gpu.getBackground()
      local fg, fgp = gpu.getForeground()
      
      local c_r, c_g, c_b = hex >> 16 & 0xFF, hex >> 8 & 0xFF, hex & 0xFF
      local perceivedLuminance = 0.299*c_r + 0.587*c_g + 0.114*c_b

      if perceivedLuminance >= 0.5 then
        gpu.setForeground(0x000000)
      else
        gpu.setForeground(0xFFFFFF)
      end
      gpu.setBackground(hex)

      gpu.set(x + a - 1, y, str:sub(a, b))

      gpu.setForeground(fg, fgp)
      gpu.setBackground(bg, bgp)

      str = str:gsub(self.p, (" "):rep(b - a + 1), 1)
    end

    return str
  end, specialFinishCondition = function(self, wholeStr, str, a, b)
    return not isolated(self, wholeStr, str, a, b)
  end }
}

for i=1,#highlights do
  if highlights[i].p:match("^%w+$") then
    table.insert(highlights, { p = "^(" .. highlights[i].p .. ")[^%a%d_]", c = highlights[i].c, captures = true })
    highlights[i].p = "%s(" .. highlights[i].p .. ")[^%a%d_]"
    highlights[i].captures = true
  end
end

local args, options = shell.parse(...)
if #args == 0 then
  io.write("Usage: edit <filename>[:linenumber]")
  io.write("Example: edit program.lua:53")
  return
end

local initial_line = tonumber(args[1]:match(":(%d+)"))
local filename = shell.resolve((args[1]:gsub(":%d+", "")))
local file_parentpath = fs.path(filename)

if fs.exists(file_parentpath) and not fs.isDirectory(file_parentpath) then
  io.stderr:write(string.format("Not a directory: %s\n", file_parentpath))
  return 1
end

local readonly = options.r or fs.get(filename) == nil or fs.get(filename).isReadOnly()

if fs.isDirectory(filename) then
  io.stderr:write("file is a directory\n")
  return 1
elseif not fs.exists(filename) and readonly then
  io.stderr:write("file system is read only\n")
  return 1
end

local function loadConfig()
  -- Try to load user settings.
  local env = {}
  local config = loadfile("/etc/edit.cfg", nil, env)
  if config then
    pcall(config)
  end
  -- Fill in defaults.
  env.keybinds = env.keybinds or {
    left = {{"left"}},
    right = {{"right"}},
    up = {{"up"}},
    down = {{"down"}},
    home = {{"home"}},
    eol = {{"end"}},
    pageUp = {{"pageUp"}},
    pageDown = {{"pageDown"}},

    backspace = {{"back"}},
    delete = {{"delete"}},
    deleteLine = {{"control", "delete"}, {"shift", "delete"}},
    newline = {{"enter"}},

    save = {{"control", "s"}},
    close = {{"control", "w"}},
    find = {{"control", "f"}},
    findnext = {{"control", "g"}, {"control", "n"}, {"f3"}}
  }
  -- Generate config file if it didn't exist.
  if not config then
    local root = fs.get("/")
    if root and not root.isReadOnly() then
      fs.makeDirectory("/etc")
      local f = io.open("/etc/edit.cfg", "w")
      if f then
        local serialization = require("serialization")
        for k, v in pairs(env) do
          f:write(k.."="..tostring(serialization.serialize(v, math.huge)).."\n")
        end
        f:close()
      end
    end
  end
  return env
end

term.clear()
term.setCursorBlink(true)

local running = true
local buffer = {}
local scrollX, scrollY = 0, 0
local config = loadConfig()

local getKeyBindHandler -- forward declaration for refind()

local function helpStatusText()
  local function prettifyKeybind(label, command)
    local keybind = type(config.keybinds) == "table" and config.keybinds[command]
    if type(keybind) ~= "table" or type(keybind[1]) ~= "table" then return "" end
    local alt, control, shift, key
    for _, value in ipairs(keybind[1]) do
      if value == "alt" then alt = true
      elseif value == "control" then control = true
      elseif value == "shift" then shift = true
      else key = value end
    end
    if not key then return "" end
    return label .. ": [" ..
           (control and "Ctrl+" or "") ..
           (alt and "Alt+" or "") ..
           (shift and "Shift+" or "") ..
           unicode.upper(key) ..
           "] "
  end
  return prettifyKeybind("Save", "save") ..
         prettifyKeybind("Close", "close") ..
         prettifyKeybind("Find", "find")
end

--

local function setStatus(value)
  local x, y, w, h = term.getGlobalArea()
  value = unicode.wlen(value) > w - 10 and unicode.wtrunc(value, w - 9) or value
  value = text.padRight(value, w - 10)
  gpu.set(x, y + h - 1, value)
end

local function getArea()
  local x, y, w, h = term.getGlobalArea()
  return x, y, w, h - 1
end

local function removePrefix(line, length)
  if length >= unicode.wlen(line) then
    return ""
  else
    local prefix = unicode.wtrunc(line, length + 1)
    local suffix = unicode.sub(line, unicode.len(prefix) + 1)
    length = length - unicode.wlen(prefix)
    if length > 0 then
      suffix = (" "):rep(unicode.charWidth(suffix) - length) .. unicode.sub(suffix, 2)
    end
    return suffix
  end
end

local function lengthToChars(line, length)
  if length > unicode.wlen(line) then
    return unicode.len(line) + 1
  else
    local prefix = unicode.wtrunc(line, length)
    return unicode.len(prefix) + 1
  end
end


local function isWideAtPosition(line, x)
  local index = lengthToChars(line, x)
  if index > unicode.len(line) then
    return false, false
  end
  local prefix = unicode.sub(line, 1, index)
  local char = unicode.sub(line, index, index)
  --isWide, isRight
  return unicode.isWide(char), unicode.wlen(prefix) == x
end

local function getMultiLineInfo(lineNr, str, timerIndex)
  if lineNr <= 1 then return false end
  if not timerIndex then
    timerIndex = lineNr
    timerStart(lineNr)
  elseif timerElapsed(timerIndex) > max_time_to_determine_if_multiline_comment_in_seconds then
    return false
  end

  local line = str or buffer[lineNr] or ""
  local prevLine = buffer[lineNr - 1] or ""

  if line:find("]]", 1, true) then
    local prevMulti, first, last = getMultiLineInfo(lineNr - 1, nil, timerIndex)
    local a, b = line:find("]]", 1, true)
    return prevMulti and not last and a ~= nil, false, a ~= nil
  end

  if line:find("--[[", 1, true) then --]]
    return true, true, false
  end

  local prevMulti, first, last = getMultiLineInfo(lineNr - 1, nil, timerIndex)
  return prevMulti and not last, false, prevMulti and line:find("]]") ~= nil
end

local function drawLine(x, y, w, h, lineNr, str)
  local yLocal = lineNr - scrollY
  if yLocal > 0 and yLocal <= h then
    str = str or removePrefix(buffer[lineNr] or "", scrollX)
    str = unicode.wlen(str) > w and unicode.wtrunc(str, w + 1) or str
    str = text.padRight(str, w)

    local fg, fgp = gpu.getForeground()
    
    local multi, first, last = getMultiLineInfo(lineNr, str)
    if multi then
      gpu.setForeground(c_highlight_comment)
      if first then
        local ca, cb = str:find("%-%-%[%[.*")
        local _, stop = str:find("]]", ca, true)
        if stop then
          cb = stop
        end

        local comment = str:sub(ca, cb)
        gpu.set(x + ca - 1, y - 1 + lineNr - scrollY, comment)
        str = str:gsub(text.escapeMagic(comment), (" "):rep(#comment), 1)
      elseif last then
        local ca, cb = str:find("^.*%]%]")
        
        local comment = str:sub(ca, cb)
        gpu.set(x + ca - 1, y - 1 + lineNr - scrollY, text.padRight(comment, w))
        str = str:gsub(text.escapeMagic(comment), (" "):rep(#comment), 1)
      else
        gpu.set(x, y - 1 + lineNr - scrollY, str)
      end
      gpu.setForeground(fg, fgp)
      return
    end

    gpu.set(x, y - 1 + lineNr - scrollY, str)
    
    local wholeStr = str
    
    local cg, cgp = fg, fgp

    for _,v in ipairs(highlights) do
      if v.process then
        local a, b
        repeat
          a, b = str:find(v.p)
          if a then
            str = v:process(x, y - 1 + lineNr - scrollY, w, h, lineNr, wholeStr, str, a, b)
          end
        until not a or (v.specialFinishCondition and v:specialFinishCondition(wholeStr, str, a, b))
      elseif v.captures then
        repeat
          local matches = { str:match(v.p) }
          local match = matches[v.captureIndex or 1]

          if match then
            if v.c ~= cg then
              cg = v.c
              gpu.setForeground(v.c)
            end

            local a, b = str:find(v.p)
            
            a = str:find(match, a, true)
            b = a + #match
            
            gpu.set(x + a - 1, y - 1 + lineNr - scrollY, match)
            str = str:gsub(text.escapeMagic(match), (" "):rep(#match), 1)
          end
        until not match
      else
        repeat
          local a, b = str:find(v.p)

          if a then
            if v.c ~= cg then
              cg = v.c
              gpu.setForeground(v.c)
            end

            local word = str:sub(a, b)
            gpu.set(x + a - 1, y - 1 + lineNr - scrollY, word)
            str = str:gsub(text.escapeMagic(word), (" "):rep(#word), 1)
          end
        until not a
      end
    end
    gpu.setForeground(fg, fgp)
  end
end

local function getCursor()
  local cx, cy = term.getCursor()
  return cx + scrollX, cy + scrollY
end

local function line()
  local cbx, cby = getCursor()
  return buffer[cby]
end

local function getNormalizedCursor()
  local cbx, cby = getCursor()
  local wide, right = isWideAtPosition(buffer[cby], cbx)
  if wide and right then
    cbx = cbx - 1
  end
  return cbx, cby
end

local function setCursor(nbx, nby)
  local x, y, w, h = getArea()
  nby = math.max(1, math.min(#buffer, nby))

  local ncy = nby - scrollY
  if ncy > h then
    term.setCursorBlink(false)
    local sy = nby - h
    local dy = math.abs(scrollY - sy)
    scrollY = sy
    if h > dy then
      gpu.copy(x, y + dy, w, h - dy, 0, -dy)
    end
    for lineNr = nby - (math.min(dy, h) - 1), nby do
      drawLine(x, y, w, h, lineNr)
    end
  elseif ncy < 1 then
    term.setCursorBlink(false)
    local sy = nby - 1
    local dy = math.abs(scrollY - sy)
    scrollY = sy
    if h > dy then
      gpu.copy(x, y, w, h - dy, 0, dy)
    end
    for lineNr = nby, nby + (math.min(dy, h) - 1) do
      drawLine(x, y, w, h, lineNr)
    end
  end
  term.setCursor(term.getCursor(), nby - scrollY)

  nbx = math.max(1, math.min(unicode.wlen(line()) + 1, nbx))
  local wide, right = isWideAtPosition(line(), nbx)
  local ncx = nbx - scrollX
  if ncx > w or (ncx + 1 > w and wide and not right) then
    term.setCursorBlink(false)
    scrollX = nbx - w + ((wide and not right) and 1 or 0)
    for lineNr = 1 + scrollY, math.min(h + scrollY, #buffer) do
      drawLine(x, y, w, h, lineNr)
    end
  elseif ncx < 1 or (ncx - 1 < 1 and wide and right) then
    term.setCursorBlink(false)
    scrollX = nbx - 1 - ((wide and right) and 1 or 0)
    for lineNr = 1 + scrollY, math.min(h + scrollY, #buffer) do
      drawLine(x, y, w, h, lineNr)
    end
  end
  term.setCursor(nbx - scrollX, nby - scrollY)
  drawLine(x, y, w, h, nby)
  --update with term lib
  nbx, nby = getCursor()
  gpu.set(x + w - 10, y + h, text.padLeft(string.format("%d,%d", nby, nbx), 10))
end

local function highlight(bx, by, length, enabled)
  local x, y, w, h = getArea()
  local cx, cy = bx - scrollX, by - scrollY
  cx = math.max(1, math.min(w, cx))
  cy = math.max(1, math.min(h, cy))
  length = math.max(1, math.min(w - cx, length))

  local fg, fgp = gpu.getForeground()
  local bg, bgp = gpu.getBackground()
  if enabled then
    gpu.setForeground(bg, bgp)
    gpu.setBackground(fg, fgp)
  end
  local indexFrom = lengthToChars(buffer[by], bx)
  local value = unicode.sub(buffer[by], indexFrom)
  if unicode.wlen(value) > length then
    value = unicode.wtrunc(value, length + 1)
  end
  gpu.set(x - 1 + cx, y - 1 + cy, value)
  if enabled then
    gpu.setForeground(fg, fgp)
    gpu.setBackground(bg, bgp)
  end
end

local function home()
  local cbx, cby = getCursor()
  setCursor(1, cby)
end

local function ende()
  local cbx, cby = getCursor()
  setCursor(unicode.wlen(line()) + 1, cby)
end

local function left()
  local cbx, cby = getNormalizedCursor()
  if cbx > 1 then
    local wideTarget, rightTarget = isWideAtPosition(line(), cbx - 1)
    if wideTarget and rightTarget then
      setCursor(cbx - 2, cby)
    else
      setCursor(cbx - 1, cby)
    end
    return true -- for backspace
  elseif cby > 1 then
    setCursor(cbx, cby - 1)
    ende()
    return true -- again, for backspace
  end
end

local function right(n)
  n = n or 1
  local cbx, cby = getNormalizedCursor()
  local be = unicode.wlen(line()) + 1
  local wide, right = isWideAtPosition(line(), cbx + n)
  if wide and right then
    n = n + 1
  end
  if cbx + n <= be then
    setCursor(cbx + n, cby)
  elseif cby < #buffer then
    setCursor(1, cby + 1)
  end
end

local function up(n)
  n = n or 1
  local cbx, cby = getCursor()
  if cby > 1 then
    local x, y, w, h = getArea()
    drawLine(x, y, w, h, cby)
    setCursor(cbx, cby - n)
  end
end

local function down(n)
  n = n or 1
  local cbx, cby = getCursor()
  if cby < #buffer then
    local x, y, w, h = getArea()
    drawLine(x, y, w, h, cby)
    setCursor(cbx, cby + n)
  end
end

local function delete(fullRow)
  local cx, cy = term.getCursor()
  local cbx, cby = getCursor()
  local x, y, w, h = getArea()
  local function deleteRow(row)
    local content = table.remove(buffer, row)
    local rcy = cy + (row - cby)
    if rcy <= h then
      gpu.copy(x, y + rcy, w, h - rcy, 0, -1)
      drawLine(x, y, w, h, row + (h - rcy))
    end
    return content
  end
  if fullRow then
    term.setCursorBlink(false)
    if #buffer > 1 then
      deleteRow(cby)
    else
      buffer[cby] = ""
      gpu.fill(x, y - 1 + cy, w, 1, " ")
    end
    setCursor(1, cby)
  elseif cbx <= unicode.wlen(line()) then
    term.setCursorBlink(false)
    local index = lengthToChars(line(), cbx)
    buffer[cby] = unicode.sub(line(), 1, index - 1) ..
                  unicode.sub(line(), index + 1)
    drawLine(x, y, w, h, cby)
  elseif cby < #buffer then
    term.setCursorBlink(false)
    local append = deleteRow(cby + 1)
    buffer[cby] = buffer[cby] .. append
    drawLine(x, y, w, h, cby)
  else
    return
  end
  setStatus(helpStatusText())
end

local function insert(value)
  if not value or unicode.len(value) < 1 then
    return
  end
  term.setCursorBlink(false)
  local cx, cy = term.getCursor()
  local cbx, cby = getCursor()
  local x, y, w, h = getArea()
  local index = lengthToChars(line(), cbx)
  buffer[cby] = unicode.sub(line(), 1, index - 1) ..
                value ..
                unicode.sub(line(), index)
  drawLine(x, y, w, h, cby)
  right(unicode.wlen(value))
  setStatus(helpStatusText())
end

local function enter()
  term.setCursorBlink(false)
  local cx, cy = term.getCursor()
  local cbx, cby = getCursor()
  local x, y, w, h = getArea()
  local index = lengthToChars(line(), cbx)
  table.insert(buffer, cby + 1, unicode.sub(buffer[cby], index))
  buffer[cby] = unicode.sub(buffer[cby], 1, index - 1)
  drawLine(x, y, w, h, cby)
  if cy < h then
    if cy < h - 1 then
      gpu.copy(x, y + cy, w, h - (cy + 1), 0, 1)
    end
    drawLine(x, y, w, h, cby + 1)
  end
  setCursor(1, cby + 1)
  setStatus(helpStatusText())
end

local findText = ""

local function find()
  local x, y, w, h = getArea()
  local cx, cy = term.getCursor()
  local cbx, cby = getCursor()
  local ibx, iby = cbx, cby
  while running do
    if unicode.len(findText) > 0 then
      local sx, sy
      for syo = 1, #buffer do -- iterate lines with wraparound
        sy = (iby + syo - 1 + #buffer - 1) % #buffer + 1
        sx = string.find(buffer[sy], findText, syo == 1 and ibx or 1, true)
        if sx and (sx >= ibx or syo > 1) then
          break
        end
      end
      if not sx then -- special case for single matches
        sy = iby
        sx = string.find(buffer[sy], findText, nil, true)
      end
      if sx then
        sx = unicode.wlen(string.sub(buffer[sy], 1, sx - 1)) + 1
        cbx, cby = sx, sy
        setCursor(cbx, cby)
        highlight(cbx, cby, unicode.wlen(findText), true)
      end
    end
    term.setCursor(7 + unicode.wlen(findText), h + 1)
    setStatus("Find: " .. findText)

    local _, address, char, code = term.pull("key_down")
    if address == term.keyboard() then
      local handler, name = getKeyBindHandler(code)
      highlight(cbx, cby, unicode.wlen(findText), false)
      if name == "newline" then
        break
      elseif name == "close" then
        handler()
      elseif name == "backspace" then
        findText = unicode.sub(findText, 1, -2)
      elseif name == "find" or name == "findnext" then
        ibx = cbx + 1
        iby = cby
      elseif not keyboard.isControl(char) then
        findText = findText .. unicode.char(char)
      end
    end
  end
  setCursor(cbx, cby)
  setStatus(helpStatusText())
end

-------------------------------------------------------------------------------

local keyBindHandlers = {
  left = left,
  right = right,
  up = up,
  down = down,
  home = home,
  eol = ende,
  pageUp = function()
    local x, y, w, h = getArea()
    up(h - 1)
  end,
  pageDown = function()
    local x, y, w, h = getArea()
    down(h - 1)
  end,

  backspace = function()
    if not readonly and left() then
      delete()
    end
  end,
  delete = function()
    if not readonly then
      delete()
    end
  end,
  deleteLine = function()
    if not readonly then
      delete(true)
    end
  end,
  newline = function()
    if not readonly then
      enter()
    end
  end,

  save = function()
    if readonly then return end
    local new = not fs.exists(filename)
    local backup
    if not new then
      backup = filename .. "~"
      for i = 1, math.huge do
        if not fs.exists(backup) then
          break
        end
        backup = filename .. "~" .. i
      end
      fs.copy(filename, backup)
    end
    if not fs.exists(file_parentpath) then
      fs.makeDirectory(file_parentpath)
    end
    local f, reason = io.open(filename, "w")
    if f then
      local chars, firstLine = 0, true
      for _, line in ipairs(buffer) do
        if not firstLine then
          line = "\n" .. line
        end
        firstLine = false
        f:write(line)
        chars = chars + unicode.len(line)
      end
      f:close()
      local format
      if new then
        format = [["%s" [New] %dL,%dC written]]
      else
        format = [["%s" %dL,%dC written]]
      end
      setStatus(string.format(format, fs.name(filename), #buffer, chars))
    else
      setStatus(reason)
    end
    if not new then
      fs.remove(backup)
    end
  end,
  close = function()
    -- TODO ask to save if changed
    running = false
  end,
  find = function()
    findText = ""
    find()
  end,
  findnext = find
}

getKeyBindHandler = function(code)
  if type(config.keybinds) ~= "table" then return end
  -- Look for matches, prefer more 'precise' keybinds, e.g. prefer
  -- ctrl+del over del.
  local result, resultName, resultWeight = nil, nil, 0
  for command, keybinds in pairs(config.keybinds) do
    if type(keybinds) == "table" and keyBindHandlers[command] then
      for _, keybind in ipairs(keybinds) do
        if type(keybind) == "table" then
          local alt, control, shift, key
          for _, value in ipairs(keybind) do
            if value == "alt" then alt = true
            elseif value == "control" then control = true
            elseif value == "shift" then shift = true
            else key = value end
          end
          local keyboardAddress = term.keyboard()
          if (not alt or keyboard.isAltDown(keyboardAddress)) and
             (not control or keyboard.isControlDown(keyboardAddress)) and
             (not shift or keyboard.isShiftDown(keyboardAddress)) and
             code == keyboard.keys[key] and
             #keybind > resultWeight
          then
            resultWeight = #keybind
            resultName = command
            result = keyBindHandlers[command]
          end
        end
      end
    end
  end
  return result, resultName
end

-------------------------------------------------------------------------------

local function onKeyDown(char, code)
  local handler = getKeyBindHandler(code)
  if handler then
    handler()
  elseif readonly and code == keyboard.keys.q then
    running = false
  elseif not readonly then
    if not keyboard.isControl(char) then
      insert(unicode.char(char))
    elseif unicode.char(char) == "\t" then
      insert("  ")
    end
  end
end

local function onClipboard(value)
  value = value:gsub("\r\n", "\n")
  local cbx, cby = getCursor()
  local start = 1
  local l = value:find("\n", 1, true)
  if l then
    repeat
      local line = string.sub(value, start, l - 1)
      line = text.detab(line, 2)
      insert(line)
      enter()
      start = l + 1
      l = value:find("\n", start, true)
    until not l
  end
  insert(string.sub(value, start))
end

local function onClick(x, y)
  setCursor(x + scrollX, y + scrollY)
end

local function onScroll(direction)
  local cbx, cby = getCursor()
  setCursor(cbx, cby - direction * 12)
end

-------------------------------------------------------------------------------

do
  local f = io.open(filename)
  if f then
    local x, y, w, h = getArea()
    local chars = 0
    for line in f:lines() do
      table.insert(buffer, line)
      chars = chars + unicode.len(line)
      if #buffer <= h then
        drawLine(x, y, w, h, #buffer)
      end
    end
    f:close()
    if #buffer == 0 then
      table.insert(buffer, "")
    end
    local format
    if readonly then
      format = [["%s" [readonly] %dL,%dC]]
    else
      format = [["%s" %dL,%dC]]
    end
    setStatus(string.format(format, fs.name(filename), #buffer, chars))
  else
    table.insert(buffer, "")
    setStatus(string.format([["%s" [New File] ]], fs.name(filename)))
  end
  
  setCursor(1, initial_line or 1)
end

while running do
  local event, address, arg1, arg2, arg3 = term.pull()
  if address == term.keyboard() or address == term.screen() then
    local blink = true
    if event == "key_down" then
      onKeyDown(arg1, arg2)
    elseif event == "clipboard" and not readonly then
      onClipboard(arg1)
    elseif event == "touch" or event == "drag" then
      local x, y, w, h = getArea()
      arg1 = arg1 - x + 1
      arg2 = arg2 - y + 1
      if arg1 >= 1 and arg2 >= 1 and arg1 <= w and arg2 <= h then
        onClick(arg1, arg2)
      end
    elseif event == "scroll" then
      onScroll(arg3)
    else
      blink = false
    end
    if blink then
      term.setCursorBlink(true)
    end
  end
end

term.clear()
term.setCursorBlink(true)
