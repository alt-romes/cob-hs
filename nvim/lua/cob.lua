-- cob://<definition id> buffers, edited like oil. A field is a line
-- `/<id> Name*+: description`, with the id concealed, indented under its
-- parent; `*` makes it mandatory and `+` duplicable. A line without an id is a
-- new field, a field whose line is gone is deleted, and reordering the lines
-- reorders the fields. `:w` shows what `cobsh apply` would change, and changes
-- it once confirmed.
local M = {}

local function def(buf) return vim.api.nvim_buf_get_name(buf):match('^cob://(%d+)$') end

local function notify(msg, level) vim.notify('cob: ' .. msg, level or vim.log.levels.ERROR) end

-- `f`, reporting its errors rather than raising them.
local function protect(f)
  return function(...)
    local ok, err = pcall(f, ...)
    if not ok then notify(err) end
  end
end

-- The output of cobsh on the buffer's definition.
local function cobsh(buf, args, input)
  local cmd = { vim.g.cobsh or 'cobsh' }
  if vim.g.cobsh_config then vim.list_extend(cmd, { '-c', vim.g.cobsh_config }) end
  vim.list_extend(cmd, { '-d', def(buf) })
  local r = vim.system(vim.list_extend(cmd, args), { text = true, stdin = input }):wait()
  if r.code ~= 0 then error(vim.trim(r.stderr), 0) end
  return vim.split(r.stdout, '\n', { trimempty = true })
end

-- A centered floating window closed by q or <esc>; `keys` maps more keys to actions.
local function float(title, lines, keys)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.tbl_map(function(l) return ' ' .. l end, lines))
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = 'wipe'
  local width = 0
  for _, l in ipairs(lines) do width = math.max(width, vim.fn.strdisplaywidth(l) + 2) end
  width = math.min(width, vim.o.columns - 4)
  local height = math.min(#lines, vim.o.lines - 4)
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor', style = 'minimal', border = 'rounded', title = ' ' .. title .. ' ', title_pos = 'center',
    width = width, height = height,
    row = math.floor((vim.o.lines - height) / 2) - 1, col = math.floor((vim.o.columns - width) / 2),
  })
  local function close()
    if vim.api.nvim_win_is_valid(win) then vim.api.nvim_win_close(win, true) end
  end
  for lhs, f in pairs(vim.tbl_extend('keep', keys or {}, { q = false, ['<esc>'] = false })) do
    vim.keymap.set('n', lhs, function() close(); if f then f() end end, { buffer = buf, nowait = true })
  end
  vim.api.nvim_create_autocmd('WinLeave', { buffer = buf, once = true, callback = vim.schedule_wrap(close) })
  return buf
end

function M.render(buf)
  local lines = cobsh(buf, { 'lines' })
  local wins = vim.fn.win_findbuf(buf)
  local cursors = vim.tbl_map(vim.api.nvim_win_get_cursor, wins)
  local ul = vim.bo[buf].undolevels
  vim.bo[buf].undolevels = -1
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].undolevels = ul
  vim.bo[buf].modified = false
  for i, win in ipairs(wins) do
    vim.api.nvim_win_set_cursor(win, { math.max(1, math.min(cursors[i][1], #lines)), cursors[i][2] })
  end
end

function M.write(buf)
  local text = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), '\n')
  local summary = cobsh(buf, { 'apply', '--dry-run' }, text)
  if summary[1] == 'Nothing to change' then return M.render(buf) end
  local width = 0
  for _, l in ipairs(summary) do width = math.max(width, vim.fn.strdisplaywidth(l)) end
  local lines = vim.list_extend(summary, { '', (' '):rep(math.floor((width - 11) / 2)) .. '[y]es  [n]o' })
  -- Scheduled, as focus returns to the written buffer once the write command ends.
  vim.schedule(function()
    local f = float('Apply to definition ' .. def(buf) .. '?', lines, {
      y = protect(function() cobsh(buf, { 'apply' }, text); M.render(buf) end),
      n = false,
    })
    vim.api.nvim_buf_call(f, function()
      vim.cmd([[
        syntax match CobAdded /^ + .*/
        syntax match CobRemoved /^ - .*/
        highlight default link CobAdded DiagnosticOk
        highlight default link CobRemoved DiagnosticError
      ]])
    end)
  end)
end

local function id() return vim.api.nvim_get_current_line():match('^/(%d+) ') end

local keymaps

local function help()
  local lines = vim.tbl_map(function(k) return ('%-6s %s'):format(k[1], k[3]) end, keymaps)
  float('cob', vim.list_extend(lines, {
    '',
    ':w     apply the edits, after confirmation',
    'line   Name*+: description, where * is mandatory and + duplicable',
    'new    a line without an id; indented under a field: a subfield',
    'dd     delete the field',
    'dd, p  reorder the field among its siblings',
  }))
end

keymaps = {
  { 'K', function()
    local i = id()
    if i then float('Field ' .. i, cobsh(0, { 'field', i })) end
  end, 'the field' },
  { 'gi', function() float('Definition ' .. def(0), cobsh(0, { 'info' })) end, 'the definition' },
  { 'g.', function() vim.wo.conceallevel = vim.wo.conceallevel == 0 and 3 or 0 end, 'show or hide field ids' },
  { '<c-l>', function()
    if vim.bo.modified then return notify('unwritten edits, kept; :e! discards them', vim.log.levels.INFO) end
    M.render(0)
  end, 'refresh' },
  { 'g?', help, 'this help' },
}

-- Keeps the cursor off the concealed id.
local function clamp()
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local prefix = vim.api.nvim_get_current_line():match('^/%d+ ')
  if vim.wo.conceallevel > 0 and prefix and col < #prefix then vim.api.nvim_win_set_cursor(0, { row, #prefix }) end
end

function M.setup()
  local group = vim.api.nvim_create_augroup('Cob', { clear = true })
  vim.api.nvim_create_autocmd('BufReadCmd', {
    group = group,
    pattern = 'cob://*',
    callback = protect(function(args)
      local buf = args.buf
      if not def(buf) then error('expected cob://<definition id>', 0) end
      vim.bo[buf].buftype = 'acwrite'
      vim.bo[buf].swapfile = false
      vim.bo[buf].filetype = 'cob'
      vim.api.nvim_buf_call(buf, function()
        vim.cmd([[
          syntax clear
          syntax match CobId /^\/\d\+ / conceal
          syntax match CobKeyword /\$\S\+/
          syntax match CobFlags /[*+]\+\ze:/
          highlight default link CobKeyword Type
          highlight default link CobFlags Special
        ]])
      end)
      for _, k in ipairs(keymaps) do
        vim.keymap.set('n', k[1], protect(k[2]), { buffer = buf, desc = 'cob: ' .. k[3] })
      end
      vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, { group = group, buffer = buf, callback = clamp })
      vim.api.nvim_create_autocmd('BufWriteCmd', { group = group, buffer = buf, callback = protect(function() M.write(buf) end) })
      M.render(buf)
    end),
  })
  vim.api.nvim_create_autocmd('BufWinEnter', {
    group = group,
    pattern = 'cob://*',
    callback = function()
      vim.opt_local.conceallevel = 3
      vim.opt_local.concealcursor = 'nvic'
      vim.opt_local.expandtab = true
      vim.opt_local.shiftwidth = 2
    end,
  })
end

return M
