-- `:Cob <definition id>` edits the definition's fields in a cob:// buffer (see lua/cob.lua).
-- cobsh finds its configuration as usual, or in g:cobsh_config.
local cob = require('cob')
cob.setup()

vim.api.nvim_create_user_command('Cob', function(opts)
  vim.cmd.edit('cob://' .. opts.args)
end, { nargs = 1 })
