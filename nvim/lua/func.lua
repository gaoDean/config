local M = {}
	M.gitf = function()
		local git_dir = vim.fn.system('git rev-parse --show-toplevel 2> /dev/null')
		if git_dir ~= '' then -- not equal
			require('fzf-lua').files({cmd = "fd . -t f --hidden --follow --exclude .git " .. git_dir })
		else
			require('fzf-lua').files()
		end
	end

	M.setpath = function()
		vim.cmd([[
		let g:filename = expand("%:t:r")
		let g:pathname = expand("%:p:h")
		]])
	end

	M.pdfview = function()
		M.setpath()
		return string.format([[<cmd>!nuke %s/view/<cr>]], vim.g.pathname)
	end

return M
