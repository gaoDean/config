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

	M.autolist = function()
		local preceding_line = vim.fn.getline(vim.fn.line(".") - 1)
		if preceding_line:match("^%d+%.%s.") then
			local list_index = preceding_line:match("^%d*")
			vim.fn.setline(".", list_index + 1 .. ". ")
			vim.cmd([[execute "normal \<esc>A\<space>"]])
		elseif preceding_line:match("^%d+%.%s$") then
			vim.fn.setline(vim.fn.line(".") - 1, "")
		elseif preceding_line:match("^%s?[-+*]") and #preceding_line:match("[-+*].*") == 1 then
			vim.fn.setline(vim.fn.line(".") - 1, "")
			vim.fn.setline(".", "")
		end
	end

return M
