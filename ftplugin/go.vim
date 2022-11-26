fun! s:FindAllImports()
    let l:winview = winsaveview()
    let l:pos = getpos('.')
    let l:imports = []

    keepjumps normal gg

    let l:import_line = search('^import\s\+($', 'W')

    if l:import_line != 0
        keepjumps normal $
        let l:end_import_line =  searchpair('(', '', ')', 'Wn')

        if l:end_import_line > 0
            let l:imports = getbufline(bufnr('%'), line('.')+1, l:end_import_line-1)
        end
    else
        " TODO: Check both, not just one?
        let l:import_line = search('^import\s\+[^(]$', 'W')

        while l:import_line != 0
            call add(l:imports, getline('.')[len("import"):])
            let l:import_line = search('^import\s\+[^(]$', 'W')
        endwhile
    end

    call map(l:imports, 'trim(v:val)')
    call winrestview(l:winview)

    return l:imports
endfun

fun! s:FindAllPackageNames()
    let l:imports = s:FindAllImports()
    let l:packages = []

    for l:import in l:imports
        if l:import =~ '^"'
            let l:path = split(substitute(l:import, '"', '', 'g'), '/')
            let l:last = l:path[len(l:path) - 1]
            call add(l:packages, split(l:last, '\.')[0])
        elseif trim(l:import) != ''
            call add(l:packages, split(l:import, ' ')[0])
        end
    endfor

    return l:packages
endfun

fun! s:RefreshPackageHighlighting()
    if !get(b:, '__vim_go_syntax', 0)
        return
    endif

    let l:packages = s:FindAllPackageNames()

    if hlexists('goImportedPackages')
        syn clear goImportedPackages
    end

    if len(l:packages) > 0
        exec 'syn keyword goImportedPackages '..join(l:packages, ' ')
        hi link goImportedPackages goPackageName
    end
endfun

if get(g:, 'go_highlight_imports', 1)
    au CursorHold,CursorHoldI <buffer> call <SID>RefreshPackageHighlighting()
    au BufEnter <buffer> ++once call <SID>RefreshPackageHighlighting()
endif
