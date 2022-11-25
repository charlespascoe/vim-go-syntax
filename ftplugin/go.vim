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
    if !get(g:, 'go_highlight_override_existing_syntax', 1)
        return
    endif

    let l:packages = s:FindAllPackageNames()

    if hlexists('goPackageCustomNames')
        syn clear goPackageCustomNames
    end

    if len(l:packages) > 0
        " TODO: Try to handle cases where the packge name happens to be a field
        " that is a struct that is spread across multiple lines (e.g.
        " `foo.\ntime.\nblah`)
        exec 'syn keyword goPackageCustomNames '..join(l:packages, ' ')
        hi link goPackageCustomNames goPackageName
    end
endfun

au CursorHold,CursorHoldI <buffer> call <SID>RefreshPackageHighlighting()
