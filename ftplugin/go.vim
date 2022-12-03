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
            let first = split(l:import, ' ')[0]

            if first =~? '^[a-z]\+$'
                call add(l:packages, first)
            endif
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
        exec 'syn keyword goImportedPackages '..join(l:packages, ' ')..' contained'
        hi link goImportedPackages goPackageName
    end
endfun

if get(g:, 'go_highlight_imports', 1)
    au CursorHold,CursorHoldI <buffer> call <SID>RefreshPackageHighlighting()
    au BufEnter <buffer> ++once call <SID>RefreshPackageHighlighting()
endif

fun SemTest(file)
    let s = system('gopls semtok '.a:file)
    let l:res = []
    let foo = substitute(s, '\%(/\*⇒\d,namespace,\[\]\*/\(\h\w\+\)\.\)\?/\*⇒\d,type,\[\]\*/\(\h\w\+\)', '\=add(l:res, [submatch(1), submatch(2)])', 'g')
    echom l:res

    for [pack, type] in l:res
        if pack != ''
            let pack_syn = 'goPackage_'.pack
            let pack_dot_syn = pack_syn.'_Dot'
            let pack_type_group = pack_syn.'_Types'

            if !hlexists(pack_syn)
                exec 'syn keyword '.pack_syn.' '.pack.' nextgroup='.pack_dot_syn
                exec 'syn match '.pack_dot_syn.' /./ contained nextgroup=@'.pack_type_group
                exec 'hi link '.pack_syn.' goPackageName'
                exec 'hi link '.pack_dot_syn.' goDot'
                echom 'syn keyword '.pack_syn.' '.pack.' nextgroup='.pack_dot_syn
                echom 'syn match '.pack_dot_syn.' /./ contained nextgroup=@'.pack_type_group
                echom 'hi link '.pack_syn.' goPackageName'
                echom 'hi link '.pack_dot_syn.' goDot'
            endif

            let pack_type_syn = pack_syn.'_Type_'.type

            if !hlexists(pack_type_syn)
                exec 'syn keyword '.pack_type_syn.' '.type.' contained nextgroup=goStructValueTypeArgs'
                exec 'syn cluster '.pack_type_group.' add='.pack_type_syn
                exec 'hi link '.pack_type_syn.' Type'
                echom 'syn keyword '.pack_type_syn.' '.type.' contained'
                echom 'syn cluster '.pack_type_group.' add='.pack_type_syn
                echom 'hi link '.pack_type_syn.' Type'
            endif
        else
            let type_syn = 'goType_'.type

            if !hlexists(type_syn)
                exec 'syn keyword '.type_syn.' '.type.' nextgroup=goStructValueTypeArgs'
                exec 'hi link '.type_syn.' Type'
            endif
        endif
    endfor

    return l:res
endfun
