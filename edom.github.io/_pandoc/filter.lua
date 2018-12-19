--[[

This is a chain of these pandoc filters:
- Hierarchicalize content blocks for numbering sections and generating table of contents.
- Count words in each section.
- Demote headings (h1 to h2, h2 to h3, etc.).
- Number sections.
- Insert table of contents right before the first child of each section that has at least one child section.

Woes:
- "--base-header-level" and "--number-sections" don't interact nicely. https://github.com/jgm/pandoc/issues/5071

]]--

-- Things seem fine without this import.
-- pandoc = require("pandoc")



function the_filter_chain()
    return {
        {
            Pandoc = function (doc)
                local sections = hierarchicalize(1, doc.blocks)
                sections_count_words(sections)
                sections_number(sections, {})
                -- Do not swap the following two lines.
                sections_prepare_tocs(sections)
                sections_decorate_heads(sections)
                sections_demote_heads(sections)
                local blocks = sections_blocks(sections)
                --tprint(sections_debug(sections), 0)
                --return doc
                return pandoc.Pandoc(blocks, doc.meta)
            end
        }
    }
end



--[[
hierarchicalize : [Block] -> [Section]

This counts words while hierarchicalizing.

In the outermost call, the "level" argument must be 1.

The "blocks" argument is obtained from the "blocks" property of a "Pandoc" object.

An important limitation:
- Each heading element must be a top-level element (not wrapped in a div, for example).

Types:
- Section has the shape { head : Heading, body : [Element], children : [Section] }
    - head and children are for table of contents.
    - body includes all descendantsnot except the head; body is for word count.

This function exists because pandoc.utils.hierarchicalize HsLua FFI is fucking tricky shit.

See also:
- https://stackoverflow.com/questions/29133416/how-to-count-the-amount-of-words-in-a-text-file-in-lua
]]--
function hierarchicalize(level, blocks)

    local sections = {}
    local current_section = { head = nil, body = {} }

    function is_empty (section)
        return section.head == nil and #(section.body) == 0
    end

    function add_section (section)
        if not is_empty(section) then
            table.insert(sections, section)
        end
    end

    for block in values(blocks) do
        if block.tag == "Header" and block.level <= level then
            add_section(current_section)
            current_section = { head = block, body = {} }
        else
            table.insert(current_section.body, block)
        end
    end
    add_section(current_section)

    for section in values(sections) do
        if section.head then
            section.children = hierarchicalize(level + 1, section.body)
        else
            section.children = {}
        end
    end

    return sections
end

-- Recursively destructively augment section and its descendants with a "word_count" property.
function sections_count_words(sections)
    -- We assume that readers read this many words per minute with 100% comprehension.
    -- This assumption may not hold for dense texts such as philosophy and mathematics.
    local wpm_assumption = 200
    for section in values(sections) do
        local string = pandoc.utils.stringify(pandoc.Div(array_merge({section.head}, section.body)))
        _, section.word_count = string:gsub("%S+", "")
        section.minute = math.ceil(section.word_count / wpm_assumption)
        sections_count_words(section.children)
    end
end

-- Recursively destructively augment section and its descendants with a "number" property.
-- In the first call, "prefix" is empty array.
function sections_number(sections, prefix)
    local number = 1
    for section in values(sections) do
        if section.head then
            section.number = array_merge(prefix, {number})
            sections_number(section.children, section.number)
            number = number + 1
        end
    end
end

--[[

DEPRECATED: Use sections_local_toc_div_nil instead.

toc_entries : [Section] -> [Div]

The "sections" parameter is the output of "hierarchicalize".

use div like latex, not ul like org

]]--
function toc_entries(sections)
    local items = {}
    for section in values(sections) do
        table.insert(items, pandoc.Div(pandoc.Plain(
            array_merge(
                section.head.content
                , {pandoc.Span(
                    {pandoc.Str("(" .. tostring(section.word_count) .. " words)")}
                    , pandoc.Attr(nil, {"word_count"})
                )}
            )
        ), pandoc.Attr(nil, {"entry", "level_" .. tostring(section.level)})))
        items = array_merge(items, toc_entries(section.children))
    end
    return items
end

-- For debugging.
function sections_debug(sections)
    local secs = {}
    for section in values(sections) do
        if section.head then
            table.insert(secs, {
                level = section.level,
                head = pandoc.utils.stringify(pandoc.Div({section.head})),
                word_count = section.word_count,
                number = section.number,
                children = sections_debug(section.children)
            })
        end
    end
    return secs
end

-- Mutate the H1...H3 elements in Pandoc DOM to decorate those headings with numbers and word counts.
function sections_decorate_heads(sections)
    for section in values(sections) do
        if section.head then
            -- This is a bit dirty:
            -- We are mutating the deep contents of the Pandoc object that was passed to the filter's Pandoc method.
            -- But pandoc seems to accept this mutation.
            local s_number = table.concat(section.number, ".")
            local dom_number = pandoc.Span(
                {pandoc.Str(s_number)}
                , pandoc.Attr(nil, {"section_number"})
            )
            section.head.content = array_join({
                {dom_number},
                {pandoc.Span(section.head.content, pandoc.Attr(nil, {"section_title"}))}
            })
            sections_decorate_heads(section.children)
        end
    end
end

function sections_demote_heads(sections)
    for section in values(sections) do
        if section.head then
            section.head.level = section.head.level + 1
        end
        sections_demote_heads(section.children)
    end
end

-- Add "dom_toc_before" property to some sections.
function sections_prepare_tocs(sections)
    local first = true -- the first section with a head
    for section in values(sections) do
        if section.head then
            if first then
                first = false
                -- The TOC to insert before the heading of this section.
                section.dom_toc_before = sections_local_toc_div_nil(sections)
            end
            sections_prepare_tocs(section.children)
        end
    end
end

    --[[
    Compute a local TOC (1 level deep).

    TODO insert right before the first section whose head is not nil.

    Return a Pandoc Div or nil.
    ]]--
    function sections_local_toc_div_nil(sections)
        local items = {}
        for section in values(sections) do
            if section.head then
                local inlines_title
                if section.head.identifier then
                    inlines_title = {pandoc.Link(section.head.content, "#" .. section.head.identifier)}
                else
                    inlines_title = section.head.content
                end
                local inlines = {
                    pandoc.Span(
                        {pandoc.Str(table.concat(section.number, "."))}
                        , pandoc.Attr(nil, {"section_number"})
                    )
                    , pandoc.Span(
                        inlines_title
                        , pandoc.Attr(nil, {"section_title"})
                    )
                    , section_length_indicator_span(section)
                }
                local item = pandoc.Plain(inlines, pandoc.Attr(nil, {"entry"}))
                table.insert(items, {item})
            end
        end
        if #items <= 0 then
            return nil
        end
        return pandoc.Div(
            -- Wasted half a fucking hour figuring out what's wrong this underdocumented fucking piece of shit.
            -- How the fuck am I supposed to know that it has to be BulletList({{...}}) and not BulletList({...})?
            -- How the fuck am I supposed to know that BulletList expects an array in an array?
            {pandoc.BulletList(items)},
            pandoc.Attr(nil, {"local_table_of_contents"}) -- BulletList doesn't accept an Attr
        )
    end

        function section_length_indicator_span(section)
            local str = string.format("(%dw~%dm)", section.word_count, section.minute)
            return pandoc.Span(
                {pandoc.Str(str)}
                , pandoc.Attr(nil, {"word_count"})
            )
        end

-- [Section] -> [Block]
-- Inverse of hierarchicalize.
function sections_blocks(sections)
    local blocks = {}
    for section in values(sections) do
        if section.head then
            if section.dom_toc_before then
                array_add(blocks, {section.dom_toc_before})
            end
            local child_blocks = sections_blocks(section.children)
            array_add(blocks, {section.head})
            array_add(blocks, child_blocks)
        else
            array_add(blocks, section.body)
        end
    end
    return blocks
end




-- Missing basic functions.

function values(t)
    local i = 0
    return function() i = i + 1; return t[i] end
end

function dict_merge(a, b)
    local c = {}
    for k,v in pairs(a) do c[k] = v end
    for k,v in pairs(b) do c[k] = v end
    return c
end

function array_merge(a, b)
    local c = {}
    for k,v in pairs(a) do table.insert(c, v) end
    for k,v in pairs(b) do table.insert(c, v) end
    return c
end

function array_join(as)
    local c = {}
    for a in values(as) do
        for v in values(a) do
            table.insert(c,v)
        end
    end
    return c
end

function array_add(as, bs)
    for b in values(bs) do
        table.insert(as, b)
    end
end

function table_copy(a)
    local b = {}
    for k,v in pairs(a) do b[k] = v end
    return b
end

-- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
function dump(o)
    if type(o) == 'table' then
       local s = '{ '
       for k,v in pairs(o) do
          if type(k) ~= 'number' then k = '"'..k..'"' end
          s = s .. '['..k..'] = ' .. dump(v) .. ','
       end
       return s .. '} '
    else
       return tostring(o)
    end
end

-- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
function tprint (tbl, indent)
  if not indent then indent = 0 end
  for k, v in pairs(tbl) do
    formatting = string.rep("  ", indent) .. k .. ": "
    if type(v) == "table" then
      print(formatting)
      tprint(v, indent+1)
    elseif type(v) == 'boolean' then
      print(formatting .. tostring(v))
    else
      print(formatting .. v)
    end
  end
end



return the_filter_chain()
