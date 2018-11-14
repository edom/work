-- pandoc = require("pandoc")

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

local counter = {
    section = {1, 1, 1, 1, 1, 1},
}

local root_sections

-- This exists because pandoc.utils.hierarchicalize HsLua FFI is fucking tricky shit.
-- hierarchicalize : [Block] -> [Section]
-- Section = { head : Heading, body : [Element], children : [Section] }
-- head and children are for table of contents.
-- body is for word count.
function hierarchicalize(level, blocks)
    local sections = {}
    local current_section = { level = level, head = nil, body = {}, word_count = 0 }
    for k,child in pairs(blocks) do
        if child.tag == "Header" and child.level <= level then
            if current_section.head then
                local body_string = pandoc.utils.stringify(pandoc.Div(current_section.body))
                -- https://stackoverflow.com/questions/29133416/how-to-count-the-amount-of-words-in-a-text-file-in-lua
                _, current_section.word_count = body_string:gsub("%S+", "")
                table.insert(sections, current_section)
            end
            current_section = { level = level, head = child, body = {}, word_count = 0 }
        else
            table.insert(current_section.body, child)
        end
    end
    for k,section in pairs(sections) do
        section.children = hierarchicalize(level + 1, section.body)
    end
    return sections
end

-- sections is output of hierarchicalize
-- use div like latex, not ul like org
-- toc_entries : [Section] -> [Div]
function toc_entries(sections)
    local items = {}
    for k,section in pairs(sections) do
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

return {
    {
        Pandoc = function (doc)
            root_sections = hierarchicalize(1, doc.blocks)
            doc.blocks = array_merge({pandoc.Div(toc_entries(root_sections), pandoc.Attr(nil, {"table_of_contents"}))}, doc.blocks)
            return doc
        end,

        Header = function (e)
            -- Number the section.
            e.section = {}
            for i = 1, e.level do
                e.section[i] = counter.section[i]
            end
            -- Stringify the counter.
            local str_number = ""
            for i = 1, e.level do
                str_number = str_number .. tostring(e.section[i])
                if i ~= e.level then str_number = str_number .. "." end
            end
            -- Insert the HTML element.
            local attr = pandoc.Attr(nil, {"number"})
            local html = pandoc.Span({pandoc.Str(str_number)}, attr)
            e.content = {html, pandoc.Span(e.content, pandoc.Attr(nil, {"title"}))}
            -- Increment the counter.
            for i = e.level, #(counter.section) do
                if i == e.level then
                    counter.section[i] = counter.section[i] + 1
                else
                    counter.section[i] = 1
                end
            end
            return e
        end,
    },
    {
        Pandoc = function (doc)
            -- doc.blocks = array_merge({toc_entries(root_sections)}, doc.blocks)
            return doc
        end,
    }
}
