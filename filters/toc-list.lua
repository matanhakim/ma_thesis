-- Table of contents page for the HTML output.
--
-- Word and Typst build the table of contents natively (see thesis.qmd). In
-- HTML the floating sidebar is the working table of contents; this filter
-- also fills the placeholder div `#toc-list` on the contents page with the
-- numbered headings, so that the page keeps the structure of the thesis.
-- (The lists of figures and tables are filled in the browser by
-- assets/lists.html.)

local headings = pandoc.List()

local function collect(doc)
  doc:walk({
    Header = function(h)
      if h.level <= 3 and not h.classes:includes("unlisted") then
        headings:insert({ level = h.level, content = h.content, id = h.identifier })
      end
    end,
  })
end

local function toc_list(entries)
  local items = pandoc.List()
  for _, h in ipairs(entries) do
    local link = pandoc.Link(h.content, "#" .. h.id)
    items:insert(pandoc.Plain({ link }))
  end
  return pandoc.Div(items, pandoc.Attr("", { "toc-list" }))
end

function Pandoc(doc)
  if not FORMAT:match("html") then
    return doc
  end
  collect(doc)
  return doc:walk({
    Div = function(div)
      if div.identifier == "toc-list" then
        return toc_list(headings)
      end
    end,
  })
end
