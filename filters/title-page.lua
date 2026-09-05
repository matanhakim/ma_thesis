-- Layout helpers shared by the three output formats.
--
-- * A div with class `title-page` holds the cover pages of the thesis; every
--   paragraph in it is centred. In Word this is done with the "Title Page"
--   paragraph style of the reference document, in Typst with `align(center)`,
--   and in HTML with the class itself (see assets/thesis.css).
-- * In the PDF every chapter (level-one heading) starts on a new page, as the
--   Word heading style of the thesis does. Typst cannot break pages from
--   inside a container, so the break is inserted here, between top-level
--   blocks, rather than in a show rule.

local function typst_block(code)
  return pandoc.RawBlock("typst", code)
end

function Div(el)
  if el.classes:includes("title-page") then
    if FORMAT:match("docx") then
      el.attributes["custom-style"] = "Title Page"
      return el
    elseif FORMAT:match("typst") then
      local blocks = pandoc.List({ typst_block("#align(center)[\n#set par(first-line-indent: 0pt, justify: false)") })
      blocks:extend(el.content)
      blocks:insert(typst_block("]"))
      return blocks
    end
    return el
  end
end

function Pandoc(doc)
  if not FORMAT:match("typst") then
    return doc
  end
  local blocks = pandoc.List()
  for i, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 and i > 1 then
      blocks:insert(typst_block("#pagebreak(weak: true)"))
    end
    blocks:insert(block)
  end
  doc.blocks = blocks
  return doc
end
