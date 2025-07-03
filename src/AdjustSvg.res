let cleanupStart = svg =>
  svg
  ->Js.String2.replaceByRe(%re("/'/g"), "\"")
  ->Js.String2.replaceByRe(%re("/\\sversion=\"1.1\"/g"), "")
  ->Js.String2.replaceByRe(%re("/<\\?xml(.*)\\?>/g"), "")
  ->Js.String2.replaceByRe(%re("/\\sxmlns=\"http:\\/\\/www\\.w3\\.org\\/2000\\/svg\"/g"), "")
  ->Js.String2.replaceByRe(%re("/\\sxmlns:xlink=\"http:\\/\\/www.w3.org\\/1999\\/xlink\"/g"), "")
  // remove useless data
  ->Js.String2.replaceByRe(%re("/<title>(.*)<\\/title>/g"), "")
  ->Js.String2.replaceByRe(%re("/<desc>(.*)<\\/desc>/g"), "")
  ->Js.String2.replaceByRe(%re("/<!--(.*)-->/g"), "")

let prepareSvgProps = svg =>
  svg
  // remove future props
  ->Js.String2.replaceByRe(%re("/<svg\\s?([^>]*)?\\swidth=\"[^\\\"]*\"/g"), j`<svg \\$1`)
  ->Js.String2.replaceByRe(%re("/<svg\\s?([^>]*)?\\sheight=\"[^\\\"]*\"/g"), j`<svg \\$1`)
  ->Js.String2.replaceByRe(%re("/<svg\\s?([^>]*)?\\sfill=\"[^\\\"]*\"/g"), j`<svg \\$1`)

let injectSvgJsProps = svg =>
  svg->Js.String2.replace(
    ">",
    " width={width} height={height} fill={fill} stroke={stroke} style={style}>",
  )

let injectSvgReScriptProps = svg =>
  svg->Js.String2.replace(">", " ?width ?height ?fill ?stroke ?style>")

let dashToCamelCaseProps = svg =>
  svg->Js.String2.unsafeReplaceBy1(%re("/\\s([a-z-]+)/g"), (
    _matchPart,
    p1,
    _offset,
    _wholeString,
  ) => " " ++ p1->Case.toCamel)

let tagToPascalCase = svg =>
  svg->Js.String2.unsafeReplaceBy2(%re("/<(\\/?)([a-z])/g"), (
    _matchPart,
    p1,
    p2,
    _offset,
    _wholeString,
  ) => "<" ++ (p1 ++ p2->Case.toPascal))

let cleanupEndWithSpace = svg =>
  svg->Js.String2.replaceByRe(%re("/>\\s+</g"), "> <")->Js.String2.replaceByRe(%re("/></g"), "> <")

let cleanupEndWithoutSpace = svg => svg->Js.String2.replaceByRe(%re("/>\\s+</g"), "><")

let deleteFill = svg => svg->Js.String2.replaceByRe(%re("/ fill=\"[^\\\"]*\"/g"), "")
let deleteStroke = svg => svg->Js.String2.replaceByRe(%re("/ stroke=\"[^\\\"]*\"/g"), "")

let toPolyCamel = (s, re) =>
  s->Js.String2.unsafeReplaceBy2(re, (_matchPart, p1, p2, _offset, _wholeString) =>
    p1 ++ ("=#" ++ p2->Case.toCamel)
  )

let transformReScriptNativeProps = svg =>
  svg
  ->toPolyCamel(
    %re(
      "/\\b(alignmentBaseline)=\"(baseline|text-bottom|alphabetic|ideographic|middle|central|mathematical|text-top|bottom|center|top|text-before-edge|text-after-edge|before-edge|after-edge|hanging)\"/g"
    ),
  )
  ->toPolyCamel(%re("/\\b(baselineShift)=\"(sub|super|baseline)\"/g"))
  ->toPolyCamel(%re("/\\b(clipRule)=\"(evenodd|nonzero)\"/g"))
  ->toPolyCamel(%re("/\\b(fillRule)=\"(evenodd|nonzero)\"/g"))
  ->toPolyCamel(
    %re(
      "/\\b(fontStretch)=\"(normal|wider|narrower|ultra-condensed|extra-condensed|condensed|semi-condensed|semi-expanded|expanded|extra-expanded|ultra-expanded)\"/g"
    ),
  )
  ->toPolyCamel(%re("/\\b(fontStyle)=\"(normal|italic|oblique)\"/g"))
  ->toPolyCamel(%re("/\\b(fontVariant)=\"(normal|smallcaps)\"/g"))
  ->toPolyCamel(%re("/\\b(fontVariantLigatures)=\"(normal|none)\"/g"))
  ->toPolyCamel(
    %re("/\\b(fontWeight)=\"(normal|bold|bolder|lighter|100|200|300|400|500|600|700|800|900)\"/g"),
  )
  ->toPolyCamel(%re("/\\b(gradientUnits)=\"(userSpaceOnUse|objectBoundingBox)\"/g"))
  ->toPolyCamel(%re("/\\b(lengthAdjust)=\"(spacing|spacingAndGlyphs)\"/g"))
  ->toPolyCamel(%re("/\\b(markerUnits)=\"(userSpaceOnUse|strokeWidth)\"/g"))
  ->toPolyCamel(%re("/\\b(maskContentUnits)=\"(userSpaceOnUse|objectBoundingBox)\"/g"))
  ->toPolyCamel(%re("/\\b(maskUnits)=\"(userSpaceOnUse|objectBoundingBox)\"/g"))
  ->toPolyCamel(%re("/\\b(method)=\"(align|stretch)\"/g"))
  ->toPolyCamel(%re("/\\b(midLine)=\"(sharp|smooth)\"/g"))
  ->toPolyCamel(%re("/\\b(patternContentUnits)=\"(userSpaceOnUse|objectBoundingBox)\"/g"))
  ->toPolyCamel(%re("/\\b(patternUnits)=\"(userSpaceOnUse|objectBoundingBox)\"/g"))
  ->toPolyCamel(%re("/\\b(spacing)=\"(auto|exact)\"/g"))
  ->toPolyCamel(%re("/\\b(strokeLinecap)=\"(butt|square|round)\"/g"))
  ->toPolyCamel(%re("/\\b(strokeLinejoin)=\"(miter|bevel|round)\"/g"))
  ->toPolyCamel(%re("/\\b(textAnchor)=\"(start|middle|end)\"/g"))
  ->toPolyCamel(%re("/\\b(textDecoration)=\"(|none|underline|overline|line-through|blink )\"/g"))
  ->toPolyCamel(%re("/\\b(vectorEffect)=\"(none|default|non-scaling-stroke|inherit|uri)\"/g"))

type t = string
@send
external unsafeReplaceBy4: (t, Js_re.t, @uncurry (t, t, t, t, t, int, t) => t) => t = "replace"
@send
external unsafeReplaceBy12: (
  t,
  Js_re.t,
  @uncurry (t, t, t, t, t, t, t, t, t, t, t, t, t, int, t) => t,
) => t = "replace"

let undefinedString: string = Js.undefined->Obj.magic

let transformReScriptNativeSizeProps = svg =>
  svg->unsafeReplaceBy4(
    %re(
      "/\\b(cx|cy|dx|dy|fontSize|fx|fy|height|inlineSize|kerning|letterSpacing|markerHeight|markerWidth|offset|originX|originY|r|refX|refY|rotate|rotation|rx|ry|scale|startOffset|strokeDashoffset|strokeMiterlimit|strokeWidth|verticalAlign|width|wordSpacing|x|x1|x2|y|y1|y2)=\"(-?[0-9]*)(\\.[0-9]+)?(%)?\"/g"
    ),
    (_matchPart, attributeName, digits, decimals, unit, _offset, _wholeString) =>
      attributeName ++
      ("={" ++
      (digits ++
      ((decimals !== undefinedString ? decimals : ".") ++
      ("->Style." ++ ((unit === "%" ? "pct" : "dp") ++ "}"))))),
  )

let transformReScriptNativeMatrixProps = svg =>
  svg->unsafeReplaceBy12(
    %re(
      "/\\bgradientTransform=\"matrix\\((-?[0-9]*(\\.[0-9]+)?\\s+)(-?[0-9]*(\\.[0-9]+)?\\s+)(-?[0-9]*(\\.[0-9]+)?\\s+)(-?[0-9]*(\\.[0-9]+)?\\s+)(-?[0-9]*(\\.[0-9]+)?\\s+)(-?[0-9]*(\\.[0-9]+)?)\)\"/g"
    ),
    (_matchPart, _1, _, _2, _, _3, _, _4, _, _5, _, _6, _, _offset, _wholeString) =>
      "gradientTransform" ++
      "=(" ++
      _1 ++
      "," ++
      _2 ++
      "," ++
      _3 ++
      "," ++
      _4 ++
      "," ++
      _5 ++
      "," ++
      _6 ++ ")",
  )
let transformReScriptNativeFixupDigits = svg =>
  svg->Js.String2.replaceByRe(%re("/([{(,]-?)\\./g"), "$10.")

// New ReScript-specific transformations for JSX fixes

let transformClassToClassName = svg =>
  svg->Js.String2.replaceByRe(%re("/\\bclass=\"([^\"]*)\"/g"), "className=\"$1\"")

let transformReservedKeywords = svg =>
  svg
  // Transform in="..." to in_="..."
  ->Js.String2.replaceByRe(%re("/\\bin=\"([^\"]*)\"/g"), "in_=\"$1\"")
  // Transform type="..." to type_="..."
  ->Js.String2.replaceByRe(%re("/\\btype=\"([^\"]*)\"/g"), "type_=\"$1\"")

// Helper function to convert kebab-case to camelCase
let kebabToCamelCase = str =>
  str->Js.String2.unsafeReplaceBy1(%re("/-([a-z])/g"), (
    _match,
    letter,
    _offset,
    _wholeString,
  ) => letter->Js.String2.toUpperCase)

let transformStyleAttributes = svg =>
  svg->Js.String2.unsafeReplaceBy1(%re("/\\bstyle=\"([^\"]*)\"/g"), (
    _match,
    styleValue,
    _offset,
    _wholeString,
  ) => {
    // Parse CSS string into individual properties
    let cssProps = 
      styleValue
      ->Js.String2.split(";")
      ->Belt.Array.map(Js.String2.trim)
      ->Belt.Array.keep(prop => Js.String2.length(prop) > 0)
      ->Belt.Array.keepMap(prop => {
        let parts = prop->Js.String2.split(":")
        switch parts {
        | [key, value] => 
          let trimmedKey = key->Js.String2.trim
          let trimmedValue = value->Js.String2.trim->Js.String2.replaceByRe(%re("/^[\"']|[\"']$/g"), "")
          if Js.String2.length(trimmedKey) > 0 && Js.String2.length(trimmedValue) > 0 {
            Some({
              "key": trimmedKey->kebabToCamelCase,
              "value": trimmedValue
            })
          } else {
            None
          }
        | _ => None
        }
      })

    if Belt.Array.length(cssProps) == 0 {
      "style=\"" ++ styleValue ++ "\""  // Return original if no valid properties
    } else {
      // Build ReScript ReactDOM.Style chain
      let styleChain = 
        cssProps
        ->Belt.Array.reduce("ReactDOM.Style.make()", (acc, prop) =>
          acc ++ "->ReactDOM.Style.unsafeAddProp(\"" ++ prop["key"] ++ "\", \"" ++ prop["value"] ++ "\")"
        )
      "style={" ++ styleChain ++ "}"
    }
  })

let fixSvgAttributes = (svg, ~isDeprecated) => {
  // Replace hardcoded className with dynamic ?className first
  let newContent = svg->Js.String2.replaceByRe(%re("/className=\"[^\"]*\"/g"), "?className")

  // Find the SVG element and fix attribute conflicts
  let svgPattern = %re("/<svg([^>]*?)(\\s*>)/")
  switch newContent->Js.String2.match_(svgPattern) {
  | Some(matches) => 
    switch matches {
    | [_, svgAttributes, closingTag] =>
      let cleanedAttributes = 
        svgAttributes
        // Remove hardcoded attributes that conflict with optional ones (case insensitive)
        ->Js.String2.replaceByRe(%re("/\\s+stroke=\"[^\"]*\"/gi"), "")
        ->Js.String2.replaceByRe(%re("/\\s+fill=\"[^\"]*\"/gi"), "")
        ->Js.String2.replaceByRe(%re("/\\s+width=\"[^\"]*\"/gi"), "")
        ->Js.String2.replaceByRe(%re("/\\s+height=\"[^\"]*\"/gi"), "")
        ->Js.String2.replaceByRe(%re("/\\s+style=\"[^\"]*\"/gi"), "")
        // Remove any existing fill/stroke ReScript attributes to avoid duplicates
        ->Js.String2.replaceByRe(%re("/\\s+\\?fill\\b/g"), "")
        ->Js.String2.replaceByRe(%re("/\\s+\\bfill\\b(?!\\s*=)/g"), "")
        ->Js.String2.replaceByRe(%re("/\\s+\\?stroke\\b/g"), "")
        ->Js.String2.replaceByRe(%re("/\\s+\\bstroke\\b(?!\\s*=)/g"), "")
        ->Js.String2.replaceByRe(%re("/\\s+\\?className\\b/g"), "")

      // Ensure proper spacing and add dynamic attributes
      let spacedAttributes = cleanedAttributes->Js.String2.endsWith(" ") ? cleanedAttributes : cleanedAttributes ++ " "
      
      // Add dynamic attributes based on parameter type
      let dynamicAttributes = if !isDeprecated {
        // For non-deprecated, add direct references (they have default values)
        "fill stroke"
      } else {
        // For deprecated, add optional references
        "?fill ?stroke"
      }
      
      let finalAttributes = spacedAttributes ++ dynamicAttributes ++ " ?className"
      let newSvgTag = "<svg" ++ finalAttributes ++ closingTag
      
      newContent->Js.String2.replace("<svg" ++ svgAttributes ++ closingTag, newSvgTag)
    | _ => newContent
    }
  | None => newContent
  }
}

// New functions to handle multiple fills
let extractFillColors = (svg: string): array<string> => {
  // Extract all fill values from child elements (not the main SVG tag)
  
  // First, split the SVG into the opening tag and content
  let svgTagEndIndex = svg->Js.String2.indexOf(">")
  if svgTagEndIndex >= 0 {
    let svgContent = svg->Js.String2.substringToEnd(~from=svgTagEndIndex + 1)
    let fillMatches = svgContent->Js.String2.match_(%re("/fill=\"([^\"]+)\"/g"))
    
    switch fillMatches {
    | None => []
    | Some(matches) => {
        let colors = matches
          ->Belt.Array.keepMap(match => {
            let colorMatch = match->Js.String2.match_(%re("/fill=\"([^\"]+)\"/"))
            switch colorMatch {
            | Some([_, color]) => Some(color)
            | _ => None
            }
          })
          ->Belt.Array.reduce([], (acc, color) => {
            // Only add if not already in the array (deduplication)
            acc->Belt.Array.some(c => c === color) ? acc : acc->Belt.Array.concat([color])
          })
        colors
      }
    }
  } else {
    // Fallback: extract from entire SVG if we can't find the tag end
    let fillMatches = svg->Js.String2.match_(%re("/fill=\"([^\"]+)\"/g"))
    
    switch fillMatches {
    | None => []
    | Some(matches) => {
        let colors = matches
          ->Belt.Array.keepMap(match => {
            let colorMatch = match->Js.String2.match_(%re("/fill=\"([^\"]+)\"/"))
            switch colorMatch {
            | Some([_, color]) => Some(color)
            | _ => None
            }
          })
          ->Belt.Array.reduce([], (acc, color) => {
            // Only add if not already in the array (deduplication)
            acc->Belt.Array.some(c => c === color) ? acc : acc->Belt.Array.concat([color])
          })
        colors
      }
    }
  }
}

let replaceFillsWithProps = (svg: string, fillColors: array<string>): string => {
  let result = ref(svg)
  let dynamicFillCounter = ref(0)
  
  // Extract the original fill from the SVG tag
  let svgTagMatch = svg->Js.String2.match_(%re("/<svg[^>]*fill=\"([^\"]+)\"/"))
  let originalSvgFill = switch svgTagMatch {
  | Some([_, fill]) => Some(fill)
  | _ => None
  }
  
  fillColors->Belt.Array.forEach((fillColor: string) => {
    // Skip replacing "none" fills - keep them as literal fill="none"
    if fillColor !== "none" {
      // Determine prop name based on whether it matches SVG tag fill or is currentColor
      let propName = if fillColor->Js.String2.toLowerCase === "currentcolor" {
        "fill"
      } else {
        switch originalSvgFill {
        | Some(svgFill) when fillColor === svgFill => "fill"
        | _ => {
            // Increment counter for dynamic fills
            dynamicFillCounter := dynamicFillCounter.contents + 1
            "fill" ++ dynamicFillCounter.contents->Belt.Int.toString
          }
        }
      }
      
      // Escape special regex characters in the fill color
      let escapedColor = fillColor
        ->Js.String2.replaceByRe(%re("/[.*+?^${}()|[\]\\\\]/g"), "\\$&")
      
      // Create dynamic regex pattern and replace ALL occurrences EXCEPT in the SVG tag
      let regexPattern = "fill=\"" ++ escapedColor ++ "\""
      let replacement = propName === "fill" ? "fill" : "fill={" ++ propName ++ "}"
      
      // Split the SVG into the opening SVG tag and the rest
      let svgTagEndIndex = result.contents->Js.String2.indexOf(">")
      if svgTagEndIndex >= 0 {
        let svgTag = result.contents->Js.String2.substring(~from=0, ~to_=svgTagEndIndex + 1)
        let svgContent = result.contents->Js.String2.substringToEnd(~from=svgTagEndIndex + 1)
        
        // Only replace in the content, not in the SVG tag
        let tempContent = ref(svgContent)
        while tempContent.contents->Js.String2.includes(regexPattern) {
          tempContent := tempContent.contents->Js.String2.replace(regexPattern, replacement)
        }
        result := svgTag ++ tempContent.contents
      } else {
        // Fallback: replace all occurrences if we can't find the SVG tag end
        let tempResult = ref(result.contents)
        while tempResult.contents->Js.String2.includes(regexPattern) {
          tempResult := tempResult.contents->Js.String2.replace(regexPattern, replacement)
        }
        result := tempResult.contents
      }
    }
  })
  
  result.contents
}

let handleMultipleFills = (svg: string): string => {
  let fillColors = extractFillColors(svg)
  if Belt.Array.length(fillColors) > 0 {
    replaceFillsWithProps(svg, fillColors)
  } else {
    svg
  }
}

// New functions to handle multiple strokes
let extractStrokeColors = (svg: string): array<string> => {
  // Extract all stroke values from child elements (not the main SVG tag)
  
  // First, split the SVG into the opening tag and content
  let svgTagEndIndex = svg->Js.String2.indexOf(">")
  if svgTagEndIndex >= 0 {
    let svgContent = svg->Js.String2.substringToEnd(~from=svgTagEndIndex + 1)
    let strokeMatches = svgContent->Js.String2.match_(%re("/stroke=\"([^\"]+)\"/g"))
    
    switch strokeMatches {
    | None => []
    | Some(matches) => {
        let colors = matches
          ->Belt.Array.keepMap(match => {
            let colorMatch = match->Js.String2.match_(%re("/stroke=\"([^\"]+)\"/"))
            switch colorMatch {
            | Some([_, color]) => Some(color)
            | _ => None
            }
          })
          ->Belt.Array.reduce([], (acc, color) => {
            // Only add if not already in the array (deduplication)
            acc->Belt.Array.some(c => c === color) ? acc : acc->Belt.Array.concat([color])
          })
        colors
      }
    }
  } else {
    // Fallback: extract from entire SVG if we can't find the tag end
    let strokeMatches = svg->Js.String2.match_(%re("/stroke=\"([^\"]+)\"/g"))
    
    switch strokeMatches {
    | None => []
    | Some(matches) => {
        let colors = matches
          ->Belt.Array.keepMap(match => {
            let colorMatch = match->Js.String2.match_(%re("/stroke=\"([^\"]+)\"/"))
            switch colorMatch {
            | Some([_, color]) => Some(color)
            | _ => None
            }
          })
          ->Belt.Array.reduce([], (acc, color) => {
            // Only add if not already in the array (deduplication)
            acc->Belt.Array.some(c => c === color) ? acc : acc->Belt.Array.concat([color])
          })
        colors
      }
    }
  }
}

let replaceStrokesWithProps = (svg: string, strokeColors: array<string>): string => {
  let result = ref(svg)
  let dynamicStrokeCounter = ref(0)
  
  // Extract the original stroke from the SVG tag
  let svgTagMatch = svg->Js.String2.match_(%re("/<svg[^>]*stroke=\"([^\"]+)\"/"))
  let originalSvgStroke = switch svgTagMatch {
  | Some([_, stroke]) => Some(stroke)
  | _ => None
  }
  
  strokeColors->Belt.Array.forEach((strokeColor: string) => {
    // Skip replacing "none" strokes - keep them as literal stroke="none"
    if strokeColor !== "none" {
      // Determine prop name based on whether it matches SVG tag stroke or is currentColor
      let propName = if strokeColor->Js.String2.toLowerCase === "currentcolor" {
        "stroke"
      } else {
        switch originalSvgStroke {
        | Some(svgStroke) when strokeColor === svgStroke => "stroke"
        | _ => {
            // Increment counter for dynamic strokes
            dynamicStrokeCounter := dynamicStrokeCounter.contents + 1
            "stroke" ++ dynamicStrokeCounter.contents->Belt.Int.toString
          }
        }
      }
      
      // Escape special regex characters in the stroke color
      let escapedColor = strokeColor
        ->Js.String2.replaceByRe(%re("/[.*+?^${}()|[\]\\\\]/g"), "\\$&")
      
      // Create dynamic regex pattern and replace ALL occurrences EXCEPT in the SVG tag
      let regexPattern = "stroke=\"" ++ escapedColor ++ "\""
      let replacement = propName === "stroke" ? "stroke" : "stroke={" ++ propName ++ "}"
      
      // Split the SVG into the opening SVG tag and the rest
      let svgTagEndIndex = result.contents->Js.String2.indexOf(">")
      if svgTagEndIndex >= 0 {
        let svgTag = result.contents->Js.String2.substring(~from=0, ~to_=svgTagEndIndex + 1)
        let svgContent = result.contents->Js.String2.substringToEnd(~from=svgTagEndIndex + 1)
        
        // Only replace in the content, not in the SVG tag
        let tempContent = ref(svgContent)
        while tempContent.contents->Js.String2.includes(regexPattern) {
          tempContent := tempContent.contents->Js.String2.replace(regexPattern, replacement)
        }
        result := svgTag ++ tempContent.contents
      } else {
        // Fallback: replace all occurrences if we can't find the SVG tag end
        let tempResult = ref(result.contents)
        while tempResult.contents->Js.String2.includes(regexPattern) {
          tempResult := tempResult.contents->Js.String2.replace(regexPattern, replacement)
        }
        result := tempResult.contents
      }
    }
  })
  
  result.contents
}

let handleMultipleStrokes = (svg: string): string => {
  let strokeColors = extractStrokeColors(svg)
  if Belt.Array.length(strokeColors) > 0 {
    replaceStrokesWithProps(svg, strokeColors)
  } else {
    svg
  }
}
