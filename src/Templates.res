type svgOutput = string

let sep = ";\n"
let importReact = commonjs =>
  commonjs ? "const React = require('react')" : "import React from 'react'"
let \"export" = (svgOutput, ~fillColors, ~strokeColors, commonjs) => {
  // Filter out "none" fills and strokes since they stay as literals
  let dynamicFillColors = fillColors->Belt.Array.keep(color => color !== "none")
  let dynamicStrokeColors = strokeColors->Belt.Array.keep(color => color !== "none")
  
  let fillPropsCount = Belt.Array.length(dynamicFillColors)
  let strokePropsCount = Belt.Array.length(dynamicStrokeColors)
  
  let fillProps = if fillPropsCount == 0 {
    "fill"
  } else {
    let props = Belt.Array.mapWithIndex(dynamicFillColors, (index, _) => {
      "fill" ++ (index + 1)->Belt.Int.toString
    })
    "fill, " ++ props->Js.Array2.joinWith(", ")
  }
  
  let strokeProps = if strokePropsCount == 0 {
    "stroke"
  } else {
    let props = Belt.Array.mapWithIndex(dynamicStrokeColors, (index, _) => {
      "stroke" ++ (index + 1)->Belt.Int.toString
    })
    "stroke, " ++ props->Js.Array2.joinWith(", ")
  }
  
  let allProps = "width, height, " ++ fillProps ++ ", " ++ strokeProps ++ ", style"
  
  commonjs
    ? "module.exports = ({" ++ allProps ++ "}) => {\n  return (" ++ svgOutput ++ ");\n}"
    : "export default ({" ++ allProps ++ "}) => {\n  return (" ++ svgOutput ++ ");\n}"
}

let web = (svgOutput: string, ~fillColors, ~strokeColors, ~commonjs) =>
  importReact(commonjs) ++ (sep ++ (\"export"(svgOutput, ~fillColors, ~strokeColors, commonjs) ++ sep))

let importReactNativeSvg = commonjs =>
  commonjs
    ? j`const {
  default as Svg,
  Circle,
  ClipPath,
  Defs,
  Ellipse,
  ForeignObject,
  G,
  Image,
  Line,
  LinearGradient,
  Marker,
  Mask,
  Path,
  Pattern,
  Polygon,
  Polyline,
  RadialGradient,
  Rect,
  Stop,
  Symbol,
  Text,
  TextPath,
  TSpan as Tspan,
  Use,
} = require('react-native-svg')`
    : j`import Svg, {
  Circle,
  ClipPath,
  Defs,
  Ellipse,
  ForeignObject,
  G,
  Image,
  Line,
  LinearGradient,
  Marker,
  Mask,
  Path,
  Pattern,
  Polygon,
  Polyline,
  RadialGradient,
  Rect,
  Stop,
  Symbol,
  Text,
  TextPath,
  TSpan as Tspan,
  Use,
} from 'react-native-svg'`

let native = (svgOutput: string, ~fillColors, ~strokeColors, ~commonjs) =>
  importReact(commonjs) ++
  (sep ++
  (importReactNativeSvg(commonjs) ++ (sep ++ (\"export"(svgOutput, ~fillColors, ~strokeColors, commonjs) ++ sep))))

// Helper function to validate SVG and extract attributes
let validateAndExtractSvgAttributes = (svgContent: string) => {
  let fillMatch = svgContent->Js.String2.match_(%re("/<svg[^>]*fill=\"([^\"]+)\"/"))
  let strokeMatch = svgContent->Js.String2.match_(%re("/<svg[^>]*stroke=\"([^\"]+)\"/"))
  
  let hasFill = Belt.Option.isSome(fillMatch)
  let hasStroke = Belt.Option.isSome(strokeMatch)
  
  // Validation: SVG must have either fill or stroke
  // if !hasFill && !hasStroke {
  //   failwith("Error: SVG tag must have either 'fill' or 'stroke' attribute set. SVG: " ++ svgContent)
  // }
  
  let originalSvgFill = switch fillMatch {
  | Some([_, fill]) => Some(fill)
  | _ => None
  }
  
  let originalSvgStroke = switch strokeMatch {
  | Some([_, stroke]) => Some(stroke)
  | _ => None
  }
  
  (originalSvgFill, originalSvgStroke, hasFill, hasStroke)
}

// Helper function to determine which colors will use numbered props
let getNumberedFillColors = (fillColors: array<string>, svgContent: string) => {
  let (originalSvgFill, _, _, _) = validateAndExtractSvgAttributes(svgContent)
  
  fillColors
  ->Belt.Array.keep(color => color !== "none")
  ->Belt.Array.keepMap(color => {
    // Filter out colors that will use base props, but return color for numbered props
    if color->Js.String2.toLowerCase === "currentcolor" {
      None
    } else {
      switch originalSvgFill {
      | Some(svgFill) when color === svgFill => None
      | _ => Some(color)
      }
    }
  })
}

let getNumberedStrokeColors = (strokeColors: array<string>, svgContent: string) => {
  let (_, originalSvgStroke, _, _) = validateAndExtractSvgAttributes(svgContent)
  
  strokeColors
  ->Belt.Array.keep(color => color !== "none")
  ->Belt.Array.keepMap(color => {
    // Filter out colors that will use base props, but return color for numbered props
    if color->Js.String2.toLowerCase === "currentcolor" {
      None
    } else {
      switch originalSvgStroke {
      | Some(svgStroke) when color === svgStroke => None
      | _ => Some(color)
      }
    }
  })
}


let nativeForRescript = (svgOutput: string, ~fillColors, ~strokeColors) => {
  let output = {
    open AdjustSvg
    svgOutput
    ->transformReScriptNativeProps
    ->transformReScriptNativeSizeProps
    ->transformReScriptNativeMatrixProps
    ->transformReScriptNativeFixupDigits
    ->transformClassToClassName
    ->transformReservedKeywords
    ->transformStyleAttributes
  }
  
  // Only generate numbered props for colors that will actually be used as numbered props
  let numberedFillColors = getNumberedFillColors(fillColors, svgOutput)
  let numberedStrokeColors = getNumberedStrokeColors(strokeColors, svgOutput)
  let fillPropsCount = Belt.Array.length(numberedFillColors)
  let strokePropsCount = Belt.Array.length(numberedStrokeColors)
  
  let additionalFillProps = if fillPropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedFillColors, (index, _) => {
      let propName = "fill" ++ (index + 1)->Belt.Int.toString
      "  ~" ++ propName ++ ": option<string>=?,\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  let additionalStrokeProps = if strokePropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedStrokeColors, (index, _) => {
      let propName = "stroke" ++ (index + 1)->Belt.Int.toString
      "  ~" ++ propName ++ ": option<string>=?,\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  "open ReactNative\nopen ReactNativeSvg\n\n@react.component\nlet make = (\n  ~width: option<Style.size>=?,\n  ~height: option<Style.size>=?,\n  ~fill: option<string>=?,\n" ++ additionalFillProps ++ "  ~stroke: option<string>=?,\n" ++ additionalStrokeProps ++ "  ~className: option<string>=?,\n  ~style: option<Style.t>=?,\n) =>" ++ output ++ ";\n"
}

let nativeForRescriptWithDefaults = (svgOutput: string, ~fillColors, ~strokeColors, ~originalSvgFill, ~originalSvgStroke, ~isDeprecated) => {
  let output = {
    open AdjustSvg
    svgOutput
    ->transformReScriptNativeProps
    ->transformReScriptNativeSizeProps
    ->transformReScriptNativeMatrixProps
    ->transformReScriptNativeFixupDigits
    ->transformClassToClassName
    ->transformReservedKeywords
    ->transformStyleAttributes
    ->fixSvgAttributes(~isDeprecated=false)
  }
  
  // Only generate numbered props for colors that will actually be used as numbered props
  let numberedFillColors = getNumberedFillColors(fillColors, svgOutput)
  let numberedStrokeColors = getNumberedStrokeColors(strokeColors, svgOutput)
  let fillPropsCount = Belt.Array.length(numberedFillColors)
  let strokePropsCount = Belt.Array.length(numberedStrokeColors)
  
  // Use the actual SVG attribute values as defaults
  let fillDefault = switch originalSvgFill {
  | Some(value) => value
  | None => "none"
  }
  let strokeDefault = switch originalSvgStroke {
  | Some(value) => value
  | None => "currentColor"
  }
  
  let additionalFillProps = if fillPropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedFillColors, (index, color) => {
      let propName = "fill" ++ (index + 1)->Belt.Int.toString
      let defaultValue = if color !== "none" && color !== "white" {
        "currentColor"
      } else {
        "none"
      }
      "  ~" ++ propName ++ ": string=\"" ++ defaultValue ++ "\",\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  let additionalStrokeProps = if strokePropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedStrokeColors, (index, color) => {
      let propName = "stroke" ++ (index + 1)->Belt.Int.toString
      let defaultValue = if color !== "none" && color !== "white" {
        "currentColor"
      } else {
        "none"
      }
      "  ~" ++ propName ++ ": string=\"" ++ defaultValue ++ "\",\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  "open ReactNative\nopen ReactNativeSvg\n\n@react.component\nlet make = (\n  ~width: option<Style.size>=?,\n  ~height: option<Style.size>=?,\n  ~fill: string=\"" ++ fillDefault ++ "\",\n" ++ additionalFillProps ++ "  ~stroke: string=\"" ++ strokeDefault ++ "\",\n" ++ additionalStrokeProps ++ "  ~className: option<string>=?,\n  ~style: option<Style.t>=?,\n) =>" ++ output ++ ";\n"
}

let webForRescript = (svgOutput: string, ~fillColors, ~strokeColors) => {
  let output = {
    open AdjustSvg
    svgOutput
    ->transformClassToClassName
    ->transformReservedKeywords
    ->transformStyleAttributes
  }
  
  // Only generate numbered props for colors that will actually be used as numbered props
  let numberedFillColors = getNumberedFillColors(fillColors, svgOutput)
  let numberedStrokeColors = getNumberedStrokeColors(strokeColors, svgOutput)
  let fillPropsCount = Belt.Array.length(numberedFillColors)
  let strokePropsCount = Belt.Array.length(numberedStrokeColors)
  
  let additionalFillProps = if fillPropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedFillColors, (index, _) => {
      let propName = "fill" ++ (index + 1)->Belt.Int.toString
      "  ~" ++ propName ++ ": option<string>=?,\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  let additionalStrokeProps = if strokePropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedStrokeColors, (index, _) => {
      let propName = "stroke" ++ (index + 1)->Belt.Int.toString
      "  ~" ++ propName ++ ": option<string>=?,\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  "@react.component\nlet make = (\n  ~width: option<string>=?,\n  ~height: option<string>=?,\n  ~fill: option<string>=?,\n" ++ additionalFillProps ++ "  ~stroke: option<string>=?,\n" ++ additionalStrokeProps ++ "  ~className: option<string>=?,\n  ~style: option<Style.t>=?,\n) => " ++ output ++ ";\n"
}

let webForRescriptWithDefaults = (svgOutput: string, ~fillColors, ~strokeColors, ~originalSvgFill, ~originalSvgStroke, ~isDeprecated) => {
  let output = {
    open AdjustSvg
    svgOutput
    ->transformClassToClassName
    ->transformReservedKeywords
    ->transformStyleAttributes
    ->fixSvgAttributes(~isDeprecated=false)
  }
  
  // Only generate numbered props for colors that will actually be used as numbered props
  let numberedFillColors = getNumberedFillColors(fillColors, svgOutput)
  let numberedStrokeColors = getNumberedStrokeColors(strokeColors, svgOutput)
  let fillPropsCount = Belt.Array.length(numberedFillColors)
  let strokePropsCount = Belt.Array.length(numberedStrokeColors)
  
  // Use the actual SVG attribute values as defaults
  let fillDefault = switch originalSvgFill {
  | Some(value) => value
  | None => "none"
  }
  let strokeDefault = switch originalSvgStroke {
  | Some(value) => value
  | None => "currentColor"
  }
  
  let additionalFillProps = if fillPropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedFillColors, (index, color) => {
      let propName = "fill" ++ (index + 1)->Belt.Int.toString
      let defaultValue = if color !== "none" && color !== "white" {
        "currentColor"
      } else {
        "none"
      }
      "  ~" ++ propName ++ ": string=\"" ++ defaultValue ++ "\",\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  let additionalStrokeProps = if strokePropsCount == 0 {
    ""
  } else {
    let props = Belt.Array.mapWithIndex(numberedStrokeColors, (index, color) => {
      let propName = "stroke" ++ (index + 1)->Belt.Int.toString
      let defaultValue = if color !== "none" && color !== "white" {
        "currentColor"
      } else {
        "none"
      }
      "  ~" ++ propName ++ ": string=\"" ++ defaultValue ++ "\",\n"
    })
    props->Js.Array2.joinWith("")
  }
  
  "@react.component\nlet make = (\n  ~width: option<string>=?,\n  ~height: option<string>=?,\n  ~fill: string=\"" ++ fillDefault ++ "\",\n" ++ additionalFillProps ++ "  ~stroke: string=\"" ++ strokeDefault ++ "\",\n" ++ additionalStrokeProps ++ "  ~className: option<string>=?,\n  ~style: option<Style.t>=?,\n) => " ++ output ++ ";\n"
}

// Helper function to generate fill props for templates
let generateFillPropsSignature = (fillColors: array<string>, ~forReScript: bool) => {
  let fillPropsCount = Belt.Array.length(fillColors)
  if fillPropsCount === 0 {
    if forReScript { "~fill: option<string>=?," } else { "fill" }
  } else if fillPropsCount === 1 {
    if forReScript { "~fill: option<string>=?," } else { "fill" }
  } else {
    // Multiple fills: fill1, fill2, etc.
    let props = Belt.Array.mapWithIndex(fillColors, (index, _) => {
      let propName = "fill" ++ (index + 1)->Belt.Int.toString
      if forReScript {
        "~" ++ propName ++ ": option<string>=?,"
      } else {
        propName
      }
    })
    if forReScript {
      props->Js.Array2.joinWith(" ")
    } else {
      props->Js.Array2.joinWith(", ")
    }
  }
}

let generateFillPropsUsage = (fillColors: array<string>) => {
  let fillPropsCount = Belt.Array.length(fillColors)
  if fillPropsCount === 0 {
    "fill"
  } else if fillPropsCount === 1 {
    "fill"
  } else {
    // Multiple fills: fill1, fill2, etc.
    let props = Belt.Array.mapWithIndex(fillColors, (index, _) => {
      "fill" ++ (index + 1)->Belt.Int.toString
    })
    props->Js.Array2.joinWith(", ")
  }
}
