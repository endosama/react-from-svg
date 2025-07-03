open Node
open Belt

@module("mkdirp") external mkdirpSync: string => unit = "sync"

let root = Process.cwd()
let sep = Path.sep

type file = {
  name: string,
  content: string,
}

let get = (globPattern: string) =>
  globPattern
  ->Glob.glob
  ->Future.mapOk(SortArray.String.stableSort)
  ->Future.mapOk(Array.reverse)
  ->Future.mapOk(files =>
    files->Array.map(item => {name: item, content: Fs.readFileAsUtf8Sync(item)})
  )

let shortenFilenames = (sourcePath, files) =>
  files->Array.map(file => {
    ...file,
    name: (file.name |> Js.String.replace(sourcePath ++ "/", "") |> Js.String.replace(".svg", ""))
      ->Case.toPascal,
  })

// Check if an icon is deprecated based on filename/path
let isDeprecatedIcon = (filePath: string) =>
  filePath->Js.String2.toLowerCase->Js.String2.includes("deprecated")

let noop = s => s

let transformSvg = (svg, ~removeFill, ~removeStroke, ~pascalCaseTag, ~js, ~template) => {
  open AdjustSvg
  svg
  ->cleanupStart
  ->prepareSvgProps
  ->(js ? injectSvgJsProps : injectSvgReScriptProps)
  ->dashToCamelCaseProps
  ->(pascalCaseTag ? tagToPascalCase : noop)
  ->(js ? cleanupEndWithoutSpace : cleanupEndWithSpace)
  ->(removeFill ? deleteFill : handleMultipleFills)
  ->(removeStroke ? deleteStroke : handleMultipleStrokes)
  ->template
}

let transformFiles = (
  files,
  ~withNative,
  ~withWeb,
  ~withNativeForRescript,
  ~withWebForRescript,
  ~removeFill,
  ~removeStroke,
  ~commonjs,
) =>
  files->Array.reduce([], (files, file) => {
    let fillColors = AdjustSvg.extractFillColors(file.content)
    let strokeColors = AdjustSvg.extractStrokeColors(file.content)
    
    // Extract original SVG tag attributes before any transformations
    let (originalSvgFill, originalSvgStroke, _hasFill, _hasStroke) = Templates.validateAndExtractSvgAttributes(file.content)
    
    Js.log2("Fill colors", fillColors)
    Js.log2("Stroke colors", strokeColors)
    let trsf = file.content->transformSvg(~removeFill, ~removeStroke)
    let trsfNative = () =>
      trsf(~js=true, ~pascalCaseTag=true, ~template=Templates.native(~fillColors, ~strokeColors, ~commonjs))
    let trsfWeb = () => trsf(~js=true, ~pascalCaseTag=false, ~template=Templates.web(~fillColors, ~strokeColors, ~commonjs))
    
    // Check if this is a deprecated icon
    let isDeprecated = isDeprecatedIcon(file.name)
    
    let trsfNativeForR = () =>
      trsf(~js=false, ~pascalCaseTag=true, ~template=Templates.nativeForRescriptWithDefaults(~fillColors, ~strokeColors, ~originalSvgFill, ~originalSvgStroke, ~isDeprecated))
    let trsfWebForR = () =>
      trsf(~js=false, ~pascalCaseTag=false, ~template=Templates.webForRescriptWithDefaults(~fillColors, ~strokeColors, ~originalSvgFill, ~originalSvgStroke, ~isDeprecated))
      
    switch (withNative, withWeb, withNativeForRescript, withWebForRescript) {
    | (false, false, false, false) => files
    | (true, false, false, false) =>
      files->Array.concat([{name: file.name ++ ".js", content: trsfNative()}])
    | (false, true, false, false) =>
      files->Array.concat([{name: file.name ++ ".js", content: trsfWeb()}])
    | (true, true, false, false) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfNative()},
        {name: file.name ++ ".web.js", content: trsfWeb()},
      ])
    | (false, false, true, false) =>
      files->Array.concat([{name: file.name ++ ".res", content: trsfNativeForR()}])
    | (false, false, false, true) =>
      files->Array.concat([{name: file.name ++ ".res", content: trsfWebForR()}])
    | (true, true, true, false) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfNative()},
        {name: file.name ++ ".web.js", content: trsfWeb()},
        {name: file.name ++ ".res", content: trsfNativeForR()},
      ])
    | (true, true, false, true) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfNative()},
        {name: file.name ++ ".web.js", content: trsfWeb()},
        {name: file.name ++ ".res", content: trsfWebForR()},
      ])
    | (true, false, true, false) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfNative()},
        {name: file.name ++ ".res", content: trsfNativeForR()},
      ])
    | (true, false, false, true) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfNative()},
        {name: file.name ++ ".res", content: trsfWebForR()},
      ])
    | (false, true, true, false) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfWeb()},
        {name: file.name ++ ".res", content: trsfNativeForR()},
      ])
    | (false, true, false, true) =>
      files->Array.concat([
        {name: file.name ++ ".js", content: trsfWeb()},
        {name: file.name ++ ".res", content: trsfWebForR()},
      ])
    | (_, _, true, true) =>
      failwith(
        "For now --with-native-for-rescript & --with-web-for-rescript cannot be used at the same time.",
      )
    }
  })

let write = (outputPath, files) => {
  files->Array.forEach(file => {
    let filename = Path.join([outputPath, "SVG" ++ file.name])
    mkdirpSync(Path.dirname(filename))
    Fs.writeFileAsUtf8Sync(filename, file.content)
  })
  files
}

type flags = {
  withNative: Js.Undefined.t<bool>,
  withWeb: Js.Undefined.t<bool>,
  withNativeForRescript: Js.Undefined.t<bool>,
  withWebForRescript: Js.Undefined.t<bool>,
  removeFill: Js.Undefined.t<bool>,
  removeStroke: Js.Undefined.t<bool>,
  commonjs: Js.Undefined.t<bool>,
}

let make = ((sourcePath, outputPath), flags) => {
  let futureFiles =
    Path.join([sourcePath, "*.svg"])
    ->get
    ->Future.map(files => files->Result.getExn)
    ->Future.tap(files => Js.log2("Files found", files->Array.length))
    ->Future.map(shortenFilenames(sourcePath))
    ->Future.map(files =>
      files->transformFiles(
        ~withNative=flags.withNative->Js.Undefined.toOption->Option.getWithDefault(false),
        ~withWeb=flags.withWeb->Js.Undefined.toOption->Option.getWithDefault(false),
        ~withNativeForRescript=flags.withNativeForRescript
        ->Js.Undefined.toOption
        ->Option.getWithDefault(false),
        ~withWebForRescript=flags.withWebForRescript
        ->Js.Undefined.toOption
        ->Option.getWithDefault(false),
        ~removeFill=flags.removeFill->Js.Undefined.toOption->Option.getWithDefault(false),
        ~removeStroke=flags.removeStroke->Js.Undefined.toOption->Option.getWithDefault(false),
        ~commonjs=flags.commonjs->Js.Undefined.toOption->Option.getWithDefault(false),
      )
    )
    ->Future.tap(files => Js.log2("Files transformed banan", files->Array.length))
    ->Future.map(write(outputPath))
    ->Future.tap(files => Js.log2("Files written", files->Array.length))

  futureFiles
}
