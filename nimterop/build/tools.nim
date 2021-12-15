import strformat, strutils

import os except findExe

import ".."/globals
import "."/[misc, shell]

proc configure*(path, check: string, flags = "") =
  ## Run the GNU `configure` command to generate all Makefiles or other
  ## build scripts in the specified path
  ##
  ## If a `configure` script is not present and an `autogen.sh` script
  ## is present, it will be run before attempting `configure`.
  ##
  ## Next, if `configure.ac` or `configure.in` exist, `autoreconf` will
  ## be executed.
  ##
  ## `check` is a file that will be generated by the `configure` command.
  ## This is required to prevent configure from running on every build. It
  ## is relative to the `path` and should not be an absolute path.
  ##
  ## `flags` are any flags that should be passed to the `configure` command.
  if (path / check).fileExists():
    return

  gecho "# Configuring " & path

  if not fileExists(path / "configure"):
    for i in @["autogen.sh", "build" / "autogen.sh"]:
      if fileExists(path / i):
        gecho "#   Running autogen.sh"

        when defined(unix):
          decho execAction(
            &"cd {(path / i).parentDir().sanitizePath} && ./autogen.sh").output
        else:
          decho execAction(
            &"cd {(path / i).parentDir().sanitizePath} && bash ./autogen.sh").output

        break

  if not fileExists(path / "configure"):
    for i in @["configure.ac", "configure.in"]:
      if fileExists(path / i):
        gecho "#   Running autoreconf"

        decho execAction(&"cd {path.sanitizePath} && autoreconf -fi").output

        break

  if fileExists(path / "configure"):
    gecho "#   Running configure " & flags

    when defined(unix):
      var
        cmd = &"cd {path.sanitizePath} && ./configure"
    else:
      var
        cmd = &"cd {path.sanitizePath} && bash ./configure"
    if flags.len != 0:
      cmd &= &" {flags}"

    decho execAction(cmd).output

  doAssert (path / check).fileExists(), "Configure failed"

proc getCmakePropertyStr(name, property, value: string): string =
  &"\nset_target_properties({name} PROPERTIES {property} \"{value}\")\n"

proc getCmakeIncludePath*(paths: openArray[string]): string =
  ## Create a `cmake` flag to specify custom include paths
  ##
  ## Result can be included in the `flag` parameter for `cmake()` or
  ## the `cmakeFlags` parameter for `getHeader()`.
  for path in paths:
    result &= path & ";"
  result = " -DCMAKE_INCLUDE_PATH=" & result[0 .. ^2].sanitizePath(sep = "/")

proc setCmakeProperty*(outdir, name, property, value: string) =
  ## Set a `cmake` property in `outdir / CMakeLists.txt` - usable in the `xxxPreBuild` hook
  ## for `getHeader()`
  ##
  ## `set_target_properties(name PROPERTIES property "value")`
  let
    cm = outdir / "CMakeLists.txt"
  if cm.fileExists():
    cm.writeFile(
      cm.readFile() & getCmakePropertyStr(name, property, value)
    )

proc setCmakeLibName*(outdir, name, prefix = "", oname = "", suffix = "") =
  ## Set a `cmake` property in `outdir / CMakeLists.txt` to specify a custom library output
  ## name - usable in the `xxxPreBuild` hook for `getHeader()`
  ##
  ## `prefix` is typically `lib`
  ## `oname` is the library name
  ## `suffix` is typically `.a`
  ##
  ## Sometimes, `cmake` generates non-standard library names - e.g. zlib compiles to
  ## `libzlibstatic.a` on Windows. This proc can help rename it to `libzlib.a` so that `getHeader()`
  ## can find it after the library is compiled.
  ##
  ## ```
  ## set_target_properties(name PROPERTIES PREFIX "prefix")
  ## set_target_properties(name PROPERTIES OUTPUT_NAME "oname")
  ## set_target_properties(name PROPERTIES SUFFIX "suffix")
  ## ```
  let
    cm = outdir / "CMakeLists.txt"
  if cm.fileExists():
    var
      str = ""
    if prefix.len != 0:
      str &= getCmakePropertyStr(name, "PREFIX", prefix)
    if oname.len != 0:
      str &= getCmakePropertyStr(name, "OUTPUT_NAME", oname)
    if suffix.len != 0:
      str &= getCmakePropertyStr(name, "SUFFIX", suffix)
    if str.len != 0:
      cm.writeFile(cm.readFile() & str)

proc setCmakePositionIndependentCode*(outdir: string) =
  ## Set a `cmake` directive to create libraries with -fPIC enabled
  let
    cm = outdir / "CMakeLists.txt"
  if cm.fileExists():
    let
      pic = "set(CMAKE_POSITION_INDEPENDENT_CODE ON)"
      cmd = cm.readFile()
    if not cmd.contains(pic):
      cm.writeFile(
        pic & "\n" & cmd
      )

proc cmake*(path, check, flags: string) =
  ## Run the `cmake` command to generate all Makefiles or other
  ## build scripts in the specified path
  ##
  ## `path` will be created since typically `cmake` is run in an
  ## empty directory.
  ##
  ## `check` is a file that will be generated by the `cmake` command.
  ## This is required to prevent `cmake` from running on every build. It
  ## is relative to the `path` and should not be an absolute path.
  ##
  ## `flags` are any flags that should be passed to the `cmake` command.
  ## Unlike `configure`, it is required since typically it will be the
  ## path to the repository, typically `..` when `path` is a subdir.
  if (path / check).fileExists():
    return

  gecho "# Running cmake " & flags
  gecho "#   Path: " & path

  mkDir(path)

  let
    cmd = &"cd {path.sanitizePath} && cmake {flags}"

  decho execAction(cmd).output

  doAssert (path / check).fileExists(), "cmake failed"

proc make*(path, check: string, flags = "", regex = false) =
  ## Run the `make` command to build all binaries in the specified path
  ##
  ## `check` is a file that will be generated by the `make` command.
  ## This is required to prevent `make` from running on every build. It
  ## is relative to the `path` and should not be an absolute path.
  ##
  ## `flags` are any flags that should be passed to the `make` command.
  ##
  ## `regex` can be set to true if `check` is a regular expression.
  ##
  ## If `make.exe` is missing and `mingw32-make.exe` is available, it will
  ## be copied over to make.exe in the same location.
  if findFile(check, path, regex = regex).len != 0:
    return

  gecho "# Running make " & flags
  gecho "#   Path: " & path

  var
    cmd = findExe("make")

  if cmd.len == 0:
    cmd = findExe("mingw32-make")
    if cmd.len != 0:
      cpFile(cmd, cmd.replace("mingw32-make", "make"))
  doAssert cmd.len != 0, "Make not found"

  cmd = &"cd {path.sanitizePath} && make -j {getNumProcs()}"
  if flags.len != 0:
    cmd &= &" {flags}"

  decho execAction(cmd).output

  doAssert findFile(check, path, regex = regex).len != 0, "make failed"

proc buildWithCmake*(outdir, flags: string): BuildStatus =
  if not fileExists(outdir / "Makefile"):
    if fileExists(outdir / "CMakeLists.txt"):
      if findExe("cmake").len != 0:
        var
          gen = ""
        when defined(Windows):
          if findExe("sh").len != 0:
            let
              uname = execAction("sh -c uname -a").output.toLowerAscii()
            if existsEnv("MSYSTEM") or uname.contains("msys"):
              gen = "MSYS Makefiles".quoteShell
            elif uname.contains("mingw"):
              gen = "MinGW Makefiles".quoteShell & " -DCMAKE_SH=\"CMAKE_SH-NOTFOUND\""
            else:
              gecho "Unsupported system: " & uname
          else:
            gen = "MinGW Makefiles".quoteShell
        else:
          gen = "Unix Makefiles".quoteShell
        if findExe("ccache").len != 0:
          gen &= " -DCMAKE_C_COMPILER_LAUNCHER=ccache -DCMAKE_CXX_COMPILER_LAUNCHER=ccache"
        result.buildPath = outdir / "buildcache"
        cmake(result.buildPath, "Makefile", &".. -G {gen} {flags}")
        result.built = true
      else:
        result.error = "cmake capable but cmake executable missing"
  else:
    result.buildPath = outdir

proc buildWithAutoConf*(outdir, flags: string): BuildStatus =
  if not fileExists(outdir / "Makefile"):
    if findExe("bash").len != 0:
      for file in @["configure", "configure.ac", "configure.in", "autogen.sh", "build/autogen.sh"]:
        if fileExists(outdir / file):
          configure(outdir, "Makefile", flags)
          result.buildPath = outdir
          result.built = true
          break
    else:
      result.error = "configure capable but bash executable missing"
  else:
    result.buildPath = outdir

proc flagBuild*(base: string, flags: openArray[string]): string =
  ## Simple helper proc to generate flags for `configure`, `cmake`, etc.
  ##
  ## Every entry in `flags` is replaced into the `base` string and
  ## concatenated to the result.
  ##
  ## E.g.
  ##   `base = "--disable-$#"`
  ##   `flags = @["one", "two"]`
  ##
  ## `flagBuild(base, flags) => " --disable-one --disable-two"`
  for i in flags:
    result &= " " & base % i
